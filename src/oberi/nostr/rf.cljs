(ns oberi.nostr.rf
  (:require
   [oberi.nostr.relay :as r]
   [clojure.set :refer [union]]   
   [re-frame.core :as re-frame :refer [reg-event-db reg-event-fx reg-sub reg-fx reg-cofx path subscribe
                                       inject-cofx]]
   [cljs.core.async :as async :refer [<! >! put! chan close! mult tap]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [cljs.pprint :refer [pprint print-table]]
   [loom.graph :as g]
   [loom.alg :as alg]
   [oberi.nostr.nostr :as nostr :refer [author-req-msg project-note truncate-string parse-contact-list
                                        parse-relay-list metadata-req-msg parse-tags-element parse-notes-tags
                                        ->CloseRequest ->QueryRequest ->Subscription  wire keywordize-map-keys encode-hex]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

;; ========== SETUP ============================================================
(def <sub (comp deref re-frame/subscribe))
(def <sub-lazy re-frame/subscribe)
(def >evt re-frame/dispatch)
(def >evt-now re-frame/dispatch-sync)

(def default-db
  {:notes {}
   :note-graph (g/digraph)
   :login-state :logged-out
   :current-view [:global-feed]
   :reactions {:registered #{}}
   :perp-subscriptions {}
   :mentions {}   
   :relays {"wss://nostr.bitcoiner.social" {:read true :write true}
            "wss://nostr.slothy.win" {:read true :write true}
            "wss://relay.damus.io" {:read true :write true}
            "wss://nostr.v0l.io" {:read true :write true}
            "wss://nostr.onsats.org" {:read true :write true}
            "wss://nostr.drss.io" {:read true :write true}}})

(def relay-evt-ch (chan))
(def relay-out-ch (chan))
(def relay-out-mult (mult relay-out-ch))

(defn cl-print [x] (doto x (print)))

;; ========== EFFECTS ==========================================================
(reg-event-fx
 ::boot
 (fn [_ _]
   (pprint "boot")
   {:db default-db}))

(reg-fx
 :connect-relay
(fn [[relay rw-map]]
  (pprint (str "Connecting to relay " relay " rw " rw-map))
  (if-not (or (:read rw-map) (:write rw-map))
    (pprint (str "Relay " relay " is r/w false"))
    (if (r/is-connected? relay)
      (pprint (str "Already connected to " relay))
      (r/connect! relay (select-keys rw-map [:read :write]))))))
     
;; Creating events for various kinds of event message.
(defn handle-event [subs-id evt relay]  
  (let [kind (:kind evt)]
    (case kind
      7 (>evt [:reactions-msg evt])
      6 (>evt [:repost-msg evt])
      3 (>evt [:contacts-msg evt])
      2 (>evt [:recommend-relay-msg evt])
      1 (>evt [:text-note evt])
      0 (>evt [:metadata-msg evt])          
      (pprint (str "Ignoring event " evt))))
  ;; close any one shot subscriptions
  ;; (>evt [:end-of-stored-events subs-id relay])
  )

(defn handle-eose [subs-id relay]
  ;; TODO store time of last update for future reference
  (>evt [:end-of-stored-events subs-id relay]))

(defn handle-message [msg relay]
  (let [type (first msg)]
    (case type
      "EVENT" (handle-event (second msg) (nth msg 2) relay)
      "EOSE" (handle-eose (second msg) relay)
      nil
      ;; (pprint {:unhandled msg :relay relay})
      )))

(defn setup-listener [ws-channel relay]
  (go-loop []
    (let [{:keys [message error]} (<! ws-channel)]
      (if (or error (nil? message))
        (do (pprint (if error (str "Relay listener error: " error) (str "nil message on channel for " relay)))
            (close! ws-channel)
            (r/disconnect! relay)
            (>evt [:relay-disconnected relay]))
        (do (handle-message message relay)
            (recur))))))
 
(reg-event-fx
 :relay-connected
 (fn [cofx [_ relay]]
   (pprint (str "Relay connected " relay))
   (let [db (assoc-in (:db cofx) [:relays relay :state] :connected)
         relay-meta (r/get-relay relay)
         channel (:channel relay-meta)
         rw-map (:rw-map relay-meta)]

     (if (:read rw-map)
       (do
         (tap relay-out-mult channel) ;; this mult/tap is for queries out (READ)
         (setup-listener channel relay)
         {:db db 
          :dispatch [:relay-read-active relay]})
       {:db db}))))

(reg-event-fx
 :relay-read-active
 (fn [cofx [_ relay]]
   (let [db (:db cofx)
         ;; use the global feed for global view, otherwise the contacts feed
         feed (or (#{(first (:current-view db))} :global-feed) :contacts-feed)
         feed-query (get-in db [:perp-subscriptions feed])
         priv-query (get-in db [:perp-subscriptions :private-messages])
         contact-list-query (get-in db [:perp-subscriptions :contacts-list-contacts])]
     {:db (-> (assoc-in db [:relays relay :subscriptions (-> feed-query :subscription :id)] (:subscription feed-query))
              (assoc-in [:relays relay :subscriptions (-> priv-query :subscription :id)] (:subscription priv-query))
              (assoc-in [:relays relay :subscriptions (-> contact-list-query :subscription :id)] (:subscription contact-list-query)))
      :fx [[:send-to-relay [relay (nostr/wire feed-query)]]
           [:send-to-relay [relay (nostr/wire priv-query)]]
           [:send-to-relay [relay (nostr/wire contact-list-query)]]]})))

(reg-event-fx
 :relay-disconnected
 (fn [cofx [_ relay]]
   (let [db (:db cofx)]
   (pprint (str "Relay disconnected " relay))   
   {:db (assoc-in db [:relays relay :state] :disconnected)
    :connect-relay [relay (get-in db [:contacts (:user-pubkey db) :relays relay])]})))

;; === message out functions

(defn send-to-relay [relay msg]
  (let [ {:keys [state rw-map channel]} (r/get-relay relay)]
    (if (or (= :disconnected state) (nil? channel))
      (pprint ["Send error" {:state state :channel channel :relay relay :msg (subs (str (js->clj msg)) 0 40)}]) 
      (go (>! channel msg)))))

(reg-fx
 :send-to-relay
 (fn [[relay msg]]
   (send-to-relay relay msg)))

(reg-event-fx
 :end-of-stored-events
 [(inject-cofx :time-now)]
 (fn [cofx [_ subscription-id relay]]
   (let [db (:db cofx)
         subscription (get-in db [:relays relay :subscriptions subscription-id])]
     (if-not (:perpetual subscription)
       {:db (assoc-in db [:relays relay :subscriptions subscription-id :complete] (:time-now cofx))
        :close-subscription [relay subscription]}))))

(reg-fx
 :close-subscription
 (fn [[relay subscription]]
   ;; (pprint {:close-sub subscription :relay relay})
   {:send-to-relay [relay (wire (->CloseRequest subscription))]}))

;; init functions =====
(reg-event-fx
 :load-relays
 [(inject-cofx :local-store-relay-list)]
 (fn [coef [_ _]]
   (let [db (:db coef)
         relays-unkw (:relay-list coef)
         relays (zipmap (keys relays-unkw) (map keywordize-map-keys (vals relays-unkw)))]     
     (if (empty? relays) ;; then bootstrap with default list
       {:fx (mapv #(vector :connect-relay %) (:relays db))}
       {:fx (mapv #(vector :connect-relay %) relays)}))))

(reg-event-fx
 :connect-extension
 (fn [coef _]
   {:promise {:call #(.getPublicKey (.-nostr js/window))
              :on-success [:logged-in-with-pubkey]}}))

(reg-event-db
 :start-login
 (fn [db _]
   (assoc db :login-state :logging-in)))

(reg-event-fx
 :logged-in-with-pubkey
 (fn [cofx [_ pubkey]]
   {:db (-> (:db cofx)
            (assoc :user-pubkey pubkey)
            (assoc :login-state :logged-in))
    :fx [[:dispatch [:load-relays]]
         [:dispatch [:init-subscriptions]]]}))

(defn contact-feed-req [pubkey contact-list]
  (nostr/->QueryRequest (nostr/->Subscription (str "cntfd" pubkey) true)
                  {:authors (conj (mapv :pubkey contact-list) pubkey)
                   :kinds [0 1 2 6 7] :limit 150}))

(defn contact-list-req [pubkey contact-list]
  (nostr/->QueryRequest (nostr/->Subscription (str "cnt" pubkey) true)
                  {:authors (conj (mapv :pubkey contact-list) pubkey)
                   :kinds [3] :limit (count contact-list)}))

(defn private-msg-req [pubkey]
  (nostr/->QueryRequest (nostr/->Subscription (str "prv" pubkey) true)
                        {:kinds [4] :#p [pubkey] :limit 100}))

(reg-event-fx
 :init-subscriptions
 [(inject-cofx :local-store-contact-list)]
 (fn [cofx _]
   ;; Elementary subscriptions
   ;;
   ;; All kinds - reg mode - user's contact list - perpetual
   ;;        - anything missing - oneshot - from relay
   ;;
   ;; All kinds - global mode - no author filter - perpetual - start from 5 mins ago - close when exiting view
   ;;
   ;; User metadata - pre-fetch user's contact list - perpetual
   ;;     - fetch other users's data if not present - oneshot
   ;; User notes
   ;;    - in normal mode - user's contact list - perpetual
   ;;        - anything missing - oneshot
   ;;    - in global mode - everything - perpetual
   (let [db (:db cofx)
         pubkey (:user-pubkey db)
         relays (:relays db)
         contact-list (:contact-list cofx)]
     (let [global-all-msgs (->QueryRequest (->Subscription (str "glb" pubkey) true)
                                           {:limit 50
                                            :kinds [0 1 2 6 7]})]       
       {:db (-> (assoc-in db [:contacts pubkey :list] contact-list)
                (assoc :perp-subscriptions {:contacts-feed (contact-feed-req pubkey contact-list)
                                            :private-messages (private-msg-req pubkey)
                                            :global-feed global-all-msgs
                                            :contacts-list-contacts (contact-list-req pubkey contact-list)}))}))))

;; === event recv handling

(defn find-edges [note tags]
  ;; list of edges (from origin 
  (for [link (remove nil? (conj (:links tags) (:root-id tags) (:reply-id tags)))]
    [link (:id note)]))
  
;; (defn metadata-queries
;;   [relays pubkey-list]
;;   (vec (for [relay relays]
;;          [:dispatch [:send-to-relay relay (wire (->QueryRequest (->Subscription (mt))))))))))
    
(defn metadata-query [pubkey]
  (->QueryRequest (->Subscription (str "meta" pubkey) false) {:authors [pubkey] :kinds [0]}))

(defn update-in-if-true [db flag ks v]
  (if flag (update-in db ks #(if (nil? %) v %)) db))

(reg-event-fx
 :text-note 
 (fn [coef [_ note]]
   (let [db (:db coef)
         notes (:notes db)
         note-id (:id note)]
     (if-not (contains? notes note-id)
       (let [graph (:note-graph db)
             pubkey (:pubkey note)
             tags (parse-notes-tags (:tags note))
             ;; query relays if pubkey is missing
             query (metadata-query pubkey)
             relays-to-query (if (contains? (:metadata db) pubkey) '()
                                 (map first (remove #(get-in (second %) [:subscriptions (-> query :subscription :id)]) (:relays db))))
             mention? (some #(= (:user-pubkey db) (first %)) (:replying tags))]
         ;; assoc the subscriptions into each relay
         {:db (-> (reduce #(assoc-in %1 [:relays %2 :subscriptions (-> query :subscription :id)]
                                     (-> query :subscription))
                          db relays-to-query)
                  (assoc-in [:notes (:id note)] (-> (assoc note :replying (:replying tags))                                                    
                                                    (merge (select-keys tags [:child-id :child-relay]))))
                  (assoc-in [:reactions (:id note)] {:likes #{} :dislikes #{}})
                  (assoc :note-graph (-> (apply (partial g/add-edges graph) (find-edges note tags))
                                         (g/add-nodes (:id note))))
                  (update-in-if-true mention? [:mentions (:id note)] {:seen nil}))
          :fx (nostr/send-to-relay-fx query relays-to-query)})))))

(reg-event-db
 :metadata-msg
 (fn [db [_ meta-msg]]
   (let [content (js->clj (.parse js/JSON (:content meta-msg)) :keywordize-keys true)      
         meta {:pubkey (:pubkey meta-msg)
               :tags (:tags meta-msg)
               :name (:name content)
               :picture (:picture content)
               :created-at (:created_at meta-msg)}]                                            
     ;; update pubkey metadata if message is newer, remove pubkey from pending queries since the query has been done
     (update-in db [:metadata (:pubkey meta)] #(if (or (nil? (:created-at %)) (> (:created-at meta) (:created-at %))) meta %)))))

(defn select-new-relays [db relay-list]
  "Returns a vector of :connect-relay effects containing any newly added relays"
  (let [current-relays (into #{} (keys (:relays db)))]
    (reduce (fn [acc [name rw-map]]
              (if (current-relays name)
                acc
                (conj acc (vector :connect-relay [name rw-map]))))
            [] relay-list)))

(reg-event-fx
 :contacts-msg
 (fn [coef [_ contacts-msg]]
   (let [db (:db coef)
         pubkey (:pubkey contacts-msg)
         created-at (:created_at contacts-msg)]
     (if (> created-at (or (get-in db [:contacts pubkey :created-at]) 0))
       ;; update stored contact list if recvd one is newer
       (let [contact-list (parse-contact-list (:tags contacts-msg))
             relay-list (parse-relay-list (:content contacts-msg))
             updated-db (-> db
                            (assoc-in [:contacts pubkey :created-at] created-at)
                            (assoc-in [:contacts pubkey :list] contact-list)
                            (assoc-in [:contacts pubkey :relays] relay-list))]
         ;; if it is the user's contact list, store updated contact list and update perp query          
         (if (= pubkey (:user-pubkey db))
           (let [contact-feed-query (contact-feed-req pubkey contact-list)
                 contact-list-query (contact-list-req pubkey contact-list)
                 relays (keys (:relays db))]             
             {:db (-> (assoc-in updated-db [:perp-subscriptions :contacts-feed] contact-feed-query)
                      (assoc-in [:perp-subscriptions :contacts-list-contacts] contact-list-query) )
              :fx (-> (select-new-relays db relay-list)
                      (conj [:store-user-contact-list [pubkey contact-list]])
                      (conj [:store-relay-list [pubkey relay-list]])
                      (into (nostr/send-to-relay-fx contact-feed-query relays))
                      (into (nostr/send-to-relay-fx contact-list-query relays)))})
           {:db updated-db}))))))

(reg-event-db
 :reactions-msg
 (fn [db [_ reactions-msg]]
   (let [reaction (:content reactions-msg)
         pubkey (:pubkey reactions-msg)
         tags (parse-tags-element (:tags reactions-msg))
         note-id (:e tags)
         pubkey-note (:p tags)]
     (if (and pubkey note-id (not (contains? (:registered (:reactions db)) (:id reactions-msg))))
       (let [new-db (-> (case reaction
                          "+" (update-in db [:reactions note-id :likes] #(union #{pubkey} %))
                          "-" (update-in db [:reactions note-id :dislikes] #(union #{pubkey} %))
                          (update-in db [:reactions note-id :other reaction] #(union #{pubkey} %)))
                        (update-in [:reactions :registered] #(conj % (:id reactions-msg))))]
         (if (= pubkey-note (db :user-pubkey))
           (assoc-in new-db [:mentions note-id] {:seen nil})
           new-db))))))

(reg-event-db
 :repost-msg
 (fn [db [_ repost-msg]]
   nil ;; TODO
   ))

(reg-event-db
 :recommended-relay-msg
 (fn [db [_ rec-msg]]
   (pprint {:recommended-relays rec-msg})))

(defn assoc-subscription-to-relays
  [db query]
  (reduce #(assoc-in %1 [:relays %2 :subscriptions (-> query :subscription :id)] (:subscription query)) db (keys (:relays db))))

(defn dissoc-subscription-to-relays
  [db query]
  ;;TODO tidy up!
  )

;; other functions ====
(defn note-fetch-db-fx [db note-id]
  (let [notes (:notes db)
        relays-to-query (keys (:relays db))]
    (if-let [note (get notes note-id)]             
      ;; request the linked notes - if they are not already in app-db
      (let [tags (parse-notes-tags (:tags note))
            notes-to-fetch (remove #(or (nil? %) (contains? notes %)) (conj (:links tags) (:root-id tags) (:reply-id tags)))
            query (->QueryRequest (->Subscription "multi-get" false) {:ids (vec notes-to-fetch) :kinds [1]})]
        (if (seq notes-to-fetch)
          {:fx (nostr/send-to-relay-fx query relays-to-query)
           :db (assoc-subscription-to-relays db query)}
          {:db db}))
      (let [query (->QueryRequest (->Subscription "multi-get" false) {:ids [note-id] :kinds [1]})]
        {:fx (nostr/send-to-relay-fx query relays-to-query)
         :db (assoc-subscription-to-relays db query)}))))

(defn user-feed-db-fx [db user-id]
  (let [query (->QueryRequest (->Subscription (str "notes" user-id) false) {:authors [user-id] :kinds [1] :limit 50})]
    {:db (assoc-subscription-to-relays db query)
     :fx (nostr/send-to-relay-fx query (keys (:relay db)))}))

(defn switch-feed-db-fx [db global?]
  (let [global-feed (get-in db [:perp-subscriptions :global-feed])
        follows-feed (get-in db [:perp-subscriptions :contacts-feed])
        relays (keys (:relays db))]
    (if global?
      {:db (assoc-subscription-to-relays db global-feed)
       :fx (into (nostr/send-to-relay-fx global-feed relays)
                 (mapv #(vector :close-subscription [% (-> follows-feed :subscription :id)]) relays))}
      {:db (assoc-subscription-to-relays db follows-feed)
       :fx (into (nostr/send-to-relay-fx follows-feed relays)
                 (mapv #(vector :close-subscription [% (-> global-feed :subscription :id)]) relays))})))

(defn mark-mentions-seen [db time-now]
  (let [new-db (assoc db :recent-mentions (filter (comp nil? :seen second) (:mentions db)))]
    {:db (reduce #(assoc-in %1 [:mentions %2 :seen] time-now) new-db (keys (:mentions new-db)))}))

(reg-event-fx
 :set-view
 [(inject-cofx :time-now)]
 (fn [coef [_ view]]
   (let [old-view (-> :db coef :current-view)
         db (-> (assoc (:db coef) :previous-view old-view)
                (assoc :current-view view))]
   (let [new-db
         (case (first view)       
           :thread-view (note-fetch-db-fx db (second view))       
           :user-feed (user-feed-db-fx db (second view))
           :follows-feed {:db db :fx []}
           :global-feed (if (not= :global-feed (first old-view)) (switch-feed-db-fx db true) {:db db})
           :notifications (mark-mentions-seen db (:time-now-secs coef))
           {:db db})]
     (if (and (= :global-feed (first old-view)) (not= :global-feed (first view)))
       {:db (:db new-db) :fx (into (:fx (switch-feed-db-fx (:db new-db) false)) (:fx new-db))}
       new-db)))))

(reg-event-db
 :show-note-info-popup
 (fn [db [_ note-id]]
   (assoc db :showing-raw-note-info note-id)))

(reg-fx
 :store-relay-list
 (fn [[pubkey relay-list]]
   (.setItem (.-localStorage js/window) "relay-list" (.stringify js/JSON (clj->js {pubkey relay-list})))))

(reg-fx
 :store-user-contact-list
 (fn [[pubkey contact-list]]
   (.setItem (.-localStorage js/window) "contact-list" (.stringify js/JSON (clj->js {pubkey contact-list})))))

;; == output

(defn generate-tags [db reply-info]
  (if-let [note (:note reply-info)]
    (if-let [raw-note (get-in db [:notes (:id note)])]
      (nostr/generate-reply-tags (:tags raw-note) (:id note) (:pubkey note)))))

(reg-event-fx
 :post-note
 [(inject-cofx :time-now)]
 (fn [cofx [_ note-text reply-info]]
   (let [db (:db cofx)
         pubkey (:user-pubkey db)
         tags (generate-tags db reply-info)
         relays (:relays db)]
     {:post-event [(nostr/->NostrEvent pubkey (quot (:time-now cofx) 1000) 1 (or tags []) note-text)]})))


(reg-event-fx
 :save-user-settings
 [(inject-cofx :time-now)]
 (fn [cofx [_ settings]]
   (let [pubkey (get-in cofx [:db :user-pubkey])
         basic {:pubkey pubkey
                :created_at (quot (:time-now cofx) 1000)
                :tags []}
         tags (nostr/unparse-contact-list (get-in cofx [:db :contacts pubkey :list]))
         metadata (select-keys settings [:name :about :picture])
         contact (-> (:relays settings) nostr/relay-list-from-settings nostr/to-json)]
     {:fx [[:post-event [(nostr/map->NostrEvent (merge basic {:kind 0 :content (nostr/to-json metadata)}))]]
           [:post-event [(nostr/map->NostrEvent (merge basic {:kind 3 :content contact :tags tags}))]]
           [:dispatch [:set-view (-> cofx :db :previous-view)]]]})))

(defn add-contact-to-user [contacts pubkey]
  ;; TODO add suitable relay
  {:tags (conj (nostr/unparse-contact-list contacts) ["p" pubkey ""])})
  

(reg-event-fx
 :follow-user
  [(inject-cofx :time-now)]
  (fn [cofx [_ pubkey]]
       (let [db (:db cofx)
             own-pubkey (:user-pubkey db)]
         (if-not (some #(= pubkey (:pubkey %)) (get-in db [:contacts own-pubkey :list]))
           {:post-event [(-> (get-in db [:contacts own-pubkey :list])
                            (add-contact-to-user pubkey)
                            (merge {:pubkey own-pubkey :kind 3 :created_at (:time-now-secs cofx)
                                    :content (nostr/unparse-relay-list (get-in db [:contacts own-pubkey :relays]))})
                            nostr/map->NostrEvent)]}
           (pprint "already following")))))

(defn remove-contact-from-user [contacts pubkey]
  {:tags (nostr/unparse-contact-list (remove #(= pubkey (:pubkey %)) contacts))})

(reg-event-fx
 :unfollow-user
 [(inject-cofx :time-now)]
 (fn [cofx [_ pubkey]]
   (let [db (:db cofx)
         own-pubkey (:user-pubkey db)]
     (if (some #(= pubkey (:pubkey %)) (get-in db [:contacts own-pubkey :list]))
       {:post-event [(-> (get-in db [:contacts own-pubkey :list])
                         (remove-contact-from-user pubkey)
                         (merge {:pubkey own-pubkey :kind 3 :created_at (:time-now-secs cofx)
                                 :content (nostr/unparse-relay-list (get-in db [:contacts own-pubkey :relays]))})
                         nostr/map->NostrEvent)]}
       (pprint "not following")))))

(reg-event-fx
 :react-to-note
 [(inject-cofx :time-now)]  
 (fn [cofx [_ [note-id {:keys [like other]}]]]
   (let [p (get-in cofx [:db :notes note-id :pubkey])
         tags [["e" note-id]["p" p]]
         content (if other "🤙" (if like "+" "-"))]                              
     {:post-event [(nostr/->NostrEvent (-> cofx :db :user-pubkey) (:time-now-secs cofx) 7 tags content)]})))

(def events-q (chan))

(go-loop []
  (let [[event key] (<! events-q)
        hashed-event (<! (nostr/->js event))
        signed-event (if key
                       (nostr/sign event key)
                       (<p! (.signEvent (.-nostr js/window) hashed-event)))
        relays (r/get-relays :write)]
    (doseq [r relays]
      (send-to-relay r #js ["EVENT" signed-event]))
    (recur)))
  
(reg-fx
 :post-event
 (fn [[event key]]
   (go (>! events-q [event key]))))

;; == coeffects

(reg-cofx    
 :local-store-contact-list
 (fn [coeffects]
   (let [pubkey (get-in coeffects [:db :user-pubkey])]
     (assoc coeffects :contact-list
            (get (js->clj (.parse js/JSON (.getItem  (.-localStorage js/window) "contact-list")) :keywordize-keys true) (keyword pubkey))))))

(reg-cofx    
 :local-store-relay-list
 (fn [coeffects]
   (let [pubkey (get-in coeffects [:db :user-pubkey])]
     (assoc coeffects :relay-list
            (get (js->clj (.parse js/JSON (.getItem (.-localStorage js/window) "relay-list"))) pubkey)))))

(reg-cofx
 :time-now
 (fn [coeffects]
   (let [time (.now js/Date)]
     (-> (assoc coeffects :time-now time)
         (assoc :time-now-secs (quot time 1000))))))

;; ========== SUBSCRIPTIONS ====================================================
(reg-sub
 :view-selector
 (fn [db _]
   (if (= :logged-in (:login-state db))
     (:current-view db)
     [:login-view])))

(reg-sub
 :note-info-raw-active
 (fn [db _]
   (if (:showing-raw-note-info db) {:class "is-active"})))

;== subqueries

(defn assoc-name-pic [db elem]
  (let [metadata (get-in db [:metadata (:pubkey elem)])]
    (-> elem
        (assoc :name (:name metadata))
        (assoc :picture (:picture metadata)))))

(defn pub-to-name [db pubkey]
  (get-in db [:metadata pubkey :name]))

(reg-sub
 :contacts
 (fn [db _]
   (:contacts db)))

(reg-sub
 :notes
 (fn [db _]
   (:notes db)))

(reg-sub
 :metadata
 (fn [db _]
   (:metadata db)))

(reg-sub
 :reactions
 (fn [db _]
   (:reactions db)))

(reg-sub
 :relays
 (fn [db _]
   (:relays db)))

(reg-sub
 :note-graph
 (fn [db _]
   (:note-graph db)))

(reg-sub
 :own-pubkey
 (fn [db _]
   (:user-pubkey db)))

(reg-sub
 :mentions
 (fn [db _]
   (:mentions db)))

(reg-sub
 :recent-mentions
 (fn [db _]
   (set (keys (:recent-mentions db)))))

(reg-sub
 :note-info-raw
 (fn [db [_ note-id]]
   (with-out-str (cljs.pprint/pprint (dissoc (get-in db [:notes (:showing-raw-note-info db)]) :content)))))


;; (reg-sub
;;  :reactions-reduced
;;  :<- [:reactions]
;;  (fn [reactions _]
;;    (let [other (:other reactions)]
;;      )

(reg-sub
 :own-metadata
 :<- [:metadata]
 :<- [:own-pubkey]
 (fn [[metadata pubkey] _]
   (assoc (get metadata pubkey) :npubkey (encode-hex "npub" pubkey))))

(reg-sub
 :own-contacts
 :<- [:contacts]
 :<- [:own-pubkey]
 (fn [[contacts pubkey] _]
   (get-in contacts [pubkey :list])))

(reg-sub
 :own-follows-set ;; set of pubkeys the user is following
 :<- [:own-contacts]
 (fn [contacts _]
   (into #{} (map :pubkey contacts))))

(reg-sub
 :own-relays
 :<- [:contacts]
 :<- [:own-pubkey]
 (fn [[contacts pubkey] _]
   (get-in contacts [pubkey :relays])))

(reg-sub
 :contacts-relays
 :<- [:contacts]
 :<- [:own-contacts]
 (fn [[contacts own-contacts] _]
   (let [ps (map :pubkey own-contacts)
         cs (select-keys contacts ps)]
     (frequencies (mapcat (comp keys :relays) (vals cs))))))

(reg-sub
 :popular-relays
 :<- [:contacts-relays]
 (fn [relays]   
   (map-indexed #(conj %2 (inc %1)) (take 20 (reverse (sort-by last relays))))))

(reg-sub
 :get-follows
 :<- [:own-contacts]
 :<- [:metadata]
 (fn [[contacts metadata] _]
   (->> (map #(merge % (get metadata (:pubkey %))) contacts)
        (map #(assoc % :npubkey (encode-hex "npub" (:pubkey %)))))))

(reg-sub
 :user-notes
 :<- [:notes] 
 (fn [notes [_ pubkey]]
   (filter #(= (:pubkey %) pubkey) notes)))

(reg-sub
 :pubs-to-names
 :<- [:metadata]
 (fn [metadata _]
   (zipmap (keys metadata) (map :name (vals metadata)))))

(reg-sub
 :pubs-to-names-pics
 :<- [:metadata]
 (fn [metadata _]
   (zipmap (keys metadata) (map #(select-keys % [:name :picture]) (vals metadata)))))

(defn replying-str [p-to-n ps]
  (let [m (group-by #(contains? p-to-n %) (map first ps))
        found (map #(get p-to-n %) (get m true))
        not-found (get m false)
        not-found-npubs (map #(truncate-string (encode-hex "npub" (first %)) 15) ps)
        cnt-found (count (vals found))]
    (cond
      (>= 4 cnt-found) found
      (< 4 cnt-found) (conj (vec (take 4 found)) (str "and " (- (count ps) cnt-found) " others"))
      :else (conj (concat found (take (- 4 cnt-found) not-found-npubs)) (str "and " (- (count ps) 4) " others")))))

(reg-sub
 :notes-with-replypubs
 :<- [:notes]
 :<- [:pubs-to-names]
 (fn [[notes pubs-to-names] _]
   (zipmap (keys notes)
           (map (fn [note]
                  (update-in note [:replying] #(replying-str pubs-to-names %)))
                (vals notes)))))

(defn placeholder [id]
  {:name  (encode-hex "npub" id)
   :id id
   :content ""   
   :display [:div.level.mt-2 [:div.icon.level-item [:ion-icon {:name "cloud-offline-outline" :style {:font-size "32px"}}]]]})

(defn note-decorator [reactions name-pics follows]
  (fn [note]
    (-> (assoc note :reactions (get reactions (:id note)))
        (merge (get name-pics (:pubkey note)))
        (assoc :follows (follows (:pubkey note)))
        project-note)))

;== view functions

(reg-sub
 :logged-out-feed
 :<- [:notes-with-replypubs]
 :<- [:pubs-to-names-pics]
 :<- [:reactions]
 (fn [[notes pubs-names reactions] _]
   (->> (vals notes)
        (sort-by :created_at >)
        (map #(assoc % :reactions (get reactions (:id %))))
        (map #(merge (get pubs-names (:pubkey %)) %))
        (map project-note)
        (take 100))))

;; recurse predecessors to note thread root
(defn recurse-pred [graph note-id seen]
  (let [preds (g/predecessors graph note-id)
        seen-set (into #{} seen)
        new-preds (remove seen-set preds)]
    (if (seq new-preds)
      (mapcat #(recurse-pred graph % (conj seen note-id)) new-preds)
      (list note-id))))
  
(reg-sub
 :get-note-view
 :<- [:note-graph]
 :<- [:notes-with-replypubs]
 :<- [:pubs-to-names-pics]
 :<- [:reactions]
 :<- [:own-follows-set]
 (fn [[graph notes names-pics reactions follows] [_ note-id]]   
   ;; (pprint {:pred (g/predecessors graph note-id)
   ;;          :sucs (g/successors graph note-id)})
   (let [name-pic (partial get names-pics)
         get-or-placehold #(get notes % (placeholder %))
         root (first (recurse-pred graph note-id '()))
         thread-ids (reverse (alg/post-traverse graph root))]
     (->> (map get-or-placehold thread-ids)
          (map #(assoc % :child (if-let [c (:child-id %)] (let [q (get-or-placehold c)]
                                                            (project-note (merge q (name-pic (:pubkey q))))))))
          (map (note-decorator reactions names-pics follows))
          (map #(if (= note-id (:id %)) (assoc % :highlighted true) %))))))

(reg-sub
 :get-own-notes
 :<- [:own-pubkey]
 :<- [:notes-with-replypubs]
 :<- [:metadata]
 :<- [:reactions]
 :<- [:own-follows-set]
 (fn [[pubkey notes metadata reactions follows] _]
   (let [name-pic (select-keys (get metadata pubkey) [:name :picture])] 
     (->> notes
          vals
          (filter #(= pubkey (:pubkey %)))
          (sort-by :created_at >)
          (map (note-decorator reactions name-pic follows))
          ;; (map (partial merge name-pic))
          ;; (map #(assoc % :reactions (get reactions (:id %))))
          ;; (map project-note)
          ))))

(reg-sub
 :get-user-notes
 :<- [:notes-with-replypubs]
 :<- [:pubs-to-names-pics]
 :<- [:reactions]
 :<- [:own-follows-set]
 (fn [[notes names-pics reactions follows] [_ pubkey]]
   (->> (vals notes)
        (filter #(= pubkey (:pubkey %)))
        (sort-by :created_at >)
        (map (note-decorator reactions names-pics follows)))))

(reg-sub
 :get-threaded-feed ;;TODO actually thread it
 :<- [:note-graph]
 :<- [:notes-with-replypubs]
 :<- [:pubs-to-names-pics]
 :<- [:own-contacts]
 :<- [:reactions]
 :<- [:own-follows-set]
 (fn [[graph notes pubs-names contacts reactions follows] _]
   (let [contact-pubs (into #{} (map :pubkey contacts))]
     (->> (vals notes)
          (filter #(contact-pubs (:pubkey %))) ; only notes from contacts
          (sort-by :created_at >)
          (map (note-decorator reactions pubs-names follows))
          ;; (map #(assoc % :reactions (get reactions (:id %))))
          ;; (map #(merge (get pubs-names (:pubkey %)) %))
          ;; (map project-note)
          (take 100)))))

(reg-sub
 :global-feed
 :<- [:notes-with-replypubs]
 :<- [:pubs-to-names-pics]
 :<- [:reactions]
 :<- [:own-follows-set]
 (fn [[notes pubs-names reactions follows] _]
   (->> (vals notes)
        (sort-by :created_at >)
        (map (note-decorator reactions pubs-names follows))
        ;; (map #(assoc % :reactions (get reactions (:id %))))
        ;; (map #(merge (get pubs-names (:pubkey %)) %))
        ;; (map project-note)
        (take 100))))

(reg-sub
 :unseen-mentions?
 :<- [:mentions]
 (fn [mentions _]
   (some nil? (map :seen (vals mentions))))) 

(reg-sub
 :mentions-list
 :<- [:mentions]
 :<- [:recent-mentions]
 :<- [:notes-with-replypubs]
 :<- [:pubs-to-names-pics]
 :<- [:reactions]
 :<- [:own-follows-set]
 (fn [[mentions recent notes pubs-names reactions follows] _]
   (->> (keys mentions)
        (select-keys notes)
        vals
        (map #(assoc % :highlighted (recent (:id %))))
        (sort-by (juxt :highlighted :created_at) >)
        (map (note-decorator reactions pubs-names follows)))))
   
(reg-sub
 :logging-in
 (fn [db _]
   (if (= :logging-in (:login-state db)) {:class "is-active"})))

(reg-sub
 :logged-in-hidden
 (fn [db _]
   (if (= :logged-in (:login-state db)) {:class "is-hidden"})))

(reg-sub
 :is-logged-in
 (fn [db _]
   (= :logged-in (:login-state db))))

(reg-sub
 :logged-in-user
 :<- [:own-metadata]
 :<- [:own-pubkey]
 (fn [[metadata pubkey] _]
   (-> (assoc metadata :npubkey (encode-hex "npub" pubkey))
       (update-in [:npubkey] #(truncate-string % 14)))))

(reg-sub
 :user-settings
 :<- [:own-metadata]
 :<- [:own-relays]
 (fn [[metadata user-relays] _]
   ;; transform the user's relay metadata into a map with keys  [:relay-url :read :write]
   (let [relay-fn (juxt first #(-> % second :read) #(-> % second :write))
         relays (mapv #(zipmap [:relay-url :read :write] (relay-fn %)) user-relays)]
     (assoc metadata :relays relays))))

(reg-sub
 :connected-relays
 (fn [db _]
   (for [relay (:relays db)]     
     [(subs (first relay) 6) (:state (second relay))])))

(reg-sub
 :db-stats
 :<- [:metadata]
 :<- [:notes]
 :<- [:own-contacts]
 (fn [[metadata notes contacts] _]
   (with-out-str
     (pprint {:notes (count notes)
              :contacts (count contacts)
              :metadata (count metadata)}))))
