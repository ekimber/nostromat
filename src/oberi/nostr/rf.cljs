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
                                        ->CloseRequest wire keywordize-map-keys encode-hex]])
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
   :previous-view [:global-feed]
   :reactions {:registered #{}}
   :perp-subscriptions {:global-feed (nostr/->QueryRequest (nostr/->Subscription "globalsub" true) {:limit 100 :kinds [0 1 2 6 7]})
                        :recommend-relays (nostr/->QueryRequest (nostr/->Subscription (str "recommend") true) {:kinds [2] :limit 10})}
   :mentions {}   
   :relays {"wss://relay.nostr.scot" {:read true :write true}
            "wss://nostr.slothy.win" {:read true :write true}
            "wss://relay.damus.io" {:read true :write true}
            "wss://nostr.v0l.io" {:read true :write true}
            "wss://nostr.onsats.org" {:read true :write true}
            "wss://nostr.drss.io" {:read true :write true}}})

(defn cl-print [x] (doto x (print)))

;; === init 
(reg-event-fx
 ::boot
 [(inject-cofx :session-pubkey)]
 (fn [cofx _]
   (pprint "boot")
   (if-let [pubkey (:user-pubkey cofx)]     
     {:db (-> (assoc default-db :user-pubkey pubkey)
              (assoc :private-key (:private-key cofx))
              (assoc :login-state :logging-in))
      :dispatch [:reload-session]}
     {:db default-db
      :dispatch [:start-new-session]})))

(reg-event-fx
 :start-new-session
 (fn [cofx]   
   {:load-relays (-> cofx :db :relays)}))

(reg-event-fx
 :reload-session
 [(inject-cofx :local-store-relay-list) (inject-cofx :local-store-contact-list)] 
 (fn [cofx _]
   (pprint "Reloading session with stored pubkey")
   (let [relays-unkw (:relay-list cofx)
         saved-relays (zipmap (keys relays-unkw) (map keywordize-map-keys (vals relays-unkw)))
         relays (if (empty? saved-relays) (-> cofx :db :relays) saved-relays)]
     {:db (assoc (:db cofx) :relays relays)
      :fx [[:load-relays relays]
           [:dispatch [:init-user-subscriptions]]
           [:dispatch [:logged-in-with-pubkey (-> cofx :db :user-pubkey)]]]})))
 
(reg-fx
 :load-relays
 (fn [relays]
   (doseq [relay relays]
     (r/connect! (first relay) (second relay)))))

(reg-event-fx
 :connect-extension
 (fn [coef _]
   {:db (assoc (:db coef) :login-state :logging-in)
    :promise {:call #(.getPublicKey (.-nostr js/window))
              :on-success [:logged-in-with-pubkey]}}))

(reg-event-db
 :start-login
 (fn [db _]
   (assoc db :login-state :logging-in)))

(reg-event-fx 
 :create-account
 [(inject-cofx :new-keypair)]
 (fn [cofx _]
   (let [db (:db cofx)]
     (if (nil? (:user-pubkey db))
       {:db (-> (assoc db :login-state :creating-account)
                (assoc :private-key (-> cofx :new-keypair first))
                (assoc :user-pubkey (-> cofx :new-keypair second)))}))))

(reg-event-db
 :provide-secret-key
 (fn [db _]
   (assoc db :login-state :providing-secret-key)))

(reg-event-fx
 :user-provided-secret
 (fn [cofx [_ secret]]
   (let [db (:db cofx)]
     {:db (assoc db :private-key secret)
      :dispatch [:logged-in-with-pubkey (nostr/public-key secret)]})))

(reg-event-fx
 :init-user-subscriptions
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
   ;; User notes
   ;;    - in normal mode - user's contact list - perpetual
   ;;        - anything missing - oneshot
   ;;    - in global mode - everything - perpetual
   (let [db (:db cofx)
         pubkey (:user-pubkey db)
         contact-list (:contact-list cofx)
         mentions-req (nostr/->QueryRequest (nostr/->Subscription "mentions" false) {:kinds [1] :#p [pubkey] :limit 100})
         active-relays (r/connected-relays)] ;;TODO cofx
     {:db (-> (assoc-in db [:contacts pubkey :list] contact-list)
              (assoc-in [:perp-subscriptions :contacts-feed] (nostr/contact-feed-req pubkey contact-list))
              (assoc-in [:perp-subscriptions :private-messages] (nostr/private-msg-req pubkey))
              (assoc-in [:perp-subscriptions :contacts-list-contacts] (nostr/contact-list-req pubkey contact-list))
              (assoc-in [:perp-subscriptions :mentions] mentions-req))
      :fx (-> (nostr/send-to-relay-fx (nostr/contact-list-req pubkey contact-list) active-relays)
              (into (nostr/send-to-relay-fx (nostr/contact-feed-req pubkey contact-list) active-relays))
              (into (nostr/send-to-relay-fx (nostr/private-msg-req pubkey) active-relays))
              (into (nostr/send-to-relay-fx mentions-req active-relays)))})))

(reg-event-fx
 :logged-in-with-pubkey
 (fn [cofx [_ pubkey]]
   {:db (-> (:db cofx)
            (assoc :user-pubkey pubkey)
            (assoc :login-state :logged-in))
    :fx [[:set-session-pubkey [pubkey (-> cofx :db :private-key)]]
         [:dispatch [:init-user-subscriptions]]
         [:update-relay-connections (-> cofx :db :relays)]]}))

;; == shutdown

(reg-event-fx
 :logout
 (fn [cofx _]
   {:db (-> (assoc (:db cofx) :user-pubkey nil)
            (assoc :private-key nil)
            (assoc :login-state :logged-out))
    :fx [[:dispatch [:set-view [:global-view nil]]]
        [:set-session-pubkey nil]]}))

;; == relay connections

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
      (pprint (str "Ignoring event " evt)))))
;; close any one shot subscriptions
;; (>evt [:end-of-stored-events subs-id relay])

(defn handle-eose [subs-id relay]
  ;; TODO store time of last update for future reference
  (>evt [:end-of-stored-events subs-id relay]))

(defn handle-message [msg relay]
  (let [type (first msg)]
    (case type
      "EVENT" (handle-event (second msg) (nth msg 2) relay)
      "EOSE" (handle-eose (second msg) relay)
      "OK" nil
      (pprint {:unhandled msg :relay relay})
      )))

(defn setup-listener [ws-channel relay]
  (go-loop []
    (let [{:keys [message error]} (<! ws-channel)]
      (if (or error (nil? message))
        (do (pprint (if error (str "Relay listener error: " error) (str "nil message on channel for " relay)))
            (r/try-to-reconnect! relay))
        (do (handle-message message relay)
            (recur))))))

(reg-fx
 :connect-relay
 (fn [[relay rw-map]]
   (pprint (str "Connecting to relay " relay " rw " rw-map))
   (if-not (or (:read rw-map) (:write rw-map))
     (pprint (str "Relay " relay " is r/w false"))
     (if (r/is-connected? relay)
       (pprint (str "Already connected to " relay))
       (r/connect! relay rw-map)))))

(reg-fx
 ;; match the active relay set with whatever has changed in the user's relay selection
 :update-relay-connections
 (fn [relay-list]
   (pprint {:new-relay-list relay-list})
   (if-not (empty? relay-list)
     (let [new-relays (into {} relay-list)
           relays-current @r/relays-atom]
       ;; update rw-maps and disconnect extraneous relays
       (doseq [[relay-url {rw :rw-map}] relays-current]
         (if (contains? new-relays relay-url)
           (r/set-rw-map! relay-url (get new-relays relay-url))
           (r/shutdown! relay-url)))
       ;;add new relays
       (doseq [[relay-url {rw :rw-map}] new-relays]
         (if-not (contains? relays-current relay-url)
           (r/connect! relay-url rw)))))))

(reg-event-fx
 :relay-connected
 (fn [cofx [_ relay]]
   (pprint (str "Relay connected " relay))
   (let [relay-meta (r/get-relay relay)
         channel (:channel relay-meta)
         rw-map (:rw-map relay-meta)]
     (if (:read rw-map)
       (do
         (setup-listener channel relay)
         {:dispatch [:relay-read-active relay]})))))

(defn add-feed-sub [relay db feed]
  (assoc-in db [:relays relay :subscriptions (-> feed :subscription :id)] (:subscription feed)))

(reg-event-fx
 :relay-read-active
 (fn [cofx [_ relay]]
   (let [db (:db cofx)
         ;; use the global feed for global view, otherwise the contacts feed
         feeds [(get-in db [:perp-subscriptions (or (#{(first (:current-view db))} :global-feed) :contacts-feed)])
                (get-in db [:perp-subscriptions :private-messages])
                (get-in db [:perp-subscriptions :contacts-list-contacts])
                (get-in db [:perp-subscriptions :recommend-relays])
                (get-in db [:perp-subscriptions :mentions])]]
     {:db (reduce (partial add-feed-sub relay) db (remove nil? feeds))
      :fx (mapv #(vector :send-to-relay [relay (nostr/wire %)]) (remove nil? feeds))})))

(defn user-relay-config [db relay] ;;r/w map
  (get-in db [:contacts (:user-pubkey db) :relays relay]))

;; (reg-event-fx
;;  :relay-disconnected
;;  (fn [cofx [_ relay]]
;;    (let [db (:db cofx)]
;;      (pprint (str "Relay disconnected " relay))   
;;      {:connect-relay [relay (user-relay-config db relay)]}
;;      )))

;; === message out functions

(defn send-to-relay [relay-url msg]
  (let [ {:keys [state rw-map channel]} (r/get-relay relay-url)]
    ;; (pprint {:relay relay-url :msg msg})
    (if (or (= :disconnected state) (nil? channel))
      (pprint ["Send error" {:state state :channel channel :relay relay-url :msg (subs (str (js->clj msg)) 0 40)}]) 
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
       (do
         {:db (assoc-in db [:relays relay :subscriptions subscription-id :complete] (:time-now cofx))
          :close-subscription [relay subscription]})))))

(reg-fx
 :close-subscription
 (fn [[relay subscription]]
   {:send-to-relay [relay (wire (->CloseRequest subscription))]}))

;; === event recv handling

(defn find-edges [note tags]
  ;; list of edges (from origin 
  (for [link (remove nil? (conj (:links tags) (:root-id tags) (:reply-id tags)))]
    [link (:id note)]))

(defn update-in-if-true [db flag ks v]
  (if flag (update-in db ks #(if (nil? %) v %)) db))

(defn handle-text-note
  [db note]
  (let [notes (:notes db)
        note-id (:id note)]
    (if-not (contains? notes note-id)
      (let [graph (:note-graph db)
            pubkey (:pubkey note)
            tags (parse-notes-tags (:tags note))
            mention? (some #(= (:user-pubkey db) (first %)) (:replying tags))]
        (go              ;; query relays if pubkey is missing
          (if-not (contains? (:metadata db) pubkey)
            (>! r/metadata-query-q pubkey))
          (let [notes-to-fetch (remove #(or (nil? %) (contains? notes %)) (conj (:links tags) (:root-id tags) (:reply-id tags)))]             
            (async/onto-chan! r/note-query-q notes-to-fetch false)))
        (-> (assoc-in db [:notes (:id note)] (-> (assoc note :replying (:replying tags))                                                    
                                                 (merge (select-keys tags [:child-id :child-relay]))))
            (assoc-in [:reactions (:id note)] {:likes #{} :dislikes #{}})
            (assoc :note-graph (-> (apply (partial g/add-edges graph) (find-edges note tags))
                                   (g/add-nodes (:id note))))
            (update-in-if-true mention? [:mentions (:id note)] {:seen nil}))))))

(reg-event-db
 :text-note 
 (fn [db [_ note]]
   (handle-text-note db note)))

;; (reg-event-db
;;  :mentions-msg
;;  (fn [db [_ msg]]   
;;    (-> db
;;        (handle-text-note msg)
;;        (update-in [:mentions (:id msg)] #(if (nil? %) {:seen nil} %)))))

(reg-event-db
 :metadata-msg
 (fn [db [_ meta-msg]]
   (let [content (js->clj (.parse js/JSON (:content meta-msg)) :keywordize-keys true)      
         meta {:pubkey (:pubkey meta-msg)
               :tags (:tags meta-msg)
               :name (:name content)
               :about (:about content)
               :picture (:picture content)
               :created-at (:created_at meta-msg)}]                                            
     ;; update pubkey metadata if message is newer, remove pubkey from pending queries since the query has been done
     (update-in db [:metadata (:pubkey meta)] #(if (or (nil? (:created-at %)) (> (:created-at meta) (:created-at %))) meta %)))))

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
           ;; (pprint {:ct  contact-list  
           ;;          :db (get-in db [:contacts pubkey :list])
           ;;          :eq (= (get-in db [:contacts pubkey :list]) contact-list)})
           (if (not= contact-list (get-in db [:contacts pubkey :list]))
             (let [contact-feed-query (nostr/contact-feed-req pubkey contact-list)
                   contact-list-query (nostr/contact-list-req pubkey contact-list)
                   relays (r/connected-relays)]
               {:db (-> (assoc-in updated-db [:perp-subscriptions :contacts-feed] contact-feed-query)
                        (assoc-in [:perp-subscriptions :contacts-list-contacts] contact-list-query))
                :fx (-> [[:update-relay-connections relay-list]]
                        (conj [:store-user-contact-list [pubkey contact-list]])
                        (conj [:store-relay-list [pubkey relay-list]])
                        (into (nostr/send-to-relay-fx contact-feed-query relays))
                        (into (nostr/send-to-relay-fx contact-list-query relays)))})
             {:db updated-db :update-relay-connections relay-list})
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
 :recommend-relay-msg
 (fn [db [_ rec-msg]]
   (update-in db [:recommended-relays (:content rec-msg)] inc)))

(defn assoc-subscription-to-relays
  [db query]
  (reduce #(assoc-in %1 [:relays %2 :subscriptions (-> query :subscription :id)] (:subscription query)) db
          (keys (get-in db [:contacts (:user-pubkey db) :relays]))))

(defn dissoc-subscription-to-relays
  [db query]
  ;;TODO tidy up!
  )

(defn note-fetch [db note-id]
  (let [notes (:notes db)]
    (if-let [note (get notes note-id)]             
      ;; request the linked notes - if they are not already in app-db
      (let [tags (parse-notes-tags (:tags note))
            notes-to-fetch (remove #(or (nil? %) (contains? notes %)) (conj (:links tags) (:root-id tags) (:reply-id tags)))]
        (go (async/onto-chan! r/note-query-q notes-to-fetch false)))
      (go (>! r/note-query-q note-id))))
  {:db db})

(defn user-feed-db-fx [db user-id]
  (let [query (nostr/->QueryRequest (nostr/->Subscription (str "notes" user-id) false) {:authors [user-id] :kinds [1] :limit 50})]
    {:db (assoc-subscription-to-relays db query)
     :fx (nostr/send-to-relay-fx query (r/connected-relays))}))

(defn switch-feed-db-fx [db global?]
  (let [global-feed (get-in db [:perp-subscriptions :global-feed])
        follows-feed (get-in db [:perp-subscriptions :contacts-feed])
        relays (r/connected-relays)]
    (if global?
      {:db (assoc-subscription-to-relays db global-feed)
       :fx (into (nostr/send-to-relay-fx global-feed relays)
                 (mapv #(vector :close-subscription [% (-> follows-feed :subscription :id)]) relays))}
      {:db (assoc-subscription-to-relays db follows-feed)
       :fx (into (nostr/send-to-relay-fx follows-feed relays)
                 (mapv #(vector :close-subscription [% (-> global-feed :subscription :id)]) relays))})))

(defn mark-mentions-seen [db time-now]
  (let [new-db (assoc db :recent-mentions (filter (comp nil? :seen second) (:mentions db)))]
    (reduce #(assoc-in %1 [:mentions %2 :seen] time-now) new-db (keys (:mentions new-db)))))

(reg-event-fx
 :set-view
 [(inject-cofx :time-now)]
 (fn [coef [_ view]]
   (if (= (-> coef :db :login-state) :logged-in)
     (let [old-view (-> :db coef :current-view)
           db (-> (assoc (:db coef) :previous-view old-view)
                  (assoc :current-view view))]
       (case (first view)    
         :thread-view (note-fetch db (second view))               
         :user-feed (user-feed-db-fx db (second view))
         :follows-feed {:db db}
         :global-feed {:db db} ;; (if (not= :global-feed (first old-view)) (switch-feed-db-fx db true) {:db db})
         :notifications {:db (mark-mentions-seen db (:time-now-secs coef))}
         {:db db})
         ;; (if (and (= :global-feed (first old-view)) (not= :global-feed (first view)))
         ;;   {:db (:db new-db) :fx (into (:fx (switch-feed-db-fx (:db new-db) false)) (:fx new-db))}
         ;;   new-db)
     ))))

(reg-event-db
 :show-note-info-popup
 (fn [db [_ note-id]]
   (assoc db :showing-raw-note-info note-id)))


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
     {:dispatch [:post-event (nostr/->NostrEvent pubkey (quot (:time-now cofx) 1000) 1 (or tags []) note-text)]})))


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
     {:fx [[:dispatch [:post-event (nostr/map->NostrEvent (merge basic {:kind 0 :content (nostr/to-json metadata)}))]]
           [:dispatch [:post-event (nostr/map->NostrEvent (merge basic {:kind 3 :content contact :tags tags}))]]
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
       {:dispatch [:post-event (-> (get-in db [:contacts own-pubkey :list])
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
       {:dispatch [:post-event (-> (get-in db [:contacts own-pubkey :list])
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
     {:dispatch [:post-event (nostr/->NostrEvent (-> cofx :db :user-pubkey) (:time-now-secs cofx) 7 tags content)]})))

(reg-event-fx
 :post-event
 (fn [cofx [_ event]]   
   {:post-event-q [event (-> cofx :db :private-key)]}))

(def events-q (chan))

(go-loop []
  (let [[event key] (<! events-q)
        hashed-event (<! (nostr/->js event))
        signed-event (if key
                       (<! (nostr/->js (<! (nostr/sign event key))))
                       (<p! (.signEvent (.-nostr js/window) hashed-event)))
        relays (r/get-relays :write)]
    ;; (pprint {:event event :key key :signed signed-event :relays relays})
    (doseq [r relays]
      (send-to-relay r #js ["EVENT" signed-event]))
    (recur)))

(reg-fx
 :post-event-q
 (fn [[event key]]
   (go (>! events-q [event key]))))

;; == coeffects

(reg-cofx
 :time-now
 (fn [coeffects]
   (let [time (.now js/Date)]
     (-> (assoc coeffects :time-now time)
         (assoc :time-now-secs (quot time 1000))))))

(reg-cofx
 :new-keypair
 (fn [cofx]
   (let [private-key (nostr/generate-key)
         public-key (nostr/public-key private-key)]
     (assoc cofx :new-keypair [private-key public-key]))))


                                        ; == local storage

(defn set-or-clear-local [key val]
  (if val
    (.setItem (.-localStorage js/window) key val)
    (.removeItem (.-localStorage js/window) key)))

(defn write-session-pubkey [pubkey private-key]
  (set-or-clear-local "user-pubkey" pubkey)
  (set-or-clear-local "user-private-key" private-key))

(reg-fx
 :set-session-pubkey
 (fn [[pubkey private-key]]
 (write-session-pubkey pubkey private-key)))

(reg-cofx
 :session-pubkey
 (fn [coeffects]
   (-> (assoc coeffects :user-pubkey (.getItem (.-localStorage js/window) "user-pubkey"))
       (assoc :private-key (.getItem (.-localStorage js/window) "user-private-key")))))

(reg-fx
 :store-relay-list
 (fn [[pubkey relay-list]]
   (.setItem (.-localStorage js/window) "relay-list" (.stringify js/JSON (clj->js {pubkey relay-list})))))

(reg-fx
 :store-user-contact-list
 (fn [[pubkey contact-list]]
   (.setItem (.-localStorage js/window) "contact-list" (.stringify js/JSON (clj->js {pubkey contact-list})))))

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

