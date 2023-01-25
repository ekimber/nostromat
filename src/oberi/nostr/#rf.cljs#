(ns oberi.nostr.rf
  (:require
   [oberi.nostr.manager :as mgr]
   [oberi.nostr.subs-manager :as sbm]
   [oberi.nostr.crypt :as crypt]
   [oberi.nostr.extension :as ext]
   [clojure.set :refer [union]]   
   [re-frame.core :as re-frame :refer [reg-event-db reg-event-fx reg-sub reg-fx reg-cofx path subscribe
                                       inject-cofx]]
   [cljs.core.async :as async :refer [<! >! put! chan close! mult tap]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [cljs.pprint :refer [pprint print-table]]
   [loom.graph :as g]
   [loom.alg :as alg]
   [oberi.nostr.nostr :as nostr :refer [project-note truncate-string parse-contact-list wire keywordize-map-keys
                                        parse-relay-list parse-tags-element parse-notes-tags]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

;; ========== SETUP ============================================================
(def <sub (comp deref re-frame/subscribe))
(def <sub-lazy re-frame/subscribe)
(def >evt re-frame/dispatch)
(def >evt-now re-frame/dispatch-sync)

(def default-relays {"wss://nostr.v0l.io" {:read true :write true}
                     "wss://nostr.onsats.org" {:read true :write true}
                     "wss://nostr.drss.io" {:read true :write true}
                     "wss://nostr.slothy.win" {:read true :write true}})

(def default-db
  {:notes {}
   :note-graph (g/digraph)
   :login-state :logged-out
   :current-view [:global-feed]
   :prev-view-list '([:global-feed])
   :reactions {:registered #{}}
   :mentions {}
   :messages {}
   :relays {:list default-relays
            :updated-at 0}})

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
   {:fx [[:base-subscriptions-start]
         [:connect-relays (-> cofx :db :relays :list)]]}))
   
(reg-fx
 :connect-relays
 (fn [relays]  
   (go     
     (let [conn-promises (for [relay relays]
                           (mgr/connect (first relay) (second relay) {:on-disconnect sbm/handle-disconnect
                                                                      :on-connect sbm/handle-connect}))
           conns (<! (async/map vector conn-promises))]
       (>evt [:relays-connected conns])))))

(reg-event-fx
 :relays-connected
 (pprint "relays connected")
 (fn [cofx [_ conns]]   
   (if-let [pubkey (-> cofx :db :user-pubkey)] ;; TODO what if pubkey and not private key or extension?
     (if-let [private-key (-> cofx :db :private-key)]
       {:fx  [[:get-relay-recommendations conns]
              [:dispatch [:logged-in-with-pubkey (-> cofx :db :user-pubkey)]]]} ;; have private key => already logged in
       {:fx  [[:get-relay-recommendations conns]
              [:dispatch [:connect-extension]]]})
     {:get-relay-recommendations conns})))

(reg-event-fx
 :reload-session
 [(inject-cofx :local-store-relay-list) (inject-cofx :local-store-contact-list) (inject-cofx :local-store-mentions)] 
 (fn [cofx _]
   (pprint "Reloading session with stored pubkey")
   (let [relays-unkw (:relay-list cofx)
         kw1 (zipmap (map keyword (keys relays-unkw)) (vals relays-unkw))
         saved-relays (update-in kw1 [:list] #(zipmap (keys %) (clojure.walk/keywordize-keys (vals %))))         
         relays (if (empty? saved-relays) (-> cofx :db :relays) saved-relays)]
     (pprint (-> cofx :db :user-pubkey))
     {:db (-> (assoc (:db cofx) :relays relays)
              (assoc :mentions (zipmap (keys (:mentions cofx)) (clojure.walk/keywordize-keys (vals (:mentions cofx))))))
      :fx [[:base-subscriptions-start]
           [:connect-relays (:list relays)]]})))

(reg-event-fx
 :connect-extension
 (fn [coef _]   
   (go
     (let [result (<! (ext/call-extension #(.getPublicKey (.-nostr js/window))))]
       (if (:result result)
         (>evt [:logged-in-with-pubkey (:result result)])
         (>evt [:login-failure (:error result)]))))
 {:db (assoc (:db coef) :login-state :logging-in)}))

(reg-event-db
 :login-failure
 (fn [db [_ err]]
   (pprint {:login-fail err})
   (assoc db :login-state :logged-out)))

;; triggered after the login has been created, or loaded from private key or extension
(reg-event-fx
 :logged-in-with-pubkey
 [(inject-cofx :local-store-contact-list)]
 (fn [cofx [_ pubkey]]
   (pprint {:logged-in pubkey})
   {:db (-> (:db cofx)
            (assoc :user-pubkey pubkey)
            (assoc :login-state :logged-in))
    :fx [[:set-session-pubkey [pubkey (-> cofx :db :private-key)]]
         [:register-user-subscriptions {:pubkey pubkey :contact-list (:contact-list cofx)}]]}))

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


;; == shutdown

(reg-event-fx
 :logout
 (fn [cofx _]
   (mgr/disconnect-all)
   {:db (-> (assoc (:db cofx) :user-pubkey nil)
            (assoc :private-key nil)
            (assoc :messages {})
            (assoc :mentions {})
            (assoc :login-state :logged-out)
            (assoc-in [:relays :list] default-relays))   
    :fx [[:dispatch [:set-view [:global-view nil]]]
         [:set-session-pubkey nil]
         [:dispatch [:start-new-session]]]}))

(reg-event-fx
 ;; match the active relay set with whatever has changed in the user's relay selection
 :update-relay-connections
 (fn [cofx [_ relay-list]]
   (pprint {:new-relay-list relay-list})
   {:db (assoc-in (-> cofx :db) [:relays :list] relay-list)
    :send-relay-list-to-connmgr relay-list}))

(reg-fx
 :send-relay-list-to-connmgr
 (fn [relay-list]
   (mgr/sync-relay-list relay-list {:on-disconnect sbm/handle-disconnect
                                    :on-connect sbm/handle-connect})))
   ;; (go (let [conn-promises (mgr/sync-relay-list relay-list {:on-disconnect (fn [_] :oberi.nostr.manager/reconnect)})
   ;;           conns (<! (async/map vector conn-promises))]
   ;;       (>evt [:relays-connected conns])))

(defn add-feed-sub [relay db feed]
  (assoc-in db [:relays relay :subscriptions (-> feed :subscription :id)] (:subscription feed)))

;; === event recv handling

(defn find-edges [note tags]
  ;; list of edges (from origin 
  (for [link (remove nil? (conj (:links tags) (:root-id tags) (:reply-id tags)))]
    [link (:id note)]))

(defn update-in-if-true [db flag ks v]
  (if flag (update-in db ks #(if (nil? %) v %)) db))

(defn handle-text-note
  [cofx note]
  (let [db (:db cofx)
        notes (:notes db)
        note-id (:id note)]
    (if-not (contains? notes note-id)
      (let [graph (:note-graph db)
            pubkey (:pubkey note)
            tags (parse-notes-tags (:tags note))
            mention? (some #(= (:user-pubkey db) (first %)) (:replying tags))
            missing-notes (remove #(or (nil? %) (contains? notes %)) (conj (:links tags) (:root-id tags) (:reply-id tags)))]
        {:db (-> (assoc-in db [:notes (:id note)] (-> (assoc note :replying (:replying tags))                                                    
                                                      (merge (select-keys tags [:child-id :child-relay]))))
                 (assoc-in [:reactions (:id note)] {:likes #{} :dislikes #{}})
                 (assoc :note-graph (-> (apply (partial g/add-edges graph) (find-edges note tags))
                                        (g/add-nodes (:id note))))
                 (update-in-if-true mention? [:mentions (:id note)] {:seen nil}))
         :fx [[:query-metadata pubkey]
              (if (seq missing-notes) [:query-notes missing-notes])]}))))

(reg-event-fx
 :text-note 
 (fn [cofx [_ note]]
   (handle-text-note cofx note)))

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

;; The tricky bit
(defn self-contacts-update [old-db db contact-list relay-list timestamp]
  ;; TODO refresh contact list subs
  (let [pubkey (:user-pubkey db)]
    ;; (if (not= contact-list (get-in old-db [:contacts pubkey :list]))
    ;;   {:db db})
    (let [fx (if (and (not= relay-list (-> old-db :relays :list))
                      (> timestamp (-> old-db :relays :updated-at)))
               [[:dispatch [:update-relay-connections relay-list]]] [])]
  
      {:db db    
       :fx (-> (conj fx [:store-user-contact-list [pubkey contact-list]])
               (conj [:store-relay-list [pubkey relay-list]]))})))

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
         ;; if it is the user's contact list, store updated contact list etc
         (if (= pubkey (:user-pubkey db))
           (self-contacts-update db updated-db contact-list relay-list created-at)
           {:db updated-db}))
       {:db db})))) ;; already have a newer entry than this msg

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

(reg-event-fx
 :private-msg
 (fn [cofx [_ private-msg]]
   (let [crypt-text (:content private-msg)
         id (:id private-msg)
         db (:db cofx)
         pubkey-from (-> private-msg :pubkey)
         pubkey-to (-> private-msg :tags first second)
         decrypt-pub (if (= (-> db :user-pubkey) pubkey-from) pubkey-to pubkey-from)]
     (when (not (contains? (:messages db) id))
       (cond->> {}  
         (nil? (get-in db [:messages id])) (merge-with into {:fx [[:decrypt-message [private-msg decrypt-pub (:private-key db)]]]})
         (nil? (get-in db [:metadata pubkey-from])) (merge-with into {:fx [[:query-metadata pubkey-to]]}))))))

(reg-event-fx
 :post-message
 (fn [cofx [_ note-text info]]
   (go
     (let [recv-pubkey (:pubkey info)
           private-key (get-in cofx [:db :private-key])
           content (<! (ext/call-extension (if private-key
                                             #(crypt/encrypt private-key recv-pubkey note-text)
                                             #(.encrypt js/window.nostr.nip04 recv-pubkey note-text))))]
       (if (:result content)
         (>evt [:encrypted-message {:recv-pubkey recv-pubkey :content (:result content)}])
         (pprint {:encrypt-error (:error content)}))))
   {}))

(reg-fx
 :decrypt-message
 (fn [[{:keys [id pubkey tags content created_at]} decrypt-pub private-key]]
   (go     
     (let [pubkey-to (-> tags first second)
           message (<! (ext/call-extension (if private-key
                                             #(crypt/decrypt private-key decrypt-pub content)
                                             #(.decrypt js/window.nostr.nip04 decrypt-pub content))))]
       (if-let [msg (:result message)]
         (>evt [:decrypted-message {:id id :pubkey-from pubkey :pubkey-to pubkey-to :created-at created_at :msg msg}])
         (pprint {:decrypt-error (:error message)}))))))

(reg-event-fx
 :encrypted-message
 [(inject-cofx :time-now)]
 (fn [cofx [_ message]]
   {:dispatch [:post-event (nostr/->NostrEvent (-> cofx :db :user-pubkey) (:time-now-secs cofx) 4 [["p" (:recv-pubkey message)]] (:content message))]}))

(reg-event-db
 :decrypted-message
 (fn [db [_ msg]]
   (assoc-in db [:messages (:id msg)] msg)))

(defn user-feed-fx [user-id]
  [[:post-request (mgr/->Request "usernotes" #js{"kinds" #js[1] "authors" #js[user-id] "limit" 60})]])

(defn mark-mentions-seen [db time-now]
  (let [new-db (assoc db :recent-mentions (filter (comp nil? :seen second) (:mentions db)))]
    (reduce #(assoc-in %1 [:mentions %2 :seen] time-now) new-db (keys (:mentions new-db)))))

(reg-event-fx
 :previous-view
 [(inject-cofx :time-now)]
 (fn [cofx _]
   {:db (as-> (:db cofx) db
          (assoc db :current-view (first (:prev-view-list db)))
          (assoc db :prev-view-list (rest (:prev-view-list db))))}))

(defn tagged-events [note]
  (mapv second (filter #(= "e" (first %)) (:tags note))))

(reg-event-fx
 :set-view
 [(inject-cofx :time-now)]
 (fn [coef [_ [view-name param]]]
   (if (= (-> coef :db :login-state) :logged-in)
     (let [db {:db (as-> (:db coef) db
                     (if (= view-name :notifications) (mark-mentions-seen db (:time-now-secs coef)) db)
                     (update-in db [:prev-view-list] #(conj % (:current-view db)))
                     (assoc db :current-view [view-name param]))}
           fx (case view-name
                :user-feed {:fx (user-feed-fx param)}
                :notifications {:store-seen-mentions [(:user-pubkey (:db coef)) (get (:db db) :mentions)]}
                :thread-view (as-> (tagged-events param) ees (if (seq ees) {:post-request (mgr/->Request "notethread" #js{"kinds" #js[1] "#e" (clj->js ees) "limit" 50})}))
                nil)]
       (merge db fx)))))

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
                :created_at (:time-now-secs cofx)
                :tags []}
         tags (nostr/unparse-contact-list (get-in cofx [:db :contacts pubkey :list]))
         metadata (select-keys settings [:name :about :picture])
         relay-list (-> settings :relays nostr/relay-list-from-settings)
         contact (nostr/to-json relay-list)]     
     {:fx [[:dispatch [:post-event (nostr/map->NostrEvent (merge basic {:kind 0 :content (nostr/to-json metadata)}))]]
           [:dispatch [:post-event (nostr/map->NostrEvent (merge basic {:kind 3 :content contact :tags tags}))]]
           [:dispatch [:previous-view]]
           [:store-relay-list [pubkey relay-list]]]})))

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
                       (try (<p! (.signEvent (.-nostr js/window) hashed-event))
                            (catch js/Error e (pprint {:err e :hashed-event hashed-event}))))]
    ;; (pprint {:event signed-event})
    (mgr/write-to-relays #js["EVENT" signed-event])
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

(defn store-get [pubkey]
  (js->clj (.parse js/JSON (.getItem (.-localStorage js/window) pubkey))))

(defn store-save [db pubkey]
  (.setItem (.-localStorage js/window) pubkey (.stringify js/JSON (clj->js db))))

(reg-fx
 :store-relay-list
 (fn [[pubkey relay-list]]
   (-> (store-get pubkey)
       (assoc :relay-list {:updated-at (quot (.now js/Date) 1000)
                           :list relay-list})
       (store-save pubkey))))

(reg-fx
 :store-user-contact-list
 (fn [[pubkey contact-list]]
   (-> (store-get pubkey)
       (assoc :contact-list contact-list)
       (store-save pubkey))))

(reg-fx
 :store-seen-mentions
 (fn [[pubkey mentions]]
   (-> (store-get pubkey)
       (assoc :mentions mentions)
       (store-save pubkey))))
   
(reg-cofx    
 :local-store-contact-list
 (fn [coeffects]
   (let [pubkey (get-in coeffects [:db :user-pubkey])
         contact-list (-> (store-get pubkey)
                          (get "contact-list")
                          clojure.walk/keywordize-keys)]
     (assoc coeffects :contact-list contact-list))))

(reg-cofx    
 :local-store-relay-list
 (fn [coeffects]
   (let [pubkey (get-in coeffects [:db :user-pubkey])
         relay-list (-> (store-get pubkey)
                        (get "relay-list"))]
     (assoc coeffects :relay-list relay-list))))

(reg-cofx    
 :local-store-mentions
 (fn [coeffects]
   (let [pubkey (get-in coeffects [:db :user-pubkey])
         mentions (-> (store-get pubkey)
                      (get "mentions"))]
     (assoc coeffects :mentions mentions))))
