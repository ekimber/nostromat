(ns oberi.nostr.subs
  (:require
   [re-frame.core :refer [reg-sub]]
   [oberi.nostr.nostr :as n]
   [loom.graph :as g]
   [loom.alg :as alg]
   [goog.date]
   [lambdaisland.deja-fu :as fu]
   [cljs.pprint :refer [pprint]])
  (:import [goog.date DateTime]))

(defn secs-to-date [secs]
  (-> (* 1000 secs)
       DateTime/fromTimestamp
       (fu/format "dd. MMM HH:mm:ss")))
  
(reg-sub
 :extension-unavailable?
 (fn [_]
   (nil? (.-nostr js/window))))

(reg-sub
 :view-selector
 (fn [db _]
   (if (= :logged-in (:login-state db))
     (:current-view db)
     [:login-view])))

(reg-sub
 :previous-view-selector
 (fn [db _]
   (if (= :logged-in (:login-state db))
     (:previous-view db)
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
   (-> db :relays :list)))

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
 :messages
 (fn [db _]
   (vals (:messages db))))

(reg-sub
 :recent-mentions
 (fn [db _]
   (set (keys (:recent-mentions db)))))

(reg-sub
 :recommended-relays
 (fn [db _]
   (:recommended-relays db)))

(reg-sub
 :private-key
 (fn [db _]
   (some->> (:private-key db) (n/encode-hex "nsec"))))

(reg-sub
 :note-info-raw
 (fn [db [_ note-id]]
   (with-out-str (pprint (dissoc (get-in db [:notes (:showing-raw-note-info db)]) :content)))))

(defn placeholder [id]
  {:name  (n/encode-hex "npub" id)
   :id id
   :content ""   
   :display [:div.level.mt-2 [:div.icon.level-item [:ion-icon {:name "cloud-offline-outline" :style {:font-size "32px"}}]]]})

(defn note-decorator [reactions name-pics follows]
  (fn [note]
    (-> (assoc note :reactions (get reactions (:id note)))
        (merge (get name-pics (:pubkey note)))
        (assoc :follows (follows (:pubkey note)))
        n/project-note)))

(reg-sub
 :get-note
 :<- [:notes-with-replypubs]
 :<- [:pubs-to-names-pics]
 :<- [:reactions]
 :<- [:own-follows-set]
 (fn [[notes pubs-names reactions follows][_ note-id]]
   (let [note (get notes note-id)]
     ;; (pprint note)
     (if note       
       ((note-decorator reactions pubs-names follows) note)
       (placeholder note-id)))))

(reg-sub
 :user-metadata
 :<- [:metadata]
 (fn [metadata [_ pubkey]]
   (assoc (get metadata pubkey) :npubkey (n/encode-hex "npub" pubkey))))

(reg-sub
 :own-metadata
 :<- [:metadata]
 :<- [:own-pubkey]
 (fn [[metadata pubkey] _]
   (assoc (get metadata pubkey) :npubkey (n/encode-hex "npub" pubkey))))

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
 :is-following?
 :<- [:own-follows-set]
 (fn [follows [_ pubkey]]
   (follows pubkey)))
   

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
; '([name count idx])
 :popular-relays
 :<- [:contacts-relays]
 :<- [:recommended-relays]
 (fn [[relays recommend] _]
   (->> (merge-with + recommend relays)
        (sort-by last)
        reverse
        (take 20)
        (map-indexed #(conj %2 (inc %1))))))
;; (map-indexed #(conj %2 (inc %1)) (take 20 (reverse (sort-by last relays))))))

(reg-sub
 :get-follows
 :<- [:own-contacts]
 :<- [:metadata]
 (fn [[contacts metadata] _]
   (->> (map #(merge % (get metadata (:pubkey %))) contacts)
        (map #(assoc % :npubkey (n/encode-hex "npub" (:pubkey %)))))))

(reg-sub
 :user-notes
 :<- [:notes] 
 (fn [notes [_ pubkey]]
   (filter #(= (:pubkey %) pubkey) notes)))

(reg-sub
 :convo-with-pubkey
 :<- [:messages]
 :<- [:own-pubkey]
 (fn [[messages own-pubkey] [_ pubkey]]
   (->> (filter #(or (= pubkey (:pubkey-from %)) (= pubkey (:pubkey-to %))) messages)
        (sort-by :created-at)
        (map #(assoc % :time (-> % :created-at secs-to-date))))))

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
  (let [m (group-by #(-> (get p-to-n %) empty?) (map first ps))
        found (map #(n/truncate-string % 15) (map #(get p-to-n %) (get m false)))
        not-found (get m true)
        not-found-npubs (map #(n/truncate-string (n/encode-hex "npub" (first %)) 15) ps)
        cnt-found (count (vals found))]
    (if (> (count ps) 4)
      (if (> cnt-found 4)
        (conj (take 4 found) (str " and " (- (count ps) 4) " others"))
        (concat found (take (- 4 cnt-found) not-found-npubs) (list (str "and " (- (count ps) 4) " others"))))
      (concat found (take (- 4 cnt-found) not-found-npubs)))))

(reg-sub
 :notes-with-replypubs
 :<- [:notes]
 :<- [:pubs-to-names]
 (fn [[notes pubs-to-names] _]
   (zipmap (keys notes)
           (map (fn [note]
                  (update-in note [:replying] #(replying-str pubs-to-names %)))
                (vals notes)))))

                                        ;=== view functions
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
        (map n/project-note)
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
                                                            (n/project-note (merge q (name-pic (:pubkey q))))))))
          (map (note-decorator reactions names-pics follows))
          (map #(if (= note-id (:id %)) (assoc % :highlighted true) %))))))

(reg-sub
 :get-own-notes
 :<- [:own-pubkey]
 :<- [:notes-with-replypubs]
 :<- [:own-metadata]
 :<- [:reactions]
 :<- [:own-follows-set]
 (fn [[pubkey notes own-metadata reactions follows] _]
   (let [name-pic {pubkey own-metadata}] 
     (->> notes
          vals
          (filter #(= pubkey (:pubkey %)))
          (sort-by :created_at >)
          (map (note-decorator reactions name-pic follows)) ))))

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
        (map #(assoc % :highlighted (contains? recent (:id %))))
        (sort-by (juxt :highlighted :created_at) >)
        (map (note-decorator reactions pubs-names follows)))))

(reg-sub
 :private-convos
 :<- [:messages]
 :<- [:own-pubkey]
 (fn [[messages pubkey]]
   (disj (->> (map #((juxt :pubkey-from :pubkey-to) %) messages)
              flatten
              (into #{})) pubkey)))

(reg-sub
 :login-state
 (fn [db _]
   (:login-state db)))

(reg-sub
 :logging-in
 :<- [:login-state]
 (fn [login-state _]
   (if (= :logging-in login-state) {:class "is-active"})))

(reg-sub
 :logged-in-hidden
 :<- [:login-state]
 (fn [login-state _]
   (if (= :logged-in login-state) {:class "is-hidden"})))

(reg-sub
 :is-logged-in
 :<- [:login-state]
 (fn [login-state _]
   (= :logged-in login-state)))

(reg-sub
 :creating-account
 :<- [:login-state]
 (fn [login-state _]
   (= :creating-account login-state)))

(reg-sub
 :providing-secret-key
 :<- [:login-state]
 (fn [login-state _]
   (= :providing-secret-key login-state)))

(reg-sub
 :logged-in-user
 :<- [:own-metadata]
 (fn [metadata _]
   (update-in metadata [:npubkey] #(n/truncate-string % 14))))

(reg-sub
 :user-settings
 :<- [:own-metadata]
 :<- [:own-relays]
 :<- [:private-key]
 (fn [[metadata user-relays private-key] _]
   ;; transform the user's relay metadata into a map with keys  [:relay-url :read :write]
   (let [relay-fn (juxt first #(-> % second :read) #(-> % second :write))
         relays (mapv #(zipmap [:relay-url :read :write] (relay-fn %)) user-relays)]
     (-> (assoc metadata :relays relays)
         (assoc :private-key private-key)))))

(reg-sub
 :configured-relays
  :<- [:relays]
 (fn [relays _]
   (for [[relay-url {:keys [read write]}] relays]
     {:url (subs relay-url 6)
      :read read
      :write write})))

(reg-sub
 :db-stats
 :<- [:metadata]
 :<- [:notes]
 :<- [:own-contacts]
 :<- [:reactions]
 :<- [:relays]
 (fn [[metadata notes contacts reactions relays] _]
   (with-out-str
     (pprint {:notes (count notes)
              :contacts (count contacts)
              :metadata (count metadata)
              :reactions (count reactions)}))))
