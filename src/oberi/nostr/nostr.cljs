(ns oberi.nostr.nostr
  (:require [lambdaisland.deja-fu :as fu]
            [clojure.string :refer [blank? includes?]]
            [goog.crypt :as crypt]
            ["linkifyjs" :as link]
            ["@scure/base" :as b]
            ["@noble/secp256k1" :as secp]
            ["nanoid/generate" :as r]
            [cljs.core.async.interop :refer-macros [<p!]]
            [cljs.core.async :as async :refer [<!]]
            [cljs.pprint :refer [pprint]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

;; Filters object
;; {
;;   "ids": <a list of event ids or prefixes>,
;;   "authors": <a list of pubkeys or prefixes, the pubkey of an event must be one of these>,
;;   "kinds": <a list of a kind numbers>,
;;   "#e": <a list of event ids that are referenced in an "e" tag>,
;;   "#p": <a list of pubkeys that are referenced in a "p" tag>,
;;   "since": <a timestamp, events must be newer than this to pass>,
;;   "until": <a timestamp, events must be older than this to pass>,
;;   "limit": <maximum number of events to be returned in the initial query>
;; }
;; {
;;   "id": <32-bytes sha256 of the the serialized event data>
;;   "pubkey": <32-bytes hex-encoded public key of the event creator>,
;;   "created_at": <unix timestamp in seconds>,
;;   "kind": <integer>,
;;   "tags": [
;;     ["e", <32-bytes hex of the id of another event>, <recommended relay URL>],
;;     ["p", <32-bytes hex of the key>, <recommended relay URL>],
;;     ... // other kinds of tags may be included later
;;   ],
;;   "content": arbitrary string,
;;   "sig": <64-bytes signature of the sha256 hash of the serialized event data, which is the same as the "id" field>
;;  }

(def decoder (js/TextDecoder. "utf-8"))
(def encoder (js/TextEncoder. "utf-8"))

(defn sha256 [s]
  (let [sha256 (crypt/Sha256.)]
    (.update sha256 s)
    (.digest sha256)))

(defprotocol Message
  (wire [m]))

;; (defprotocol RequestMessage
;;   (subscription-id [m]))

(defn to-json [obj]
  (.stringify js/JSON (clj->js obj)))
   
(defprotocol EventMessage
  (id [m])
  (sig [m key])
  (sign [m key])
  (verify [m])
  (->js [m]))

(defrecord NostrEvent [pubkey created_at kind tags content]
  EventMessage
  (id [m] (go (or (:id m)
                  (->> #js[0 pubkey created_at kind (clj->js (or tags [])) (or content "")]
                       (.stringify js/JSON)
                       (.encode encoder)                   
                       (.sha256 secp/utils)
                       <p!
                       (.bytesToHex secp/utils)))))
  (sig [m key] (go (.bytesToHex secp/utils (<p! (.sign secp/schnorr (<! (id m)) key)))))
  (sign [m key] (go (let [hashed (assoc m :id (<! (id m)))]
                      (assoc hashed :sig (<! (sig hashed key))))))
  (verify [m] (go (<p! (.verify secp/schnorr (:sig m) (:id m) (:pubkey m)))))
  (->js [m] (go #js{ "id" (<! (id m))
                    "pubkey" (:pubkey m)
                    "created_at" (:created_at m)
                    "kind" (:kind m)
                    "tags" (clj->js (:tags m))
                    "content" (:content m)
                    "sig" (:sig m)
                    })))

(extend-protocol Message
  NostrEvent
  (wire [m] (to-json (conj (vals m) 0))))

(defrecord Subscription [id perpetual])

(defrecord QueryRequest [^Subscription subscription filter]
  Message
  (wire [m] ["REQ" (-> m :subscription :id) filter]))

(defrecord CloseRequest [^Subscription subscription]
  Message
  (wire [m] ["CLOSE" (-> m :subscription :id)]))

(defn truncate-string [s len]  
  (if (or (nil? s) (> 9 len))
    s
    (let [end-size (/ (- len 3) 2)
          start (subs s 0 end-size)
          end (subs s (- (count s) end-size) (count s))]
      (str start "…" end))))

(defn encode-hex [prefix hex]
  (.encode b/bech32 prefix (->> (.hexToBytes secp/utils hex)
                                (.toWords b/bech32)) 1500))
(defn decode-b32 [b32]
  (->> (.decode b/bech32 b32)
      .-words
      (.fromWords b/bech32)
      (.bytesToHex secp/utils)))
  

(defn ^RequestMessage author-req-msg [subs-id pubkey-list]
  ["REQ" subs-id {:authors pubkey-list
                  :limit 200}])

(defn ^RequestMessage metadata-req-msg [subs-id pubkey-list]
  ["REQ" subs-id {:authors pubkey-list
                  :limit 200
                  :kinds [0]}])

(defn ^RequestMessage fetch-notes-req [subs-id notes-list]
  ["REQ" subs-id ])

(defn short-date [epoch-sec]
  (-> (js/goog.date.DateTime.fromTimestamp (* 1000 epoch-sec))
      (fu/format "dd MMM")))

(defn reactions-to-text [reactions]
  {:likes (count (:likes reactions))
   :dislikes (count (:dislikes reactions))
   :other-text (apply str (interpose " " (for [[sym p-set] (:other reactions)
                                               :let [cnt (count p-set)]]
                                           (str sym (if (> cnt 1) (str " " cnt) "")))))})
  
(defn link-url [be4 href aft]
  [:span.m-0 {:style {:word-break "break-word" :text-align "left"}} be4
       [:a {:href href :target "_blank" :on-click #(.stopPropagation %)} href]
   aft])

(defn link-img [be4 href aft]
  [:div.is-flex.is-flex-direction-column      
   [:span.m-0 {:style {:word-break "break-word" :text-align "left"}} be4]
   [:img.is-align-self-baseline {:src href :style {:max-height 240 :max-width 480}}]
   [:span.m-0 {:style {:word-break "break-word" :text-align "left"}} aft]])

(defn insert-urls [s urls]
  (let [url (first urls)
        be4 (subs s 0 (:start url))
        aft (subs s (:end url))
        href (:href url)]
    (if (re-find #".*\.(png|jpeg|GIF|gif|jpg)$" href)
      (link-img be4 href aft)
      (link-url be4 href aft))))
                 

(defn project-note [note]
  (let [age-seconds (- (/ (.now js/Date) 1000)  (:created_at note))
        readable-age (cond
                       (< age-seconds 60) (str (int age-seconds) "s")
                       (< age-seconds 3600) (str (int (/ age-seconds 60)) "m")
                       (< age-seconds 86400) (str (int (/ age-seconds 3600)) "h")
                       :else (short-date (:created_at note)))
        content (:content note)
        urls (js->clj (link/find (clj->js content) "url") :keywordize-keys true)]
    
    {:id (:id note)
     :highlighted (:highlighted note)
     :pubkey (:pubkey note)
     :npubkey (if-let [p (:pubkey note)] (encode-hex "npub" p))
     :display (if-let [d (:display note)] d
                      (if-let [urls (seq urls)] (insert-urls content urls)
                              [:span.m-0 {:style {:word-break "break-word" :text-align "left"}} content]))
     :age readable-age
     :reactions (reactions-to-text (:reactions note))
     :replying (:replying note)
     :name (:name note)
     :picture (:picture note)
     :child (:child note)
     :following (:follows note)}))

(defn parse-contact-list [tags]
  (map #(zipmap [:p :pubkey :relay] %) tags))

(defn unparse-contact-list [l]
  (mapv (comp vec vals) l))

(defn keywordize-map-keys [m]
  (zipmap (map keyword (keys m)) (vals m)))

(defn parse-relay-list [relays]
    (if (seq relays)
      (try
        (let [json (js->clj (.parse js/JSON relays))]
          (zipmap (keys json) (map keywordize-map-keys (vals json))))
        (catch :default e
          (pprint ["Error parsing JSON" relays])
          []))
      []))

(defn unparse-relay-list [relays]
  (if-not relays "" (.stringify js/JSON (clj->js relays))))
 
(defn relay-list-from-settings [settings-r]
  (reduce #(assoc %1 (:relay-url %2) (select-keys %2 [:read :write])) {} settings-r))

(defn user-metadata-req [subs-id pubkey]
  ["REQ" subs-id {:authors [pubkey]
                  :kind 0
                  :limit 1}])

(defn parse-tags-element [tags]  
 (into {} (map #(vector (keyword (first %)) (second %)) tags)))

(defn e-tag [tag]
  ;; id relay type
  (case (nth tag 2 nil)
    "mention" {:child-id (first tag) :child-relay (second tag)}
    "root"   {:root-id (first tag) :root-relay (second tag)}
    "reply"  {:reply-id (first tag) :reply-relay (second tag)}
    {:link (first tag) :link-relay (second tag)}))

(defn parse-notes-tags [tags]
  ;; e id relay type
  (let [g (group-by first tags)
        ees (map (comp e-tag rest) (get g "e"))
        pees (map rest (get g "p"))]
    (-> (apply merge ees)
        (dissoc :link)
        (merge {:links (remove #(or (nil? %) (blank? %)) (map :link ees))
                :replying (distinct pees)}))))

(defn generate-reply-tags [tags note-id pubkey]
 (let [etags (filterv #(= "e" (first %)) tags)
       ptags (filterv #(= "p" (first %)) tags)
       root-tag (some #(if (= "root" (get % 3)) %) tags)]
   (into (if root-tag
           (conj [root-tag] ["e" note-id "" "reply"])
           (if (seq etags) (conj [(first etags)] ["e" note-id]) [["e" note-id]]))
         (conj ptags ["p" pubkey]))))

(defn send-to-relay-fx [query relays]
  (mapv #(vector :send-to-relay [% (wire query)]) relays))

(defn generate-key []
  (r "1234567890abcdef" 64))

(defn public-key [private-key]
  (.bytesToHex secp/utils (.getPublicKey secp/schnorr private-key)))

(defn contact-feed-req [pubkey contact-list]
  (->QueryRequest (->Subscription (str "cntfd" pubkey) true)
                        {:authors (conj (mapv :pubkey contact-list) pubkey)
                         :kinds [1 2 6 7] :limit 150}))

(defn contact-list-req [pubkey contact-list]
  (->QueryRequest (->Subscription (str "cnt" pubkey) true)
                        {:authors (conj (mapv :pubkey contact-list) pubkey)
                         :kinds [0 3] :limit (max 5 (inc (count contact-list)))}))

(defn private-msg-req [pubkey]
  (->QueryRequest (->Subscription (str "prv" pubkey) true)
                        {:kinds [4] :#p [pubkey] :limit 100}))
