(ns oberi.nostr.nostr
  (:require [lambdaisland.deja-fu :as fu]
            [clojure.string :refer [blank? includes?]]
            [goog.crypt :as crypt]
            [reagent.core :as reagent]
            ["linkifyjs" :as link]
            ["@scure/base" :as b]
            ["@noble/secp256k1" :as secp]
            ["nanoid/generate" :as r]
            ["bolt11" :as inv]
            ["qrcode.react" :refer [QRCodeSVG]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [cljs.core.async :as async :refer [<!]]
            [cljs.pprint :refer [pprint]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def decoder (js/TextDecoder. "utf-8"))
(def encoder (js/TextEncoder. "utf-8"))

(defn sha256 [s]
  (let [sha256 (crypt/Sha256.)]
    (.update sha256 s)
    (.digest sha256)))

(defprotocol Message
  (wire [m]))

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

(defn truncate-string [s len]  
  (if (or (nil? s) (> 9 len) (> (+ 3 len) (count s)))
    s
    (let [end-size (/ (- len 3) 2)
          start (subs s 0 end-size)
          end (subs s (- (count s) end-size) (count s))]
      (str start "…" end))))

(defn truncate-end [s len]
  (if (and s (> (count s) (inc len)))
    (str (subs s 0 len) "…")
    s))

(defn encode-hex [prefix hex]
  (.encode b/bech32 prefix (->> (.hexToBytes secp/utils hex)
                                (.toWords b/bech32)) 1500))
(defn decode-b32 [b32]
  (->> (.decode b/bech32 b32)
      .-words
      (.fromWords b/bech32)
      (.bytesToHex secp/utils)))

(defn short-date [epoch-sec]
  (-> (js/goog.date.DateTime.fromTimestamp (* 1000 epoch-sec))
      (fu/format "dd MMM")))

(defn reactions-to-text [reactions]
  {:likes (count (:likes reactions))
   :dislikes (count (:dislikes reactions))
   :other-text (apply str (interpose " " (for [[sym p-set] (:other reactions)
                                               :let [cnt (count p-set)]]
                                           (str sym (if (> cnt 1) (str " " cnt) "")))))})

(defn decode-invoice [text]
  (if-let [bolt11 (re-find #"lnbc[a-zA-Z0-9]+" text)]
    (try
      (merge (js->clj (.decode inv bolt11) :keywordize-keys :true) {:paymentRequest bolt11})
      (catch js/Error e (pprint {:error "Could not decode invoice"
                                 :msg e}))))) ;; paranoia!!
    
(defn url-elem [href]  
  (if (re-find #".*\.(png|jpeg|GIF|gif|jpg)$" href)
    [:img.is-align-self-baseline {:src href :style {:max-height 240 :max-width 480} :data-reactid href}]
    [:a {:href href :target "_blank" :on-click #(.stopPropagation %) :data-reactid href} href]))

(defn insert-urls [s urls offset]
  (let [url (first urls)]
    (if (seq (rest urls))
      (conj (insert-urls s (rest urls) (:end url))
              ^{:key (str offset (:href url))}[url-elem (:href url)]
              ^{:key (str (subs s 16))}(subs s offset (:start url)))
        (list ^{:key (:href url)}[url-elem (:href url)] (subs s (:end url))))))

(defn url-find [content]
  (js->clj (link/find (clj->js content) "url") :keywordize-keys true))

(defn create-display-element [content urls]  
  (let [invoice (re-find #"lnbc[a-zA-Z0-9]+" content)
        rep-ctnt (if invoice (clojure.string/replace content invoice "") content)]
    [:span.m-0 {:style {:word-break "break-word" :text-align "left"}} 
     (if (seq urls) (insert-urls content urls 0) rep-ctnt)]))

(defn at-ref []) 

(defn project-note [note]
  (if (nil? (:content note)) (pprint {:nil? note}))
  (let [age-seconds (- (/ (.now js/Date) 1000)  (:created_at note))
        readable-age (cond
                       (< age-seconds 60) (str (int age-seconds) "s")
                       (< age-seconds 3600) (str (int (/ age-seconds 60)) "m")
                       (< age-seconds 86400) (str (int (/ age-seconds 3600)) "h")
                       :else (short-date (:created_at note)))
        content (:content note)
        urls (url-find content)]
    
    {:id (:id note)
     :invoice (decode-invoice content)
     :highlighted (:highlighted note)
     :pubkey (:pubkey note)
     :npubkey (if-let [p (:pubkey note)] (encode-hex "npub" p))
     :display (if-let [d (:display note)] d (create-display-element content urls))
     :age readable-age
     :reactions (reactions-to-text (:reactions note))
     :replying (:replying note) ;; (map #(truncate-end % 17) (:replying note))
     :name (truncate-end (:name note) 19)
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
  ;; e id relay? type?
  ;; p id relay?
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
