(ns oberi.nostr.relay
  (:require
   [oberi.nostr.nostr :as n]
   [re-frame.core :as re-frame]   
   [reagent.core :as reagent]
   [chord.client :refer [ws-ch]]
   [cljs.pprint :refer [pprint]]
   [cljs.core.async :as async :refer [<! >! put! chan close! mult tap]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def >evt re-frame/dispatch)

(def relays-atom (reagent/atom {}))

(def metadata-query-q (chan))

(def note-query-q (chan))

(defn get-relay [relay]
  (get @relays-atom relay))

(defn get-relays [rw]
  "Get relays matching either :read or :write"  
  (for [relay @relays-atom :let [m (-> relay second)] :when (and (-> m :rw-map (get rw)) (-> m :state (= :connected)))] 
        (first relay)))

(defn is-connected? [relay]
  (= :connected (get-in @relays-atom [relay :state])))

(defn connected-relays []
  (map first (filter #(-> % second :state (= :connected)) @relays-atom)))

(defn close-ws! [relay-url]
  (if-let [ch (get-in @relays-atom [relay-url :channel])]
    (close! ch)))

(defn connect!
  ([relay-url]
   (connect! relay-url (get-in @relays-atom [relay-url :rw-map])))
  ([relay-url rw-map]
   (go
     ;; close any existing channel
     (close-ws! relay-url)    
     (let [{:keys [ws-channel error]} (<! (ws-ch relay-url {:format :json-kw :read-channel (chan 100) :write-channel (chan 5)}))]
       (if error
         (do
           (swap! relays-atom
                  #(-> (assoc-in % [relay-url :state] :disconnected)
                       (assoc-in [relay-url :channel] ws-channel)
                       (assoc-in [relay-url :rw-map] rw-map)))         
           (pprint (str "Error [ " error " ] connecting " relay-url " relay")))          
         (do
           (swap! relays-atom
                  #(-> (assoc-in % [relay-url :state] :connected)
                       (assoc-in [relay-url :channel] ws-channel)
                       (assoc-in [relay-url :rw-map] rw-map)))
           (>evt [:relay-connected relay-url rw-map])))))))

(defn set-rw-map! [relay-url rw-map]
  (swap! relays-atom #(assoc-in % [relay-url :rw-map] rw-map)))

(defn disconnect! [relay-url]
  (swap! relays-atom #(assoc-in % [relay-url :state] :disconnected))
  (if-let [ch (get-in @relays-atom [relay-url :channel])]
    (async/close! ch)))

(defn shutdown! [relay-url]
  (disconnect! relay-url)
  (swap! relays-atom #(dissoc % relay-url)))

(defn try-to-reconnect! [relay-url]
  (disconnect! relay-url)
  (go
    (<! (async/timeout 6000))
    (connect! relay-url)))

;; == msg sending (read)

(defn send-to-relay [relay-url msg]
  (let [ {:keys [state rw-map channel]} (get-relay relay-url)]
    (if (or (= :disconnected state) (nil? channel))
      (pprint ["Send error" {:state state :channel channel :relay relay-url :msg (subs (str (js->clj msg)) 0 40)}]) 
      (go (>! channel msg)))))

(defn send-to-active-relays [msg]
   (doseq [relay-url (get-relays :read)]
     (send-to-relay relay-url msg)))

(go-loop []
  (let [batch (<! (async/into [] (async/take 20 metadata-query-q)))
        q (n/->QueryRequest (n/->Subscription "metabatch" false) {:authors (distinct batch) :kinds [0]})]
    (if (< 0 (count batch))
      (send-to-active-relays (n/wire q))))
  (recur))

(def batch-q (chan))

;; (go-loop []
  ;; (let [;; closeable (chan) (async/pipe note-query-q closeable)
  ;;       timeout-c (async/timeout 500)]  
  ;;   ;; (go (<! timeout-c)
  ;;   ;;     (async/close! closeable))
  ;;   (let [take-ch (async/take 10 note-query-q)
  ;;         batch-ch (async/into [] take-ch)
  ;;         [v c] (alts! [batch-ch timeout-c])
  ;;         a (pprint {:v v :c c})
  ;;         batch (if v
  ;;                 ;; the batch filled
  ;;                 (do
  ;;                   (async/close! timeout-c)
  ;;                   v)
  ;;                 (do
  ;;                   (async/close! take-ch)
  ;;                   (async/close! batch-ch)
;;                   (<! batch-ch)))    
;; (let [batch (<! (async/into [] (alts! (async/take 10 closeable)))
(go-loop []
  (let [batch (<! (async/into [] (async/take 4 note-query-q)))
        q (n/->QueryRequest (n/->Subscription "notebatch" false) {:ids (distinct batch) :kinds [1]})]
    (if (< 0 (count batch))
      (send-to-active-relays (n/wire q))))
  (recur))
