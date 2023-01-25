(ns oberi.nostr.manager
  (:require [oberi.nostr.utils :as u]
            [clojure.walk :refer [keywordize-keys]]
            [cljs.spec.alpha :as s]
            [haslett.client :as ws]
            [haslett.format :as fmt]
            [cljs.core.async :as async :refer [<! >! put! chan close! mult tap]]
            [reagent.core :as reagent]
            [re-frame.core :as rf]
            [cljs.pprint :refer [pprint cl-format]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(s/def :unq/read boolean?)
(s/def :unq/write boolean?)
(s/def :unq/rw-map (s/keys :req-un [:unq/read :unq/write]))

(def conns (reagent/atom {}))

(def log-atom (reagent/atom {}))

(def connection-mgr-q (chan))

(def string-fmt
  "Stringifies writes, clojurify reads"
  (reify fmt/Format
    (read  [_ s] (js->clj (js/JSON.parse s)))
    (write [_ v] (.stringify js/JSON v))))

(defn log
  [key msg]
  (swap! log-atom (fn [a] (update-in a [key] #(conj % msg)))))

(defn end-msg? [msg])

(defprotocol Subscription
  (subscribe-msg [s])
  (close-msg [s])
  (msg-filter [s])
  (msg-xform [s])
  (end-msg? [s])
  (timeout-chan [s]))

(defrecord Connection [id source sink])

(defn event-filter-and-convert [s]
  (comp (filter #(= ["EVENT" (:id s)] (subvec % 0 2))) (map #(nth % 2)) (map keywordize-keys)))

;; A request - so this terminates on EOSE
(defrecord Request [id filt]
  Subscription
  (subscribe-msg [s] #js["REQ" (:id s) filt])
  (close-msg [s] #js["CLOSE" (:id s)])
  (msg-filter [s] (filter #(= (:id s) (second %))))
  (msg-xform [s] (comp (map #(nth % 2)) (map keywordize-keys)))
  (end-msg? [s] #(and (= "EOSE" (first %)) (= (:id s) (second %))))
  (timeout-chan [s] (async/timeout 60000)))

(defrecord IndefiniteSub [id filt]
  Subscription
  (subscribe-msg [s] #js["REQ" (:id s) filt])
  (close-msg [s] #js["CLOSE" (:id s)])
  (msg-filter [s] (event-filter-and-convert s))
  (msg-xform [s] identity)
  (end-msg? [s] (fn [_] false))
  (timeout-chan [s] (chan))) ;;will never time out

(defn closed? [{:keys [socket-id]}]
  (= :disconnected (get-in @conns [socket-id :state])))

(defn connect-timeout []
  (async/timeout 5000))

(defn connection-failed [conn socket-id reason]  
  (swap! conns #(assoc-in % [socket-id :state] :disconnected))
  (pprint {:socket-id socket-id :msg "connection failed" :reason reason})
  (close! conn))

(defn new-connection [socket-id {:keys [read write]} & {:keys [on-connect on-disconnect] :as opts}]
  (let [conn (async/promise-chan)]
    (swap! conns #(assoc-in % [socket-id] {:state :connecting :conn conn :opts opts}))
    (go
      (let [[val port] (async/alts! [(ws/connect socket-id {:format string-fmt}) (connect-timeout)])]
        (if (nil? val)
          (connection-failed conn socket-id "timed out")
          (let [{:keys [socket source sink close-status]} val            
                multi (mult source)]
            (if-not (async/poll! close-status)
              (do
                (swap! conns #(assoc-in % [socket-id] {:state :connected :read read :write write
                                                       :multi multi :sink sink :socket socket :conn conn :source source}))
                (>! conn {:multi multi :sink sink :socket-id socket-id})
                (if on-connect (on-connect conn))
                (let [t (tap multi (chan))]
                  (loop []
                    (when-let [v (<! t)]                    
                      (when (= "NOTICE" (first v)) (pprint {:notice v :from socket-id}))
                      (recur))))
                (cl-format true "Connection to ~S ended, reason: ~S, on disconnect ~S" socket-id (<! close-status) on-disconnect)
                (swap! conns #(assoc-in % [socket-id :state] :disconnected))
                (if on-disconnect
                  (when (= (on-disconnect socket-id) ::reconnect)
                    (>! connection-mgr-q [::reconnect socket-id 30000 {:read read :write write} opts]))
                  (cl-format true "No on-disconnect hook for socket ~S" socket-id)))
              (connection-failed conn socket-id (async/poll! close-status)))))))
    conn))

(defn connect [relay-url rw-map & {:keys [on-connect on-disconnect] :as opts}]
  (if-let [relay (get-in @conns [relay-url])] ;;TODO race condition!
    (case (-> relay :state)
      :connected (-> relay :conn)
      :connecting (-> relay :conn)
      :disconnected (new-connection relay-url rw-map opts)
      nil)
    (new-connection relay-url rw-map opts)))

(defn disconnect [relay-url]
  (if-let [relay (get-in @conns [relay-url])]
    (if (-> relay :state (= :connected))
      (do
        (-> relay :sink close!)
        (-> relay :source close!)
        (swap! conns #(assoc % relay-url {:state :disconnected}))))))

(defn disconnect-all []
  (doseq [relay (keys @conns)]
    (disconnect relay)))

(defn get-conns [conns-map rw]
  (map :conn (filter rw (vals conns-map))))

(defn read-loop [{:keys [source out sink]} sub socket-id]
  (swap! conns #(assoc-in % [socket-id :subs (:id sub)] {:state :reading :source source}))
  (let [end? (end-msg? sub)]
    (go
      (loop []
        (let [timeout (timeout-chan sub)
              [data port] (async/alts! [source timeout])]                             
          (when (= timeout port) (cl-format true "Sub ~S timed out on socket ~S" (:id sub) socket-id))
          (when (not (or (nil? data) (end? data)))
            (>! out data)
            (recur))))
      (>! sink (close-msg sub))
      (swap! conns #(assoc-in % [socket-id :subs (:id sub)] {:state :complete :source source}))
      (close! out)
      (close! source))))

(defn start-subscription [{:keys [multi sink socket-id]} sub]
  ;; (pprint {:start sub})
  ;; TODO if sub is current running
  (if (as-> (get-in @conns [socket-id :subs (:id sub) :state]) s (or (= s :reading) (= s :subscribing)))
    (do (pprint {:warning "Already running!" :sub sub :on-socket socket-id})
        (async/timeout 0)) ;; return a closed channel
    (when (-> (get @conns socket-id) :read)
      (swap! conns #(assoc-in % [socket-id :subs (:id sub)] {:state :subscribing}))
      (let [sub-filter (msg-filter sub)
            chans {:source (tap multi (chan 10 sub-filter))
                   :out (chan 100 (msg-xform sub))
                   :sink sink}]
        (go      
          (if (>! sink (subscribe-msg sub socket-id))
            ;; success                          
            (read-loop chans sub socket-id)
            (do ;; failure
              (swap! conns #(assoc-in % [socket-id :subs (:id sub)] {:state :disconnected}))
              (close! (:out chans))
              (close! (:source chans)))))
        (:out chans)))))

(defn replace-subscription [sub])

(defn end-subscription [socket-id sub]
  (if-let [sub (get-in @conns [socket-id :subs (:id sub)])]
    (if (= (:state sub) :reading)
      (close! (:source sub))
      (cl-format true "Subscription ~S already closed or not ready" (:id sub)))))

(defn subscribe-multi
  ([sub] (let [out (chan 100)]
           (go
             (let [cs (<! (async/map vector (get-conns @conns :read)))]
               (async/pipe (subscribe-multi cs sub) out)))
           out))
  ([relay-conns sub]
   (async/pipe
    (async/merge
     (remove nil? (map #(start-subscription % sub) relay-conns)) 100)
    (chan 100 (u/distinct-by :id)))))

;; sub-fn must accept an array of batched item and produce a subscription
;; function returns an assoc containing `in` and `out` channels.
(defn subscribe-batch [{:keys [multi sink socket-id] :as conn} sub-fn]
  (pprint ["Subscribe batch to " socket-id])
  (let [in (chan 10 (distinct))
        out (chan 10)
        batch-c (u/batch in (chan) 500 20)]
    (go-loop []     
      (let [batch (<! batch-c)]
        (when (seq batch)
          (async/onto-chan!
           out (<! (async/into [] (or (start-subscription conn (sub-fn batch)) (async/timeout 0)))) false)))
      (if (closed? conn)
        (close! out)
        (recur)))
    {:in in :out out}))

(defn batch-multi [relay-conns sub-fn]
  (let [in-out-chans (map #(subscribe-batch % sub-fn) relay-conns)
        ins (into [] (map :in in-out-chans))
        outs (into [] (map :out in-out-chans))
        in (chan 100)
        mult-in (async/mult in)]
    (doseq [in-chan ins]
      (tap mult-in in-chan))
    {:in in
     :out (async/pipe (async/merge outs 100) (chan 100 (u/distinct-by :id)))}))

(defn write-to-relays [msg]
  (let [relays (->> @conns vals (filter :write) (filter #(= :connected (:state %))))]
    (go (doseq [relay relays]
          (-> relay :sink (>! msg))))))

(defn validate-relay-list [relay-list]
  (and (every? #(s/valid? :unq/rw-map %) (vals relay-list))
       (not-any? nil? (keys relay-list))))

;; it's a map not a list btw
(defn sync-relay-list [relay-list & {:keys [on-connect on-disconnect] :as opts}]
  (if-not (validate-relay-list)
    (pprint "Invalid relay list")    
    (let [current @conns
          new-relays (remove #(contains? current %) (keys relay-list))
          to-remove (remove #(contains? relay-list %) (keys current))]
      (doseq [relay to-remove]
        (disconnect relay)
        (swap! conns #(dissoc % relay))
        (cl-format true "Disconnected ~S relay" relay))
      (doseq [relay new-relays]
        (connect relay (get relay-list relay) opts)))))

(defn reconnect-check? [socket-id]
  (let [sock (get @conns socket-id)]
    (and sock (== :disconnected (:state sock)))))

(go-loop []
  ;; infinite re-connection loop
  (let [[cmd socket-id interval rw-map opts] (<! connection-mgr-q)]
    (when (and (= ::reconnect cmd) (contains? @conns socket-id))
      (cl-format true "Reconnecting socket ~S after ~Sms" socket-id interval)
      (<! (async/timeout interval))
      (when (reconnect-check? socket-id)
        (cl-format true "Reconnecting socket ~S with opts ~S now" socket-id opts)
        (connect socket-id rw-map opts)))
    (recur)))
