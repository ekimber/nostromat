(ns oberi.nostr.manager
  (:require [oberi.nostr.relay :as r]
            [reagent.core :as reagent]))

(def subs (reagent/atom {}))

(def log-atom (reagent/atom {}))

(defn log
  [key msg]
  (swap! log-atom (fn [a] (update-in a [key] #(conj % msg)))))

(defn end-msg? [msg])

(defn start-subscription [relay sub]
  (swap! subs #(assoc % sub {:state :reading}))
  (let [req-ch (:sink relay)
        res-ch (:source relay)
        out-ch (chan)]
    (go
      (if (>! req-ch (wire sub))
        ;; success
        (loop [data (<! res-ch)]
          (if-not (end-msg? data)
            (do (>! data out-ch)
                (recur)))))
      (close! res-ch)
      (close! req-ch)
      (close! out-ch))
    out-ch))

(defn replace-subscription [sub])

(defn end-subscription [sub])


  ;; (go (let [stream (<! (ws/connect "wss://nostr.slothy.win" {:format fmt/json}))]
  ;;       (>! (:sink stream) {:foo [1 2 3]})
  ;;       (js/console.log (pr-str (<! (:source stream))))
  ;;       (ws/close stream)))
