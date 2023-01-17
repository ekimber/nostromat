(ns oberi.nostr.relay
  (:require
   [re-frame.core :as re-frame]   
   [reagent.core :as reagent]
   [chord.client :refer [ws-ch]]
   [cljs.pprint :refer [pprint]]
   [cljs.core.async :as async :refer [<! >! put! chan close! mult tap]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def >evt re-frame/dispatch)

(def relays-atom (reagent/atom {}))

(defn get-relay [relay]
  (get @relays-atom relay))

(defn get-relays [rw]
  (for [relay @relays-atom :when (-> relay second :rw-map (get rw))] 
        (first relay)))

(defn is-connected? [relay]
  (= :connected (get-in @relays-atom [relay :state])))

(defn connect! [relay rw-map]
  (go
    (let [{:keys [ws-channel error]} (<! (ws-ch relay {:format :json-kw}))]
      (if error
        (do
          (swap! relays-atom
                 #(-> (assoc-in % [relay :state] :disconnected)
                      (assoc-in [relay :channel] ws-channel)
                      (assoc-in [relay :rw-map] rw-map)))         
          (pprint (str "Error [ " error " ] connecting " relay " relay")))          
        (do
          (swap! relays-atom
                 #(-> (assoc-in % [relay :state] :connected)
                      (assoc-in [relay :channel] ws-channel)
                      (assoc-in [relay :rw-map] rw-map)))
          (>evt [:relay-connected relay rw-map]))))))

(defn disconnect! [relay]
  (swap! relays-atom #(assoc-in % [relay :state] :disconnected)))
