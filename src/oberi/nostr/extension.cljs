(ns oberi.nostr.extension
  (:require
   [cljs.core.async :as async :refer [<! >! put! chan close! mult tap]]
   [cljs.core.async.interop :refer-macros [<p!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def extension-q (chan))

(defn call-extension
  [exec-fn]
  (let [promise (async/promise-chan)]
    (go
      (>! extension-q {:exec-fn exec-fn :promise promise}))
    promise))

(go-loop []
  (let [{:keys [exec-fn promise]} (<! extension-q)
        result (try
                 {:result (<p! (exec-fn))}
                 (catch js/Error e {:error e}))]
    (>! promise result))
  (recur))
