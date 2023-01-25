(ns oberi.nostr.utils
  (:require
   [cljs.core.async :as async :refer [>! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn batch [in out max-time max-count]
  (let [lim-1 (dec max-count)]
    (async/go-loop [buf [] t (async/timeout max-time)]
      (let [[v p] (async/alts! [in t])]
        (cond
          (= p t)
          (do
            (async/>! out buf)
            (recur [] (async/timeout max-time)))
          
          (nil? v)
          (if (seq buf)
            (async/>! out buf))

          (== (count buf) lim-1)
          (do
            (async/>! out (conj buf v))
            (recur [] (async/timeout max-time)))
          
          :else
          (recur (conj buf v) t)))))
  out)

(defn distinct-by
  "Returns a lazy sequence of the elements of coll, removing any elements that
  return duplicate values when passed to a function f. Returns a transducer
  when no collection is provided."
  ([f]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result x]
          (let [fx (f x)]
            (if (contains? @seen fx)
              result
              (do (vswap! seen conj fx)
                  (rf result x)))))))))
  ([f coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                 ((fn [[x :as xs] seen]
                    (when-let [s (seq xs)]
                      (let [fx (f x)]
                        (if (contains? seen fx)
                          (recur (rest s) seen)
                          (cons x (step (rest s) (conj seen fx)))))))
                  xs seen)))]
     (step coll #{}))))

(defn merge-promises [promises]
  (let [p-out (async/promise-chan)
        c (async/chan)]
    (go
      (doseq [p promises]
        (let [v (<! p)]
          (cljs.pprint/pprint v)
          (>! c v)))
      (async/close! c)
      (>! p-out (async/into '() c)))
    p-out))

  
  
