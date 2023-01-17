(ns oberi.nostr.storage
  (:require [cljs.reader]
            [re-frame.core :refer [reg-cofx]]))

(def user-store-key "oberi-nostr-user") 

(defn set-user-ls
  [user]
  (.setItem js/localStorage conduit-user-key (str user)))

(reg-cofx
 :local-store-user
 (fn [cofx _]
   (assoc cofx :local-store-user  ;; put the local-store user into the coeffect under :local-store-user
          (into (sorted-map)               
                (some->> (.getItem js/localStorage user-store-key)
                         (cljs.reader/read-string)))))) 
