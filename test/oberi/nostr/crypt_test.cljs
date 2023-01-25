(ns oberi.nostr.crypt-test
  (:require [cljs.test :as t :refer-macros [deftest is async]]
            [cljs.core.async.interop :refer-macros [<p!]]
            ;; [oberi.nostr.crypt :as c]
            [oberi.nostr.nostr :as n]
            ["nostr-tools" :refer [nip04 getPublicKey generatePrivateKey]]
            [cljs.pprint :refer [pprint]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def alice-prv "747f40fd59de76296ec5017ed2cd7fa3262c09801362943e7273aa81bde349df")
(def bob-prv "622936409c8c6780df64d14f20a8a37de3b682ac49894260907ac8f43d8f3d3f")
(def alice-pub (n/public-key alice-prv))
(def bob-pub (n/public-key bob-prv))

(deftest encrypt-decrypt
  (async done
         (go
           (let [msg "hello"
                 encrypted (.encrypt nip04 alice-prv bob-pub "hello")]
             (is (= msg (<p! (.decrypt nip04 bob-prv alice-pub (<p! encrypted)))))))))
    
