(ns oberi.nostr.nostr-test
  (:require [cljs.test :as t :refer-macros [deftest is async]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [oberi.nostr.nostr :as n]
            [cljs.pprint :refer [pprint]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def note-event
  {
  :id "2d578a83c021cbd550ab4b2162432ad3afb3e7e6d6d1fed74ab6fc6ce7fa6f92"
  :pubkey "917309413fce216b66ba467f940ffa0c6ef9741aa2f547613894f22f04209eb5"
  :created_at 1673596541
  :kind 1
  :tags [["p" "a7f72cd8c8c7cf18fa6f44c131e01d5b88c2f47723a56626ef33d6990e6a9f15" "wss://nostr.onsats.org"]
         ["p" "d307643547703537dfdef811c3dea96f1f9e84c8249e200353425924a9908cf8" "wss://nostr.onsats.org"]
         ["e" "1c9a73fb9d999fc4ab930c9bf0cede1efe3d53d55f4398b6be8427cedcfa4d1f" "wss://nostr-relay.wlvs.space" "root"]
         ["e" "1711344fd5fef76cb54f5fbfd77032c91d41a1ee032878d42f12572737633952" "wss://nostr-pub.semisol.dev" "reply"]]
   :content "This is why I create a new private nostr key every day",
   :sig "ca95200dfa9186261d846270e91b6cf51f2b034026ca104c42a810bfdf294df67956b99462457f81e7513407e7dc3e532107af8e9a463d5dcedc1e50ce99e9ab"})

(def private-key "49f99501d5a5478f4968a4f84bdc528ed9e1e01bfc0d4fedf12c41d95e2421ee")

(def relay-list-json "{\"wss://student.chadpolytechnic.com\":{\"write\":true,\"read\":true},\"wss://relay.stoner.com\":{\"write\":true,\"read\":true},\"wss://nostr.fmt.wiz.biz\":{\"write\":true,\"read\":true},\"wss://nostr-relay.wlvs.space\":{\"write\":true,\"read\":true},\"wss://nostr.bitcoiner.social\":{\"write\":true,\"read\":true}}")

(deftest generate-a-sha-id
  (async done
         (go
           (is (= (:id note-event)
                  (<! (n/id (n/map->NostrEvent (-> (dissoc note-event :sig :id)))))))
           (done))))

(deftest sign-and-verify
  (async done
         (go
           (let [unsigned (n/map->NostrEvent (-> (dissoc note-event :sig :id)))
                 signed (<! (n/sign unsigned private-key))]
             (is (true? (<! (n/verify signed))))
             (done)))))

(deftest parse-relay-list
  (let [exp-list nil
        actual-list (n/parse-relay-list relay-list-json)]
    (is (= 5 (count actual-list)))))

(deftest recreate-relay-list
  (let [relay-list (n/parse-relay-list relay-list-json)]
    (is (= relay-list-json (n/unparse-relay-list relay-list)))))

(def note-tags
  [["e" "5f2d83be4baf8ba797199fa1e03faa49405f36e83ec77c8291166b6351a008e2"]
   ["e" "368007f38569936f1b403169b971e9cde7722a11760b948a4efd60979fe083cc"]
   ["p" "32e1827635450ebb3c5a7d12c1f8e7b2b514439ac10a67eef3d9fd9c5c68e245"]
   ["p" "4af08c8b32591fd670924087635a4f7ccaae61367818cf7d52c3a9c3ef069a1d"]
   ["p" "6f0ec447e0da5ad4b9a3a2aef3e56b24601ca2b46ad7b23381d1941002923274"]
   ["p" "ee6ea13ab9fe5c4a68eaf9b1a34fe014a66b40117c50ee2a614f4cda959b6e74"]
   ["p" "5b0183ab6c3e322bf4d41c6b3aef98562a144847b7499543727c5539a114563e"]])

(def reply-tags
  [["e" "5f2d83be4baf8ba797199fa1e03faa49405f36e83ec77c8291166b6351a008e2"]
   ["e" "dc99fcfbaa93b9130d7f20766d481538425cd74243b684279c6eb7b1284a93f1"]
   ["p" "32e1827635450ebb3c5a7d12c1f8e7b2b514439ac10a67eef3d9fd9c5c68e245"]
   ["p" "4af08c8b32591fd670924087635a4f7ccaae61367818cf7d52c3a9c3ef069a1d"]
   ["p" "6f0ec447e0da5ad4b9a3a2aef3e56b24601ca2b46ad7b23381d1941002923274"]
   ["p" "ee6ea13ab9fe5c4a68eaf9b1a34fe014a66b40117c50ee2a614f4cda959b6e74"]
   ["p" "5b0183ab6c3e322bf4d41c6b3aef98562a144847b7499543727c5539a114563e"]
   ["p" "b83a28b7e4e5d20bd960c5faeb6625f95529166b8bdb045d42634a2f35919450"]])

(def note-tags-1
  [["e" :aa]["e" :bb]["p" :11]["p" :22]["p" :33]])
(def reply-tags-1
  [["e" :aa]["e" :cc]["p" :11]["p" :22]["p" :33]["p" :44]])
(def note-tags-2
  [["e" :aa "wss://relay" "reply"]["e" :bb "wss://relay" "root"]["p" :11]["p" :22]["p" :33]])
(def reply-tags-2
  [["e" :bb "wss://relay" "root"]["e" :cc "" "reply"]["p" :11]["p" :22]["p" :33]["p" :55]])

(deftest generate-tags-ps
  (is (= reply-tags-1 (n/generate-reply-tags note-tags-1 :cc :44))))

(deftest generate-root-reply-tags
  (is (= reply-tags-2 (n/generate-reply-tags note-tags-2 :cc :55))))

(deftest generate-empty-root-tags
  (is (= [["e" :aa]["p" :11]]
         (n/generate-reply-tags [] :aa :11))))

(deftest parse-tags
  (let [tags [["e" "bf" "wss://relay.nostr.pro"]
              ["e" "fe"]
              ["p" "20"]
              ["p" "7d" "wss://foo.bar"]]]
  (is (= '(("20") ("7d" "wss://foo.bar")) (:replying (n/parse-notes-tags tags))))))
            
;; (deftest likes-test
;;   (let [one-like  {:likes 1, :other {"🤙" 2}}
;;         two-likes {:likes 2}]
;;     (is (= "❤ 🤙2" (n/reactions-to-text one-like)))
;;     (is (= "❤2" (n/reactions-to-text two-likes)))))

(deftest b32-decode
  (let [pk (n/generate-key)
        pk-b32 (n/encode-hex "nsec" pk)]    
    (is (= pk (n/decode-b32 pk-b32)))))    
        
;; (deftest url-linking
;;   (let [url-text "foo bar http://duck.com walrus https://yaaya.de xxx"]
;;       (is (= (n/insert-urls url-text (n/url-find url-text) 0) "foo"))))
