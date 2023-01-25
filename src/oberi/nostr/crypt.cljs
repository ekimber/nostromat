(ns oberi.nostr.crypt
  (:require
   ;; ["@noble/secp256k1" :as secp]
   ;; ["nanoid/generate" :as r]
   ;; [goog.crypt :as c]
   [cljs.pprint :refer [pprint]]
   ["nostr-tools" :refer [nip04 getPublicKey generatePrivateKey]]
   ;; [goog.crypt.Aes :as Aes]
   ;; [goog.crypt.Cbc :as Cbc]
   ))

(defn encrypt
  [private-key public-key message]
  (.encrypt nip04 private-key public-key message))
  
(defn decrypt
  [private-key public-key crypt-text]
   (.decrypt nip04 private-key public-key crypt-text))

;; (def aes-256 (.Aes 256))

;; (defn x-encrypt [our-priv their-pub msg]
;;   (let [shared-point (.getSharedSecret secp our-priv (str "02" their-pub))
;;         shared-x (.subarray shared-point 2 64)
;;         iv (r "1234567890abcdef" 32)
;;         ;; buf (.from js/Buffer shared-x "hex")
;;         ;; aes (.Aes buf)
;;         key (c/hexToByteArray shared-x)
;;         aes (.c/Aes key)
;;         cbc (.c/Cbc aes)]
;;     (pprint key)))


;; (defn encrypt [our-priv their-pub msg]
;;   (let [shared-point (.getSharedSecret secp our-priv (str "02" their-pub))
;;         shared-x (.subarray shared-point 2 64)
;;         iv 
        
  
;; import crypto from 'crypto'
;; import * as secp from 'noble-secp256k1'

;; let sharedPoint = secp.getSharedSecret(ourPrivateKey, '02' + theirPublicKey)
;; let sharedX = sharedPoint.substr(2, 64)

;; let iv = crypto.randomFillSync(new Uint8Array(16))
;; var cipher = crypto.createCipheriv(
;;   'aes-256-cbc',
;;   Buffer.from(sharedX, 'hex'),
;;   iv
;; )
;; let encryptedMessage = cipher.update(text, 'utf8', 'base64')
;; encryptedMessage += cipher.final('base64')
;; let ivBase64 = Buffer.from(iv.buffer).toString('base64')

;; let event = {
;;   pubkey: ourPubKey,
;;   created_at: Math.floor(Date.now() / 1000),
;;   kind: 4,
;;   tags: [['p', theirPublicKey]],
;;   content: encryptedMessage + '?iv=' + ivBase64
;; }

