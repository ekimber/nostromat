(ns oberi.nostr.crypt
  (:require ["@noble/secp256k1" :as secp]
            ["nanoid/generate" :as r]
            ["goog.crypt.Cbc" :as cbc]))

(defn encrypt [our-priv their-pub]
  (let [shared-point (.getSharedSecret secp our-opriv (str "02" their-pub))
        shared-x (subs 2 64)
    

  
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

