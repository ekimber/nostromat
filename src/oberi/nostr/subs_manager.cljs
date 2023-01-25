(ns oberi.nostr.subs-manager
  (:require
   [oberi.nostr.utils :as u]
   [oberi.nostr.manager :as mgr]
   [reagent.core :as reagent]
   [re-frame.core :as re-frame :refer [reg-fx]]
   [cljs.core.async :as async :refer [<! >! put! chan close! mult tap]]
   [cljs.pprint :refer [pprint cl-format]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def >evt re-frame/dispatch)

;; Holds subscription state so that it can manage subscriptions as relays
;; connect and disconnect

(def subs-atom (reagent/atom {}))

(def note-q (chan 100))
(def note-q-mult (async/mult note-q))
(def metadata-q (chan 100))
(def metadata-q-mult (async/mult metadata-q))

(def big-input-pipe (chan 200 (u/distinct-by :id)))

(def mix (async/mix big-input-pipe))

;== event processing
(defn dispatch-event [evt]
  (case (:kind evt)
    0 (>evt [:metadata-msg evt])
    1 (>evt [:text-note evt])
    2 (>evt [:recommend-relay-msg evt])
    3 (>evt [:contacts-msg evt])
    4 (>evt [:private-msg evt])
    7 (>evt [:reactions-msg evt])
    (pprint {:unhandled-msg evt})))

(go-loop []
  (let [event (<! big-input-pipe)]
    (dispatch-event event))
  (recur))

(defn register-sub [^Subscription s]
  (swap! subs-atom #(assoc % (:id s) s)))

(defn start-sub [conn ^Subscription s]
  (let [out (mgr/start-subscription conn s)]
    (async/admix mix out)))

(defn stop-sub [^Subscription s]
  (swap! subs-atom #(dissoc % (:id s))))
                                        ; TODO

(defn batch-subs [conn]
  (let [metabatch (fn [ids] (mgr/->Request "metabatch" #js{"kinds" #js[0] "authors" (clj->js (vec ids)) "limit" 50}))
        notebatch (fn [ids] (mgr/->Request "notebatch" #js{"kinds" #js[1] "ids" (clj->js (vec ids)) "limit" 50}))]
    (go (let [{:keys [in out]} (mgr/subscribe-batch conn metabatch)]
          (async/tap metadata-q-mult in)
          ;; TODO replace with pipe to big input?
          (loop []
            (let [m (<! out)]
              (when m
                (>evt [:metadata-msg m])
                (recur))))
          (async/untap metadata-q-mult in))
        (pprint ["metabatch ended on" conn]))
    (go (let [{:keys [in out]} (mgr/subscribe-batch conn notebatch)]
          (async/tap note-q-mult in)
          (loop []
            (let [m (<! out)]
              (when m
                (>evt [:text-note m])
                (recur))))
          (async/untap note-q-mult in))
        (pprint ["notebatch ended on" conn]))))

;; handler fns

(defn handle-connect [conn-p]
  (go (let [conn (<! conn-p)]
        (pprint {:connecting conn})
        (doseq [s @subs-atom]
          (start-sub conn (second s)))
        ;; start batches
        (batch-subs conn))))

(defn handle-disconnect [socket-id]
  :oberi.nostr.manager/reconnect)

;; re-frame event/fx

(reg-fx
 :base-subscriptions-start
 (fn []
   (let [global (mgr/->IndefiniteSub "global" #js{"kinds" #js[0 1 7] "limit" 100})]
     (register-sub global))))

(reg-fx
 :register-user-subscriptions
 (fn [{:keys [pubkey contact-list]}]
   (let [authors (clj->js (conj (map :pubkey contact-list) pubkey))
         reqs [(mgr/->IndefiniteSub "mentions/pms" #js{"kinds" #js[1 4] "#p" #js[pubkey] "limit" 100})
               (mgr/->IndefiniteSub "own-pms" #js{"kinds" #js[4] "authors" #js[pubkey] "limit" 100})
               (mgr/->IndefiniteSub "contact-meta" #js{"kinds" #js[0 3] "authors" authors  "limit" 100})]]
     (doseq [req reqs]
       (let [out (mgr/subscribe-multi req)]
         (swap! subs-atom #(assoc % (:id req) req))
         (async/admix mix out))))))

(reg-fx
 :unregister-user-subscriptions
 (fn []
   ))

;; re-frame fx

(reg-fx
 :get-relay-recommendations
 (fn [conns]
   (go
     (let [relay-req (mgr/->Request "relayrecs" #js{"kinds" #js[2] "limit" 20})
           data-ch (mgr/subscribe-multi conns relay-req)]
       (loop []
         (when-let [evt (<! data-ch)]
           (>evt [:recommend-relay-msg evt])
           (recur)))))))

;; query fx

(reg-fx
 :query-metadata
 (fn [pubkey]
   (go (>! metadata-q pubkey))))

(reg-fx
 :query-notes
 (fn [note-ids]
   (go (async/onto-chan! note-q (remove nil? note-ids) false))))

(reg-fx
 :post-request
 (fn [req]
   (go
     (let [ch (mgr/subscribe-multi req)]
       (loop []
         (when-let [evt (<! ch)]
           (dispatch-event evt)
           (recur)))))))
