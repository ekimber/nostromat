(ns oberi.nostr.main
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as re-frame]
   [oberi.nostr.rf :as rf :refer [<sub >evt >evt-now]]
   [oberi.nostr.views :as views]
   [oberi.nostr.config :as config]))

(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (let [root (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root)
    (rdom/render [views/main] root)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn init []
  (>evt-now [::rf/boot])
  (dev-setup)
  (mount-root))
