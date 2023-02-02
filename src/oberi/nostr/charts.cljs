(ns oberi.nostr.charts
  (:require [oberi.nostr.manager :as mgr]
            ["react" :as react]
            ["react-dom" :refer [render]]
            ["@upsetjs/react" :refer [VennDiagram extractSets generateCombinations asSets ISetLike createVennJSAdapter UpSetJS]]
            ["@upsetjs/venn.js" :refer [layout]]
            [helix.core :refer [defnc $]]
            [helix.hooks :as hooks]
            [helix.dom :as d]
            ["react-dom/client" :as rdom]
            [reagent.core :as reagent]
            [cljs.pprint :refer [pprint]]
   ))


(defn euler-component []
  (let [relay-data (:presence @mgr/relay-stats)
        set-data (for [entry relay-data] {:name (first entry) :sets (vec (map #(subs % 6) (second entry)))})
        sets (extractSets (clj->js set-data))]
    ($ :div
       ;; ($ UpSetJS {:sets sets :width 600 :height 400 :combinations (generateCombinations sets)})
       ($ VennDiagram {:layout (createVennJSAdapter layout)
                       :sets sets
                       :fontSizes #js{"setLabel" "12px"}
                       :combinations (generateCombinations sets)
                       :width 600 :height 400 :theme "light"})
       )))

(defn relay-analyzer []
  (let [visible (reagent/atom false)]
    (fn []
      [:div
       [:button.button.mb-2.ml-4 {:on-click #(swap! visible not)} "Analyze"]
       [:div.modal (if @visible {:class :is-active})
        [:div.modal-background {:on-click #(swap! visible not)}]
        [:div.modal-content
         [:div.box
          (if @visible [euler-component])]]
        [:button.modal-close.is-large {:aria-label "close"}]]]
      )))
  ;; [:div
  ;; (let [visible (reagent/atom false)]
  ;;   (fn []
  ;;     [:div [:button.button.mb-2.ml-4 "Analyze"]
  ;;     [:div.modal (if @visible {:class :is-active})
  ;;      [:div.modal-background]
  ;;      [:div.modal-content
  ;;       (let [relay-data (:presence @mgr/relay-stats)
  ;;             set-data (for [entry relay-data] {:name (first entry) :sets (vec (second entry))})
  ;;             sets (extractSets (clj->js set-data))]
  ;;         [(reagent/adapt-react-class VennDiagram)
  ;;          {:layout (createVennJSAdapter layout)
  ;;           :sets sets
  ;;           :combinations (generateCombinations sets)
  ;;           :width 500 :height 500 :theme "light"}])
        ;; ]]])))
