(ns oberi.nostr.views
  (:require [oberi.nostr.rf :as rf :refer [<sub >evt]]
            [oberi.nostr.manager :as mgr]
            [oberi.nostr.nostr :as n]
            [oberi.nostr.subs]
            [clojure.string :refer [blank?]]
            [reagent.core :as reagent]
            ["emoji-picker-element" :as emo]
            [react :as react]
            ["qrcode.react" :refer [QRCodeSVG]]
            ["webln" :as webln]
            [goog.events :as gevents]
            [goog.object]
            [goog.dom :as gdom]
            [goog.object :as g]
            [goog.string :as gstring]
            [fork.re-frame :as fork]
            [fork.bulma :as bulma]
            [cljs.core.async :as async :refer [<! >!]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [cljs.pprint :refer [pprint]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def note-text (reagent/atom ""))

(defn note-info-modal []
  [:div.modal (<sub [:note-info-raw-active])
   [:div.modal-background {:on-click #(>evt [:show-note-info-popup nil])}]
    [:div.modal-content
     [:div.box [:title "Note"]
      [:small.is-family-code.is-size-7  {:style {:word-break "break-word"}}
       [:pre {:style {:word-break "break-word"}} (<sub [:note-info-raw]) ]]]]])

(defn note-entry-box [reply-info]
  (if (<sub [:is-logged-in])
    (let [;; note-text (reagent/atom "")
          picker-visible (reagent/atom false)
          click-handler #(do (>evt [(or (:event-target reply-info) :post-note) @note-text reply-info]) (reset! note-text ""))]
      (fn []
        (reagent/create-class
         {:reagent-render
          (fn []
            [:div.box.p-2.m-2
             (if-let [n (:note reply-info)]
               ^{:key (:npubkey n)}[:small
                                    {:style {:max-width "120pt" :text-overflow "ellipsis" :overflow "hidden"}}
                                    "Replying to " (or (:name n) (:npubkey n))])
             [:div.block.m-2
              [:textarea.textarea.is-size-5
               {:id "entry-box" :placeholder (or (:placeholder reply-info) "Hello Nostr") :rows 2
                :on-change #(reset! note-text (-> % .-target .-value)) :value @note-text}]]
             [:div.block.m-2
              [:div.level
               [:div.level-left
                [:button.button.is-primary.level-item {:on-click click-handler} "Post"]
                [:div.dropdown (if @picker-visible {:class "is-active"})
                 [:div.dropdown-trigger
                  [:button.button {:on-click #(swap! picker-visible not) :aria-haspopup "true" :aria-controls "emoji-picker-drop"}
                   [:ion-icon {:name "happy-outline" :style {:font-size "20px"}}]]]
                 [:div.dropdown-menu {:id "emoji-picker-drop" :role "menu"}
                  [:div.dropdown-content
                   [:div.dropdown-item.is-unselectable [(reagent/adapt-react-class "emoji-picker") {:id "emo-picker"}]]]]]]]]]) 
          :component-did-mount (fn [comp]
                                 (let [picker (gdom/getElement "emo-picker")
                                       element (.getElementById js/document "emoji-picker-drop")
                                       entry-box (.getElementById js/document "entry-box")]
                                   (gevents/listen picker "emoji-click"
                                                   (fn [e] (do (swap! note-text #(str % (-> e (g/get "event_") .-detail .-unicode)))
                                                               (swap! picker-visible not))))
                                   (let [picker (gdom/getElement "emo-picker")]
                                     (.catch (.close (.-database picker))) #(%))
                                   (if-not (undefined? element)
                                     (.scrollIntoView (.-parentElement element) #js{"block" "end" "behaviour" "smooth"}))
                                   (if-not (undefined? entry-box)
                                     (.focus entry-box))))
          :component-will-unmount (fn [comp]
                                    (let [picker (gdom/getElement "emo-picker")]
                                      (.close (.-database picker))))
          :display-name "emoji picker"})))))


(def invoice-keys [:timeExpireDateString :satoshis :timestampString])

(defn send-payment [^js/webln.WebLNProvider p pay-req]
  (.sendPayment p pay-req))

(defn invoice-element [invoice]
  (let [c (async/promise-chan)
        _ (go (>! c (<p! (webln/requestProvider))))]
    [:div.is-size-7.is-family-code
     [:div.pr-6 {:style {:z-index 3 :word-break "break-word"}} (:paymentRequest invoice)][:br]
     [:div.columns
      [:div.column.is-one-quarter "Amount"[:br]"Created"[:br]"Expires"[:br]"Payee Node"]
      [:div.column.is-one-half (:satoshis invoice) " satoshis"[:br](:timestampString invoice)[:br](:timeExpireDateString invoice)[:br](:payeeNodeKey invoice)]]   
     [:button.button.is-small.is-dark {:on-click #(go (send-payment (<! c) (:paymentRequest invoice)))} "⚡ Pay (webln)"]]))

(defn note-element [note-id]
  (let [note (<sub [:get-note note-id])
        inv-toggle (if (:invoice note) (reagent/atom false))]
    (fn []
      (let [note (<sub [:get-note note-id])]
    [:article.media.m-0.p-1.pr-2 (if (:highlighted note) {:class "has-background-white-ter"})
     [:figure.media-left.m-2
      [:div.image.is-32x32.is-rounded {:on-click #(do (>evt [:set-view [:user-feed (:pubkey note)]]) (.stopPropagation %))}
       (if-let [pic (:picture note)]
         [:img.image.is-rounded.is-32x32 {:src pic}]
         [:identicon-svg {:username (:pubkey note)}])
       [:div.level (if (:following note)
                     [:a.icon.level-item.has-text-danger-dark {:on-click #(do (>evt [:unfollow-user (:pubkey note)]) (.stopPropagation %))}
                      [:ion-icon {:name "person-remove-outline"}]]
                     [:a.icon.level-item.has-text-success {:on-click #(do (>evt [:follow-user (:pubkey note)]) (.stopPropagation %))}
                      [:ion-icon {:name "person-add-outline"}]])]]]
     [:div.media-content      
      [:div.content.m-0.is-clickable {:on-click #(do (>evt [:set-view [:thread-view note]]) (.stopPropagation %))}
       [:div.level.m-0
        [:span.level-item.level-left [:strong {:style {:max-width "130pt" :text-overflow "ellipsis" :overflow "hidden"}} (:name note)] (gstring/unescapeEntities "&nbsp;") 
         [:small.is-size-7.is-family-code (:npubkey note)]]
        [:span.icon.level-item.level-right {:on-click (fn [e] (do (>evt [:show-note-info-popup (:id note)]) (.stopPropagation e)))}
         [:ion-icon {:name "receipt-outline"}]]]
       (if (seq (:replying note))
         [:div {:on-click #(.stopPropagation %)}
          [:span {:height "16px"} [:ion-icon {:name "return-down-forward-outline"}]]
          [:span.is-small.is-size-6 (interpose ", " (:replying note))][:br]])
       [:div {:style {:position "relative"}}
        (if (:invoice note) [:span.is-pulled-right.is-clickable.is-size-6 "Lightning Invoice"
                             [:div.mt-1 {:on-click #(do (swap! inv-toggle not) (.stopPropagation %))}
                              [(reagent/adapt-react-class QRCodeSVG)
                               {:value (-> note :invoice :paymentRequest) :size 128}]]])
        (:display note)
        (if (:invoice note)
          [:div.card (if (and inv-toggle @inv-toggle) {:style {:position "absolute" :top "0px" :z-index "3"
                                                               :on-click #(.stopPropagation %)}} {:style {:display "none"}})
           [:div.card-content.p-2
            [:button.button.is-small.is-light.is-pulled-right.is-rounded {:on-click #(do (swap! inv-toggle not) (.stopPropagation %))} "Close"]
            [invoice-element (:invoice note)]]])
        (if (:child note) (note-element (:child note)))]]
      [:div.level.pb-1.is-mobile.has-text-weight-light.has-text-grey.is-align-content-space-between {:style {:width "100%"}}
       [:span.level-item.level-left.has-text-centered
        (if-not (blank? (:other-text (:reactions note)))
          [:span.tag.is-size-6.pt-1.pb-1.m-1 (:other-text (:reactions note))])
        [:a.icon {:on-click #(>evt [:react-to-note [(:id note) {:other "🤙"}]])}
         [:ion-icon {:name "add-outline"}]]]
       [:span.level-item.level-left
        [:span.icon-text
         [:a.icon {:on-click #(>evt [:react-to-note [(:id note) {:like true}]])}
          [:ion-icon {:name "heart-outline"}]]
         [:span (-> note :reactions :likes)]]]
       [:span.level-item.level-left
        [:span.icon-text.has-text
         [:a.icon {:on-click #(>evt [:react-to-note [(:id note) {:like false}]])}
          [:ion-icon {:name "heart-dislike-outline"}]]
         [:span (-> note :reactions :dislikes)]]]
       
       [:small.level-item-level-right (:age note)]]]]))))

(defn follows-feed []
  [:div.column.is-half.is-flex.is-flex-wrap-nowrap.is-flex-direction-column
   [note-entry-box nil]
   (for [note (<sub [:get-threaded-feed])]
     ^{:key (:id note)}[note-element (:id note)])])

(defn own-feed []
  [:div.column.is-half.is-flex.is-flex-wrap-nowrap.is-flex-direction-column
   [note-entry-box nil]
   (for [note (<sub [:get-own-notes])]
     ^{:key (:id note)}[note-element (:id note)])])

(defn thread-view [note]
  [:div.column.is-half.is-flex.is-flex-wrap-nowrap.is-flex-direction-column
   (reduce (fn [acc v]
             (-> (conj acc ^{:key (:id v)}[note-element (:id v)])
                 (#(if (= (:id note)(:id v)) (conj % [note-entry-box {:note v}]) %))))
           [:div] (<sub [:get-note-view (:id note)]))])
               ;; (for [note (<sub [:get-note-view note-id])] (note-element note)))

(defn user-card [user]
  ^{:key (:pubkey user)}
  [:article.media.m-0.p-0.is-clickable {:on-click #(>evt [:set-view [:user-feed (:pubkey user)]])}
   [:figure.media-left.m-2
    [:div.image.is-48x48
     (if-let [pic (:picture user)]              
       [:img.image.is-48x48 {:src pic :style {:max-height 48}}]
       [:identicon-svg {:username (:pubkey user)}])]]
   [:div.media-content.m-2
    [:div [:strong (:name user)] [:span.tag.ml-2 (:npubkey user)]
     [:br]
     [:small (:about user)]]]])

(defn large-user-card [pubkey]
  (let [user (<sub [:user-metadata pubkey])]
  ^{:key (:pubkey user)}
  [:article.media.m-0.p-0  
   [:figure.media-left.m-2
    [:div.image {:style {:height 200 :width 200}}
     (if-let [pic (:picture user)]              
       [:img.image {:src pic :style {:height 200 :width 200}}]
       [:identicon-svg {:username pubkey}])]]
   [:div.m-2.is-flex.is-flex-direction-column.is-justify-content-space-between
    [:div.block.m-1 [:strong (:name user)] [:span.tag.ml-2 (:npubkey user)]]
    [:hr.m-1]
    [:div.block.m-1
     [:p (:about user)]]
    [:div
    [:div.level
     [:div.level-left
      [:div.level-item (if (<sub [:is-following? pubkey])        
        [:a [:span.has-text-danger-dark.m-1.is-align-items-center.is-flex {:on-click #(do (>evt [:unfollow-user pubkey]) (.stopPropagation %))}
             [:ion-icon {:name "person-remove-outline" }]
             [:span.tag.ml-1 "Unfollow"]]]
        [:a [:span.has-text-success.m-1.is-align-items-center.is-flex {:on-click #(do (>evt [:follow-user pubkey]) (.stopPropagation %))}  
             [:ion-icon {:name "person-add-outline" :size "large"}]
             [:span.tag.ml-1 "Follow"]]])]
      [:div.level-item.ml-3
       [:a [:span.has-text-black.is-align-items-center.is-flex {:on-click #(do (>evt [:set-view [:convo-view pubkey]]) (.stopPropagation %))}
            [:ion-icon {:name "chatbubbles-outline" :size "large" }]
            [:span.tag.ml-1 "Private Messages"]]]]]]]]]))

(defn user-feed [pubkey]
  [:div
   ;; [note-entry-box nil]
   [large-user-card pubkey]
   (for [note (<sub [:get-user-notes pubkey])]
     ^{:key (:id note)}[note-element (:id note)])])

(defn follows-item [user]
  [user-card user])

(defn follows-list []
  [:div.is-flex.is-flex-direction-column.is-justify-content-flex-start	
   (for [follow (<sub [:get-follows])]
     ^{:key (:pubkey follow)}[user-card follow])])

(defn global-feed []
  [:div.column.is-half.is-flex.is-flex-wrap-nowrap.is-flex-direction-column        
   [note-entry-box nil]
   (for [note (<sub [:global-feed])]
     ^{:key (:id note)}[note-element (:id note)])])

(defn mentions-view []
  [:div.column.is-half.is-flex.is-flex-wrap-nowrap.is-flex-direction-column
   (for [note (<sub [:mentions-list])]
     ^{:key (:id note)}[note-element (:id note)])])

(defn login-view []
  [:div.column.is-half.is-flex.is-flex-wrap-nowrap.is-flex-direction-column
   (for [note (<sub [:logged-out-feed])]
     ^{:key (:id note)}[note-element (:id note)])])

(defn convo-elem [pubkey]
  (let [user (<sub [:user-metadata pubkey])]
    [:article.media.m-0.p-0.is-clickable {:on-click #(>evt [:set-view [:convo-view pubkey]])}                                                      
     [:figure.media-left.m-2
      [:div.image.is-48x48
       (if-let [pic (:picture user)]              
         [:img.image.is-48x48 {:src pic :style {:max-height 48}}]
         [:identicon-svg {:username (:pubkey user)}])]]
     [:div.media-content.m-2
      [:div [:strong (:name user)] [:span.tag.ml-2 (:npubkey user)]
       [:br]
       [:small (:about user)]]]]))

(defn messages-view []
  [:div.column.is-half.is-flex.is-flex-wrap-nowrap.is-flex-direction-column
   (let [pubkeys (<sub [:private-convos])]
     (if (seq pubkeys)
       (for [pubkey pubkeys]
         ^{:key pubkey}[convo-elem pubkey])
       [:div.container [:p.m-6.has-text-grey "You have no messages :'("]]))])

(defn convo-view [pubkey]
  (let [messages (<sub [:convo-with-pubkey pubkey])
         from-meta (<sub [:user-metadata pubkey])
        user-meta (<sub [:own-metadata])]
    [:div.column.is-half.is-flex.is-flex-wrap-nowrap.is-flex-direction-column   
     [:div.is-flex.is-justify-content-space-between [:div (:name user-meta)][:div (:name from-meta)]]
     [:div.is-flex.is-justify-content-space-between 
      (if-let [pic (:picture user-meta)]     
        [:img.image.is-48x48 {:src pic :style {:max-height 48}}]
        [:identicon-svg {:username (:pubkey user-meta)}])
      (if-let [pic (:picture from-meta)]     
        [:img.image.is-48x48 {:src pic :style {:max-height 48}}]
        [:identicon-svg {:username (:pubkey from-meta)}])]     
     (if (seq messages)
       (for [message messages]
         ^{:key (:id message)}
         [:div.is-flex (if (= pubkey (:pubkey-from message)) {:class "is-justify-content-flex-end"})
          [:div [:span.is-size-7 (:time message)]
           [:span.box.is-rounded.pt-1.pb-1.pl-2.pr-2.is-shadowless
            (if (= pubkey (:pubkey-to message)) {:class "has-background-warning-light"} {:class "has-background-info-light"})
                [:p (:msg message)]]]])
       [:div.container [:p.m-6.has-text-grey "Hmmm... there don't appear to be any messages in this conversation."]])
     [note-entry-box {:placeholder "" :event-target :post-message :pubkey pubkey}]]))

(defn field-array-fn
  [_props
   {:fieldarray/keys [fields
                      insert
                      remove
                      handle-change
                      handle-blur]}]
  [:<>
   [:div.is-flex.is-flex-direction-row.is-align-content-top
    [:label.label "Relays"]
    [:button.button.is-small.ml-5.p-2.is-light.is-success {:type "button" :on-click #(insert {:relay-url "wss://" :read true :write true})} "New relay"]]
   (map-indexed
    (fn [idx field]
      ^{:key idx}
      [:div       
       [:div.is-flex.is-flex-direction-row.is-align-content-center.p-0
        [:button.button.is-small.m-2.is-align-self-center.is-danger.is-light
        {:type "button"
         :on-click #(when (> (count fields) 1) (remove idx))}
         [:span "Remove"]]
        [:div.field.m-0.is-align-self-center
         [:input.input.pb-0.pt-0.is-size-6
          {:name :relay-url
           :value (get field :relay-url)
           :on-change #(handle-change % idx)
           :on-blur #(handle-blur % idx)}]]       
         [:div.field.pl-3
          [:label.checkbox [:input
                           {:name :read
                            :type "checkbox"
                            :checked (get field :read) 
                            :value (get field :read)
                            :on-change #(handle-change % idx)
                            :on-blur #(handle-blur % idx)}] "Read"]]
         [:div.field.pl-3
          [:label.checkbox [:input
                           {:name :write
                            :type "checkbox"
                            :checked (get field :write) 
                            :value (get field :write)
                            :on-change #(handle-change % idx)
                            :on-blur #(handle-blur % idx)}] "Write"]]]
       ])
    fields)
   ])

(defn user-settings-form [{:keys [values] :as props}]
  [:form
    [bulma/input props
     {:name :name
      :label [:div.mt-2 [:span "Name"]
              [:div {:style {:float "right"}}
               [:span.tag.ml-2.is-clickable
                {:on-click #(.writeText js/navigator.clipboard (:npubkey values))} (:npubkey values)]
               (if (:private-key values)
                [:span.tag.ml-2.is-clickable {:on-click #(.writeText js/navigator.clipboard (:private-key values))} (:private-key values)])]]
               
      :placeholder "Name"
      :type "text"}]
    [bulma/input props
     {:name :about
      :label "About"
      :placeholder "About"
      :type "text"}]
    [bulma/input props
     {:name :picture
      :label "Picture URL"
      :type "text"}]
    [:div
     [fork/field-array {:props props
                        :name :relays}
      field-array-fn]]])

(defn pop-relays-col [relays state]
  (let [add-relay-fn (fn [r]
                       (swap! state
                              (fn [s] (update-in s [:values :relays]
                                                 #(if ((set (map :relay-url %)) r) % (conj % {:relay-url r :write true :read true}))))))]
    [:div.content.m-2
     [:table.table.is-size-6.is-striped.is-fullwidth.is-narrow
      [:thead [:tr
               [:th "Add"]
               [:th [:abbr {:title "Position"} "Pos"]]
               [:th "Relay"]
               [:th [:abbr {:title "Count"} "C"]]]]
      [:tbody
       (for [relay relays]
         ^{:key (nth relay 2)}
         [:tr [:th [:a.has-text-success {:on-click #(add-relay-fn (first relay)) } [:ion-icon {:name "add-circle"} ]]]
          [:th (nth relay 2)] [:td (first relay)][:td (second relay)]])]]]))

(defn pop-relays-table [state]
  (let [relays (<sub [:popular-relays])]
    [:div.columns
     (pop-relays-col (take 10 relays) state)
     (pop-relays-col (drop 10 relays) state)]))

(defn user-settings []
  (let [state (reagent/atom nil)]
    [:div.content
     [fork/form {:state state
                 :initial-values (<sub [:user-settings])
                 :keywordize-keys true
                 :prevent-default? true}
      user-settings-form]
     [:div.block.mt-4 [:button.button.is-success {:on-click #(>evt [:save-user-settings (:values @state)])} "Save"]]
     [:div.content.mt-2
      [:label.label "Recommended Relays"]
      [pop-relays-table state]]]))
  
  ;; (let [settings (<sub [:user-settings])
  ;;       settings-atom (reagent/atom settings)]
  ;;   [:div.content   
  ;;    [:div.field [:label.label "Name"]
  ;;     [:div.control
  ;;      [:input.input {:type "text" :placeholder "name" :value (:name settings)
  ;;                     :on-change #(pprint (js->clj (.-value (.-target %)))) }]]]
  ;;    [:div.file
  ;;     [:label.file-label
  ;;      [:input.file-input {:type "file"}]
  ;;      [:span.file-cta [:span.file-icon [:ion-icon {:name "image-outline"}]]
  ;;       [:span.file-label "Choose image file…"]]]]
  ;;    ]))
(defn logginin-modal []
  [:div.modal (<sub [:logging-in])
   [:div.modal-background]
   [:div.modal-content
   [:div.modal-card
    [:header.modal-card-head
     [:p.modal-card-title "Logging In"]]
    [:section.modal-card-body
     [:div.level
      [:div.level-item
       [:button.button.is-loading.is-large.is-success.is-rounded {:disabled true}]]]]]]])

(defn create-account-modal []
  [:div.modal.is-active
   [:div.modal-background]
   [:div.modal-content
   [:div.modal-card    
    [:header.modal-card-head
     [:p.modal-card-title "Creating Account"]]
    [:section.modal-card-body
     (let [state (reagent/atom nil) pubkey (<sub [:own-pubkey])]
       [:div.content
        [fork/form {:state state
                    :initial-values {:relays (mapv #(hash-map :relay-url % :read true :write true)  (keys (<sub [:relays])))
                                     :npubkey (n/encode-hex "npub" pubkey)
                                     }
                    :keywordize-keys true
                    :prevent-default? true}
         user-settings-form]
        [:div.block.mt-4 [:button.button.is-success {:on-click #(do (>evt [:save-user-settings (:values @state)])
                                                                    (>evt [:logged-in-with-pubkey pubkey]))} "Save"]]
        [:div.content.mt-2
         [:label.label "Popular Relays"]
         [pop-relays-table state]]])
     ]]]])

(defn key-form [{:keys [values] :as props}]
  [:form
   [bulma/input props         
    {:name :secret
     :label "Private Key"
     :placeholder "nsec"
     :type "text"}]])

(defn provide-key-modal []
  [:div.modal.is-active  
   [:div.modal-background]
   [:div.modal-content
    [:div.modal-card
     [:section.modal-card-body
      (let [state (reagent/atom nil) pubkey (<sub [:own-pubkey])]
        [:div.content
         [fork/form {:state state
                     :initial-values {}
                     :keywordize-keys true
                     :prevent-default? true}        
          key-form]
        [:div.block.mt-4
         [:button.button.is-success
          {:on-click #(>evt [:user-provided-secret (-> @state :values :secret (n/decode-b32))])} "Save"]]])]]]])

(defn main-view-selector [] 
  (let [view (<sub [:view-selector])]
    (case (first view)
      :follows-feed [follows-feed]
      :own-feed [own-feed]
      :user-feed [user-feed (second view)]
      :follows-list [follows-list]      
      :thread-view [thread-view (second view)]
      :global-feed [global-feed]
      :settings-view [user-settings]
      :login-view [login-view]
      :notifications [mentions-view]
      :private-messages [messages-view]
      :convo-view [convo-view (second view)]
      [:div.content.has-text-centered [:ion-icon.is-large {:name "infinite-outline"}]])))

(defn user-ident []
  [:a {:on-click #(>evt [:set-view [:own-feed]])} (user-card (<sub [:logged-in-user]))])

(defn nav-list []
  (let [selected (first (<sub [:view-selector]))]
    (if (<sub [:is-logged-in])
      [:div.is-flex.is-flex-direction-column.is-pulled-right
       [:aside.menu
        [:div [:div.media-content [user-ident]]      
         [:ul.menu-list.m-1.is-size-5
          [:li (if (= :follows-feed selected) {:class "has-background-white-bis"})
           [:a {:on-click #(>evt [:set-view [:follows-feed]])}
            [:span.icon-text [:span.icon [:ion-icon {:name "fast-food-outline"}]][:span "Feed"]]]]
          [:li (if (= :global-feed selected) {:class "has-background-white-bis"})
           [:a {:on-click #(>evt [:set-view [:global-feed]])}
            [:span.icon-text [:span.icon [:ion-icon {:name "earth-outline"}]][:span "Global"]]]]
          [:li (if (= :notifications selected) {:class "has-background-white-bis"})
           [:a {:on-click #(>evt [:set-view [:notifications]])}
            [:span.icon-text [:span.icon
                              [:ion-icon {:name (if (<sub [:unseen-mentions?]) "mail-unread-outline" "mail-outline")}]]
             [:span "Mentions"]]]]
          [:li (if (= :private-messages selected) {:class "has-background-white-bis"})
           [:a {:on-click #(>evt [:set-view [:private-messages]])}
            [:span.icon-text [:span.icon [:ion-icon {:name "document-lock-outline"}]][:span "Messages"]]]]
          [:li (if (= :follows-list selected) {:class "has-background-white-bis"})
           [:a {:on-click #(>evt [:set-view [:follows-list]])}
            [:span.icon-text [:span.icon [:ion-icon {:name "paw-outline" }]][:span "Following"]]]]
          [:li (if (= :settings-view selected) {:class "has-background-white-bis"})
           [:a {:on-click #(>evt [:set-view [:settings-view]])}
            [:span.icon-text [:span.icon [:ion-icon {:name "settings-outline"}]][:span "Settings"]]]]]]]
       [:div.is-pulled-right.mt-4.p-4.is-align-self-flex-end
        [:button.button.is-danger.is-light {:on-click #(>evt [:logout])}
         [:ion-icon {:name "log-out-outline"}] "Logout"]]]
      [:div.m-4.is-pulled-right
       [:div.box.is-rounded.is-shadowless.has-background-white-bis.is-flex.is-flex-direction-column
        [:div.ml-2 (if (<sub [:extension-unavailable?]) [:span.tag.is-danger.is-light "No extension detected!"]
                       [:span.tag.is-success.is-light "Recommended"])]
        [:button.button.is-success.m-2.is-medium (merge (if (<sub [:extension-unavailable?]) {:disabled true})
                                                        {:on-click #(>evt [:connect-extension])}) "Login with extension"]
        (if (<sub [:extension-unavailable?]) [:div.notification.is-info.is-size-6.is-light "The private keys to your account are more secure if you use a browser extension such as "
                                              [:a {:href "https://getalby.com/"} "Alby"] " or "
                                              [:a {:href "https://chrome.google.com/webstore/detail/nos2x"} "nos2x"]])
        [:hr]
        [:button.button.m-2 {:on-click #(>evt [:create-account])} "Create new account" ]
        [:button.button.m-2 {:on-click #(>evt [:provide-secret-key])} "Provide secret key" ]]])))

(defn relay-elem [relay conf-relays]
  [:span [:br]
   [:span.icon-text.is-size-6 {:style {:text-overflow "ellipsis" :white-space "nowrap" :overflow "hidden"
                                       :opacity (if (contains? conf-relays (first relay)) "1.0" "0.3")
                                       }}
    [:span.icon [:ion-icon
                 (case (-> relay second :state)
                       :connected {:name "cloud-done-outline" :style {:color "green"}}
                       :connecting {:name "cloud-outline" :style {:color "yellow"}}
                       :disconnected {:name "cloud-offline-outline" :style {:color "red"}}
                       {:name "help-outline"})]]
    (first relay)]])

(defn connected-relays []
  [:div.box.is-pulled-left.is-shadowless.p-0
   [:p.mt-1.ml-4.mb-4 "Relays"
    (let [conf-relays (<sub [:own-relays])]
      (for [relay @mgr/conns]
        ^{:key (first relay)}[relay-elem relay conf-relays]))]])

(defn relay-info-detail []
  )

(defn relay-info []
  (let [visible (reagent/atom true)]    
    (fn []
      [:div
       [:span.tag.is-clickable.is-unselectable {:on-click #(swap! visible not)} "Debug Info"]
       [:div (if @visible {:class "is-hidden"})
        [:pre.is-size-7
         (let [conns @mgr/conns
               subs-san-fn (fn [s] (zipmap (keys s) (map #(dissoc % :source) (vals s))))
               sanitised (zipmap (keys conns) (map (comp #(dissoc % :multi :sink :source :socket :conn) #(update-in % [:subs] subs-san-fn)) (vals conns)))]
           (with-out-str (cljs.pprint/pprint sanitised)))]]])))

(defn main []
  [:<>
   [:nav.navbar.is-fixed-top {:role "navigation" :aria-label "main navigation"}
    [:div.navbar-brand ;; [:div.navbar-item.pb-3 [:svg {:style {:width "36px" :height "36px"} :viewBox "0 0 24 24"}
     ;;    [:path {:fill "#553121" :d "M14.83,11.17C16.39,12.73 16.39,15.27 14.83,16.83C13.27,18.39 10.73,18.39 9.17,16.83L14.83,11.17M6,2H18A2,2 0 0,1 20,4V20A2,2 0 0,1 18,22H6A2,2 0 0,1 4,20V4A2,2 0 0,1 6,2M7,4A1,1 0 0,0 6,5A1,1 0 0,0 7,6A1,1 0 0,0 8,5A1,1 0 0,0 7,4M10,4A1,1 0 0,0 9,5A1,1 0 0,0 10,6A1,1 0 0,0 11,5A1,1 0 0,0 10,4M12,8A6,6 0 0,0 6,14A6,6 0 0,0 12,20A6,6 0 0,0 18,14A6,6 0 0,0 12,8Z"}]]]     
     [:div.navbar-item.is-unselectable.pl-6.m-0.pb-0.pt-0 {:style {:font-family "Slackey" :font-size "32px" :color "#553121"}}
      "nostromat"]]
    ;; [:div.navbar-item
     [:nav.level.is-overlay
      (if (<sub [:is-logged-in])
        [:div.level-item.mt-5
         [:button.button.is-focused.is-rounded.is-link.is-inverted.p-0.m-2 {:on-click #(>evt [:previous-view])}
          [:ion-icon.m-0.p-0 {:name "arrow-back-outline" :size "large"}]]
         [:button.button.is-focused.is-rounded.is-link.is-inverted.p-0.m-2 {:on-click #(.scrollTo js/window #js{"top" 0 "behaviour" "smooth"})}          
          [:ion-icon.m-0.p-0 {:name "arrow-up-outline" :size "large"}]]])]]
   (if (<sub [:creating-account])
     [create-account-modal])
   (if (<sub [:providing-secret-key])
     [provide-key-modal])
   (logginin-modal)
   (note-info-modal)
   [:div.content.pt-2
   [:div.columns
    [:div.column.is-one-fifth.is-flex.is-flex-direction-column.is-flex-wrap-nowrap 
     [:div {:style {:position "sticky" :top "60px"}} [nav-list]] [:br]]
    [main-view-selector]
    ;; [:div.column.is-half.is-flex.is-flex-wrap-nowrap 
    ;;  ]   
    [:div.columm
     [:div.is-flex.is-flex-wrap-nowrap.is-flex-direction-column.is-justify-content-flex-start {:style {:position "sticky" :top "52px"}}
      [:div.m-0
       [connected-relays]]
      ;; [:div [:pre.is-size-7 (with-out-str (pprint @r/relays-atom))]]
      [:div [:pre.is-size-7 "Stats" (<sub [:db-stats])]]
      [:div [relay-info]]]]]]
   [:footer.footer.p-4 [:div.content [:p "nostromat" ]]]])
