(ns nr.gameboard.board
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljc.java-time.duration :as duration]
   [cljc.java-time.instant :as inst]
   [cljc.java-time.temporal.chrono-unit :as chrono]
   [cljs.core.async :refer [<! chan put!] :as async]
   [clojure.string :as s :refer [capitalize ends-with? join lower-case split
                                 starts-with?]]
   [game.core.card :refer [active? asset? corp? facedown? faceup?
                           get-counters get-title has-subtype? ice? program? rezzed?
                           same-card? operation? condition-counter?]]
   [jinteki.cards :refer [all-cards]]
   [jinteki.utils :refer [add-cost-to-label select-non-nil-keys
                          str->int] :as utils]
   [nr.appstate :refer [app-state current-gameid]]
   [nr.cardbrowser :refer [card-as-text]]
   [nr.end-of-game-stats :refer [build-game-stats]]
   [nr.gameboard.actions :refer [send-command]]
   [nr.gameboard.card-preview :refer [card-highlight-mouse-out
                                      card-highlight-mouse-over card-preview-mouse-out
                                      card-preview-mouse-over put-game-card-in-channel zoom-channel]]
   [nr.gameboard.player-stats :refer [stat-controls stats-view]]
   [nr.gameboard.replay :refer [replay-panel]]
   [nr.gameboard.right-pane :refer [content-pane]]
   [nr.gameboard.state :refer [game-state not-spectator? replay-side]]
   [nr.sounds :refer [update-audio]]
   [nr.translations :refer [tr tr-side tr-game-prompt]]
   [nr.utils :refer [banned-span checkbox-button cond-button get-image-path
                     image-or-face map-longest render-icons render-message]]
   [nr.ws :as ws]
   [reagent.core :as r]))

(declare stacked-card-view show-distinct-cards)

(defonce board-dom (atom {}))
(defonce card-menu (r/atom {}))

(defonce corp-prompt-state (r/cursor game-state [:corp :prompt-state]))
(defonce runner-prompt-state (r/cursor game-state [:runner :prompt-state]))

(defn is-replay? [] (= "local-replay" (:gameid @app-state [:gameid])))

(defn- any-prompt-open?
  [side]
  (if (= side :corp)
    @corp-prompt-state
    @runner-prompt-state))

(defn- image-url [{:keys [side code] :as card}]
  (let [lang (get-in @app-state [:options :language] "en")
        res (get-in @app-state [:options :card-resolution] "default")
        art "stock"
        card (if (or (:face card) (:images card)) card (get @all-cards (get-title card)))
        images (image-or-face card)]
    (get-image-path images (keyword lang) (keyword res) (keyword art))))

(defonce button-channel (chan))

(defn open-card-menu
  ([source] (open-card-menu source nil))
  ([source ghost]
   (swap! card-menu assoc :source source :ghost ghost)))

(defn close-card-menu []
  (swap! card-menu dissoc :source :keep-menu-open))

(defn action-list
  [{:keys [type zone rezzed advanceable
           advancementcost current-advancement-requirement exhausted] :as card}]
  (r/with-let [active-player (r/cursor game-state [:active-player])
               side (r/cursor game-state [:side])]
    ;; TODO -> rezzed items can be exiled
    ;;      -> facedown items can be archived
    (cond->> []
      ;; archive -> unrezzed (unforged) items are archived on trash
      (and (#{"Agent" "Obstacle" "Source"} type) (not rezzed))
      (cons "archive")
      ;; exile   -> rezzed (forged) items are exiled on trash
      (and (#{"Agent" "Obstacle" "Source"} type) rezzed)
      (cons "exile")
      ;; exhaust  -> manually exhaust a card
      (and (#{"Agent" "Obstacle" "Source" "Seeker"} type) (not exhausted))
      (cons "exhaust")
      ;; unexhaust -> manually unexhaust a card
      (and (#{"Agent" "Obstacle" "Source" "Seeker"} type) exhausted)
      (cons "unexhaust")
      ;; forge     -> forge a card
      (and (#{"Agent" "Obstacle" "Source"} type) (not rezzed))
      (cons "forge")
      ;; unforge   -> manually unforge a card
      (and (#{"Agent" "Obstacle" "Source"} type) rezzed)
      (cons "unforge"))))

(def click-card-keys
  [:cid :side :host :type :zone :ghost])

(defn card-for-click [card]
  (select-non-nil-keys
    (if (:host card) (update card :host card-for-click) card)
    click-card-keys))

(defn playable?
  "Checks whether a card or ability is playable"
  [action]
  (:playable action))

(defn handle-abilities
  [side {:keys [abilities corp-abilities runner-abilities subroutines facedown] :as card}]
  (let [actions (action-list card)
        c (+ (count actions) (count abilities))
        card-side (keyword (lower-case (:side card)))]
    (swap! card-menu dissoc :keep-menu-open)

    (cond
      ;; Toggle abilities panel
      (or (< 1 c)
          (pos? (+ (count corp-abilities)
                   (count runner-abilities)
                   (count subroutines)))
          (some #{"forge" "unforge" "archive" "exile"} actions))
      (do (when (= side card-side)
            (if (= (:cid card) (:source @card-menu))
              (close-card-menu)
              (open-card-menu (:cid card) (:ghost card))))
          (when (and (= :runner card-side)
                     (= :corp side)
                     corp-abilities)
            (if (= (:cid card) (:source @card-menu))
              (close-card-menu)
              (open-card-menu (:cid card) (:ghost card))))
          (when (and (= :corp card-side)
                     (= :runner side)
                     (or subroutines runner-abilities))
            (if (= (:cid card) (:source @card-menu))
              (close-card-menu)
              (open-card-menu (:cid card) (:ghost card)))))

      ;; Trigger first (and only) ability / action
      (and (= c 1)
           (= side card-side))
      (if (= (count abilities) 1)
        (when (playable? (first abilities))
          (send-command "ability" {:card (card-for-click card) :ability 0}))
        (send-command (first actions) {:card (card-for-click card)})))))

(defn- prompt-button-from-card?
  [clicked-card {:keys [card msg prompt-type choices] :as prompt-state}]
  (when-not (or (some #{:counter :card-title :number} choices)
                (= choices "credit"))
    (some (fn [{:keys [_ uuid value]}]
            (when (= (:cid value) (:cid clicked-card)) uuid))
          choices)))

(defn handle-card-click [{:keys [type zone exhausted rezzed installed] :as card} shift-key-held]
  (let [side (:side @game-state)]
    (when (and (not-spectator?)
               (not (contains? #{"stage" "shift" "bluff" "waiting"} (get-in @game-state [side :prompt-state :prompt-type]))))
      (cond
        ;; Selecting card
        (= (get-in @game-state [side :prompt-state :prompt-type]) "select")
        (send-command "select" {:card (card-for-click card) :shift-key-held shift-key-held})

        ;; A selectable card is clicked outside of a select prompt (ie it's a button on a choices prompt)
        (contains? (into #{} (get-in @game-state [side :prompt-state :selectable])) (:cid card))
        (send-command "choice" {:choice {:uuid (prompt-button-from-card? card (get-in @game-state [side :prompt-state]))}})

        ;; we're trying to rush a card out
        (and (= side (keyword (lower-case (:side card))))
             (not (any-prompt-open? side))
             (contains? #{"hand"} (first zone))
             (:rushable card)
             shift-key-held)
        (send-command "rush" {:card (card-for-click card)})

        ;; we're trying to play a card at instant speed
        (and (= side (keyword (lower-case (:side card))))
             (not (any-prompt-open? side))
             (contains? #{"hand"} (first zone))
             (:flashable card))
        (send-command "flash" {:card (card-for-click card)})

        ;; player clicking on their own playable card
        (and (or (and (= side :runner) (= "Runner" (:side card)))
                 (and (= side :corp) (= "Corp" (:side card))))
             (= "hand" (first zone))
             (playable? card))
        (send-command "play" {:card (card-for-click card) :shift-key-held shift-key-held})

        ;; just rez if the shift key is held
        (and (= side (keyword (lower-case (:side card))))
             installed (not rezzed) shift-key-held)
        (send-command "forge" {:card (card-for-click card)})

        ;; fire collect abilities with just the shift key held, if possible
        (and (= side (keyword (lower-case (:side card))))
             (or (= (:type card) "Seeker")
                 (contains? #{"onhost" "play-area" "scored" "paths"} (first zone))))
        (if (and shift-key-held (or rezzed (= (:type card) "Seeker")) (not exhausted))
          ;; fire a collect ability if possible
          (send-command "collect" {:card (card-for-click card) :shift-key-held shift-key-held})
          (handle-abilities side card))

        ;; no match
        :else nil))))

(defn spectate-side []
  (let [corp-specs (get-in @app-state [:current-game :corp-spectators])
        runner-specs (get-in @app-state [:current-game :runner-spectators])
        me (:user @app-state)]
    (cond
      (some #(= (:uid %) (:uid me)) corp-specs)
      :corp
      (some #(= (:uid %) (:uid me)) runner-specs)
      :runner
      :else
      nil)))

(defn spectator-view-hidden?
  "Checks if spectators are allowed to see hidden information, such as hands and face-down cards"
  []
  (and (get-in @game-state [:options :spectatorhands])
       (not (not-spectator?))))

(defn handle-dragstart [e card]
  (-> e .-target js/$ (.addClass "dragged"))
  (-> e .-dataTransfer (.setData "card" (.stringify js/JSON (clj->js card)))))

(defn handle-drop [e server]
  (-> e .-target js/$ (.removeClass "dragover"))
  (let [card (-> e .-dataTransfer (.getData "card") ((.-parse js/JSON)) (js->clj :keywordize-keys true))]
    (js/console.log (str "dropped on server: " server))
    (when (not= "Seeker" (:type card))
      (send-command "move" {:card card :server server}))))

;; touch support
(defonce touchmove (atom {}))

(defn release-touch [^js/$ card]
  (-> card (.removeClass "disable-transition"))
  (-> card (.css "position" ""))
  (-> card (.css "top" "")))

(defn update-card-position [^js/$ card touch]
  (-> card (.css "left" (str (- (int (aget touch "pageX")) 30) "px")))
  (-> card (.css "top"  (str (- (int (aget touch "pageY")) 42) "px"))))

(defn get-card [e _server]
  (-> e .-target js/$ (.closest ".card-wrapper")))

(defn get-server-from-touch [touch]
  (let [cX (.. touch -clientX)
        cY (.. touch -clientY)
        server (-> (js/document.elementFromPoint cX cY)
                   js/$
                   (.closest "[data-server]")
                   (.attr "data-server"))]
    [server (> (+ (abs (- (:x @touchmove) cX))
                  (abs (- (:y @touchmove) cY)))
               30)]))

(defn remote->num [server]
  (-> server str (clojure.string/split #":remote") last str->int))

(defn remote->name [server]
  (let [num (remote->num server)]
    (str (tr [:game.server "Server"]) " " num)))

(defn zone->sort-key [zone]
  (case (if (keyword? zone) zone (last zone))
    :archives -3
    :rd -2
    :hq -1
    (str->int
      (last (clojure.string/split (str zone) #":remote")))))

(defn get-remotes [servers]
  (->> servers
       (filter #(not (#{:hq :rd :archives} (first %))))
       (sort-by #(zone->sort-key (first %)))))

(defn facedown-card
  "Image element of a facedown card"
  ([side] (facedown-card side [] nil))
  ([side class-list alt-alt-text]
   (let [tag (->> class-list
                  vec
                  (concat ["img" "card"])
                  (join ".")
                  keyword)
         alt (if (nil? alt-alt-text)
               "Facedown card"
               alt-alt-text)]
     [tag {:src (str "/img/hubworld-card-back-2.png")
           :alt alt}])))

(defn sort-archives
  [cards] (->> cards (sort-by get-title) (sort-by #(not (faceup? %)))))

(defn sort-heap
  [cards] (sort-by get-title cards))

(defn sort-archives?
  [] (get-in @app-state [:options :archives-sorted] false))

(defn sort-heap?
  [] (get-in @app-state [:options :heap-sorted] false))

(defn card-img
  "Build an image of the card (is always face-up). Only shows the zoomed card image, does not do any interaction."
  [{:keys [code] :as card}]
  (when code
    [:div.card-frame
     [:div.blue-shade.card {:on-mouse-enter #(put-game-card-in-channel card zoom-channel)
                            :on-mouse-leave #(put! zoom-channel false)}
      (when-let [url (image-url card)]
        [:div
         [:span.cardname (get-title card)]
         [:img.card.bg {:src url :alt (get-title card) :onError #(-> % .-target js/$ .hide)}]])]]))

(defn card-implementation [zoom-card]
  (when-let [card @zoom-card]
    (let [implemented (:implementation card)]
      (case implemented
        (:full "full") nil
        [:div.panel.blue-shade.implementation {:style {:right (get-in @app-state [:options :log-width])}}
         (case implemented
           nil [:span.unimplemented (tr [:game.unimplemented "Unimplemented"])]
           [:span.impl-msg implemented])]))))

(defn card-zoom-display
  [zoom-card img-side]
  (when-let [card @zoom-card]
    [:<>
     [:div.card-preview.blue-shade
      {:on-click #(reset! img-side (not @img-side))}
      (let [url (image-url card)
            show-img (= "image" (get-in @app-state [:options :card-zoom] "image"))]
        (if (and @img-side url show-img)
          [:img {:src url :alt (get-title card) :onLoad #(-> % .-target js/$ .show)}]
          [card-as-text card false]))]
     (when (get-in @app-state [:options :pin-zoom] false)
       [:button.win-right {:on-click #(reset! zoom-card false) :type "button"} "✘"])]))

(defn card-zoom [zoom-card img-side]
  (if @zoom-card
    (do (-> ".card-zoom" js/$ (.addClass "fade"))
        [card-zoom-display zoom-card img-side])
    (do (-> ".card-zoom" js/$ (.removeClass "fade")) nil)))

(defn card-zoom-view [_zoom-card]
  (let [zoomed-card (r/atom nil)
        img-side (r/atom true)]
    (fn [zoom-card]
      (let [pin (get-in @app-state [:options :pin-zoom] false)]
        (when (or @zoom-card
                  (and (not @zoom-card) (not pin)))
          (reset! zoomed-card @zoom-card)
          (reset! img-side true))
        [:<>
         [:div.card-zoom
          [card-zoom zoomed-card img-side]]
         [card-implementation zoomed-card]]))))

(defn- card-menu-item
  "Creates a li item for a card menu using the given id and key.
   Performs the function provided on click or pressing the Enter or Space keys"
  ([label func] (card-menu-item label func true))
  ([label func enabled]
   (if enabled
     [:li {:tab-index 0
           :on-click func
           :on-key-down #(when (= "Enter" (.-key %))
                           (func))
           :on-key-up #(when (= " " (.-key %))
                         (func))}
      label]
     [:li.disabled label])))

(defn server-menu
  "The pop-up on a card in hand when clicked"
  [card]
  (let [servers (get-in @game-state [:corp :install-list])
        active-menu? (= (:cid card) (:source @card-menu))
        label-fn (fn [label]
                   (if (:cid label)
                     (:title label)
                     (tr-game-prompt label)))]
    (when servers
      [:div.panel.blue-shade.servers-menu (when active-menu?
                                            {:class "active-menu"
                                             :style {:display "inline"}})
       [:ul (doall
              (map-indexed
                (fn [_ label]
                  ^{:key label}
                  [card-menu-item (label-fn label)
                   #(do (close-card-menu)
                        (if (= "Expend" label)
                          (send-command "expend" {:card card :server label})
                          (send-command "play" {:card card :server label})))])
                servers))]])))

(defn list-abilities
  [ab-type card abilities]
  (doall
    (map-indexed
      (fn [i ab]
        (let [command (case ab-type
                        :runner "runner-ability"
                        :corp "corp-ability"
                        :ability (if (:dynamic ab) "dynamic-ability" "ability"))
              args (merge {:card card}
                          (if (:dynamic ab)
                            (select-keys ab [:dynamic :source :index])
                            {:ability i}))]
          ^{:key i}
          [card-menu-item (render-icons (add-cost-to-label ab))
           #(do (send-command command args)
                (if (:keep-menu-open ab)
                  (swap! card-menu assoc :keep-menu-open (keyword (:keep-menu-open ab)))
                  (close-card-menu)))
           (:playable ab)]))
      abilities)))

(defn check-keep-menu-open
  [card]
  (let [side (:side @game-state)
        keep-menu-open (case (:keep-menu-open @card-menu)
                        :while-credits-left
                        (pos? (get-in @game-state [side :credit]))

                        :while-clicks-left
                        (pos? (get-in @game-state [side :click]))

                        :while-2-clicks-left
                        (>= (get-in @game-state [side :click]) 2)

                        :while-3-clicks-left
                        (>= (get-in @game-state [side :click]) 3)

                        :while-4-clicks-left
                        (>= (get-in @game-state [side :click]) 4)

                        :while-cards-in-hand
                        (not-empty (get-in @game-state [side :hand]))

                        :while-power-tokens-left
                        (pos? (get-counters card :power))

                        :while-2-power-tokens-left
                        (>= (get-counters card :power) 2)

                        :while-3-power-tokens-left
                        (>= (get-counters card :power) 3)

                        :while-5-power-tokens-left
                        (>= (get-counters card :power) 5)

                        :while-advancement-tokens-left
                        (pos? (get-counters card :advancement))

                        :while-agenda-tokens-left
                        (pos? (get-counters card :agenda))

                        :while-virus-tokens-left
                        (pos? (get-counters card :virus))

                        :while-2-virus-tokens-left
                        (>= (get-counters card :virus) 2)

                        :if-abilities-available
                        (pos? (+ (count (:corp-abilities card))
                                 (count (:runner-abilities card))
                                 (count (remove #(or (starts-with? (:label % "") "Toggle auto-resolve on")
                                                     (ends-with? (:label % "") "(start of turn)"))
                                                (:abilities card)))))

                        :for-agendas
                        (or (some #(= "score" %) (action-list card))          ; can score
                            (not (zero? (get-in @game-state [side :click])))) ; clicks left

                        :forever true

                        false)]
    (when-not keep-menu-open (close-card-menu))
    keep-menu-open))

(defn runner-abs [card runner-abilities subroutines title]
  (when (= (:cid card) (:source @card-menu))
    [:div.panel.blue-shade.runner-abilities.active-menu {:style {:display "inline"}}
     [:button.win-right {:on-click #(close-card-menu) :type "button"} "✘"]
     (when (or (seq runner-abilities)
               (seq subroutines))
       [:span.float-center (tr [:game.abilities "Abilities"]) ":"])
     [:ul
      (list-abilities :runner card runner-abilities)
      (when (seq subroutines)
        [card-menu-item (tr [:game.let-subs-fire "Let unbroken subroutines fire"])
         #(do (send-command "system-msg"
                            {:msg (str "indicates to fire all unbroken subroutines on " title)})
              (close-card-menu))])]
     (when (seq subroutines)
       [:span.float-center (tr [:game.subs "Subroutines"]) ":"])
     (doall
       (map-indexed
         (fn [i sub]
           [:span {:style {:display "block"}
                   :key i}
            [:span (cond (:broken sub)
                         {:class :disabled
                          :style {:font-style :italic}}
                         (false? (:resolve sub))
                         {:class :dont-resolve
                          :style {:text-decoration :line-through}})
             (render-icons (str " [Subroutine]" " " (:label sub)))]
            [:span.float-right
             (cond (:broken sub) banned-span
                   (:fired sub) "✅")]])
         subroutines))]))

(defn corp-abs [card corp-abilities]
  (when (= (:cid card) (:source @card-menu))
    [:div.panel.blue-shade.corp-abilities.active-menu {:style {:display "inline"}}
     [:button.win-right {:on-click #(close-card-menu) :type "button"} "✘"]
     (when (seq corp-abilities)
       [:span.float-center (tr [:game.abilities "Abilities"]) ":"])
     [:ul (list-abilities :corp card corp-abilities)]]))

(defn encounter-info-div
  "Displays encounter information including current ice strength and subroutines"
  [ice]
  (let [subtypes (sort-by #(case %
                             "Mythic" 1
                             ("Barrier" "Code Gate" "Sentry") 2
                             ("Bioroid" "Trap") 3
                             4) (:subtypes ice))
        current-strength (or (:current-strength ice)
                             (:strength ice)
                             0)
        subroutines (:subroutines ice)]
    [:div.panel.blue-shade.encounter-info {:style {:display "inline"}}
     [:span.active.float-center (get-title ice)]
     [:span.info {:style {:display "block"}} (join " - " subtypes)]
     [:span.float-center (tr [:card-browser.strength] "Strength") ": " current-strength]
     [:hr]
     (when (seq subroutines)
       [:span.float-center (tr [:game.subs "Subroutines"]) ":"])
     (doall
       (map-indexed
         (fn [i sub]
           (let [fire-sub #(when (= :corp (:side @game-state))
                             (send-command "subroutine" {:card ice
                                                         :subroutine i})
                             (close-card-menu))]
             [:div {:key i
                    :tab-index 0
                    :on-click fire-sub
                    :on-key-down #(when (= "Enter" (.-key %))
                                    (fire-sub))
                    :on-key-up #(when (= " " (.-key %))
                                  (fire-sub))}
              [:span (cond (:broken sub)
                           {:class :disabled
                            :style {:font-style :italic}}
                           (false? (:resolve sub))
                           {:class :dont-resolve
                            :style {:text-decoration :line-through}})
               (render-icons (str " [Subroutine] " (:label sub)))]
              [:span.float-right
               (cond (:broken sub) banned-span
                     (:fired sub) "✅")]]))
         subroutines))]))

(defn card-abilities [card abilities subroutines]
  (let [actions (action-list card)]
    (when (and (= (:cid card) (:source @card-menu))
               (= (:ghost card) (:ghost @card-menu))
               (or (nil? (:keep-menu-open @card-menu))
                   (check-keep-menu-open card))
               (or (pos? (+ (count actions)
                            (count abilities)
                            (count subroutines)))
                   (some #{"forge" "unforge" "archive" "exile"} actions)))
      [:div.panel.blue-shade.abilities.active-menu {:style {:display "inline"}}
       [:button.win-right {:on-click #(close-card-menu) :type "button"} "✘"]
       (when (seq actions)
         [:span.float-center (tr [:game.actions "Actions"]) ":"])
       (when (seq actions)
         [:ul
          (doall
            (map-indexed
              (fn [_ action]
                (let [keep-menu-open (case action
                                       "unforge" false
                                       "forge" :if-abilities-available
                                       "archive" false
                                       "exile" false
                                       false)]
                  ^{:key action}
                  [card-menu-item (capitalize (tr-game-prompt action))
                   #(do (send-command action {:card card})
                        (if keep-menu-open
                          (swap! card-menu assoc :keep-menu-open keep-menu-open)
                          (close-card-menu)))]))
              actions))])
       (when (or (seq abilities)
                 (and (active? card)
                      (seq (remove #(or (:fired %) (:broken %)) subroutines))))
         [:span.float-center (tr [:game.abilities "Abilities"]) ":"])
       [:ul
        (when (seq abilities)
          (list-abilities :ability card abilities))]])))

(defn draw-facedown?
  "Returns true if the installed card should be drawn face down."
  [{:keys [host] :as card}]
  (or (facedown? card)
      (and (:installed card)
           (not (or (faceup? card)
                    (condition-counter? card))))))

(defn card-view
  [{:keys [zone code type abilities counter exhausted rezzed
           subtypes barrier current-barrier presence current-presence selected hosted
           side facedown card-target icon new shifted ghost runner-abilities subroutines
           subtype-target corp-abilities]
    :as card} flipped disable-click]
  (let [title (get-title card)]
    (r/with-let [prompt-state (r/cursor game-state [(keyword (lower-case side)) :prompt-state])]
      [:div.card-frame.menu-container
       [:div.blue-shade.card {:class (str (cond selected "selected"
                                                (contains? (into #{} (get-in @prompt-state [:selectable])) (:cid card)) "selectable"
                                                (same-card? card (:button @app-state)) "hovered"
                                                (same-card? card (-> @game-state :encounters :ice)) "encountered"
                                                (and (not (any-prompt-open? side)) (playable? card)) "playable"
                                                (and (not (any-prompt-open? side)) (:flashable card)) "playable"
                                                ghost "ghost"
                                                shifted "shifted"
                                                new "new")
                                          (when exhausted " exhausted"))
                              :tab-index (when (and (not disable-click)
                                                    (or (active? card)
                                                        (playable? card)))
                                           0)
                              :draggable (when (and (not-spectator?) (not disable-click)) true)
                              :on-drag-start #(handle-dragstart % card)
                              :on-drag-end #(-> % .-target js/$ (.removeClass "dragged"))
                              :on-mouse-enter #(when (or (not (or (not code) flipped facedown))
                                                         (spectator-view-hidden?)
                                                         (= (:side @game-state) (keyword (lower-case side))))
                                                 (put-game-card-in-channel card zoom-channel))
                              :on-mouse-leave #(put! zoom-channel false)
                              :on-click #(when (not disable-click)
                                           (handle-card-click card (.-shiftKey %)))
                              :on-key-down #(when (and (= "Enter" (.-key %))
                                                       (not disable-click))
                                              (handle-card-click card (.-shiftKey %)))
                              :on-key-up #(when (and (= " " (.-key %))
                                                     (not disable-click))
                                            (handle-card-click card (.-shiftKey %)))}
        (if (or (not code) flipped facedown)
          (let [facedown-but-known (or (not (or (not code) flipped facedown))
                                       (spectator-view-hidden?)
                                       (= (:side @game-state) (keyword (lower-case side))))
                alt-str (when facedown-but-known (str "Facedown " title))]
            [facedown-card side ["bg"] alt-str])
          (when-let [url (image-url card)]
            [:div
             [:img.card.bg {:src url :alt title :onError #(-> % .-target js/$ .hide)}]]))
        [:span.cardname title]
        [:div.counters
         (when counter
           (doall
             (map (fn [[type num-counters]]
                    (when (pos? num-counters)
                      (let [selector (str "div.darkbg." (lower-case (name type)) "-counter.counter")]
                        [(keyword selector) {:key type} num-counters])))
                  (sort-by key counter))))
         (when (pos? (get-counters card :advancement))
           [:div.darkbg.advance-counter.counter {:key "adv"} (get-counters card :advancement)])]
        (let [is-my-side? (= (:side @game-state) (keyword (lower-case side)))]
          (when-let [bval (and (rezzed? card)
                               (or current-barrier barrier))]
            (when (pos? bval)
              [:div.darkbg.barrier {:class (when-not is-my-side? "opponent")} bval])))
        (let [is-my-side? (= (:side @game-state) (keyword (lower-case side)))]
          (when-let [pval (and (rezzed? card)
                               (or current-presence presence))]
            [:div.darkbg.presence {:class (when-not is-my-side? "opponent")} pval]))
        (when-let [{:keys [char color]} icon] [:div.darkbg.icon {:class color} char])
        (when card-target [:div.darkbg.card-target card-target])
        (when subtype-target [:div.darkbg.subtype-target subtype-target])
        (when (active? card)
          (let [server-card (get @all-cards title)]
            [:div.darkbg.additional-subtypes
             (join " - " (remove (into #{} (:subtypes server-card)) subtypes))]))]

       (cond
         (and (= zone ["hand"])
              (#{"Agenda" "Asset" "ICE" "Upgrade"} type))
         [server-menu card]

         (and (= :runner (:side @game-state))
              (pos? (+ (count runner-abilities) (count subroutines))))
         [runner-abs card runner-abilities subroutines title]

         (and (= :corp (:side @game-state))
              (pos? (count corp-abilities)))
         [corp-abs card corp-abilities]

         (= (:side @game-state) (keyword (lower-case side)))
         [card-abilities card abilities subroutines])

       (when (pos? (count hosted))
         [:div.hosted
          (if (and (not (ice? card))
                   (get-in @app-state [:options :stacked-cards] false))
                                        ; stacked mode
            (let [distinct-hosted (vals (group-by get-title hosted))]
              (show-distinct-cards distinct-hosted))

            (doall
              (for [card hosted]
                (let [flipped (draw-facedown? card)]
                  ^{:key (:cid card)}
                  [card-view card flipped]))))])])))

(defn show-distinct-cards
  [distinct-cards]
  (doall (apply concat (for [cards distinct-cards] ; apply concat for one-level flattening
                         (let [hosting (remove #(zero? (count (:hosted %))) cards) ; There are hosted cards on these
                               others (filter #(zero? (count (:hosted %))) cards)
                               facedowns (filter draw-facedown? others)
                               others (remove draw-facedown? others)]
                           [; Hosting
                            (for [c hosting]
                              ^{:key (:cid c)} [:div.card-wrapper {:class (when (playable? c) "playable")}
                                                [card-view c (draw-facedown? c)]])
                            ; Facedown
                            (for [c facedowns]
                              ^{:key (:cid c)} [:div.card-wrapper {:class (when (playable? c) "playable")}
                                                [card-view c true]])
                            ; Rest
                            (when (not-empty others)
                              (if (= 1 (count others))
                                (let [c (first others)
                                      flipped (draw-facedown? c)]
                                  ^{:key (:cid c)}
                                  [:div.card-wrapper {:class (when (playable? c) "playable")}
                                   [card-view c flipped]])
                                [stacked-card-view others]))])))))

(defn stacked-card-view
  [cards]
  [:div.stacked
   (doall
     (for [c cards]
       (let [flipped (draw-facedown? c)]
         ^{:key (:cid c)} [:div.card-wrapper {:class (when (playable? c) "playable")}
                           [card-view c flipped]])))])

(defn drop-area [server hmap]
  (merge hmap {:on-drop #(handle-drop % server)
               :on-drag-enter #(-> % .-target js/$ (.addClass "dragover"))
               :on-drag-leave #(-> % .-target js/$ (.removeClass "dragover"))
               :on-drag-over #(.preventDefault %)
               :data-server server}))

(defn close-popup [event ref msg shuffle? deck?]
  (-> ref js/$ .fadeOut)
  (cond
    shuffle? (send-command "shuffle" {:close "true"})
    deck? (send-command "close-deck")
    msg (send-command "system-msg" {:msg msg}))
  (.stopPropagation event))

(defn label [cursor opts]
  (let [fn (or (get-in opts [:opts :fn]) count)
        classes (str (when (pos? (count cursor)) "darkbg ")
                     (get-in opts [:opts :classes]))]
    [:div.header {:class classes}
     (str (get-in opts [:opts :name])
          (when (not (get-in opts [:opts :hide-cursor])) (str " (" (fn cursor) ")")))]))

(defn- this-user?
  [user]
  (if (:replay @game-state)
    (= (get-in @game-state [@replay-side :user :_id]) (:_id user))
    (= (:_id user) (-> @app-state :user :_id))))

(defn build-hand-card-view
  [hand size wrapper-class]
  [:div
   (doall
     (map-indexed
       (fn [i card]
         [:div {:key (or (:cid card) i)
                :class (str wrapper-class)
                :style {:left (when (< 1 size) (* (/ 320 (dec size)) i))}}
          (cond
            (spectator-view-hidden?)
            [card-view (dissoc card :new :selected)]
            (:cid card)
            [card-view card]
            :else
            [facedown-card (:side card)])])
       hand))])

(defn hand-view []
  (let [s (r/atom {})]
    (fn [side hand hand-size hand-count popup popup-direction]
      (let [size (if (nil? @hand-count) (count @hand) @hand-count)
            filled-hand (concat @hand (repeat (- size (count @hand)) {:side (if (= :corp side) "Corp" "Runner")}))]
        [:div.hand-container
         [:div.hand-controls
          [:div.panel.blue-shade.hand
           (drop-area (if (= :corp side) "HQ" "the Grip") {:class (when (> size 6) "squeeze")})
           [build-hand-card-view filled-hand size "card-wrapper"]
           [label filled-hand {:opts {:name (tr [:game.council "Council"])
                                      :fn (fn [_] (str size "/" (:total @hand-size)))}}]]
          (when popup
            [:div.panel.blue-shade.hand-expand
             {:on-click #(-> (:hand-popup @s) js/$ .fadeToggle)}
             "+"])]
         (when popup
           [:div.panel.blue-shade.popup {:ref #(swap! s assoc :hand-popup %) :class popup-direction}
            [:div
             [:a {:on-click #(close-popup % (:hand-popup @s) nil false false)} (tr [:game.close "Close"])]
             [:label (tr [:game.card-count] size)]
             (let [{:keys [total]} @hand-size]
               (stat-controls :hand-size [:div.hand-size (str total " " (tr [:game.max-hand "Max hand size"]))]))
             [build-hand-card-view filled-hand size "card-popup-wrapper"]]])]))))

(defn show-deck [event ref]
  (-> ((keyword (str ref "-content")) @board-dom) js/$ .fadeIn)
  (-> ((keyword (str ref "-menu")) @board-dom) js/$ .fadeOut)
  (send-command "view-deck"))

(defn identity-view [render-side identity hand-count]
  (let [is-runner (= :runner render-side)
        title (tr [:game.council "Council"])]
    [:div.blue-shade.identity
     [card-view @identity]
     [:div.header {:class "darkbg server-label"}
      (str title " (" hand-count ")")]]))

(defn deck-view [render-side player-side identity deck deck-count]
  (let [is-runner (= :runner render-side)
        title (tr [:game.commons "Commons"])
        ref (str "commons-" (name render-side)) ;;if is-runner "stack" "rd")
        menu-ref (keyword (str ref "-menu"))
        content-ref (keyword (str ref "-content"))]
    (fn [render-side player-side identity deck]
      ;; deck-count is only sent to live games and does not exist in the replay
      (let [deck-count-number (if (nil? @deck-count) (count @deck) @deck-count)]
        [:div.deck-container (drop-area (str title "-"(name (utils/other-side player-side))) {})
         [:div.blue-shade.deck {:on-click (when (and (= render-side player-side) (not-spectator?))
                                            #(let [popup-display (-> (content-ref @board-dom) .-style .-display)]
                                               (if (or (empty? popup-display)
                                                       (= "none" popup-display))
                                                 (-> (menu-ref @board-dom) js/$ .toggle)
                                                 (close-popup % (content-ref @board-dom) "stops looking at their deck" false true))))}
          (when (pos? deck-count-number)
            [facedown-card (:side @identity) ["bg"] nil])
          [:div.header {:class "darkbg server-label"}
           (str title " (" deck-count-number ")")]]
         (when (and (= render-side player-side) (not (is-replay?)))
           [:div.panel.blue-shade.menu {:ref #(swap! board-dom assoc menu-ref %)}
            [:div {:on-click #(do (send-command "shuffle")
                                  (-> (menu-ref @board-dom) js/$ .fadeOut))} (tr [:game.shuffle "Shuffle"])]
            [:div {:on-click #(show-deck % ref)} (tr [:game.show "Show"])]])
         (when (and (= render-side player-side) (not (is-replay?)))
           [:div.panel.blue-shade.popup {:ref #(swap! board-dom assoc content-ref %)}
            [:div
             [:a {:on-click #(close-popup % (content-ref @board-dom) "stops looking at their deck" false true)}
              (tr [:game.close "Close"])]
             [:a {:on-click #(close-popup % (content-ref @board-dom) "stops looking at their deck" true true)}
              (tr [:game.close-shuffle "Close & Shuffle"])]]
            (doall
              (for [card @deck]
                ^{:key (:cid card)}
                [card-view card]))])]))))

(defn exile-view [exile-side player-side exile]
  (let [s (r/atom {})]
    (fn [exile-side player-side exile]
      [:div.exile-container (drop-area (str "Exile-" (name (utils/other-side player-side))) {})
       [:div.blue-shade.discard {:on-click #(-> (:popup @s) js/$ .fadeToggle)}
        (when-not (empty? @exile)
          [card-view (last @exile) nil true])
        [:div.header {:class (str "server-label darkbg")}
         (str (tr [:game.exile "Exile"]) " (" (count @exile) ")")]]
       [:div.panel.blue-shade.popup {:ref #(swap! s assoc :popup %)
                                     :class (if (not= player-side (utils/other-side exile-side)) "me" "opponent")}
        [:div
         [:a {:on-click #(close-popup % (:popup @s) nil false false)} (tr [:game.close "Close"])]]
        (doall
          (for [card (if (sort-heap?) (sort-heap @exile) @exile)]
            ^{:key (:cid card)}
            [card-view card]))]])))

(defn discard-view [discard-side player-side discard]
  (let [s (r/atom {})]
    (fn [discard-side player-side discard]
      (let [draw-card #(if (faceup? %1)
                         [card-view %1 nil %2]
                         (if (or (= player-side discard-side)
                                 (spectator-view-hidden?))
                           [:div.unseen [card-view %1 nil %2]]
                           [facedown-card "corp"]))] ;; TODO - this becomes whatever the facedown is
        [:div.discard-container (drop-area (str "Archives-" (name (utils/other-side player-side))) {})
         [:div.blue-shade.discard {:on-click #(-> (:popup @s) js/$ .fadeToggle)}
          (when-not (empty? @discard)
            [:<> {:key (str "discard-" (name discard-side))} (draw-card (last @discard) true)])
          [:div.header {:class "server-label darkbg"}
           (let [total (count @discard)
                 face-up (count (filter faceup? @discard))]
             (str (tr [:game.archives "Archives"]) " (" total ")"))]]
         [:div.panel.blue-shade.popup {:ref #(swap! s assoc :popup %)
                                       :class (if (not= player-side (utils/other-side discard-side)) "me" "opponent")}
          [:div
           [:a {:on-click #(close-popup % (:popup @s) nil false false)} (tr [:game.close "Close"])]
           [:label (let [total (count @discard)
                         face-up (count (filter faceup? @discard))]
                     (tr [:game.face-down-count] total face-up))]]
          (doall
            (for [[idx c] (map-indexed vector (if (sort-archives?) (sort-archives @discard) @discard))]
              ^{:key idx}
              [:div (draw-card c false)]))]]))))

(defn rfg-view
  ([cards name popup] (rfg-view cards name popup nil))
  ([cards name popup noclick]
   (let [dom (atom {})]
     (fn [cards name popup]
       (when-not (empty? @cards)
         (let [size (count @cards)]
           [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")
                                       :on-click (when popup #(-> (:rfg-popup @dom) js/$ .fadeToggle))}
            (doall
              (map-indexed (fn [i card]
                             [:div.card-wrapper {:key i
                                                 :style {:left (when (> size 1) (* (/ 128 size) i))}}
                              [:div [card-view card nil noclick]]])
                           @cards))
            [label @cards {:opts {:name name}}]
            (when popup
              [:div.panel.blue-shade.popup {:ref #(swap! dom assoc :rfg-popup %)
                                            :class "opponent"}
               [:div
                [:a {:on-click #(close-popup % (:rfg-popup @dom) nil false false)} (tr [:game.close "Close"])]
                [:label (tr [:game.card-count] size)]]
               (doall
                 (for [c @cards]
                   ^{:key (:cid c)}
                   [card-view c]))])]))))))

(defn play-area-view [user name cards]
  (fn [user name cards]
    (let [size (count @cards)]
      (when (pos? size)
        [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")}
         (doall
           (map-indexed (fn [i card]
                          [:div.card-wrapper {:key i
                                              :style {:left (when (> size 1) (* (/ 128 size) i))}}
                           (if (or (:seen card)
                                   (this-user? @user))
                             [card-view card]
                             [facedown-card (:side card)])])
                        @cards))
         [label @cards {:opts {:name name}}]]))))

(defn scored-view [scored agenda-point agenda-point-req me?]
  (let [size (count @scored)
        ctrl (if me? stat-controls (fn [key content] content))]
    [:div.panel.blue-shade.scored.squeeze
     (doall
       (map-indexed (fn [i card]
                      [:div.card-wrapper {:key i
                                          :style {:left (when (> size 1) (* (/ 128 (dec size)) i))}}
                       [:div [card-view card]]])
                    @scored))
     [label @scored {:opts {:name (tr [:game.secured-agents "Secured Agents"])}}]]))
     ;; [:div.stats-area
     ;;  (ctrl :agenda-point [:div (tr [:game.agenda-count] @agenda-point)
     ;;                       (tr [:game.agenda-point-req (if-not (= 7 agenda-point-req) (str " (" agenda-point-req " required)") "")] @agenda-point-req)])]]))

(defn run-arrow [run]
  [:div.run-arrow [:div {:class (cond
                                  (= "movement" (:phase run))
                                  "movement"
                                  (= "approach-ice" (:phase run))
                                  "approach"
                                  (= "encounter-ice" (:phase run))
                                  "encounter"
                                  :else
                                  "")}]])

(defn delve-arrow [{:keys [phase position server] :as delve} me?] ;; todo - rename
  [:div {:class (if me? "delve-arrow-me" "delve-arrow-op")}
   [:div {:class (str (case (:phase delve)
                        "initiation" "initiation"
                        "approach-slot" "approach"
                        "encounter" "encounter"
                        "post-encounter" "encounter"
                        "approach-district" "approach-district"
                        "success" "success"
                        "breach" "breach"
                        "")
                      (when-not (contains? #{"approach-district" "success" "breach"} phase)
                        (str " delve-" position))
                      " delve-" server
                      (if me? " as-delver" " as-defender"))}]])

(defn server-view [{:keys [server central-view run]} opts]
  (let [content (:content server)
        ices (:ices server)
        run-pos (:position run)
        current-ice (when (and run (pos? run-pos) (<= run-pos (count ices)))
                      (nth ices (dec run-pos)))
        max-hosted (apply max (map #(count (:hosted %)) ices))]
    [:div.server
     [:div.ices {:style {:width (when (pos? max-hosted)
                                  (+ 84 3 (* 42 (dec max-hosted))))}}
      (when-let [run-card (:source-card run)]
        [:div.run-card [card-img run-card]])
      (doall
        (for [ice (reverse ices)]
          [:div.ice {:key (:cid ice)
                     :class (when (not-empty (:hosted ice)) "host")
                     ; Since CSS flex display does not work correctly on rotated objects
                     ; (and we're doing a lot of rotating in our front end), this hack is
                     ; necessary to align ice with hosted cards. -- lostgeek, 17.04.2021
                     :style {:left (when (not-empty (:hosted ice)) (* 21 (dec (count (:hosted ice)))))}}
           (let [flipped (not (:rezzed ice))]
             [card-view ice flipped])
           (when (and current-ice (= (:cid current-ice) (:cid ice)))
             [run-arrow run])]))
      (when (and run (not current-ice))
        [run-arrow run])]
     [:div.content
      (when central-view
        central-view)
      (when (not-empty content)
        (doall
          (for [card content]
            (let [is-first (= card (first content))
                  flipped (not (faceup? card))]
              [:div.server-card {:key (:cid card)
                                 :class (str (when central-view "central ")
                                             (when (or central-view
                                                       (and (< 1 (count content)) (not is-first)))
                                               "shift"))}
               [card-view card flipped]]))))
      [label content (update-in opts [:opts] assoc :classes "server-label" :hide-cursor true)]]]))

(defn stacked-label [cursor similar-servers opts]
  (let [similar-server-names (->> similar-servers
                                  (map first)
                                  (map remote->name))
        full-server-names (cons (get-in opts [:opts :name]) similar-server-names)
        numbers (map #(second (split % " ")) full-server-names)]
    [label full-server-names (update-in opts [:opts] assoc
                                        :classes "server-label"
                                        :name (str "Servers " (join ", " numbers))
                                        :hide-cursor true)]))

(defn stacked-view [{:keys [key server similar-servers central-view run]} opts]
  (let [content (apply conj
                       (:content server)
                       ; this unfolds all servers and picks the first item in it
                       ; since this creates a sequence, we need to apply it to conj
                       (map #(-> % second :content first) similar-servers))
        ices (:ices server)
        run-pos (:position run)
        current-ice (when (and run (pos? run-pos) (<= run-pos (count ices)))
                      (nth ices (dec run-pos)))]
    [:div.server
     [:div.ices
      (when-let [run-card (:source-card run)]
        [:div.run-card [card-img run-card]])
      (when (and run (not current-ice))
        [run-arrow run])]
     [:div.content
      [:div.stacked
       (doall (for [card content]
                (let [is-first (= card (first content))
                      flipped (not (faceup? card))]
                  [:div.server-card {:key (:cid card)
                                     :class (str (when (and (< 1 (count content)) (not is-first))
                                                   "shift"))}
                   [card-view card flipped]])))
       [stacked-label content similar-servers opts]]]]))

(defn compare-servers-for-stacking [s1]
  (fn [s2]
    (let [ss1 (second s1)
          ss2 (second s2)]
      (and (= (-> ss1 :content first :normalizedtitle)
              (-> ss2 :content first :normalizedtitle))
           (not= s1 s2)
           (empty? (:ices ss1))
           (empty? (:ices ss2))
           (= 1 (count (:content ss1)))
           (= 1 (count (:content ss2)))
           (-> ss1 :content first asset?)
           (-> ss2 :content first asset?)
           (-> ss1 :content first :rezzed)
           (-> ss2 :content first :rezzed)
           (-> ss1 :content first :hosted empty?)
           (-> ss2 :content first :hosted empty?)))))

;; HERE

(defn hubworld-server-view [viewing-side player-side server-name slots server]
  (let [side-class (if (= player-side viewing-side) "opponent-grid-slot" "me-grid-slot")]
    [:div.server
     [:div.ices
      (let [delve (r/cursor game-state [:delve])]
        (when-let [run-card (:source-card @delve)]
          [:div.run-card [card-img run-card]])
        ;; here
        (doall
          (for [slot (take slots [:inner :middle :outer])]
            ^{:key (str player-side "-" server-name "-" (name slot))}
            [:div.grid-slot {:class (str
                                      (when (and (if-not (get-in @game-state [viewing-side :prompt-state :other-side?])
                                                   (not= viewing-side player-side)
                                                   (= viewing-side player-side))
                                                 (or (= (get-in @game-state [viewing-side :prompt-state :prompt-type]) "stage")
                                                     ;; TODO - we can shift stuff on our opponent's board!
                                                     (and (= (get-in @game-state [viewing-side :prompt-state :prompt-type]) "shift")
                                                          (get-in @game-state [viewing-side :prompt-state :target-paths (keyword server-name) slot]))))
                                        "staging ")
                                      side-class " "
                                      server-name)
                             :on-click (cond (and (not= viewing-side player-side)
                                                  (= (get-in @game-state [viewing-side :prompt-state :prompt-type]) "stage"))
                                             #(send-command "stage-select" {:server server-name :slot slot})
                                             (and (if-not (get-in @game-state [viewing-side :prompt-state :other-side?])
                                                    (not= viewing-side player-side)
                                                    (= viewing-side player-side))
                                                  (= (get-in @game-state [viewing-side :prompt-state :prompt-type]) "shift")
                                                  (get-in @game-state [viewing-side :prompt-state :target-paths (keyword server-name) slot]))
                                             #(send-command "stage-select" {:server server-name :slot slot}))}
             (if-let [{:keys [rezzed seen] :as staged-card}
                      (get-in @game-state [(utils/other-side player-side) :paths (keyword server-name) slot 0])]
               [:div.staged-card {:key (:cid staged-card)}
                [card-view staged-card (not (or rezzed seen))]]
               [:span.district-tier (str (capitalize (name slot)))]) ;; todo - option for if these display
             (when (and (= (:server @delve) server-name)
                        (= (:position @delve) (name slot))
                        (= (:delver @delve) (name player-side)))
               [delve-arrow @delve (= (:delver @delve) (name viewing-side))])
             ])))]
      server]))

(defn replacement-board-view [viewing-side player-side identity deck deck-count hand hand-count discard rfg]
  (let [side-class (if (= player-side viewing-side) "opponent" "me")
        hand-count-number (if (nil? @hand-count) (count @hand) @hand-count)
        centrals [:div.runner-centrals
                  [hubworld-server-view viewing-side player-side "exile"   0 [exile-view (utils/other-side viewing-side) player-side rfg]]
                  [hubworld-server-view viewing-side player-side "archives" 3 [discard-view (utils/other-side viewing-side) player-side discard]]
                  [hubworld-server-view viewing-side player-side "council" 3 [identity-view (utils/other-side viewing-side) identity hand-count-number]]
                  [hubworld-server-view viewing-side player-side "commons" 3 [deck-view (utils/other-side viewing-side) player-side identity deck deck-count]]]]
    [:div.runner-board {:class side-class}
     centrals]))

(defn board-view-corp [player-side identity deck deck-count hand hand-count discard servers run]
  (let [rs (:server @run)
        server-type (first rs)
        side-class (if (= player-side :runner) "opponent" "me")
        hand-count-number (if (nil? @hand-count) (count @hand) @hand-count)]
    [:div.outer-corp-board {:class [side-class
                                    (when (get-in @app-state [:options :sides-overlap]) "overlap")]}
     [:div.corp-board {:class side-class}
      (doall
        (for [server (reverse (get-remotes @servers))
              :let [num (remote->num (first server))
                    similar-servers (filter #((compare-servers-for-stacking server) %) (get-remotes @servers))
                    all-servers (conj similar-servers server)]
              :when (or (empty? similar-servers)            ; it is a normal server-view
                      (not (get-in @app-state [:options :stacked-cards] false)) ; we're not in stacked mode
                      ; otherwise only show one view for the stacked remote
                      (< num (remote->num (first (first similar-servers)))))]
          (if (or (empty? similar-servers)
                (not (get-in @app-state [:options :stacked-cards] false)))
            [server-view {:key num
                          :server (second server)
                          :run (when (= server-type (str "remote" num)) @run)}
             {:opts {:name (remote->name (first server))}}]
            [stacked-view {:key num
                           :server (second server)
                           :similar-servers similar-servers
                           :run (when
                                  (some #(= server-type (str "remote" %)) (map #(remote->num (first %)) all-servers))
                                  (= server-type (str "remote" num)) @run)}
             {:opts {:name (remote->name (first server))}}])))
      [server-view {:key "hq"
                    :server (:hq @servers)
                    :central-view [identity-view :corp identity hand-count-number]
                    :run (when (= server-type "hq") @run)}]
      [server-view {:key "rd"
                    :server (:rd @servers)
                    :central-view [deck-view :corp player-side identity deck deck-count]
                    :run (when (= server-type "rd") @run)}]
      [server-view {:key "archives"
                    :server (:archives @servers)
                    :central-view [discard-view :corp player-side discard]
                    :run (when (= server-type "archives") @run)}]
      [server-view {:key "exile"
                    :server (:archives @servers)
                    :central-view [discard-view :corp player-side discard]
                    :run (when (= server-type "archives") @run)}]]]))

(defn- ghost-card
  "recursively ghosts a card and all hosted cards"
  [card]
  (let [hosted (map ghost-card (:hosted card))]
    (assoc card :ghost true :hosted hosted)))

(defn- find-hosted-programs
  "finds all programs hosted on ice, and makes them have the 'ghost' key"
  [servers]
  (let [servers (concat [(:archives @servers) (:rd @servers) (:hq @servers)] (map second (get-remotes @servers)))
        ices (mapcat :ices servers)
        hosted (mapcat :hosted ices)
        hosted-programs (filter program? hosted)]
    (map ghost-card hosted-programs)))

(defn build-win-box
  "Builds the end of game pop up game end"
  [game-state]
  (let [win-shown (r/atom false)]
    (fn [game-state]
      (when (and (:winner @game-state)
                 (not @win-shown))
        (let [winner (:winner @game-state)
              winning-user (:winning-user @game-state)
              turn (:turn @game-state)
              reason (:reason @game-state)
              time (get-in @game-state [:stats :time :elapsed])]
          [:div.win.centered.blue-shade
           [:div
            winning-user
            " (" (capitalize (tr-side winner)) ") "
            (cond
              (= "Decked" (capitalize reason))
              (tr [:game.win-decked] turn)

              (= "Flatline" (capitalize reason))
              (tr [:game.win-flatlined] turn)

              (= "Concede" (capitalize reason))
              (tr [:game.win-conceded] turn)

              (= "Claim" (capitalize reason))
              (tr [:game.win-claimed] turn)

              (= "Agenda" (capitalize reason))
              (tr [:game.win-points] turn)

              :else
              (tr [:game.win-other] turn reason))]
           [:div (tr [:game.time-taken] time)]
           [:br]
           [build-game-stats (get-in @game-state [:stats :corp]) (get-in @game-state [:stats :runner])]
           (when (not= :spectator (:side @game-state))
             [:br]
             [:div {:class "end-of-game-buttons"}
              (when (= :corp (:side @game-state))
                [:button#rez-all
                 {:on-click #(ws/ws-send! [:game/say {:gameid (current-gameid app-state)
                                                      :msg "/rez-all"}])}
                 (tr [:game.rez-all "Rez All"])])
              [:button#reveal-hand
               {:on-click #(ws/ws-send! [:game/say {:gameid (current-gameid app-state)
                                                    :msg "/show-hand"}])}
               (tr [:game.reveal-my-hand "Reveal My Hand"])]])
           [:button.win-right {:on-click #(reset! win-shown true) :type "button"} "✘"]])))))

(defn- build-in-game-decklists
  "Builds the in-game decklist display"
  [corp-list runner-list]
  (let [lists (map-longest list nil corp-list runner-list)
        card-qty (fn [c] (second c))
        card-name (fn [c] [:div {:text-align "left"
                                 :on-mouse-over #(card-preview-mouse-over % zoom-channel)
                                 :on-mouse-out #(card-preview-mouse-out % zoom-channel)}
                           (render-message (first c))])]
    [:div
     [:table.decklists.table
      [:tbody
       [:tr.win.th
        [:td.win.th (tr [:side.corp "Corp"])] [:td.win.th]
        [:td.win.th (tr [:side.runner "Runner"])] [:td.win.th]]
       (doall (map-indexed
                (fn [i [corp runner]]
                  [:tr {:key i}
                   [:td (card-qty corp)] [:td (card-name corp)]
                   [:td (card-qty runner)] [:td (card-name runner)]])
                lists))]]]))

(defn build-decks-box
  "Builds the decklist display box for open decklists"
  [game-state]
  (let [show-decklists (r/cursor app-state [:display-decklists])]
    (fn [game-state]
      (when (and @show-decklists
                 (get-in @game-state [:decklists]))
        (let [corp-list (or (get-in @game-state [:decklists :corp]) {:- 1})
              runner-list (or (get-in @game-state [:decklists :runner]) {:- 1})]
          [:div.decklists.blue-shade
           [:br]
           [build-in-game-decklists corp-list runner-list]])))))

;; (defn build-start-box
;;   "Builds the start-of-game pop up box"
;;   [my-ident my-user my-hand prompt-state my-keep op-ident op-user op-keep me-quote op-quote my-side]
;;   (let [visible-quote (r/atom true)
;;         mulliganed (r/atom false)
;;         start-shown (r/cursor app-state [:start-shown])
;;         card-back (get-in @app-state [:options :card-back])]
;;     (fn [my-ident my-user my-hand prompt-state my-keep op-ident op-user op-keep me-quote op-quote my-side]
;;       (when (and (not @start-shown)
;;                  (:username @op-user)
;;                  (pos? (count @my-hand)))
;;         (let [squeeze (< 5 (count @my-hand))]
;;           [:div.win.centered.blue-shade.start-game
;;            [:div
;;             [:div
;;              [:div.box
;;               [:div.start-game.ident.column
;;                {:class (case @my-keep "mulligan" "mulligan-me" "keep" "keep-me" "")}
;;                (when-let [url (image-url @my-ident)]
;;                  [:img {:src     url :alt (get-title @my-ident) :onLoad #(-> % .-target js/$ .show)
;;                         :class   (when @visible-quote "selected")
;;                         :onClick #(reset! visible-quote true)}])]
;;               [:div.column.contestants
;;                [:div (:username @my-user)]
;;                [:div.vs "VS"]
;;                [:div (:username @op-user)]
;;                [:div.intro-blurb
;;                 (if @visible-quote
;;                   (str "\"" @me-quote "\"")
;;                   (str "\"" @op-quote "\""))]]
;;               [:div.start-game.ident.column
;;                {:class (case @op-keep "mulligan" "mulligan-op" "keep" "keep-op" "")}
;;                (when-let [url (image-url @op-ident)]
;;                  [:img {:src url
;;                         :alt (get-title @op-ident)
;;                         :onLoad #(-> % .-target js/$ .show)
;;                         :class (when-not @visible-quote "selected")
;;                         :onClick #(reset! visible-quote false)}])]]
;;              (when (not= :spectator @my-side)
;;                [:div.start-hand
;;                 [:div {:class (when squeeze "squeeze")}
;;                  (doall (map-indexed
;;                           (fn [i {:keys [title] :as card}]
;;                             [:div.start-card-frame {:style (when squeeze
;;                                                              {:left (* (/ 610 (dec (count @my-hand))) i)
;;                                                               :position "absolute"})
;;                                                     :id (str "startcard" i)
;;                                                     :key (str (:cid card) "-" i "-" @mulliganed)}
;;                              [:div.flipper
;;                               [:div.card-back
;;                                [:img.start-card {:src (str "/img/hubworld-card-back-2.png")}]]
;;                               [:div.card-front
;;                                (when-let [url (image-url card)]
;;                                  [:div {:on-mouse-enter #(put-game-card-in-channel card zoom-channel)
;;                                         :on-mouse-leave #(put! zoom-channel false)}
;;                                   [:img.start-card {:src url :alt title :onError #(-> % .-target js/$ .hide)}]])]]
;;                              (when-let [elem (.querySelector js/document (str "#startcard" i))]
;;                                (js/setTimeout #(.add (.-classList elem) "flip") (+ 1000 (* i 300))))])
;;                           @my-hand))]])
;;              [:div.mulligan
;;               (if (or (= :spectator @my-side)
;;                       (and @my-keep @op-keep))
;;                 [cond-button (if (= :spectator @my-side)
;;                                (tr [:game.close "Close"]) (tr [:game.start "Start Game"]))
;;                  true #(swap! app-state assoc :start-shown true)]
;;                 (list ^{:key "keepbtn"} [cond-button (tr [:game.keep "Keep"])
;;                                          (= "mulligan" (:prompt-type @prompt-state))
;;                                          #(send-command "choice" {:choice {:uuid (->> (:choices @prompt-state)
;;                                                                                       (filter (fn [c] (= "Keep" (:value c))))
;;                                                                                       first
;;                                                                                       :uuid)}})]
;;                       ^{:key "mullbtn"} [cond-button (tr [:game.mulligan "Mulligan"])
;;                                          (= "mulligan" (:prompt-type @prompt-state))
;;                                          #(do (send-command "choice" {:choice {:uuid (->> (:choices @prompt-state)
;;                                                                                           (filter (fn [c] (= "Mulligan" (:value c))))
;;                                                                                           first
;;                                                                                           :uuid)}})
;;                                               (reset! mulliganed true))]))]]]
;;            [:br]
;;            [:button.win-right {:on-click #(swap! app-state assoc :start-shown true) :type "button"} "✘"]])))))

(defn get-run-ices []
  (let [server (-> (:run @game-state)
                   :server
                   first
                   keyword)]
    (get-in @game-state (concat [:corp :servers] [server] [:ices]))))

(defn get-current-ice []
  (let [run-ice (get-run-ices)
        pos (get-in @game-state [:run :position])
        phase (get-in @game-state [:run :phase])
        encounter-ice (-> @game-state :encounters :ice)
        get-ice-from-pos? (or (= "movement" phase)
                              (get-in @game-state [:run :approached-ice-in-position?]))]
    (or encounter-ice
        (when (and get-ice-from-pos?
                   pos
                   (pos? pos)
                   (<= pos (count run-ice)))
          (nth run-ice (dec pos))))))

;; (def phase->title
;;   {"initiation" (tr [:game.initiation "Initiation"])
;;    "approach-ice" (tr [:game.approach-ice "Approach ice"])
;;    "encounter-ice" (tr [:game.encounter-ice "Encounter ice"])
;;    "movement" (tr [:game.movement "Movement"])
;;    "success" (tr [:game.success "Success"])})

;; (defn phase->next-phase-title
;;   ([run] (phase->next-phase-title (:phase @run) (:position @run)))
;;   ([phase position]
;;    (case phase
;;      "initiation" (tr [:game.approach-ice "Approach ice"])
;;      "approach-ice" (if (rezzed? (get-current-ice))
;;                       (tr [:game.encounter-ice "Encounter ice"])
;;                       (tr [:game.movement "Movement"]))
;;      "encounter-ice" (tr [:game.movement "Movement"])
;;      "movement" (if (zero? position)
;;                   (tr [:game.success "Success"])
;;                   (tr [:game.approach-ice "Approach ice"]))
;;      "success" (tr [:game.run-ends "Run ends"])
;;      ;; Error
;;      (tr [:game.no-current-run "No current run"]))))

;; (defn corp-run-div
;;   [run encounters]
;;   (let [ice (get-current-ice)]
;;     [:div.panel.blue-shade
;;      (when (and @encounters
;;                 ice)
;;        [:<>
;;         [:div {:style {:text-align "center"}
;;                :on-mouse-over #(card-highlight-mouse-over % ice button-channel)
;;                :on-mouse-out #(card-highlight-mouse-out % ice button-channel)}
;;          (tr [:game.encounter-ice "Encounter ice"]) ": " (render-message (get-title ice))]
;;         [:hr]
;;         (when (or (:button @app-state) (get-in @app-state [:options :display-encounter-info]))
;;           [encounter-info-div ice])])
;;      (when @run
;;        [:h4 (tr [:game.current-phase "Current phase"]) ":" [:br] (get phase->title (:phase @run) (tr [:game.unknown-phase "Unknown phase"]))])

;;      (cond
;;        (and (= "approach-ice" (:phase @run))
;;             ice)
;;        [cond-button
;;         (str (tr [:game.rez "Rez"]) " " (get-title ice))
;;         (not (rezzed? ice))
;;         #(send-command "rez" {:card ice
;;                               :press-continue (get-in @app-state [:options :pass-on-rez])})]

;;        (or (= "encounter-ice" (:phase @run))
;;            @encounters)
;;        [cond-button
;;         (tr [:game.fire-unbroken "Fire unbroken subroutines"])
;;         (and (seq (:subroutines ice))
;;              (some #(and (not (:broken %))
;;                          (not (:fired %))
;;                          (:resolve % true))
;;                    (:subroutines ice)))
;;         #(send-command "unbroken-subroutines" {:card ice})])

;;      (if @encounters
;;        ;;Encounter continue button
;;        (let [pass-ice? (and (= "encounter-ice" (:phase @run))
;;                             (= 1 (:encounter-count @encounters)))]
;;          [cond-button
;;           (if pass-ice?
;;             (str (tr [:game.continue-to "Continue to"]) " " (phase->next-phase-title run))
;;             (tr [:game.continue "Continue"]))
;;           (not= "corp" (:no-action @encounters))
;;           #(send-command "continue")])
;;        ;;Non-encounter continue button
;;        [cond-button
;;         (if (or (:next-phase @run)
;;                 (zero? (:position @run)))
;;           (tr [:game.no-further "No further actions"])
;;           (str (tr [:game.continue-to "Continue to"]) " " (phase->next-phase-title run)))
;;         (and (not= "initiation" (:phase @run))
;;              (not= "success" (:phase @run))
;;              (not= "corp" (:no-action @run)))
;;         #(send-command "continue")])

;;      (when (and @run
;;                 (<= (:encounter-count @encounters) 1)
;;                 (not= "success" (:phase @run)))
;;        [checkbox-button
;;         (tr [:game.stop-auto-pass "Stop auto-passing priority"])
;;         (tr [:game.auto-pass "Auto-pass priority"])
;;         (:corp-auto-no-action @run)
;;         #(send-command "toggle-auto-no-action")])]))

;; (defn runner-run-div
;;   [run encounters]
;;   (let [phase (:phase @run)
;;         next-phase (:next-phase @run)
;;         ice (get-current-ice)
;;         pass-ice? (and (= "encounter-ice" phase)
;;                        (= 1 (:encounter-count @encounters)))]
;;     [:div.panel.blue-shade
;;      (when (and @encounters
;;                 ice)
;;        [:<>
;;         [:div {:style {:text-align "center"}
;;                :on-mouse-over #(card-highlight-mouse-over % ice button-channel)
;;                :on-mouse-out #(card-highlight-mouse-out % ice button-channel)}
;;          (tr [:game.encounter-ice "Encounter ice"]) ": " (render-message (get-title ice))]
;;         [:hr]
;;         (when (or (:button @app-state)  (get-in @app-state [:options :display-encounter-info]))
;;           [encounter-info-div ice])])
;;      (when @run
;;        [:h4 (tr [:game.current-phase "Current phase"]) ":" [:br] (get phase->title phase)])

;;      (cond
;;        (:next-phase @run)
;;        [cond-button
;;         (phase->title next-phase)
;;         (and next-phase
;;              (not (:no-action @run)))
;;         #(send-command "start-next-phase")]

;;        (and (not next-phase)
;;             (not (zero? (:position @run)))
;;             (not @encounters))
;;        [cond-button
;;         (str (tr [:game.continue-to "Continue to"]) " " (phase->next-phase-title run))
;;         (not= "runner" (:no-action @run))
;;         #(send-command "continue")]

;;        (and (zero? (:position @run))
;;             (not @encounters)
;;             (= "movement" phase))
;;        [cond-button (tr [:game.breach-server "Breach server"])
;;         (not= "runner" (:no-action @run))
;;         #(send-command "continue")])

;;      (when @encounters
;;        [cond-button
;;         (tr [:game.let-subs-fire "Let unbroken subroutines fire"])
;;         (and (seq (:subroutines ice))
;;              (some #(and (not (:broken %))
;;                          (not (:fired %))
;;                          (:resolve % true))
;;                    (:subroutines ice)))
;;         #(send-command "system-msg"
;;                        {:msg (str "indicates to fire all unbroken subroutines on " (get-title ice))})])

;;      (when @encounters
;;        [cond-button
;;         (if pass-ice?
;;           (str (tr [:game.continue-to "Continue to"]) " " (phase->next-phase-title run))
;;           (tr [:game.continue "Continue"]))
;;         (not= "runner" (:no-action @encounters))
;;         #(send-command "continue")])

;;      (when (and @run
;;                 (not= "success" phase))
;;        [cond-button
;;         (tr [:game.jack-out "Jack Out"])
;;         (and (= "movement" phase)
;;              (not (:cannot-jack-out @run))
;;              (not= "runner" (:no-action @run)))
;;         #(send-command "jack-out")])]))

(def delve->phase-title
  {"initiation" (tr [:game.initiation "Initiation"])
   "approach-slot" (tr [:game.approach-slot "Approach"])
   "encounter" (tr [:game.encounter "Encounter"])
   "post-encounter" (tr [:game.encounter-completion "Post-Encounter"])
   "approach-district" (tr [:game.approach-district "Approach District"])
   "success" (tr [:game.success "Success"])
   "breach" (tr [:game.success "Breach"])})

(defn delve->next-phase-title
  [phase position]
  (case phase
    "initiation" (tr [:game.approach-slot "Approach"])
    "approach-slot" (tr [:game.encounter "Encounter"])
    "encounter" (tr [:game.encounter-completion "Post-Encounter"])
    "post-encounter" (if (= position :inner)
                       (tr [:game.approach-district] "Approach District")
                       (tr [:game.approach-slot "Approach"]))
    "approach-district" (tr [:game.breach "Breach"])
    (tr [:game.unknown "(Unknown)"])))

(defn delve-div-defender
  [delve side]
  (let [phase (:phase @delve)
        next-phase (:next-phase @delve)
        approach (:approached-card @delve)]
    [:div.panel.blue-shade
     (when (and @delve (not= phase "encounter"))
       [:h4 (tr [:game.current-phase "Current phase"]) ":" [:br] (get delve->phase-title phase)])

     (when (and @delve (= phase "encounter"))
       [:h4 (tr [:game.encounter-start "Your opponent is encountering"]) ":" [:br] (if approach (:title approach) "(unknown)")])

     (when (and approach (= phase "approach-slot"))
       [cond-button
        (str (tr [:game.forge "Forge"]) " " (get-title approach))
        (and approach (not (rezzed? approach)) (:forgeable approach))
        #(send-command "forge" {:card approach})])

     (when-not (contains? #{"post-encounter" "encounter" "success" "breach"} phase)
       [cond-button
        (str (tr [:game.continue-to "Continue to"]) " " (delve->next-phase-title phase (:position @delve)))
        (not (get-in @delve [:no-action side]))
        #(send-command "delve-continue")])

     (when (and @delve (not (contains? #{"success" "breach"} phase)))
       [checkbox-button
        (tr [:game.stop-pass "Stop passing priority"])
        (tr [:game.auto-pass "Auto-pass priority"])
        (get-in @delve [:auto-pass-priority])
        #(send-command "delve-toggle-auto-pass")])]))

(defn delve-div-attacker
  [delve side]
  (let [phase (:phase @delve)
        next-phase (:next-phase @delve)
        approach (:approached-card @delve)]
    [:div.panel.blue-shade
     (when (and @delve (not= phase "encounter"))
       [:h4 (tr [:game.current-phase "Current phase"]) ":" [:br] (get delve->phase-title phase)])

     (when (and @delve (= phase "encounter"))
       [:h4 (tr [:game.encounter-start "You are encountering"]) ":" [:br] (if (rezzed? approach) (:title approach) "a facedown card")])

     ;; there's one where we can jack out - that's only post-encounter (after passing a slot)
     (when-not (contains? #{"post-encounter" "encounter" "success" "breach"} phase)
       [cond-button
        (str (tr [:game.continue-to "Continue to"]) " " (delve->next-phase-title phase (:position @delve)))
        (not (get-in @delve [:no-action side]))
        #(send-command "delve-continue")])

     (when (= phase "post-encounter")
       [cond-button
        (str (tr [:game.continue-to "Continue to"]) " " (delve->next-phase-title phase (:position @delve)))
        true
        #(send-command "delve-continue-post-encounter")])

     (when-not (contains? #{"encounter"} phase)
       [cond-button
        (tr [:game.end-delve "End the Delve"])
        (= phase "post-encounter") ;; TODO - not "cannot-end-delve"
        #(send-command "delve-end")])

     ;; when during an encounter, there are buttons for the options
     (when (and (= phase "encounter") (rezzed? approach))
       [cond-button
        (tr [:game.confront "Confront"])
        (rezzed? approach)
        #(send-command "delve-confront")])

     (when (and (= phase "encounter") (not (rezzed? approach)))
       [cond-button
        (tr [:game.discover "Discover"])
        (not (rezzed? approach))
        #(send-command "delve-discover")])

     (when (= phase "encounter")
       [cond-button
        (tr [:game.bypass "Bypass"])
        true
        #(send-command "delve-bypass")])
     ]))


       ;; and the other one is just "continue" on both sides

     ;; (cond
     ;;   (:next-phase @run)
     ;;   [cond-button
     ;;    (phase->title next-phase)
     ;;    (and next-phase
     ;;         (not (:no-action @run)))
     ;;    #(send-command "start-next-phase")]

     ;;   (and (not next-phase)
     ;;        (not (zero? (:position @run)))
     ;;        (not @encounters))
     ;;   [cond-button
     ;;    (str (tr [:game.continue-to "Continue to"]) " " (phase->next-phase-title run))
     ;;    (not= "runner" (:no-action @run))
     ;;    #(send-command "continue")]

     ;;   (and (zero? (:position @run))
     ;;        (not @encounters)
     ;;        (= "movement" phase))
     ;;   [cond-button (tr [:game.breach-server "Breach server"])
     ;;    (not= "runner" (:no-action @run))
     ;;    #(send-command "continue")])

     ;; (when @encounters
     ;;   [cond-button
     ;;    (tr [:game.let-subs-fire "Let unbroken subroutines fire"])
     ;;    (and (seq (:subroutines ice))
     ;;         (some #(and (not (:broken %))
     ;;                     (not (:fired %))
     ;;                     (:resolve % true))
     ;;               (:subroutines ice)))
     ;;    #(send-command "system-msg"
     ;;                   {:msg (str "indicates to fire all unbroken subroutines on " (get-title ice))})])

     ;; (when @encounters
     ;;   [cond-button
     ;;    (if pass-ice?
     ;;      (str (tr [:game.continue-to "Continue to"]) " " (phase->next-phase-title run))
     ;;      (tr [:game.continue "Continue"]))
     ;;    (not= "runner" (:no-action @encounters))
     ;;    #(send-command "continue")])

     ;; (when (and @run
     ;;            (not= "success" phase))
     ;;   [cond-button
     ;;    (tr [:game.jack-out "Jack Out"])
     ;;    (and (= "movement" phase)
     ;;         (not (:cannot-jack-out @run))
     ;;         (not= "runner" (:no-action @run)))
     ;;    #(send-command "jack-out")])]))

(defn delve-div
  [side delve]
  (if (= (name side) (:delver @delve))
    [delve-div-attacker delve side]
    [delve-div-defender delve side]))

(defn now-unix-ms [] (-> (inst/now) (inst/to-epoch-milli)))

(defn calculate-progress [start-time end-time]
  (let [current-time (now-unix-ms)]
    (if (>= current-time end-time)
      100
      (max 0 (* 100 (/ (- current-time start-time) (- end-time start-time)))))))

(defonce interval-id (atom nil))

(defn progress-bar [start-time end-time done]
  (let [progress (r/atom (calculate-progress start-time end-time))]
    (r/create-class
      {:component-did-mount
       (fn []
         (when @interval-id (js/clearInterval @interval-id))
         (reset! interval-id
                 (js/setInterval (fn [] (let [new-progress (calculate-progress start-time end-time)]
                                          (when (and (< @progress 100) (>= new-progress 100))
                                            (do (when @interval-id (js/clearInterval @interval-id))
                                                (done)))
                                          (reset! progress new-progress)))
                                 250))) ;; Update every 25ms second
       :component-will-unmount (fn [this]
                                 ;; Cleanup on component removal
                                 (when @interval-id
                                   (js/clearInterval @interval-id)
                                   (reset! interval-id nil)))
       :reagent-render
       (fn []
         [:div
          [:div {:style {:width "100%"
                         :background-color "#00000000"
                         :border-radius "0px"}}
           [:div {:style {:width (str (- 100 @progress) "%")
                          :transition "width 0.25s linear"
                          :background-color "#d0ba2f"
                          :height "6px"
                          :border-radius "0px"}}]]])})))

(defn prompt-div
  [me {:keys [card msg prompt-type choices start-time end-time] :as prompt-state}]
  (let [id (atom 0)]
    [:div.panel.blue-shade
     (when (and card (not= "Basic Action" (:type card)))
       [:<>
        (let [get-nested-host (fn [card] (if (:host card)
                                           (recur (:host card))
                                           card))
              get-zone (fn [card] (:zone (get-nested-host card)))
              in-play-area? (fn [card] (= (get-zone card) ["play-area"]))
              in-scored? (fn [card] (= (get-zone card) ["scored"]))
              installed? (fn [card] (or (:installed card)
                                        (= "servers" (first (get-zone card)))))]
          (if (or (nil? (:side card))
                  (installed? card)
                  (in-scored? card)
                  (in-play-area? card))
            [:div {:style {:text-align "center"}
                   :on-mouse-over #(card-highlight-mouse-over % card button-channel)
                   :on-mouse-out #(card-highlight-mouse-out % card button-channel)}
             (tr [:game.card "Card"]) ": " (render-message (get-title card))]
            [:div.prompt-card-preview [card-view card false]]))
        [:hr]])
     [:h4 (render-message msg)]
     (cond
       ;; number prompt
       (:number choices)
       (let [n (:number choices)]
         [:div
          [:div.credit-select
           [:select#credit {:default-value (:default choices 0)
                            :onKeyUp #(when (= "Enter" (.-key %))
                                        (-> "#number-submit" js/$ .click)
                                        (.stopPropagation %))}
            (doall (for [i (range (inc n))]
                     [:option {:key i :value i} i]))]]
          [:button#number-submit {:on-click #(send-command "choice"
                                                           {:choice (-> "#credit" js/$ .val str->int)})}
           (tr [:game.ok "OK"])]])

       ;; stage/shift prompts just provide a done button
       (or (= prompt-type "stage") (= prompt-type "shift"))
       [:div [:button#stage-done {:on-click #(send-command "stage-done" {})}
              (tr [:game.done "Done"])]]

       (= prompt-type "bluff")
       [:div
        [progress-bar start-time end-time (fn [] (send-command "bluff-done" {}))]
        [:button#bluff-done {:on-click #(do (when @interval-id
                                              (js/clearInterval @interval-id)
                                              (reset! interval-id nil))
                                            (send-command "bluff-done" {}))}
         (tr [:game.done "Done"])]]

       ;; choice of number of credits
       (= choices "credit")
       (let [n (get-in @game-state [(:side @game-state) :credit])]
         [:div
          [:div.credit-select
           [:select#credit {:default-value (:default choices 0)
                            :onKeyUp #(when (= "Enter" (.-key %))
                                        (-> "#credit-submit" js/$ .click)
                                        (.stopPropagation %))}
            (doall (for [i (range (inc n))]
                     [:option {:key i :value i} i]))]]
          [:button#credit-submit {:on-click #(send-command "choice"
                                             {:choice (-> "#credit" js/$ .val str->int)})}
           (tr [:game.ok "OK"])]])

       ;; auto-complete text box
       (:card-title choices)
       [:div
        [:div.credit-select
         [:input#card-title {:placeholder "Enter a card title"
                             :onKeyUp #(when (= "Enter" (.-key %))
                                         (-> "#card-submit" js/$ .click)
                                         (.stopPropagation %))}]]
        [:button#card-submit {:on-click #(send-command "choice" {:choice (-> "#card-title" js/$ .val)})}
         (tr [:game.ok "OK"])]]

       ;; choice of specified counters on card
       (:counter choices)
       (let [counter-type (keyword (:counter choices))
             num-counters (get-in prompt-state [:card :counter counter-type] 0)]
         [:div
          [:div.credit-select
           [:select#credit {:onKeyUp #(when (= "Enter" (.-key %))
                                        (-> "#counter-submit" js/$ .click)
                                        (.stopPropagation %))}
            (doall (for [i (range (inc num-counters))]
                     [:option {:key i :value i} i]))] (str " " (tr [:game.credits "credits"]))]
          [:button#counter-submit {:on-click #(send-command "choice"
                                             {:choice (-> "#credit" js/$ .val str->int)})}
           (tr [:game.ok "OK"])]])

       ;; otherwise choice of all present choices
       :else
       (doall (for [{:keys [idx uuid value]} choices
                    :when (not= value "Hide")]
                ;; HERE
                [:button {:key idx
                          :on-click #(do (send-command "choice" {:choice {:uuid uuid}})
                                         (card-highlight-mouse-out % value button-channel))
                          :on-mouse-over
                          #(card-highlight-mouse-over % value button-channel)
                          :on-mouse-out
                          #(card-highlight-mouse-out % value button-channel)}
                 (render-message (or (not-empty (get-title value)) value))])))]))

(defn basic-actions [{:keys [delve side active-player end-turn runner-phase-12 corp-phase-12 me opponent delve]}]
  (let [turn (r/cursor game-state [:turn])]
    [:div.panel.blue-shade
     (when (and (zero? (:click @me))
                (pos? (:click @opponent))
                (= (keyword @active-player) side))
       [:button {:on-click #(do (close-card-menu)
                                (send-command "pass"))}
        (tr [:game.pass "Pass Turn"])])

     (when (and (zero? (:click @me))
                (zero? (:click @opponent)))
       [cond-button (tr [:game.end-turn "End Turn"])
        (and (not (get-in @turn [:ending :initiated]))
             (not (get-in @turn [:ending side]))
             (not @end-turn))
        #(do (close-card-menu)
             (send-command "end-turn"))])

     (if (= (keyword @active-player) side)
       (when @end-turn
         [:button {:on-click #(do
                                (swap! app-state assoc :start-shown true)
                                (send-command "start-turn"))}
          (tr [:game.start-turn "Start Turn"])]))
     [:div.run-button.menu-container
      [cond-button (tr [:game.delve "Delve"])
       (and (pos? (:click @me))
            (= (keyword @active-player) side)
            (playable? (get-in @me [:basic-action-card :abilities  6])))
       #(do (if (= :run-button (:source @card-menu))
              (close-card-menu)
              (open-card-menu :run-button)))]
      [:div.panel.blue-shade.servers-menu (when (= :run-button (:source @card-menu))
                                            {:class "active-menu"
                                             :style {:display "inline"}})
       [:ul
        (let [servers ["Archives" "Council" "Commons"]]
          (doall
            (map-indexed (fn [_ label]
                           ^{:key label}
                           [card-menu-item (tr-game-prompt label)
                            #(do (close-card-menu)
                                 (send-command "delve" {:server label}))])
                         servers)))]]]
     [cond-button (tr [:game.draw "Draw"])
      (and (playable? (get-in @me [:basic-action-card :abilities 1]))
           (pos? (:deck-count @me)))
      #(send-command "draw")]
     [cond-button (tr [:game.gain-credit "Gain Credit"])
      (playable? (get-in @me [:basic-action-card :abilities 0]))
      #(send-command "credit")]
     [cond-button (tr [:game.shift "Shift Card"])
      (playable? (get-in @me [:basic-action-card :abilities 5]))
      #(send-command "shift")]]))

(defn button-pane [{:keys [side prompt-state]}]
  (let [autocomp (r/track (fn [] (get-in @prompt-state [:choices :autocomplete])))
        show-discard? (r/track (fn [] (get-in @prompt-state [:show-discard])))
        show-exile? (r/track (fn [] (get-in @prompt-state [:show-exile])))
        prompt-type (r/track (fn [] (get-in @prompt-state [:prompt-type])))
        opened-by-system (r/atom false)]
    (r/create-class
      {:display-name "button-pane"

       :component-did-update
       (fn []
         (when (pos? (count @autocomp))
           (-> "#card-title" js/$ (.autocomplete (clj->js {"source" @autocomp}))))
         (cond @show-discard? (do (-> ".me .discard-container .popup" js/$ .fadeIn)
                                  (reset! opened-by-system true))
               @show-exile? (do (-> ".me .exile-container .popup" js/$ .fadeIn)
                                  (reset! opened-by-system true))
               @opened-by-system (do (-> ".me .discard-container .popup" js/$ .fadeOut)
                                     (-> ".me .exile-container .popup" js/$ .fadeOut)
                                     (reset! opened-by-system false)))
         (if (= "select" @prompt-type)
           (set! (.-cursor (.-style (.-body js/document))) "url('/img/gold_crosshair.png') 12 12, crosshair")
           (set! (.-cursor (.-style (.-body js/document))) "default"))
         (when (= "card-title" @prompt-type)
           (-> "#card-title" js/$ .focus)))

       :reagent-render
       (fn [{:keys [side run encounters prompt-state me opponent turn] :as button-pane-args}]
         [:div.button-pane {:on-mouse-over #(card-preview-mouse-over % zoom-channel)
                            :on-mouse-out  #(card-preview-mouse-out % zoom-channel)}
          (let [delve (r/cursor game-state [:delve])]
            (cond
              (and @prompt-state (not= "delve" (get-in @prompt-state [:prompt-type])))
              [prompt-div me @prompt-state]
              ;; if it's a delve, show the delve shit
              @delve [delve-div side delve]
            ;; otherwise just show basic shit
              :else [basic-actions button-pane-args]))])})))

(defn- time-until
  "Helper method for timer. Computes how much time is left until `end`"
  [end]
  (let [now (inst/now)
        diff (duration/between now end)
        total-seconds (duration/get diff chrono/seconds)
        minutes (abs (quot total-seconds 60))
        seconds (mod (abs total-seconds) 60)
        positive (pos? total-seconds)]
        {:minutes minutes :seconds seconds :pos positive}))

(defn- warning-class
  "Styling for the timer display."
  [{:keys [minutes pos]}]
  (if pos
    (condp >= minutes
      2 "red"
      5 "yellow"
      nil)
    "danger"))

(defn time-remaining
  "Component which displays a readout of the time remaining on the timer."
  [start-date timer hidden]
  (let [end-time (-> start-date
                     (inst/parse)
                     (inst/plus timer chrono/minutes))
        remaining (r/atom nil)
        interval (r/atom nil)]
    (r/create-class
      {:component-did-mount
       (fn []
         (reset! interval
                 ;; Update timer at most every 1 sec
                 (js/setInterval #(reset! remaining (time-until end-time)) 1000)))
       :component-will-unmount
       (fn []
         (js/clearInterval @interval)
         (reset! interval nil))
       :reagent-render
       (fn []
         (when (and @remaining (not @hidden))
           [:span.float-center.timer
            {:class (warning-class @remaining)}
            (str
              (when-not (:pos @remaining) "-")
              (:minutes @remaining) (tr [:game.minutes "m:"])
              (:seconds @remaining) (tr [:game.seconds-remaining "s remaining"]))]))})))

(defn- time-since
  "Helper method for match duration. Computes how much time since game start"
  [start]
  (let [start-time (-> start
                       (inst/parse))
        now (inst/now)
        diff (duration/between start-time now)
        total-seconds (duration/get diff chrono/seconds)
        minutes (abs (quot total-seconds 60))
        seconds (mod (abs total-seconds) 60)]
    {:minutes minutes :seconds seconds}))

(defn match-duration
  "Component which displays a readout of the time since the start of the match."
  [start-date hidden]
  (let [duration (r/atom nil)
        interval (r/atom nil)]
    (r/create-class
      {:component-did-mount
       (fn []
         (reset! interval
                 ;; Update timer at most every 1 sec
                 (js/setInterval #(reset! duration (time-since start-date)) 1000)))
       :component-will-unmount
       (fn []
         (js/clearInterval @interval)
         (reset! interval nil))
       :reagent-render
       (fn []
         (when (not @hidden)
           [:span.float-center.timer
            (str
              (:minutes @duration) (tr [:game.minutes "m:"])
              (:seconds @duration) (tr [:game.seconds "s"]))])
         )})))

(defn starting-timestamp [start-date timer]
  ;; I don't like using js/Date, but `toLocalTimeString`
  ;; is just too convenient
  (let [start-time-string (str (tr [:game.game-start "Game start"])
                               ": " (.toLocaleTimeString (js/Date. start-date)))
        hide-timer (r/atom false)]
    (fn []
      [:div.panel.blue-shade.timestamp
       [:span.float-center start-time-string]
       [:<>
        [:span.pm {:on-click #(swap! hide-timer not)}
         (if @hide-timer "+" "-")]
        (if timer [:span {:on-click #(swap! hide-timer not)}
                        [time-remaining start-date timer hide-timer]]
                  [:span {:on-click #(swap! hide-timer not)}
                        [match-duration start-date hide-timer]])]])))


(defn- handle-click [{:keys [render-board?]} e]
  (when render-board?
    (when (-> e .-target (.closest ".menu-container") nil?)
      (close-card-menu))))

(defn- get-element-for-num
  [num]
  (when-let [container (or (-> js/document (.getElementsByClassName "active-menu") array-seq first)
                           (-> js/document (.getElementsByClassName "button-pane") array-seq first))]
    (let [index (if (zero? num)
                  9
                  (dec num))
          elements (if (:source @card-menu)
                     (-> container (.getElementsByTagName "li") array-seq)
                     (-> container (.getElementsByTagName "button") array-seq))]
      (nth elements index nil))))

(defn- handle-num-key-down
  [num]
  (when-let [element (get-element-for-num num)]
    (.focus element)))

(defn- handle-num-key-up
  [num]
  (when-let [element (get-element-for-num num)]
    (when (= element (.-activeElement js/document))
      (.click element)
      (.blur element))))

(defn- focus-log-input [clear-input?]
  (when-let [log-input (-> js/document (.getElementById "log-input"))]
    (.focus log-input)
    (when clear-input?
      (set! (.-value log-input) ""))))

(defn- handle-key-down [{:keys [render-board?]} e]
  (when render-board?
    (let [active-element-type (.-type (.-activeElement js/document))
          not-text-input? (not= "text" active-element-type)
          can-focus? (-> js/document .-activeElement js/$ (.attr "tabindex"))]
      (case (.-key e)
        "Escape" (do (-> js/document .-activeElement .blur)
                     (close-card-menu))
        "Enter" (when-not (or active-element-type
                              can-focus?)
                  (focus-log-input false)
                  (.preventDefault e))
        "/" (when not-text-input?
              (focus-log-input true))
        ("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
        (when not-text-input?
          (handle-num-key-down (str->int (.-key e))))
        ;; else
        nil))))

(defn- handle-key-up [{:keys [side active-player render-board?
                             corp-phase-12 runner-phase-12
                             end-turn run
                             encounters active-page]} e]
  (when (and render-board?
             (not= "text" (.-type (.-activeElement js/document))))
    (let [clicks (:click (@side @game-state))
          active-player-kw (keyword @active-player)
          prompt-state (:prompt-state (@side @game-state))
          prompt-type (keyword (:prompt-type prompt-state))
          no-action (keyword (or (:no-action @run)
                                 (:no-action @encounters)))]
      (case (.-key e)
        " " (cond
              ;; keep default space behavior for focusable items
              (or (.-type (.-activeElement js/document))
                  (-> js/document .-activeElement js/$ (.attr "tabindex")))
              nil
              ;; continue run
              (or @run
                  @encounters)
              (when (and (or (not prompt-state)
                             (= :run prompt-type))
                         (not= @side no-action))
                (send-command "continue")
                (.stopPropagation e))
              ;; no action for prompt
              prompt-state
              nil
              ;; click for credits
              (pos? clicks)
              (do (send-command "credit")
                  (.stopPropagation e))
              ;; end turn
              (and (= active-player-kw @side)
                   (not (or @runner-phase-12 @corp-phase-12))
                   (zero? clicks)
                   (not @end-turn))
              (do (send-command "end-turn")
                  (close-card-menu)
                  (.stopPropagation e))
              ;; gain clicks/mandatory draw
              (and (= active-player-kw @side)
                   (or @runner-phase-12 @corp-phase-12))
              (do (send-command "end-phase-12")
                  (.stopPropagation e))
              ;; start turn
              (and (not= active-player-kw @side)
                   @end-turn)
              (do (send-command "start-turn")
                  (.stopPropagation e)))
        "Alt" (.preventDefault e)
        ("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
        (handle-num-key-up (str->int (.-key e)))
        ;; else
        nil))))

(defn gameboard []
  (let [active (r/cursor app-state [:active-page])
        start-date (r/cursor game-state [:start-date])
        timer (r/cursor game-state [:options :timer])
        run (r/cursor game-state [:run])
        encounters (r/cursor game-state [:encounters])
        side (r/cursor game-state [:side])
        turn (r/cursor game-state [:turn :index])
        end-turn (r/cursor game-state [:end-turn])
        corp-phase-12 (r/cursor game-state [:corp-phase-12])
        runner-phase-12 (r/cursor game-state [:runner-phase-12])
        corp (r/cursor game-state [:corp])
        runner (r/cursor game-state [:runner])
        active-player (r/cursor game-state [:active-player])
        zoom-card (r/cursor app-state [:zoom])
        background (r/cursor app-state [:options :background])
        custom-bg-url (r/cursor app-state [:options :custom-bg-url])
        labeled-unrezzed-cards (r/cursor app-state [:options :labeled-unrezzed-cards])
        labeled-cards (r/cursor app-state [:options :labeled-cards])]

    (go (while true
          (let [zoom (<! zoom-channel)]
            (swap! app-state assoc :zoom zoom))))

    (go (while true
          (let [button (<! button-channel)]
            (swap! app-state assoc :button button))))

    (r/create-class
      {:display-name "gameboard"

       :component-did-mount
       (fn [this]
         (-> js/document (.addEventListener
                           "keydown"
                           (partial handle-key-down {:render-board? (and @corp @runner @side true)})))
         (-> js/document (.addEventListener
                           "keyup"
                           (partial handle-key-up {:side side :active-player active-player :render-board? (and @corp @runner @side true)
                                                   :corp-phase-12 corp-phase-12 :runner-phase-12 runner-phase-12
                                                   :end-turn end-turn :run run
                                                   :encounters encounters})))
         (-> js/document (.addEventListener
                           "click"
                           (partial handle-click {:render-board? (and @corp @runner @side true)}))))

       :component-will-unmount
       (fn [this]
         (-> js/document (.addEventListener
                           "keydown"
                           (partial handle-key-down {:render-board? (and @corp @runner @side true)})))
         (-> js/document (.removeEventListener
                           "keyup"
                           (partial handle-key-up {:side side :active-player active-player :render-board? (and @corp @runner @side true)
                                                   :corp-phase-12 corp-phase-12 :runner-phase-12 runner-phase-12
                                                   :end-turn end-turn :run run
                                                   :encounters encounters})))
         (-> js/document (.addEventListener
                           "click"
                           (partial handle-click {:render-board? (and @corp @runner @side true)}))))

       :reagent-render
       (fn []
        (when (and @corp @runner @side true)
           (let [me-side (if (= :spectator @side)
                          (or (spectate-side) :corp)
                          @side)
                 op-side (utils/other-side me-side)
                 me (r/cursor game-state [me-side])
                 opponent (r/cursor game-state [op-side])
                 ;; hands
                 me-hand (r/cursor game-state [me-side :hand])
                 me-hand-count (r/cursor game-state [me-side :hand-count])
                 op-hand (r/cursor game-state [op-side :hand])
                 op-hand-count (r/cursor game-state [op-side :hand-count])
                 me-hand-size (r/cursor game-state [me-side :hand-size])
                 op-hand-size (r/cursor game-state [op-side :hand-size])
                 ;; decks
                 me-deck (r/cursor game-state [me-side :deck])
                 me-deck-count (r/cursor game-state [me-side :deck-count])
                 op-deck (r/cursor game-state [op-side :deck])
                 op-deck-count (r/cursor game-state [op-side :deck-count])
                 ;; discards
                 me-discard (r/cursor game-state [me-side :discard])
                 op-discard (r/cursor game-state [op-side :discard])
                 me-rfg (r/cursor game-state [me-side :rfg])
                 op-rfg (r/cursor game-state [op-side :rfg])
                 ;; user settings
                 me-user (r/cursor game-state [me-side :user])
                 op-user (r/cursor game-state [op-side :user])
                 ;; prompts
                 me-prompt (r/cursor game-state [me-side :prompt])
                 prompt-state (r/cursor game-state [me-side :prompt-state])
                 ;; identity cards
                 me-ident (r/cursor game-state [me-side :identity])
                 op-ident (r/cursor game-state [op-side :identity])
                 ;; score areas
                 me-scored (r/cursor game-state [me-side :scored])
                 op-scored (r/cursor game-state [op-side :scored])
                 me-agenda-point (r/cursor game-state [me-side :agenda-point])
                 op-agenda-point (r/cursor game-state [op-side :agenda-point])
                 me-agenda-point-req (r/cursor game-state [me-side :agenda-point-req])
                 op-agenda-point-req (r/cursor game-state [op-side :agenda-point-req])
                 ;; servers
                 corp-servers (r/cursor game-state [:corp :servers])
                 runner-rig (r/cursor game-state [:runner :rig])
                 sfx (r/cursor game-state [:sfx])]
             [:div.gameview
              [:div {:class [:gameboard
                             (when @labeled-unrezzed-cards :show-unrezzed-card-labels)
                             (when @labeled-cards :show-card-labels)]}
               [build-decks-box game-state]
               [build-win-box game-state]

               [:div {:class (if (:replay @game-state)
                               (case @replay-side
                                 :runner (get-in @game-state [:runner :user :options :background] "lobby-bg")
                                 :corp (get-in @game-state [:corp :user :options :background] "lobby-bg")
                                 :spectator @background)
                               @background)
                         :style (if (= @background "custom-bg")
                                  {:background (str "url(\"" @custom-bg-url "\")")
                                   :background-size "cover"}
                                  {})}]

               [:div.right-pane
                [card-zoom-view zoom-card]

                (if (:replay @game-state)
                  [content-pane :log :settings :notes :notes-shared]
                  [content-pane :log :settings :help])]

               [:div.centralpane
                (if (= op-side :corp)
                  [replacement-board-view :runner me-side op-ident op-deck op-deck-count op-hand op-hand-count op-discard op-rfg]
                  [replacement-board-view :corp me-side op-ident op-deck op-deck-count op-hand op-hand-count op-discard op-rfg])
                (if (= me-side :corp)
                  [replacement-board-view :corp op-side me-ident me-deck me-deck-count me-hand me-hand-count me-discard me-rfg]
                  [replacement-board-view :runner op-side me-ident me-deck me-deck-count me-hand me-hand-count me-discard me-rfg])]
                  ;;[board-view-corp me-side me-ident me-deck me-deck-count me-hand me-hand-count me-discard corp-servers run]
               ;;[board-view-runner me-side me-ident me-deck me-deck-count me-hand me-hand-count me-discard runner-rig run corp-servers me-rfg])]

               [:div.leftpane
                [:div.opponent
                 [hand-view op-side op-hand op-hand-size op-hand-count (atom nil) (= @side :spectator)]]

                [:div.inner-leftpane
                 [:div.left-inner-leftpane
                  [:div
                   [stats-view opponent op-side]
                   [scored-view op-scored op-agenda-point op-agenda-point-req false]]
                  [:div
                   [scored-view me-scored me-agenda-point me-agenda-point-req true]
                   [stats-view me me-side]]]

                 [:div.right-inner-leftpane
                  (let [op-set-aside (r/cursor game-state [op-side :set-aside])
                        op-current (r/cursor game-state [op-side :current])
                        op-play-area (r/cursor game-state [op-side :play-area])
                        last-revealed (r/cursor game-state [:last-revealed])
                        me-set-aside (r/cursor game-state [me-side :set-aside])
                        me-current (r/cursor game-state [me-side :current])
                        me-play-area (r/cursor game-state [me-side :play-area])]
                    [:div
                     (when-not (:replay @game-state)
                       [starting-timestamp @start-date @timer])
                     ;;[rfg-view op-rfg (tr [:game.rfg "Removed from the game"]) true]
                     ;;[rfg-view me-rfg (tr [:game.rfg "Removed from the game"]) true]
                     [rfg-view op-set-aside (tr [:game.set-aside "Set aside"]) false]
                     [rfg-view me-set-aside (tr [:game.set-aside "Set aside"]) false]
                     [play-area-view op-user (tr [:game.play-area "Play Area"]) op-play-area]
                     [play-area-view me-user (tr [:game.play-area "Play Area"]) me-play-area]
                     ;;[rfg-view op-current (tr [:game.current "Current"]) false]
                     ;;[rfg-view me-current (tr [:game.current "Current"]) false]
                     ;;[rfg-view last-revealed (tr [:game.last-revealed "Last Revealed"]) false true]
                     ])
                  (when (or (not= @side :spectator)
                            (and (spectator-view-hidden?) (spectate-side)))
                    [button-pane {:side me-side :active-player active-player :run run :encounters encounters
                                  :end-turn end-turn :runner-phase-12 runner-phase-12
                                  :corp-phase-12 corp-phase-12 :corp corp :runner runner
                                  :me me :opponent opponent :prompt-state prompt-state}])]]

                [:div.me
                 [hand-view me-side me-hand me-hand-size me-hand-count prompt-state true]]]]
              (when (:replay @game-state)
                [:div.bottompane
                 [replay-panel]])])))})))

(defonce sfx (r/track #(select-keys @game-state [:sfx :sfx-current-id])))
(defonce trigger-sfx (r/track! #(update-audio @sfx)))
