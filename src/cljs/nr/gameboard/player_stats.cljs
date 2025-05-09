(ns nr.gameboard.player-stats
  (:require
   [clojure.string :as s :refer [capitalize lower-case]]
   [nr.appstate :refer [app-state]]
   [nr.avatar :refer [avatar]]
   [nr.gameboard.actions :refer [send-command]]
   [nr.gameboard.state :refer [game-state not-spectator?]]
   [nr.translations :refer [tr tr-pronouns]]))

(defn stat-controls
  "Create an overlay to increase/decrease a player attribute (e.g. credits)."
  ([key content] (stat-controls key 1 -1 content))
  ([key increment decrement content]
   (if (not-spectator?)
     [:div.stat-controls
      content
      [:div.controls
       [:button.small {:on-click #(send-command "change" {:key key :delta decrement}) :type "button"} "-"]
       [:button.small {:on-click #(send-command "change" {:key key :delta increment}) :type "button"} "+"]]]
     content)))

(defn- stat-controls-for-side
  [side]
  (if (= (:side @game-state) side)
    stat-controls
    (fn [key content] content)))

(defn- name-area
  [user]
  [:div.name-area [avatar user {:opts {:size 32}}]
   [:div.name-box
    [:div.username (:username user)]
    (if-let [pronouns (get-in user [:options :pronouns])]
      (let [pro-str (if (= "blank" pronouns) "" (tr-pronouns pronouns))]
        [:div.pronouns (lower-case pro-str)]))]])

(defn stats-area-hubworld [player side]
  (let [ctrl (stat-controls-for-side side)]
    (fn [player]
      (let [{:keys [user click credit heat]} @player
            icons? (get-in @app-state [:options :player-stats-icons] true)]
        [:div.stats-area
         (let [{:keys [base additional]} heat]
           (if icons?
             [:div.icon-grid
              (ctrl :click  [:div click  " " [:span.anr-icon.click]])
              (ctrl :credit [:div credit " " [:span.anr-icon.credit]])
              (ctrl :heat   [:div base (when (pos? additional) (str " + " additional))   " " [:span.anr-icon.heat]])
              (when (= side (-> @game-state :turn :first-player keyword))
                [:span.first-player "First Player"])]
             [:<>
              (ctrl :click  [:div (tr [:game.click-count] click)])
              (ctrl :credit [:div (tr [:game.credit-count] credit)])
              (ctrl :heat   [:div (tr [:game.heat-count] heat)] base additional)
              (when (= side (-> @game-state :turn :first-player keyword))
                [:span.first-player "First Player"])]))]))))

(defn stats-view
  [player side]
  (fn [player side]
    [:div.panel.blue-shade.stats {:class (when (:active @player) "active-player")}
     (name-area (:user @player))
     ;; note: react doesn't like defmulti much, and caches the first hit
     ;; when it redoes stats - so it needs a key to re-render if sides
     ;; change (ie playing a corp game after playing a runner game) - nbk
     ^{:key (get-in @player [:identity :side])} [stats-area-hubworld player side]]))
