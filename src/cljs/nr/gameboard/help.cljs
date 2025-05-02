(ns nr.gameboard.help
  (:require
   [nr.account :refer [post-options]]
   [nr.translations :refer [tr]]))

(defn tips-box [header tips]
  [:div {:class "bg-gray-100 p-4 rounded-xl shadow-md text-sm space-y-2 max-w-xs"}
   [:h3 {:class "font-semibold text-gray-700"} header]
   (for [tip tips]
     ^{:key tip}
     [:div {:class "text-gray-600 leading-snug"} tip])])

(defn help-pane []
  (fn []
    [:div.help
     [tips-box "Card Shortcuts"
      [[:span [:code.inline-code "Shift+click"] " -> Forge"]
       [:span [:code.inline-code "Shift+click"] " -> Rush"]
       [:span [:code.inline-code "Shift+click"] " -> Collect"]
       [:span [:code.inline-code "Shift+click"] " -> Play Instant"]]]
     [tips-box "I made a mistake"
      [[:span [:code.inline-code "/undo-click"] " undoes the last action"]
       [:span [:code.inline-code "/undo-turn"] " (both players must do this)"]
       [:span [:code.inline-code"/close-prompt"] " to close prompts"]]]]))
