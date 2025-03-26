(ns game.core.shifting
  (:require
   [clojure.string :as str]
   [game.core.card :refer [get-card]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [resolve-ability]]
   [game.core.moving :refer [move trash swap-installed shift-installed]]
   [game.core.prompts :refer [show-shift-prompt]]
   [game.core.say :refer [system-msg]]
   [game.core.to-string :refer [card-str]]
   [game.macros :refer [msg req wait-for]]
   [jinteki.utils :refer [adjacent-zones]]))

(defn shift
  [state side card target-server target-slot args]
  (if-let [card-to-swap (get-in @state [side :paths target-server target-slot])]
    (swap-installed state side card card-to-swap)
    (shift-installed state side card target-server target-slot)))

(defn shift-a-card
  ([state side eid source-card card-to-shift]
   (shift-a-card state side eid source-card card-to-shift nil))
  ([state side eid source-card card-to-shift {:keys [cost] :as args}]
   (show-shift-prompt
     state side eid source-card (adjacent-zones card-to-shift)
     (str "Shift " (:title card-to-shift) " where?")
     {:cost cost
      :msg (msg (let [server (:server context)
                      slot (:slot context)
                      old-card (get-in @state [side :paths server slot 0])]
                  ;; TODO - account for shifting oppo cards!
                  (if old-card
                    (str "swap " (card-str state source-card) " with " (card-str state old-card))
                    (str "shift " (card-str state source-card) " to the " (name slot) " of [their] " (str/capitalize (name server)) " path"))))
      :effect (req (let [server (:server context)
                         slot (:slot context)]
                     (shift state side card-to-shift server slot nil)))}
     {:waiting-prompt true})))
