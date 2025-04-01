(ns game.core.shifting
  (:require
   [clojure.string :as str]
   [game.core.card :refer [get-card]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [resolve-ability]]
   [game.core.moving :refer [move trash swap-installed shift-installed]]
   [game.core.prompts :refer [show-shift-prompt]]
   [game.core.say :refer [system-msg]]
   [game.core.to-string :refer [hubworld-card-str]]
   [game.macros :refer [msg req wait-for]]
   [jinteki.utils :refer [other-side adjacent-zones]]))

(defn shift
  [state side card target-server target-slot args]
  (if-let [card-to-swap (get-in @state [side :paths target-server target-slot 0])]
    (swap-installed state side card card-to-swap)
    (shift-installed state side card target-server target-slot)))

(defn shift-a-card
  ([state side eid source-card card-to-shift]
   (shift-a-card state side eid source-card card-to-shift nil))
  ([state side eid source-card card-to-shift {:keys [cost other-side? no-wait-prompt?] :as args}]
   (show-shift-prompt
     state side eid source-card (adjacent-zones card-to-shift)
     (str "Shift " (:title card-to-shift) " where?")
     {:cost cost
      :msg (msg (let [server (:server context)
                      slot (:slot context)
                      old-card (get-in @state [(if other-side? (other-side side) side) :paths server slot 0])]
                  (if old-card
                    (str "swap " (hubworld-card-str state card-to-shift {:opponent? other-side}) " with " (hubworld-card-str state old-card {:opponent? other-side}))
                    (str "shift " (hubworld-card-str state card-to-shift {:opponent? other-side}) " to the " (name slot) " position of the " (str/capitalize (name server)) " path"))))
      :async true
      :effect (req (let [server (:server context)
                         slot (:slot context)]
                     (shift state (if other-side? (other-side side) side) card-to-shift server slot nil))
                   (effect-completed state side eid))}
     {:waiting-prompt (not no-wait-prompt?)
      :other-side? other-side?})))
