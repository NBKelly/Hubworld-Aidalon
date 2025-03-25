(ns game.cards.basic
  (:require
   [game.core.card :refer [agenda? asset? event? get-card hardware? ice?
                           in-hand? operation? program? resource? upgrade?]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.drawing :refer [draw]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.payment :refer [->c]]
   [game.core.say :refer [play-sfx system-msg]]
   [game.core.staging :refer [stage]]
   [game.core.to-string :refer [card-str]]
   [game.macros :refer [effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))
;; Card definitions

(defcard "Hubworld Basic Action Card"
  {:abilities [{:action true
                :label "Gain 1 [Credits]"
                :cost [(->c :click)]
                :msg "gain 1 [Credits]"
                :async true
                :effect (req (wait-for (gain-credits state side 1)
                                       (swap! state update-in [:stats side :click :credit] (fnil inc 0))
                                       (play-sfx state side "click-credit")
                                       (effect-completed state side eid)))}
               {:action true
                :label "Draw 1 card"
                :req (req (seq (get-in @state [side :deck])))
                :cost [(->c :click)]
                :msg "draw 1 card"
                :async true
                :effect (req (swap! state update-in [:stats side :click :draw] (fnil inc 0))
                             (play-sfx state side "click-card")
                             (draw state side eid 1))}
               {:action :true
                :label "Pass"
                :req (req (zero? (get-in @state [side :click])))
                :msg "pass (I have no actions available)"
                :async true
                :effect (req ;; TODO - sound effect for passing
                          (swap! state update-in [:stats side :click :pass] (fnil inc 0))
                          (effect-completed state side eid))}
               {:action :true
                :label "Stage a card"
                :cost [(->c :click 1)]
                :msg (msg "stage a card from [their] council in the " (name (:slot context)) " row of [their] " (capitalize (name (:server context))))
                :async true
                :effect (req (stage state side eid (:card context) (:server context) (:slot context)))}
                          ;; if there's already a card there, it gets trashed
                          ;; than can be handled by stage itself, though
               ;; TODO - stage a card
               ;; TODO - shift a card
               ;; TODO - play a moment with an action cost
               ;; TODO - use a click ability
               ;; TODO - delve
               ]})
