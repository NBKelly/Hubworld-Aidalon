(ns game.cards.basic
  (:require
   [game.core.agendas :refer [update-advancement-requirement]]
   [game.core.board :refer [all-active-installed installable-servers]]
   [game.core.card :refer [agenda? asset? event? get-card hardware? ice?
                           in-hand? operation? program? resource? upgrade?]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.drawing :refer [draw use-bonus-click-draws!]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid]]
   [game.core.effects :refer [get-effects]]
   [game.core.engine :refer [pay resolve-ability trigger-event]]
   [game.core.flags :refer [untrashable-while-resources?]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.installing :refer [corp-can-pay-and-install? corp-install
                                 runner-can-pay-and-install? runner-install]]
   [game.core.moving :refer [trash]]
   [game.core.payment :refer [build-cost-string can-pay? merge-costs ->c]]
   [game.core.play-instants :refer [can-play-instant? play-instant]]
   [game.core.props :refer [add-prop]]
   [game.core.purging :refer [purge]]
   [game.core.runs :refer [make-run]]
   [game.core.say :refer [play-sfx system-msg]]
   [game.core.tags :refer [lose-tags]]
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
               ;; TODO - stage a card
               ;; TODO - shift a card
               ;; TODO - play a moment with an action cost
               ;; TODO - use a click ability
               ;; TODO - delve
               ]})
