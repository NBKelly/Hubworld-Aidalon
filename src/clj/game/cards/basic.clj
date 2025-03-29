(ns game.cards.basic
  (:require
   [clojure.string :as str]
   [game.core.board :refer [hubworld-all-installed]]
   [game.core.card :refer [get-card in-hand? installed? moment? rezzed?]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.delving :refer [make-delve]]
   [game.core.drawing :refer [draw]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid]]
   [game.core.events :refer [event-count]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.moving :refer [archive-or-exile]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.play-instants :refer [can-play-instant? play-instant]]
   [game.core.say :refer [play-sfx system-msg]]
   [game.core.shifting :refer [shift]]
   [game.core.staging :refer [stage]]
   [game.core.to-string :refer [card-str]]
   [game.macros :refer [effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))
;; Card definitions

(defcard "Hubworld Basic Action Card"
  {:abilities [;; --> 0
               {:action true
                :label "Gain 1 [Credits]"
                :cost [(->c :click)]
                :msg "gain 1 [Credits]"
                :async true
                :effect (req (wait-for (gain-credits state side 1)
                                       (swap! state update-in [:stats side :click :credit] (fnil inc 0))
                                       (play-sfx state side "click-credit")
                                       (effect-completed state side eid)))}

               ;; --> 1
               {:action true
                :label "Draw 1 card"
                :req (req (seq (get-in @state [side :deck])))
                :cost [(->c :click)]
                :msg "draw 1 card"
                :async true
                :effect (req (swap! state update-in [:stats side :click :draw] (fnil inc 0))
                             (play-sfx state side "click-card")
                             (draw state side eid 1))}

               ;; --> 2
               {:action :true
                :label "Pass"
                :req (req (zero? (get-in @state [side :click])))
                :msg "pass (I have no actions available)"
                :async true
                :effect (req ;; TODO - sound effect for passing
                          (swap! state update-in [:stats side :click :pass] (fnil inc 0))
                          (effect-completed state side eid))}

               ;; --> 3
               {:action :true
                :label "Stage a card"
                :cost [(->c :click 1)]
                :msg (msg "stage a card from [their] council in the " (name (:slot context)) " row of [their] " (str/capitalize (name (:server context))))
                :async true
                :effect (req (let [c (:card context) server (:server context) slot (:slot context)]
                               (if-let [old-card (get-in @state [side :paths server slot 0])]
                                 (do
                                   (system-msg state side
                                               (str (if (rezzed? old-card)
                                                      (str "exiles " (:title old-card))
                                                      (str "archives a facedown card"))
                                                    " in the " (name slot) " row of [their] " (str/capitalize (name server))))
                                   (wait-for
                                     (archive-or-exile state side old-card {:unpreventable true})
                                     (stage state side eid c server slot)))
                                 (stage state side eid c server slot))))}

               ;; --> 4
               {:action true
                :label "Play a moment"
                :async true
                :req (req (let [target-card (:card context)]
                            (and (in-hand? target-card)
                                 (moment? target-card)
                                 (can-play-instant? state side eid target-card))))
                :effect (req (play-instant state side eid (:card context)))}

               ;; --> 5
               {:action true
                :label "Shift a card"
                :msg (msg (let [server (:server context)
                                slot (:slot context)
                                target-card (:card context)
                                old-card (get-in @state [side :paths server slot 0])]
                            (if old-card
                              (str "swap " (card-str state target-card) " with " (card-str state old-card))
                              (str "shift " (card-str state target-card) " to the " (name slot) " of [their] " (str/capitalize (name server)) " path"))))
                :cost [(->c :click 1)]
                :req (req (> (count (hubworld-all-installed state side)) 1))
                :effect (req (shift state side (:card context) (:server context) (:slot context) nil))}

               ;; --> 6
               {:action true
                :label "Delve a district"
                ;; note: you may not delve on click 1 of round 1
                :req (req (and
                            (can-pay? state side eid card nil [(->c :click 1)])

                            (if (= (-> @state :turn :index) 1)
                              (pos? (event-count state side :action-resolved #(= side (:player (first %)))))
                              true)))
                :msg (msg "delve " (:server context))
                :effect (req (make-delve state side eid (str/lower-case (:server context)) card {:click-delve true}))}]})
