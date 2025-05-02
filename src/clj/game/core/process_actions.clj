(ns game.core.process-actions
  (:require
   [clojure.string :as str]
   [game.core.actions :refer [click-advance click-credit click-draw click-delve
                              close-deck do-purge generate-install-list cmd-shift
                              move-card expend-ability
                              pass play play-ability play-corp-ability play-collect
                              play-runner-ability play-subroutine play-unbroken-subroutines remove-tag
                              play-rush
                              resolve-prompt score select stage-done bluff-done stage-select trash-resource view-deck]]
   [game.core.card :refer [get-card]]
   [game.core.change-vals :refer [change]]
   [game.core.checkpoint :refer [fake-checkpoint]]
   [game.core.commands :refer [parse-command]]
   [game.core.delving :refer [continue-delve continue-delve-post-encounter delve-discover-clicked delve-confront-clicked delve-bypass-clicked end-the-delve! delve-toggle-pass-priority]]
   [game.core.eid :refer [make-eid]]
   [game.core.exhausting :refer [exhaust unexhaust]]
   [game.core.moving :refer [exile trash]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.say :refer [indicate-action say system-msg system-say]]
   [game.core.set-up :refer [keep-hand mulligan]]
   [game.core.shuffling :refer [shuffle-deck]]
   [game.core.toasts :refer [ack-toast toast]]
   [game.core.turns :refer [start-hubworld-turn hubworld-refresh-phase end-turn-consent]]
   [game.core.winning :refer [concede]]))

(defn checkpoint+clean-up
  [state]
  (fake-checkpoint state))

(defn set-property
  "set properties of the game state that need to be adjustable by the frontend
  ie: * do we want an offer to trash like cards on installs?"
  [state side {:keys [key value]}]
  (case key
    :trash-like-cards (swap! state assoc-in [side :trash-like-cards] value)))

(defn command-parser
  [state side {:keys [user text] :as args}]
  (let [author (or user (get-in @state [side :user]))
        text (if (= (str/trim text) "null") " null" text)]
    (if-let [command (parse-command state text)]
      (when (and (not= side nil) (not= side :spectator))
        (command state side)
        (system-say state side (str "[!]" (:username author) " uses a command: " text)))
      (say state side args))))

(def commands
  {"ability" #'play-ability
   "advance" #'click-advance
   "change" #'change
   "choice" #'resolve-prompt
   "close-deck" #'close-deck
   "concede" #'concede
   "corp-ability" #'play-corp-ability
   "credit" #'click-credit
   "unforge" #(derez %1 %2 (:card %3))

   "delve"                             #'click-delve
   "delve-continue"                   (fn [state side _] (continue-delve state side (make-eid state)))
   "delve-continue-post-encounter"    (fn [state side _] (continue-delve-post-encounter state side (make-eid state)))
   "delve-end"                        (fn [state side _] (end-the-delve! state side (make-eid state) nil))
   "delve-discover"                   (fn [state side _] (delve-discover-clicked state side (make-eid state)))
   "delve-bypass"                     (fn [state side _] (delve-bypass-clicked state side (make-eid state)))
   "delve-confront"                   (fn [state side _] (delve-confront-clicked state side (make-eid state)))
   "delve-toggle-auto-pass"           (fn [state side _]        (delve-toggle-pass-priority state side (make-eid state)))
   "exile"                            #(exile %1 %2 (make-eid %1) (get-card %1 (:card %3)) (dissoc %3 :card))
   "collect" #'play-collect
   "rush" #'play-rush

   "draw" #'click-draw

   "exhaust" #(exhaust %1 %2 (make-eid %1) (:card %3) {:no-event true})
   "end-turn" (fn [state side _] (end-turn-consent state side (make-eid state)))
   "generate-install-list" #'generate-install-list ;; OBSOLETE - but keep
   "indicate-action" #'indicate-action
   "keep" #'keep-hand
   "move" #'move-card
   "mulligan" #'mulligan
   "play" #'play
   "expend" #'expend-ability ;; OBSOLETE
   "pass" #'pass
   "purge" #'do-purge ;; OBSOLETE
   "remove-tag" #'remove-tag ;; OBSOLETE
   "forge" #(rez %1 %2 (make-eid %1) (:card %3) (dissoc %3 :card))
   "runner-ability" #'play-runner-ability
   "score" #(score %1 %2 (make-eid %1) (get-card %1 (:card %3)) nil) ;; OBSOLETE
   "select" #'select
   "set-property" #'set-property
   "shift" #'cmd-shift
   "shuffle" #'shuffle-deck
   "start-turn" #'start-hubworld-turn
   "stage-done" #'stage-done
   "bluff-done" #'bluff-done
   "stage-select" #'stage-select
   "subroutine" #'play-subroutine ;; OBSOLETE
   "system-msg" #(system-msg %1 %2 (:msg %3))
   "toast" #'ack-toast
   ;; "toggle-auto-no-action" #'toggle-auto-no-action - todo - this is fine
   "trash" #(trash %1 %2 (make-eid %1) (get-card %1 (:card %3)) (dissoc %3 :card))
   "trash-resource" #'trash-resource    ;; OBSOLETE
   "unexhaust" #(unexhaust %1 %2 (make-eid %1) (:card %3) {:no-event true})
   "unbroken-subroutines" #'play-unbroken-subroutines  ;; OBSOLETE
   "view-deck" #'view-deck})

(defn process-action
  [command state side args]
  (if-let [c (get commands command)]
    (do (c state side args)
        (checkpoint+clean-up state)
        true)
    (do (toast state side (str "The command: " command " is not currently implemented!"))
        true)))
