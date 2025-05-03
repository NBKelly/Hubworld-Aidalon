(ns game.core.actions
  (:require
    [clj-uuid :as uuid]
    [clojure.stacktrace :refer [print-stack-trace]]
    [clojure.string :as string]
    [game.core.agendas :refer [update-advancement-requirement update-all-advancement-requirements update-all-agenda-points]]
    [game.core.board :refer [installable-servers]]
    [game.core.card :refer [get-agenda-points get-card installed? rezzed? exhausted? seeker? moment? in-hand?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [break-sub-ability-cost card-ability-cost card-ability-cost score-additional-cost-bonus rez-cost]]
    [game.core.delving :refer [reset-delve-continue!]]
    [game.core.effects :refer [any-effects is-disabled-reg?]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [ability-as-handler checkpoint register-once register-pending-event pay queue-event resolve-ability trigger-event-simult]]
    [game.core.flags :refer [can-advance? can-score?]]
    [game.core.ice :refer [break-subroutine! break-subs-event-context get-current-ice get-pump-strength get-strength pump resolve-subroutine! resolve-unbroken-subs! substitute-x-credit-costs]]
    [game.core.initializing :refer [card-init]]
    [game.core.moving :refer [move archive-or-exile trash]]
    [game.core.payment :refer [build-spend-msg can-pay? merge-costs build-cost-string ->c]]
    [game.core.expend :refer [expend expendable?]]
    [game.core.play-instants :refer [play-instant can-play-instant?]]
    [game.core.prompt-state :refer [remove-from-prompt-queue]]
    [game.core.prompts :refer [cancel-bluff cancel-stage cancel-shift resolve-select show-stage-prompt show-shift-prompt resolve-stage resolve-shift clear-wait-prompt]]
    [game.core.props :refer [add-counter add-prop set-prop]]
    [game.core.say :refer [play-sfx system-msg implementation-msg]]
    [game.core.staging :refer [stage]]
    [game.core.servers :refer [name-zone zones->sorted-names]]
    [game.core.to-string :refer [card-str]]
    [game.core.toasts :refer [toast]]
    [game.core.update :refer [update!]]
    [game.macros :refer [continue-ability req wait-for msg]]
    [game.utils :refer [dissoc-in quantify remove-once same-card? same-side? server-cards to-keyword]]
    [jinteki.utils :refer [adjacent-zones card-side other-side]]))

(defn- update-click-state
  "Update :click-states to hold latest 4 moments before performing actions."
  [state ability]
  (when (:action ability)
    (let [state' (dissoc @state :log :history :click-states :turn-state)
          click-states (vec (take-last 10 (conj (:click-states @state) state')))]
      (swap! state assoc :click-states click-states))))

(defn- no-blocking-prompt?
  [state side]
  (let [prompt-type (get-in @state [side :prompt-state :prompt-type])]
    (or (= nil prompt-type)
        (= :run prompt-type)
        (= :prevent prompt-type))))

(defn- no-blocking-or-prevent-prompt?
  [state side]
  (let [prompt-type (get-in @state [side :prompt-state :prompt-type])]
    (or (= nil prompt-type)
        (= :run prompt-type))))

;;; Neutral actions
(defn- do-play-ability [state side eid {:keys [card ability ability-idx targets ignore-cost]}]
  (let [source {:source card
                :source-type :ability
                :source-info {:ability-idx ability-idx
                              :ability-targets targets}}
        eid (or eid (make-eid state source))
        cost (when-not ignore-cost
               (seq (card-ability-cost state side ability card targets)))
        ability (assoc ability :cost cost)]
    (when (or (nil? cost)
              (can-pay? state side eid card (:title card) cost))
      (update-click-state state ability)
      (if (:action ability)
        (let [stripped-card (select-keys card [:cid :type :title])]
          (wait-for
            (trigger-event-simult state side :action-played nil {:ability-idx ability-idx :card stripped-card :player side})
            (wait-for
              (resolve-ability state side ability card targets)
              (when-not (:delve @state) (swap! state update :active-player other-side))
              (trigger-event-simult state side eid :action-resolved nil {:ability-idx ability-idx :card stripped-card :player side}))))
        (resolve-ability state side eid ability card targets)))))

(defn play-ability
  "Triggers a card's ability using its zero-based index into the card's card-def :abilities vector."
  ([state side args] (play-ability state side nil args))
  ([state side eid {:keys [card] ability-idx :ability :as args}]
   (let [card (get-card state card)
         args (assoc args :card card)
         ability (nth (:abilities card) ability-idx)
         blocking-prompt? (not (no-blocking-prompt? state side))
         cannot-play (or (:disabled card)
                         ;; cannot play actions during delves
                         (and (:action ability) (:delve @state))
                         ;; while resolving another ability or promppt
                         blocking-prompt?
                         (not= side (to-keyword (:side card)))
                         (any-effects state side :prevent-paid-ability true? card [ability ability-idx]))]
     (when blocking-prompt?
       (toast state side (str "You cannot play abilities while other abilities are resolving.")
              "warning"))
     (when-not cannot-play
       (reset-delve-continue! state (other-side side))
       (do-play-ability state side eid (assoc args :ability-idx ability-idx :ability ability))))))

(defn play-collect
  "Triggers a collect ability"
  ([state side args] (play-collect state side nil args))
  ([state side eid {:keys [card] :as args}]
   (let [card (get-card state card)
         ability (first (filter :collect (:abilities card)))]
     (if (and ability (or (rezzed? card) (seeker? card)) (not (exhausted? card)))
       (let [args (assoc args :card card)
             ability-idx 0
             blocking-prompt? (not (no-blocking-prompt? state side))
             cannot-play (or (:disabled card)
                             ;; cannot play actions during runs
                             (and (:action ability) (:delve @state))
                             ;; while resolving another ability or promppt
                             blocking-prompt?
                             (not= side (to-keyword (:side card)))
                             (any-effects state side :prevent-paid-ability true? card [ability ability-idx]))]
         (when blocking-prompt?
           (toast state side (str "You cannot play abilities while other abilities are resolving.")
                  "warning"))
         (when-not cannot-play
           (reset-delve-continue! state (other-side side))
           (do-play-ability state side eid (assoc args :ability-idx ability-idx :ability ability))))
       (if eid (effect-completed state side eid) nil)))))

(defn expend-ability
  "Called when the player clicks a card from hand."
  [state side {:keys [card]}]
  (if (no-blocking-or-prevent-prompt? state side)
    (let [card (get-card state card)
          eid (make-eid state {:source card :source-type :ability})
          expend-ab (expend (:expend (card-def card)))]
      (resolve-ability state side eid expend-ab card nil))
    (toast state side (str "You cannot play abilities while other abilities are resolving.")
              "warning")))

(defn play-rush
  "Called when the player plays a card from hand as a rush"
  [state side {:keys [card] :as context}]
  (when-let [card (get-card state card)]
    (let [context (assoc context :card card)
          card-to-stage card
          rush-abi {:msg (msg (println context) "rush itself to the " (name (:slot context)) " row of [their] " (string/capitalize (name (:server context))))
                    :async true
                    :cost [(->c :credit (rez-cost state side card-to-stage nil))]
                    :effect (req (let [c (:card context) server (:server context) slot (:slot context)]
                                   (if-let [old-card (get-in @state [side :paths server slot 0])]
                                     (do
                                       (system-msg state side
                                                   (str (if (rezzed? old-card)
                                                          (str "exiles " (:title old-card))
                                                          (str "archives a facedown card"))
                                                        " in the " (name slot) " row of [their] " (string/capitalize (name server))))
                                       (wait-for
                                         (archive-or-exile state side old-card {:unpreventable true})
                                         (stage state side eid c server slot {:rushed true})))
                                     (stage state side eid c server slot {:rushed true}))))}]
      (assert (not (moment? card)))
      (show-stage-prompt
        state side nil
        (str "Rush " (:title card-to-stage) " where?")
        {:async true
         :effect (req (let [server (:server context)
                            slot (:slot context)
                            old-card (get-in @state [side :paths server slot 0])
                            ctx {:card card-to-stage
                                 :ability rush-abi
                                 :ability-idx nil
                                 :targets [(assoc context :card card-to-stage)]}]
                              (if old-card
                                (resolve-ability
                                  state side eid
                                  {:optional
                                   {:prompt (str (if (rezzed? old-card) "Exile " "Archive ") (:title old-card) " in the " (name slot) " row of your " (string/capitalize (name server)) "?")
                                    :yes-ability {:async true
                                                  :effect (req (do-play-ability state side eid ctx))}}}
                                  nil nil)
                                (do-play-ability state side eid ctx))))}
          {:waiting-prompt true}))))

(defn flash
  "Called when the player flashes a card from hand."
  ([state side context] (flash state side (make-eid state) context))
  ([state side eid {:keys [card] :as context}]
   (if-let [card (and (not (get-in @state [side :prompt :prompt-type])) (get-card state card))]
     (let [ctx {:card card
                :ability-idx nil
                :targets [(assoc context :card card)]
                :ability {:async true
                          :req (req (let [target-card (:card context)]
                                      (and (in-hand? target-card)
                                           (moment? target-card)
                                           (can-play-instant? state side eid target-card {:flash true}))))
                          :effect (req (play-instant state side eid (:card context) {:flash true}))}}]
       (do-play-ability state side eid ctx))
     (effect-completed state side eid))))

(defn play
  "Called when the player clicks a card from hand."
  [state side {:keys [card] :as context}]
  (when-let [card (get-card state card)]
    (when (not (get-in @state [side :prompt :prompt-type]))
      (let [context (assoc context :card card)]
        (case (:type card)
          ("Moment")
          (let [bac (get-in @state [side :basic-action-card])]
            (play-ability state side {:card bac
                                      :ability 4
                                      :targets [(assoc context :card card)]}))
          ("Agent" "Obstacle" "Source")
          (let [card-to-stage card
                bac (get-in @state [side :basic-action-card])
                eid (make-eid state {:source bac})]
            (show-stage-prompt
              state side eid bac
              (str "Stage " (:title card-to-stage) " where?")
              {:async true
               :effect (req (let [server (:server context)
                                  slot (:slot context)
                                  old-card (get-in @state [side :paths server slot 0])]
                              (if old-card
                                (resolve-ability
                                  state side eid
                                  ;; TODO - distinguish between archive/exile here
                                  {:optional
                                   {:prompt (str (if (rezzed? old-card) "Exile " "Archive ") (:title old-card) " in the " (name slot) " row of your " (string/capitalize (name server)) "?")
                                    :yes-ability {:async true
                                                  :effect (req (play-ability
                                                                 state side eid
                                                                 {:card bac
                                                                  :ability 3
                                                                  :targets [{:card card-to-stage
                                                                             :server server
                                                                             :slot slot}]}))}}}
                                  bac nil)
                                (play-ability
                                  state side eid
                                  {:card bac
                                   :ability 3
                                   :targets [{:card card-to-stage
                                              :server (:server context)
                                              :slot (:slot context)}]}))))}
              {}))
          nil)))))

(defn cmd-shift
  "Called when the player clicks the shift button"
  [state side _]
  (if (no-blocking-or-prevent-prompt? state side)
    (resolve-ability
      state side (make-eid state)
      {:prompt "Choose an installed card to shift"
       :choices {:req (req (and (installed? target)
                                (= side (card-side target))))}
       :waiting-prompt true
       :async true
       :effect (req (let [card-to-shift target
                          bac (get-in @state [side :basic-action-card])]
                      (show-shift-prompt
                        state side eid bac (adjacent-zones card-to-shift)
                        (str "Shift " (:title card-to-shift) " where?")
                        {:async true
                         :effect (req
                                   (clear-wait-prompt state (other-side side))
                                   (play-ability
                                        state side eid
                                        {:card bac
                                         :ability 5
                                         :targets [{:card card-to-shift
                                                    :server (:server context)
                                                    :slot (:slot context)}]}))}
                        nil)))}
      nil nil)))

;; (defn play
;;   "Called when the player clicks a card from hand."
;;   [state side {:keys [card] :as context}]
;;   (when-let [card (get-card state card)]
;;     (when (and (not (get-in @state [side :prompt-state :prompt-type]))
;;                (not (and (= side :corp) (:corp-phase-12 @state)))
;;                (not (and (= side :runner) (:runner-phase-12 @state))))
;;       (let [context (assoc context :card card)]
;;         (case (:type card)
;;           ("Event" "Operation")
;;           (play-ability state side {:card (get-in @state [side :basic-action-card])
;;                                     :ability 3
;;                                     :targets [context]})
;;           ("Hardware" "Resource" "Program" "ICE" "Upgrade" "Asset" "Agenda")
;;           (play-ability state side {:card (get-in @state [side :basic-action-card])
;;                                     :ability 2
;;                                     :targets [context]})

(defn click-draw
  "Click to draw."
  [state side _]
  (play-ability state side {:card (get-in @state [side :basic-action-card])
                            :ability 1}))

(defn click-credit
  "Click to gain 1 credit."
  [state side _]
  (play-ability state side {:card (get-in @state [side :basic-action-card])
                            :ability 0}))

(defn pass
  "Pass when there are no actions left."
  [state side _]
  (play-ability state side {:card (get-in @state [side :basic-action-card])
                            :ability 2}))

(defn move-card
  "Called when the user drags a card from one zone to another."
  [state side {:keys [card server]}]
  (let [c (get-card state card)
        last-zone (last (:zone c))
        src (name-zone (:side c) (:zone c))
        from-str (card-str state c)
        s (if (or (string/includes? server "-corp") (= "HQ" server))
            :corp :runner)
        server (first (string/split server #"-" 2))]
    ;; allow moving from play-area always, otherwise only when same side, and to valid zone
    (when (and (not= src server)
               (same-side? s (:side card))
               (not= :select (get-in @state [side :prompt-state :prompt-type]))
               (or (= last-zone :play-area)
                   (same-side? side (:side card))))
      (let [move-card-to (partial move state s c)
            card-prompts (filter #(same-card? :title % c) (get-in @state [side :prompt]))
            log-move (fn [verb & text]
                       (system-msg state side (str verb " " from-str
                                                   (when (seq text)
                                                     (apply str " " text)))))]
        (case server
          ("Heap" "Archives")
          (do (when (pos? (count card-prompts))
                ;; Remove all prompts associated with the trashed card
                (doseq [prompt card-prompts]
                  (remove-from-prompt-queue state side prompt)
                  (effect-completed state side (:eid prompt))))
              (if (= :hand (first (:zone c)))
                ;; Discard from hand, do not trigger trash
                (do (move-card-to :discard {:force true})
                    (log-move "discards"))
                (do (trash state s (make-eid state) c {:unpreventable true})
                    (log-move "trashes"))))
          ("the Grip" "HQ" "[their] Council")
          (do (move-card-to :hand {:force true})
              (log-move "moves" "to [their] Council"))
          ("Stack" "R&D" "Commons")
          (do (move-card-to :deck {:front true :force true})
              (log-move "moves" "to the top of [their] Commons"))
          ;; default
          nil)))))

(defn- finish-prompt [state side prompt card]
  (when-let [end-effect (:end-effect prompt)]
    (end-effect state side (make-eid state) card nil))
  true)

(defn- prompt-error
  [context prompt prompt-args]
  (.println *err* (with-out-str (print-stack-trace (Exception. (str "Error " context)))))
  (.println *err* (str "Prompt: " prompt))
  (.println *err* (str "Prompt args: " prompt-args)))

(defn- maybe-pay
  [state side eid card choices choice]
  (if (= choices :credit)
    (pay state side eid card (->c :credit (min choice (get-in @state [side :credit]))))
    (effect-completed state side eid)))

;; TODO - resolve-prompt does some evil things with eids, maybe we can fix it later - nbk, 2025
(defn resolve-prompt
  "Resolves a prompt by invoking its effect function with the selected target of the prompt.
  Triggered by a selection of a prompt choice button in the UI."
  [state side {:keys [choice] :as args}]
  (let [prompt (first (get-in @state [side :prompt]))
        effect (:effect prompt)
        card (get-card state (:card prompt))
        choices (:choices prompt)]
    (cond
      ;; Integer prompts
      (or (= choices :credit)
          (= :trace (:prompt-type prompt))
          (:counter choices)
          (:number choices))
      (if (number? choice)
        (do (remove-from-prompt-queue state side prompt)
            (let [eid (make-eid state (:eid prompt))]
              (wait-for (maybe-pay state side eid card choices choice)
                        (when (:counter choices)
                          ;; :Counter prompts deduct counters from the card
                          (add-counter state side (make-eid state eid) card (:counter choices) (- choice)))
                        ;; trigger the prompt's effect function
                        (when effect
                          (effect (or choice card)))
                        (finish-prompt state side prompt card))))
        (prompt-error "in an integer prompt" prompt args))

      ;; List of card titles for auto-completion
      (:card-title choices)
      (if (string? choice)
        (let [title-fn (:card-title choices)
              found (some #(when (= (string/lower-case choice) (string/lower-case (:title % ""))) %) (server-cards))]
          (if found
            (if (title-fn state side (make-eid state) card [found])
              (do (remove-from-prompt-queue state side prompt)
                  (when effect
                    (effect (or choice card)))
                  (finish-prompt state side prompt card))
              (toast state side (str "You cannot choose " choice " for this effect.") "warning"))
            (toast state side (str "Could not find a card named " choice ".") "warning")))
        (prompt-error "in a card-title prompt" prompt args))

      ;; Otherwise, choices is a sequence of strings and/or cards
      ;; choice is a string and should match one of the strings, or the title of one of the cards
      (:uuid choice)
      (let [uuid (uuid/as-uuid (:uuid choice))
            match (first (filter #(= uuid (:uuid %)) choices))]
        (when match
          (remove-from-prompt-queue state side prompt)
          (if (= (:value match) "Cancel")
            (do (if-let [cancel-effect (:cancel-effect prompt)]
                  ;; trigger the cancel effect
                  (cancel-effect choice)
                  (effect-completed state side (:eid prompt)))
                (finish-prompt state side prompt card))
            (do (effect match)
                (finish-prompt state side prompt card)))))

      :else
      (prompt-error "in an unknown prompt type" prompt args))))

(defn stage-done
  [state side args]
  (let [prompt (first (get-in @state [side :prompt]))
        card (:card prompt)]
    (case (:prompt-type prompt)
      :stage (cancel-stage state side card update! resolve-ability)
      :shift (cancel-shift state side card update! resolve-ability))))

(defn bluff-done
  [state side args]
  (let [prompt (first (get-in @state [side :prompt]))]
    (case (:prompt-type prompt)
      :bluff (cancel-bluff state side resolve-ability)
      (println "? " (str "prompt: " prompt)))))

(defn stage-select
  [state side {:keys [server slot shift-key-held]}]
  (let [prompt (first (get-in @state [side :prompt]))
        card (:card prompt)]
    (swap! state assoc-in [side :shift-key-select] shift-key-held)
    (case (:prompt-type prompt)
      :stage (resolve-stage state side card {:server (keyword server) :slot slot} update! resolve-ability)
      :shift (resolve-shift state side card {:server (keyword server) :slot slot} update! resolve-ability))
    ;; TODO - if shift held, skip asking about overwriting :)
    nil))

(defn select
  "Attempt to select the given card to satisfy the current select prompt. Calls resolve-select
  if the max number of cards has been selected."
  [state side {:keys [card shift-key-held]}]
  (let [target (get-card state card)
        prompt (first (get-in @state [side :selected]))
        ability (:ability prompt)
        card-req (:req prompt)
        card-condition (:card prompt)
        cid (:not-self prompt)]
    (swap! state assoc-in [side :shift-key-select] shift-key-held)
    (when (and (not= (:cid target) cid)
               (cond
                 card-condition (card-condition target)
                 card-req (card-req state side (:eid ability) (get-card state (:card ability)) [target])
                 :else true))
      (let [c (update-in target [:selected] not)]
        (update! state side c)
        (if (:selected c)
          (swap! state update-in [side :selected 0 :cards] #(conj % c))
          (swap! state update-in [side :selected 0 :cards]
                 (fn [coll] (remove-once #(same-card? % target) coll))))
        (let [selected (get-in @state [side :selected 0])
              prompt (first (get-in @state [side :prompt]))
              card (:card prompt)]
          (when (= (count (:cards selected)) (or (:max selected) 1))
            (resolve-select state side card (select-keys prompt [:cancel-effect]) update! resolve-ability)))))))

(defn play-corp-ability
  "Triggers a runner card's corp-ability using its zero-based index into the card's card-def :corp-abilities vector."
  ([state side args] (play-corp-ability state side nil args))
  ([state side eid {:keys [card] ability-idx :ability :as args}]
   (let [card (get-card state card)
         cdef (card-def card)
         ability (get-in cdef [:corp-abilities ability-idx])
         cannot-play (or (:disabled card)
                         (any-effects state side :prevent-paid-ability true? card [ability ability-idx]))]
     (when-not cannot-play
       (do-play-ability state side eid (assoc args :ability-idx ability-idx :ability ability))))))

(defn play-runner-ability
  "Triggers a corp card's runner-ability using its zero-based index into the card's card-def :runner-abilities vector."
  ([state side args] (play-runner-ability state side nil args))
  ([state side eid {:keys [card] ability-idx :ability :as args}]
   (let [card (get-card state card)
         cdef (card-def card)
         ability (get-in cdef [:runner-abilities ability-idx])
         cannot-play (or (:disabled card)
                         (any-effects state side :prevent-paid-ability true? card [ability ability-idx]))]
     (when-not cannot-play
       (do-play-ability state side eid (assoc args :ability-idx ability-idx :ability ability))))))

(defn play-subroutine
  "Triggers a card's subroutine using its zero-based index into the card's :subroutines vector."
  [state side {:keys [card subroutine]}]
  (if (no-blocking-or-prevent-prompt? state side)
    (let [card (get-card state card)
          sub (nth (:subroutines card) subroutine nil)]
      (when card
        (resolve-subroutine! state side card sub)))
    (toast state side (str "You cannot fire subroutines while abilities are being resolved.")
           "warning")))

(defn play-unbroken-subroutines
  "Triggers each unbroken subroutine on a card in order, waiting for each to complete"
  [state side {:keys [card]}]
  (if (no-blocking-or-prevent-prompt? state side)
    (when-let [card (get-card state card)]
      (resolve-unbroken-subs! state side card))
    (toast state side (str "You cannot fire subroutines while abilities are being resolved.")
           "warning")))

;;; Corp actions
(defn trash-resource
  "Click to trash a resource."
  [state side _]
  (play-ability state side {:card (get-in @state [:corp :basic-action-card])
                            :ability 5}))

(defn do-purge
  "Purge viruses."
  [state side _]
  (play-ability state side {:card (get-in @state [:corp :basic-action-card])
                            :ability 6}))

(defn click-advance
  "Click to advance installed card."
  [state side {:keys [card] :as context}]
  (when-let [card (get-card state card)]
    (let [context (assoc context :card card)]
      ;; note that can-advance potentially generates toasts (effcom),
      ;; so it cannot go in the req of basic, since that can generate infinite toast loops
      ;; when update-and-send-diffs causes more updates that need to be updated and sent...
      (if (can-advance? state side card)
        (play-ability state side {:card (get-in @state [:corp :basic-action-card])
                                  :ability 4
                                  :targets [context]})
        (toast state :corp "Cannot advance cards this turn." "warning")))))

(defn click-delve
  "Click to start a delve."
  [state side context]
  (play-ability state side {:card (get-in @state [side :basic-action-card])
                            :ability 6
                            :targets [context]}))

(defn remove-tag
  "Click to remove a tag."
  [state side _]
  (play-ability state side {:card (get-in @state [:runner :basic-action-card])
                            :ability 5}))

(defn view-deck
  "Allows the player to view their deck by making the cards in the deck public."
  [state side _]
  (system-msg state side "looks at [their] deck")
  (swap! state assoc-in [side :view-deck] true))

(defn close-deck
  "Closes the deck view and makes cards in deck private again."
  [state side _]
  (system-msg state side "stops looking at [their] deck")
  (swap! state update-in [side] dissoc :view-deck))

(defn generate-install-list
  [state _ {:keys [card]}]
  (if-let [card (get-card state card)]
    (if (expendable? state card)
      (swap! state assoc-in [:corp :install-list] (conj (installable-servers state card) "Expend")) ;;april fools we can make this "cast as a sorcery"
      (swap! state assoc-in [:corp :install-list] (installable-servers state card)))
    (swap! state dissoc-in [:corp :install-list])))

(defn advance
  "Advance a corp card that can be advanced.
   If you pass in a truthy value as the no-cost parameter, it will advance at no cost (for the card Success)."
  ([state side {:keys [card]}] (advance state side (make-eid state) card nil))
  ([state side card no-cost] (advance state side (make-eid state) card no-cost))
  ([state side eid card no-cost]
   (let [card (get-card state card)
         eid (assoc eid :source-type :advance)]
     (if (can-advance? state side card)
       (wait-for (pay state side
                      (make-eid state (assoc eid :action :corp-advance))
                      card
                      (->c :click (if-not no-cost 1 0))
                      (->c :credit (if-not no-cost 1 0)))
                 (if-let [payment-str (:msg async-result)]
                   (do (system-msg state side (str (build-spend-msg payment-str "advance") (card-str state card)))
                       (update-advancement-requirement state card)
                       (wait-for
                         (add-prop state side (get-card state card) :advance-counter 1)
                         (play-sfx state side "click-advance")
                         (effect-completed state side eid)))
                   (effect-completed state side eid)))
       (effect-completed state side eid)))))

(defn resolve-score
  "resolves the actual 'scoring' of an agenda (after costs/can-steal has been worked out)"
  [state side eid card]
  (let [moved-card (move state :corp card :scored)
        c (card-init state :corp moved-card {:resolve-effect false
                                             :init-data true})
        _ (update-all-advancement-requirements state)
        _ (update-all-agenda-points state)
        c (get-card state c)
        points (get-agenda-points c)]
    (system-msg state :corp (str "scores " (:title c)
                                 " and gains " (quantify points "agenda point")))
    (implementation-msg state card)
    (set-prop state :corp (get-card state c) :advance-counter 0)
    (swap! state update-in [:corp :register :scored-agenda] #(+ (or % 0) points))
    (play-sfx state side "agenda-score")
    (when-let [on-score (:on-score (card-def c))]
      (register-pending-event state :agenda-scored c on-score))
    (queue-event state :agenda-scored {:card c
                                       :points points})
    (checkpoint state nil eid {:duration :agenda-scored})))

(defn score
  "Score an agenda."
  ([state side eid card] (score state side eid card nil))
  ([state side eid card {:keys [no-req ignore-turn]}]
   (if-not (can-score? state side card {:no-req no-req :ignore-turn ignore-turn})
     (effect-completed state side eid)
     (let [cost (score-additional-cost-bonus state side card)
           cost-strs (build-cost-string cost)
           can-pay (can-pay? state side (make-eid state (assoc eid :additional-costs cost)) card (:title card) cost)]
       (cond
         (string/blank? cost-strs) (resolve-score state side eid card)
         (not can-pay) (effect-completed state side eid)
         :else (wait-for (pay state side (make-eid state
                                                   (assoc eid
                                                          :additional-costs cost
                                                          :source card
                                                          :source-type :corp-score))
                              card cost)
                         (let [payment-result async-result]
                           (if (string/blank? (:msg payment-result))
                             (effect-completed state side eid)
                             (do
                               (system-msg state side (str (:msg payment-result) " to score " (:title card)))
                               (resolve-score state side eid card))))))))))
