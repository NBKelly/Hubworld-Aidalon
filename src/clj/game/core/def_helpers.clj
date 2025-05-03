(ns game.core.def-helpers
  (:require
    [clojure.string :as str]
    [game.core.access :refer [access-bonus]]
    [game.core.board :refer [all-installed]]
    [game.core.card :refer [active? can-be-advanced? corp? faceup? get-card get-counters has-subtype? in-discard? runner? in-hand? moment?]]
    [game.core.card-defs :as card-defs]
    [game.core.drawing :refer [draw]]
    [game.core.damage :refer [damage]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [queue-event register-events resolve-ability trigger-event-sync unregister-event-by-uuid]]
    [game.core.effects :refer [is-disabled-reg? sum-effects]]
    [game.core.gaining :refer [gain-credits]]
    [game.core.moving :refer [move trash]]
    [game.core.payment :refer [build-cost-string can-pay? ->c]]
    [game.core.play-instants :refer [async-rfg]]
    [game.core.prompts :refer [clear-wait-prompt]]
    [game.core.props :refer [add-counter]]
    [game.core.revealing :refer [conceal-hand reveal-hand reveal-loud]]
    [game.core.say :refer [system-msg system-say]]
    [game.core.shifting :refer [shift-a-card]]
    [game.core.staging :refer [stage-a-card]]
    [game.core.to-string :refer [card-str]]
    [game.core.toasts :refer [toast]]
    [game.macros :refer [continue-ability effect msg req wait-for]]
    [game.utils :refer [enumerate-str remove-once same-card? server-card to-keyword  quantify]]
    [jinteki.utils :refer [other-side]]))

(defn combine-abilities
  "Combines two or more abilities to a single one. Labels are joined together with a period between parts."
  ([ab-x ab-y]
   {:label (str (:label ab-x) ". " (:label ab-y))
    :async true
    :effect (req (wait-for (resolve-ability state side ab-x card nil)
                           (continue-ability state side ab-y card nil)))})
  ([ab-x ab-y & ab-more]
   (reduce combine-abilities (combine-abilities ab-x ab-y) ab-more)))

(def corp-rez-toast
  "Effect to be placed with `:runner-turn-ends` to remind players of 'when turn begins'
  triggers"
  {:event :runner-turn-ends
   :effect (req (toast state :corp "Reminder: You have unrezzed cards with \"when turn begins\" abilities." "info"))})

(declare reorder-final) ; forward reference since reorder-choice and reorder-final are mutually recursive

(defn reorder-choice
  "Generates a recursive prompt structure for cards that do reordering (Indexing, Making an Entrance, etc.)

  reorder-side is the side to be reordered, i.e. :corp for Indexing and Precognition.
  wait-side is the side that has a wait prompt while ordering is in progress, i.e. :corp for Indexing and Spy Camera.

  This is part 1 - the player keeps choosing cards until there are no more available choices. A wait prompt should
  exist before calling this function. See Indexing and Making an Entrance for examples on how to call this function."

  ([reorder-side cards] (reorder-choice reorder-side (other-side reorder-side) cards `() (count cards) cards nil))
  ([reorder-side wait-side remaining chosen n original] (reorder-choice reorder-side wait-side remaining chosen n original nil))
  ([reorder-side wait-side remaining chosen n original dest]
   (when (not-empty remaining)
     {:prompt (str "Choose a card to move next "
                   (if (= dest "bottom") "under " "onto ")
                   (if (= reorder-side :corp) "R&D" "the stack"))
      :choices remaining
      :async true
      :effect (req (let [chosen (cons target chosen)]
                     (if (< (count chosen) n)
                       (continue-ability
                         state side
                         (reorder-choice reorder-side wait-side (remove-once #(= target %) remaining) chosen n original dest)
                         card nil)
                       (continue-ability
                         state side
                         (reorder-final reorder-side wait-side chosen original dest)
                         card nil))))})))

(defn- reorder-final
  "Generates a recursive prompt structure for cards that do reordering (Indexing, Making an Entrance, etc.)
  This is part 2 - the player is asked for final confirmation of the reorder and is provided an opportunity to start over."

  ([reorder-side wait-side chosen original] (reorder-final reorder-side wait-side chosen original nil))
  ([reorder-side wait-side chosen original dest]
   {:prompt (if (= dest "bottom")
              (str "The bottom cards of " (if (= reorder-side :corp) "R&D" "the stack")
                   " will be " (enumerate-str (map :title (reverse chosen))) ".")
              (str "The top cards of " (if (= reorder-side :corp) "R&D" "the stack")
                   " will be " (enumerate-str (map :title chosen)) "."))
   :choices ["Done" "Start over"]
   :async true
   :effect (req
             (cond
               (and (= dest "bottom") (= target "Done"))
               (do (swap! state update-in [reorder-side :deck]
                          #(vec (concat (drop (count chosen) %) (reverse chosen))))
                   (when (and (= :corp reorder-side)
                              (:run @state)
                              (:access @state))
                     (swap! state assoc-in [:run :shuffled-during-access :rd] true))
                   (clear-wait-prompt state wait-side)
                   (effect-completed state side eid))

               (= target "Done")
               (do (swap! state update-in [reorder-side :deck]
                          #(vec (concat chosen (drop (count chosen) %))))
                   (when (and (= :corp reorder-side)
                              (:run @state)
                              (:access @state))
                     (swap! state assoc-in [:run :shuffled-during-access :rd] true))
                   (clear-wait-prompt state wait-side)
                   (effect-completed state side eid))

               :else
               (continue-ability state side (reorder-choice reorder-side wait-side original '() (count original) original dest) card nil)))}))

(defn breach-access-bonus
  "Access additional cards when breaching a server"
  ([server bonus] (breach-access-bonus server bonus nil))
  ([server bonus {:keys [duration msg] :as args}]
   {:event :breach-server
    :duration duration
    :req (if (:req args)
           (:req args)
           (req (= server target)))
    :msg msg
    :effect (effect (access-bonus :runner server bonus))}))

(defn do-net-damage
  "Do specified amount of net-damage."
  [dmg]
  {:label (str "Do " dmg " net damage")
   :async true
   :msg (str "do " dmg " net damage")
   :effect (effect (damage eid :net dmg {:card card}))})

(defn do-meat-damage
  "Do specified amount of meat damage."
  [dmg]
  {:label (str "Do " dmg " meat damage")
   :async true
   :msg (str "do " dmg " meat damage")
   :effect (effect (damage eid :meat dmg {:card card}))})

(defn do-brain-damage
  "Do specified amount of core damage."
  [dmg]
  {:label (str "Do " dmg " core damage")
   :async true
   :msg (str "do " dmg " core damage")
   :effect (effect (damage eid :brain dmg {:card card}))})

(defn rfg-on-empty
  "Used in :event maps for effects like Malandragem"
  [counter-type]
  {:event :counter-added
   :req (req (and (same-card? card (:card context))
                  (not (get-in card [:special :skipped-loading]))
                  (not (pos? (get-counters card counter-type)))))
   :effect (effect (system-msg (str "removes " (:title card) " from the game"))
                   (move card :rfg))})

(defn trash-on-empty
  "Used in :event maps for effects like Daily Casts"
  [counter-type]
  {:event :counter-added
   :req (req (and (same-card? card (:card context))
                  (not (get-in card [:special :skipped-loading]))
                  (not (pos? (get-counters card counter-type)))))
   :async true
   :effect (effect (system-msg (str "trashes " (:title card)))
                   (trash eid card {:unpreventable true :source-card card}))})

(defn take-credits
  "Take n counters from a card and place them in your credit pool as if they were credits (if possible)"
  ([state side eid card type n] (take-credits state side eid card type n nil))
  ([state side eid card type n args]
   (if-let [card (get-card state card)]
     (let [n (if (= :all n) (get-counters card type) n)
           n (min n (get-counters card type))]
       (if (pos? n)
         (wait-for (add-counter state side card type (- n) {:placed true :suppress-checkpoint true})
                   ;; (queue-event state side :spent-credits-from-card card)
                   (gain-credits state side eid n args))
         (effect-completed state side eid)))
     (effect-completed state side eid))))

;; NOTE - update this as the above (take credits) is updated
(defn spend-credits
  "Take n counters from a card and place them in your credit pool (if possible) - and trigger an event as if the credits were spent"
  ([state side eid card type n] (spend-credits state side eid card type n nil))
  ([state side eid card type n args]
   (if-let [card (get-card state card)]
     (let [n (if (= :all n) (get-counters card type) n)
           n (min n (get-counters card type))]
       (if (pos? n)
         (wait-for (add-counter state side card type (- n) {:placed true :suppress-checkpoint true})
                   (queue-event state :spent-credits-from-card {:card card})
                   (gain-credits state side eid n args))
         (effect-completed state side eid)))
     (effect-completed state side eid))))

(defn make-recurring-ability
  [ability]
  (if (:recurring ability)
    (let [recurring-ability
          {:msg "take 1 [Recurring Credits]"
           :req (req (pos? (get-counters card :recurring)))
           :async true
           :effect (req (spend-credits state side eid card :recurring 1))}]
      (update ability :abilities #(conj (into [] %) recurring-ability)))
    ability))

(defn trash-or-rfg
  [state _ eid card]
  (let [side (to-keyword (:side card))
        title (:title card)]
    (if (:rfg-instead-of-trashing card)
      (do (system-say state side (str title " is removed from the game."))
          (async-rfg state side eid card))
      (do (system-say state side (str title " is trashed."))
          (trash state side eid card {:unpreventable true :game-trash true})))))

(defn get-x-fn []
  (fn get-x-fn-inner
    [state side eid card targets]
    (if-let [x-fn (and (not (is-disabled-reg? state card)) (:x-fn card))]
      (x-fn state side eid card targets)
      0)))

(defn make-current-event-handler
  [title ability]
  (let [card (server-card title)]
    (if (has-subtype? card "Current")
      (let [event-keyword (if (corp? card) :agenda-stolen :agenda-scored)
            static-ab {:type :trash-when-expired
                       :req (req (some #(let [event (:event %)
                                              context-card (:card %)]
                                          (or (= event event-keyword)
                                              (and (#{:play-event :play-operation} event)
                                                   (and (not (same-card? card context-card))
                                                        (has-subtype? context-card "Current")
                                                        true))))
                                       targets))
                       :value trash-or-rfg}]
        (update ability :static-abilities #(conj (into [] %) static-ab)))
      ability)))

(defn add-default-abilities
  [title ability]
  (->> ability
       (make-current-event-handler title)
       (make-recurring-ability)))

(defn something-can-be-advanced?
  "There's either a card on the field that can be advanced, or a card that has the potential to be an advancable card (hidden info)"
  [state]
  (some #(or (not (faceup? %)) (can-be-advanced? state %)) (all-installed state :corp)))

(defn corp-recur
  ([] (corp-recur (constantly true)))
  ([pred]
   {:label "add card from Archives to HQ"
    :prompt "Choose a card to add to HQ"
    :does-something (req (seq (:discard corp)))
    :waiting-prompt true
    :show-discard true
    :choices {:card #(and (corp? %)
                       (in-discard? %)
                       (pred %))}
    :msg (msg "add " (card-str state target {:visible (faceup? target)}) " to HQ")
    :effect (effect (move :corp target :hand))}))

(defn collect-ability
  [base-shards base-cards]
  (letfn [(total-shards [state side card] (max 0 (+ base-shards (sum-effects state side :collect-shards-bonus card))))
          (total-cards  [state side card] (max 0 (+ base-cards  (sum-effects state side :collect-cards-bonus card))))]
    {:label (str "collect " (when (pos? base-shards) (str base-shards " [Credits]"))
                 (when (and (pos? base-shards) (pos? base-cards)) " and ")
                 (when (pos? base-cards) (quantify base-cards "card")))
     :async true
     :collect true
     :cost [(->c :exhaust-self)]
     :req (req (or (pos? (total-shards state side card))
                   (pos? (total-cards state side card))))
     :msg (msg (let [shards (total-shards state side card)
                     cards (total-cards state side card)]
                 (str "collect " (when (pos? shards) (str shards " [Credits]"))
                      (when (and (pos? shards) (pos? cards)) " and ")
                      (when (pos? cards) (quantify cards "card")))))
     :effect (req (let [shards (total-shards state side card)
                        cards (total-cards state side card)]
                    (cond
                      ;; do both
                      (and (pos? shards) (pos? cards))
                      (wait-for (gain-credits state side shards {:suppress-checkpoint true})
                                (draw state side eid cards))
                      ;; just gain
                      (pos? shards)
                      (gain-credits state side eid shards)
                      ;; just draw
                      (pos? cards)
                      (draw state side eid cards)
                      ;; shouldn't be possible
                      :else (effect-completed state side eid))))}))

(defn collect
  "Makes a card which collects n shards and n cards"
  [{:keys [shards cards]} cdef]
  (assoc cdef :abilities (concat [(collect-ability (or shards 0) (or cards 0))] (:abilities cdef))))

(defn shift-self-abi
  [cost]
  {:fake-cost cost
   :req (req (can-pay? state side eid card nil cost))
   :label "Shift this card"
   :async true
   :effect (req (shift-a-card state side eid card card {:cost cost}))})

(def card-defs-cache (atom {}))

(defn stage-n-cards
  [n {:keys [action cost additional-cost rep]}]
  {:label (str "stage up to " n " cards")
   :async true
   :prompt (if (> n 1)
             (str "Stage a card (" n " remaining)")
             "Stage a card")
   :action action
   :cost cost
   :additional-cost additional-cost
   :waiting-prompt true
   :msg (when-not rep (msg "stage up to " (quantify n "card")))
   :req (req (seq (get-in @state [side :hand])))
   :choices {:req (req (and (in-hand? target)
                            (= (:side card) (:side target))
                            (not (moment? target))))}
   :effect (req (wait-for
                  (stage-a-card state side card target)
                  (if (and async-result (> n 1))
                    (continue-ability
                      state side
                      (stage-n-cards (dec n) {:rep true})
                      card nil)
                    (effect-completed state side eid))))})


(defn with-revealed-hand
  "Resolves an ability while a player has their hand revealed (so you can click cards in their hand)
  You can set the side that triggers the reveal (event-side) and if it displays as a forced reveal
  (forced) via the args"
  ([target-side abi] (with-revealed-hand target-side nil abi))
  ([target-side {:keys [event-side forced skip-reveal] :as args} abi]
   ;; note - if the target draws (ie with steelskin), then the hand should be unrevealed again
   ;; this only matters if a card like buffer drive or aniccam is in play that causes a prompt
   ;; and the server sends the paused state back with the new cards faceup
   (letfn [(maybe-register-ev
             [state side card was-open?]
             (if-not was-open?
               (let [uuid (:uuid (first (register-events state side card
                                                         [{:event :card-moved
                                                           :req (req (let [sidefn (if (= :corp target-side) corp? runner?)]
                                                                       (and (sidefn (:moved-card context))
                                                                            (in-hand? (:moved-card context)))))
                                                           :silent (req true)
                                                           :effect (req (conceal-hand state target-side))}])))]
                 (fn [] (unregister-event-by-uuid state side uuid)))
               (fn [] nil)))
           (maybe-reveal
             [state side eid card target-side {:keys [event-side forced skip-reveal] :as args}]
             (if skip-reveal
               (effect-completed state side eid)
               (reveal-loud state (or event-side side) eid card args (get-in @state [target-side :hand]))))]
     {:async true
      :effect (req (wait-for
                     (maybe-reveal state side card target-side args)
                     (let [was-open? (get-in @state [target-side :openhand])
                           unregister-ev-callback (maybe-register-ev state side card was-open?)]
                       (when-not was-open? (reveal-hand state target-side))
                       (wait-for (resolve-ability state side abi card targets)
                                 (when-not was-open? (conceal-hand state target-side))
                                 (unregister-ev-callback)
                                 (effect-completed state side eid)))))})))

(defmacro defcard
  [title ability]
  `(do (swap! card-defs-cache dissoc ~title)
       (defmethod card-defs/defcard-impl ~title [~'_]
         (or (get @card-defs-cache ~title)
             (let [ability# (add-default-abilities ~title ~ability)]
               (swap! card-defs-cache assoc ~title ability#)
               ability#)))))
