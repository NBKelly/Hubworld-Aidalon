(ns game.core.costs
  (:require
   [clojure.set :as set]
   [game.core.board :refer [hubworld-all-installed all-active all-active-installed all-installed all-installed-runner-type]]
   [game.core.barrier :refer [get-barrier]]
   [game.core.card :refer [active? agenda? corp? facedown? get-card get-counters hardware? has-subtype? ice? in-hand?  program? resource?  runner?
                           rezzed? in-discard? installed?
                           seeker? agent?
                           in-front-row? in-back-row?
                           in-archives-path? in-council-path?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.damage :refer [damage]]
   [game.core.eid :refer [complete-with-result make-eid]]
   [game.core.engine :refer [checkpoint queue-event resolve-ability]]
   [game.core.effects :refer [any-effects is-disabled-reg?]]
   [game.core.exhausting :refer [exhaust]]
   [game.core.flags :refer [is-scored?]]
   [game.core.gaining :refer [deduct lose]]
   [game.core.heat :refer [gain-heat lose-heat]]
   [game.core.moving :refer [discard-from-hand flip-facedown forfeit mill move trash trash-cards exile exile-cards swap-installed]]
   [game.core.payment :refer [handler label payable? value stealth-value]]
   [game.core.pick-counters :refer [pick-credit-providing-cards pick-credit-reducers pick-virus-counters-to-spend]]
   [game.core.props :refer [add-counter add-prop]]
   [game.core.revealing :refer [reveal reveal-and-queue-event]]
   [game.core.rezzing :refer [derez]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.to-string :refer [card-str hubworld-card-str]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [number-of-virus-counters]]
   [game.macros :refer [continue-ability req wait-for]]
   [game.utils :refer [enumerate-str quantify same-card? same-side?]]
   [jinteki.utils :refer [other-side]]))

;; Click
(defmethod value :click [cost] (:cost/amount cost))
(defmethod label :click [cost]
  (->> (repeat "[Click]")
       (take (value cost))
       (apply str)))
(defmethod payable? :click
  [cost state side _ _]
  (<= 0 (- (get-in @state [side :click]) (value cost))))
(defmethod handler :click
  [cost state side eid _card]
  (let [a (:action eid)
        idx (get-in eid [:source-info :ability-idx])
        source-abilities (get-in eid [:source :abilities])
        is-game-action? (when (and (= :ability (:source-type eid))
                                   (number? idx)
                                   (seq source-abilities))
                          (:action (nth source-abilities idx {})))
        source (get-in eid [:source])]
    (swap! state update-in [:stats side :lose :click] (fnil + 0) (value cost))
    (deduct state side [:click (value cost)])
    (queue-event state (if (= side :corp) :corp-spent-click :runner-spent-click)
                 {:action a
                  :is-game-action? is-game-action?
                  :stripped-source-card (select-keys source [:cid :title :type])
                  :value (value cost)
                  :ability-idx (:ability-idx (:source-info eid))})
              ;; sending the idx is mandatory to make wage workers functional
              ;; and so we can look through the events and figure out WHICH abilities were used
              ;; I don't think it will break anything
    (swap! state assoc-in [side :register :spent-click] true)
    (complete-with-result state side eid {:paid/msg (str "spends " (label cost))
                                          :paid/type :click
                                          :paid/value (value cost)})))

;; Lose Click
(defn lose-click-label
  [cost]
  (->> (repeat "[Click]")
       (take (value cost))
       (apply str)))
(defmethod value :lose-click [cost] (:cost/amount cost))
(defmethod label :lose-click [cost]
  (str "Lose " (lose-click-label cost)))
(defmethod payable? :lose-click
  [cost state side _ _]
  (<= 0 (- (get-in @state [side :click]) (value cost))))
(defmethod handler :lose-click
  [cost state side eid _card]
  (swap! state update-in [:stats side :lose :click] (fnil + 0) (value cost))
  (deduct state side [:click (value cost)])
  (queue-event state (if (= side :corp) :corp-spent-click :runner-spent-click) {:value (value cost)})
  (swap! state assoc-in [side :register :spent-click] true)
  (complete-with-result state side eid {:paid/msg (str "loses " (lose-click-label cost))
                                        :paid/type :lose-click
                                        :paid/value (value cost)}))


(defn- all-active-pay-credit-cards
  [state side eid card]
  (filter #(when-let [pc (-> % card-def :interactions :pay-credits)]
             ;; All payment should be "inbetween" checkpoints
             ;; If there's ever some obscure issue with timing for this,
             ;; then we can always manually update the reg here
             ;; --nk, Apr 2024
             (when-not (is-disabled-reg? state %)
                 (if (:req pc)
                   ((:req pc) state side eid % [card])
                   true)))
          (all-active state side)))

(defn- eligible-pay-credit-cards
  [state side eid card]
  (filter
    #(case (-> % card-def :interactions :pay-credits :type)
       :recurring
       (pos? (get-counters (get-card state %) :recurring))
       :credit
       (pos? (get-counters (get-card state %) :credit))
       :custom
       ((-> % card-def :interactions :pay-credits :req) state side eid % [card]))
    (all-active-pay-credit-cards state side eid card)))

(defn total-available-credits
  [state side eid card]
  (if-not (any-effects state side :cannot-pay-credit)
    (+ (get-in @state [side :credit])
       (->> (eligible-pay-credit-cards state side eid card)
            (map #(+ (get-counters % :recurring)
                     (get-counters % :credit)
                     (-> (card-def %) :interactions :pay-credits ((fn [x] (:custom-amount x 0))))))
            (reduce +)))
    0))

(defn- eligible-pay-stealth-credit-cards
  [state side eid card]
  (filter #(has-subtype? % "Stealth") (eligible-pay-credit-cards state side eid card)))

(defn- total-available-stealth-credits
  [state side eid card]
  (->> (eligible-pay-stealth-credit-cards state side eid card)
       (map #(+ (get-counters % :recurring)
                (get-counters % :credit)
                (-> (card-def %) :interactions :pay-credits ((fn [x] (:custom-amount x 0))))))
       (reduce +)))

;; Credit
(defmethod value :credit [cost] (:cost/amount cost))
(defmethod stealth-value :credit [cost] (let [v (:cost/stealth cost)]
                                          (cond
                                            (= :all-stealth v) (value cost)
                                            v v
                                            :else 0)))
(defmethod label :credit [cost] (str (value cost) " [Credits]"))
(defmethod payable? :credit
  [cost state side eid card]
  (and (<= 0 (- (total-available-stealth-credits state side eid card) (stealth-value cost)))
       (<= 0 (- (value cost) (stealth-value cost)))
       (or (<= 0 (- (get-in @state [side :credit]) (value cost)))
           (<= 0 (- (total-available-credits state side eid card) (value cost))))))
(defmethod handler :credit
  [cost state side eid card]
  (let [provider-func #(eligible-pay-credit-cards state side eid card)]
    (wait-for
      (resolve-ability state side (pick-credit-reducers provider-func eid (value cost) (stealth-value cost)) card nil)
      (let [updated-cost (max 0 (- (value cost) (or (:reduction async-result) 0)))]
        (cond
          (and (pos? updated-cost)
               (pos? (count (provider-func))))
          (wait-for (resolve-ability state side (pick-credit-providing-cards provider-func eid updated-cost (stealth-value cost)) card nil)
                    (let [pay-async-result async-result]
                      (queue-event state
                                   (if (= side :corp) :corp-spent-credits :runner-spent-credits)
                                   {:value updated-cost})
                      (swap! state update-in [:stats side :spent :credit] (fnil + 0) updated-cost)
                      (complete-with-result state side eid
                                            {:paid/msg (str "pays " (:msg pay-async-result))
                                             :paid/type :credit
                                             :paid/value (:number pay-async-result)
                                             :paid/targets (:targets pay-async-result)})))
          (pos? updated-cost)
          (do (lose state side :credit updated-cost)
              (queue-event state (if (= side :corp) :corp-spent-credits :runner-spent-credits) {:value updated-cost})
              (swap! state update-in [:stats side :spent :credit] (fnil + 0) updated-cost)
              (complete-with-result state side eid {:paid/msg (str "pays " updated-cost " [Credits]")
                                                    :paid/type :credit
                                                    :paid/value updated-cost}))
          :else
          (complete-with-result state side eid {:paid/msg "pays 0 [Credits]"
                                                :paid/type :credit
                                                :paid/value 0}))))))

;; X Credits - can take ':maximum' to specify a max number that can be paid
(defmethod value :x-credits [_] 0)
;We put stealth credits in the third slot rather than the empty second slot for consistency with credits
(defmethod stealth-value :x-credits [cost] (or (:cost/stealth cost) 0))
(defmethod label :x-credits [_] (str "X [Credits]"))
(defmethod payable? :x-credits
  [cost state side eid card]
  (and (pos? (total-available-credits state side eid card))
       (<= (stealth-value cost) (total-available-stealth-credits state side eid card))))
(defmethod handler :x-credits
  [cost state side eid card]
  (continue-ability
    state side
    {:async true
     :prompt "How many credits do you want to spend?"
     :choices {:number (req (if-let [maximum (->> cost :cost/args :maximum)]
                              (min (total-available-credits state side eid card) maximum)
                              (total-available-credits state side eid card)))}
     :effect
     (req
       (let [stealth-value (if (= -1 (stealth-value cost)) cost (stealth-value cost))
             cost target
             provider-func #(eligible-pay-credit-cards state side eid card)]
         (cond
           (and (pos? cost)
                (pos? (count (provider-func))))
           (wait-for (resolve-ability state side (pick-credit-providing-cards provider-func eid cost stealth-value) card nil)
                     (swap! state update-in [:stats side :spent :credit] (fnil + 0) cost)
                     (complete-with-result state side eid {:paid/msg (str "pays " (:msg async-result))
                                                           :paid/type :x-credits
                                                           :paid/value (:number async-result)
                                                           :paid/targets (:targets async-result)}))
           (pos? cost)
           (do (lose state side :credit cost)
               (queue-event state (if (= side :corp) :corp-spent-credits :runner-spent-credits) {:value cost})
               (swap! state update-in [:stats side :spent :credit] (fnil + 0) cost)
               (complete-with-result state side eid {:paid/msg (str "pays " cost " [Credits]")
                                                     :paid/type :x-credits
                                                     :paid/value cost}))
           :else
           (complete-with-result state side eid {:paid/msg (str "pays 0 [Credits]")
                                                 :paid/type :x-credits
                                                 :paid/value 0}))))}
    card nil))

;; Trash
(defmethod value :trash-can [cost] (:cost/amount cost))
(defmethod label :trash-can [cost] "[trash]")
(defmethod payable? :trash-can
  [cost state side eid card]
  (and (installed? (get-card state card))
       (= 1 (value cost))))
(defmethod handler :trash-can
  [cost state side eid card]
  (wait-for (trash state side card {:cause :ability-cost
                                    :unpreventable true
                                    :suppress-checkpoint true})
            (complete-with-result state side eid {:paid/msg (str "trashes " (:printed-title card))
                                                  :paid/type :trash-can
                                                  :paid/value 1
                                                  :paid/targets [card]})))

;; Forfeit
(defmethod value :forfeit [cost] (:cost/amount cost))
(defmethod label :forfeit [cost] (str "forfeit " (quantify (value cost) "Agenda")))
(defmethod payable? :forfeit
  [cost state side _eid _card]
  (<= 0 (- (count (get-in @state [side :scored])) (value cost))))
(defmethod handler :forfeit
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "Agenda") " to forfeit")
     :async true
     :choices {:max (value cost)
               :all true
               :card #(is-scored? state side %)}
     :effect (req (doseq [agenda targets]
                    ;; We don't have to await this because we're suppressing the
                    ;; checkpoint and forfeit makes all of the trashing unpreventable,
                    ;; meaning that there will be no potential for input. Once
                    ;; everything is queued, then we perform the actual checkpoint.
                    (forfeit state side (make-eid state eid) agenda {:msg false
                                                                     :suppress-checkpoint true}))
                  (complete-with-result
                    state side eid
                    {:paid/msg (str "forfeits " (quantify (value cost) "agenda")
                                    " (" (enumerate-str (map :title targets)) ")")
                     :paid/type :forfeit
                     :paid/value (value cost)
                     :paid/targets targets}))}
    card nil))

;; ForfeitSelf
(defmethod value :forfeit-self [_cost] 1)
(defmethod label :forfeit-self [_cost] "forfeit this Agenda")
(defmethod payable? :forfeit-self
  [_cost state side _eid card]
  (is-scored? state side (get-card state card)))
(defmethod handler :forfeit-self
  [_cost state side eid card]
  (wait-for (forfeit state side (make-eid state eid) card {:msg false
                                                           :suppress-checkpoint true})
            (complete-with-result
              state side eid
              {:paid/msg (str "forfeits " (:title card))
               :paid/type :forfeit-self
               :paid/value 1
               :paid/targets [card]})))

;; ReturnToHand
(defmethod value :return-to-hand [cost] 1)
(defmethod label :return-to-hand [cost] "return this card to your hand")
(defmethod payable? :return-to-hand
  [cost state side eid card]
  (active? (get-card state card)))
(defmethod handler :return-to-hand
  [cost state side eid card]
  (move state side card :hand)
  (complete-with-result
    state side eid
    {:paid/msg (str "returns " (:title card)
                   " to " (if (= :corp side) "HQ" "[their] grip"))
     :paid/type :return-to-hand
     :paid/value 1
     :paid/targets [card]}))

;; RemoveFromGame
(defmethod value :remove-from-game [cost] 1)
(defmethod label :remove-from-game [cost] "remove this card from the game")
(defmethod payable? :remove-from-game
  [cost state side eid card]
  (active? (get-card state card)))
(defmethod handler :remove-from-game
  [cost state side eid card]
  (move state side card :rfg)
  (complete-with-result
    state side eid
    {:paid/msg (str "removes " (:title card) " from the game")
     :paid/type :remove-from-game
     :paid/value 1
     :paid/targets [card]}))

;; TrashOtherInstalledCard - this may NOT target the source card (itself), use :trash-installed instead
(defmethod value :trash-other-installed [cost] (:cost/amount cost))
(defmethod label :trash-other-installed [cost]
  (str "trash " (quantify (value cost) "installed card")))
(defmethod payable? :trash-other-installed
  [cost state side eid card]
  (<= 0 (- (count (filter #(not (same-card? card %)) (all-installed state side))) (value cost))))
(defmethod handler :trash-other-installed
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "installed card") " to trash")
     :choices {:all true
               :max (value cost)
               :card #(and (installed? %)
                           (not (same-card? % card))
                           (if (= side :runner)
                             (runner? %)
                             (corp? %)))}
     :async true
     :effect (req (wait-for (trash-cards state side targets {:cause :ability-cost
                                                             :suppress-checkpoint true
                                                             :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "trashes " (quantify (count async-result) "installed card")
                                             " (" (enumerate-str (map #(card-str state %) targets)) ")")
                               :paid/type :trash-other-installed
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

;; TrashInstalledCard - this may target the source card (itself)
(defmethod value :trash-installed [cost] (:cost/amount cost))
(defmethod label :trash-installed [cost]
  (str "trash " (quantify (value cost) "installed card")))
(defmethod payable? :trash-installed
  [cost state side eid card]
  (<= 0 (- (count (all-installed state side)) (value cost))))
(defmethod handler :trash-installed
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "installed card") " to trash")
     :choices {:all true
               :max (value cost)
               :card #(and (installed? %)
                           (if (= side :runner)
                             (runner? %)
                             (corp? %)))}
     :async true
     :effect (req (wait-for (trash-cards state side targets {:cause :ability-cost
                                                             :suppress-checkpoint true
                                                             :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "trashes " (quantify (count async-result) "installed card")
                                             " (" (enumerate-str (map #(card-str state %) targets)) ")")
                               :paid/type :trash-installed
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

;; RandomlyTrashFromHand
(defmethod value :randomly-trash-from-hand [cost] (:cost/amount cost))
(defmethod label :randomly-trash-from-hand [cost]
  (str "trash " (quantify (value cost) "card") " randomly from your hand"))
(defmethod payable? :randomly-trash-from-hand
  [cost state side eid card]
  (<= 0 (- (count (get-in @state [side :hand])) (value cost))))
(defmethod handler :randomly-trash-from-hand
  [cost state side eid card]
  (wait-for (discard-from-hand state side side (value cost) {:suppress-checkpoint true})
            (complete-with-result
              state side eid
              {:paid/msg (str "trashes " (quantify (count async-result) "card")
                             " randomly from "
                             (if (= :corp side) "HQ" "the grip"))
               :paid/type :randomly-trash-from-hand
               :paid/value (count async-result)
               :paid/targets async-result})))

;; TrashEntireHand
(defmethod value :trash-entire-hand [cost] 1)
(defmethod label :trash-entire-hand [cost] "trash all cards in your hand")
(defmethod payable? :trash-entire-hand
  [cost state side eid card] true)
(defmethod handler :trash-entire-hand
  [cost state side eid card]
  (let [cards (get-in @state [side :hand])]
    (wait-for (trash-cards state side cards {:unpreventable true :suppress-checkpoint true :cause :ability-cost})
              (complete-with-result
                state side eid
                {:paid/msg (str "trashes all (" (count async-result) ") cards in "
                               (if (= :runner side) "[their] grip" "HQ")
                               (when (and (= :runner side)
                                          (pos? (count async-result)))
                                 (str " (" (enumerate-str (map :title async-result)) ")")))
                 :paid/type :trash-entire-hand
                 :paid/value (count async-result)
                 :paid/targets async-result}))))

;; AddRandomToBottom
(defmethod value :add-random-from-hand-to-bottom-of-deck [cost] (:cost/amount cost))
(defmethod label :add-random-from-hand-to-bottom-of-deck [cost]
  (str "add " (quantify (value cost) "random card") " to the bottom of your deck"))
(defmethod payable? :add-random-from-hand-to-bottom-of-deck
  [cost state side eid card]
  (<= (value cost) (count (get-in @state [side :hand]))))
(defmethod handler :add-random-from-hand-to-bottom-of-deck
  [cost state side eid card]
  (let [deck (if (= :corp side) "R&D" "the stack")
        hand (get-in @state [side :hand])
        chosen (take (value cost) (shuffle hand))]
    (doseq [c chosen]
      (move state side c :deck))
    (complete-with-result
      state side eid
      {:paid/msg (str "adds " (quantify (value cost) "random card")
                     " to the bottom of " deck)
       :paid/type :add-random-from-hand-to-bottom-of-deck
       :paid/value (value cost)
       :paid/targets chosen})))

;; HUBWORLD COSTS
;; exhaust self
(defmethod value :exhaust-self [cost] (:cost/amount cost))
(defmethod label :exhaust-self [cost] "Exhaust")
(defmethod payable? :exhaust-self
  [cost state side eid card]
  (and (not (:exhausted card))
       (or (seeker? card)
           (rezzed? card))))
(defmethod handler :exhaust-self
  [cost state side eid card]
  (wait-for (exhaust state side card {:unpreventable true :suppress-checkpoint true :no-msg true})
            (complete-with-result state side eid {:paid/msg (str "exhausts " (:title card))
                                                  :paid/type :exhaust-self
                                                  :paid/value 1
                                                  :paid/targets [card]})))

;; exhaust your seeker
(defmethod value :exhaust-seeker [cost] (:cost/amount cost))
(defmethod label :exhaust-seeker [cost] "Exhaust your Seeker")
(defmethod payable? :exhaust-seeker
  [cost state side eid card]
  (not (get-in @state [side :identity :exhausted])))
(defmethod handler :exhaust-seeker
  [cost state side eid card]
  (wait-for (exhaust state side (get-in @state [side :identity])
                     {:unpreventable true :suppress-checkpoint true :no-msg true})
            (complete-with-result state side eid {:paid/msg (str "exhausts " (get-in @state [side :identity :title]))
                                                  :paid/type :exhaust-seeker
                                                  :paid/value 1
                                                  :paid/targets [(get-in @state [side :identity])]})))

;; exhaust any number of cards - this may target the source card (itself)
(defmethod value :exhaust [cost] (:cost/amount cost))
(defmethod label :exhaust [cost] (str "exhaust " (quantify (value cost) " cards")))
(defmethod payable? :exhaust
  [cost state side eid card]
  (<= 0 (- (count (filter (complement :exhausted)
                          (hubworld-all-installed state side)))
           (value cost))))
(defmethod handler :exhaust
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) " card") " to exhaust")
     :choices {:all true
               :max (value cost)
               :card #(and (or (installed? %)
                               (= "Seeker" (:type %)))
                           (if (= side :runner)
                             (runner? %)
                             (corp? %)))}
     :async true
     :effect (req (wait-for (exhaust state side targets {:suppress-checkpoint true
                                                         :no-msg true
                                                         :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "exhausts " (quantify (count async-result) " card")
                                              " (" (enumerate-str (map #(hubworld-card-str state %) targets)) ")")
                               :paid/type :exhaust
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

;; exhaust any number of FORGED cards - this may target the source card (itself)
(defmethod value :exhaust-forged [cost] (:cost/amount cost))
(defmethod label :exhaust-forged [cost] (str "exhaust " (quantify (value cost) "forged card")))
(defmethod payable? :exhaust-forged
  [cost state side eid card]
  (<= 0 (- (count (filter (every-pred (complement :exhausted) rezzed?)
                          (hubworld-all-installed state side)))
           (value cost))))
(defmethod handler :exhaust-forged
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "forged card") " to exhaust")
     :choices {:all true
               :max (value cost)
               :card #(and (installed? %)
                           (rezzed? %)
                           (not= "Seeker" (:type %))
                           (if (= side :runner)
                             (runner? %)
                             (corp? %)))}
     :async true
     :effect (req (wait-for (exhaust state side targets {:suppress-checkpoint true
                                                         :no-msg true
                                                         :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "exhausts " (quantify (count async-result) " forged card")
                                              " (" (enumerate-str (map :title targets)) ")")
                               :paid/type :exhaust-forged
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

(defmethod value :exhaust-forged-with-4-barrier [cost] (:cost/amount cost))
(defmethod label :exhaust-forged-with-4-barrier [cost] (str "exhaust " (quantify (value cost) "forged card")))
(defmethod payable? :exhaust-forged-with-4-barrier
  [cost state side eid card]
  (<= 0 (- (count (filter #(and (rezzed? %)
                                (not (:exhausted %))
                                (>= (get-barrier %) 4))
                          (hubworld-all-installed state side)))
           (value cost))))
(defmethod handler :exhaust-forged-with-4-barrier
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "forged card") " to exhaust")
     :choices {:all true
               :max (value cost)
               :card #(and (installed? %)
                           (rezzed? %)
                           (not= "Seeker" (:type %))
                           (>= (get-barrier %) 4)
                           (if (= side :runner)
                             (runner? %)
                             (corp? %)))}
     :async true
     :effect (req (wait-for (exhaust state side targets {:suppress-checkpoint true
                                                         :no-msg true
                                                         :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "exhausts " (quantify (count async-result) " forged card")
                                              " (" (enumerate-str (map :title targets)) ")")
                               :paid/type :exhaust-forged-with-4-barrier
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

(defmethod value :exhaust-council [cost] (:cost/amount cost))
(defmethod label :exhaust-council [cost] (str "exhaust " (quantify (value cost) "card") " protecting your Council"))
(defmethod payable? :exhaust-council
  [cost state side eid card]
  (<= 0 (- (count (filter (every-pred (complement :exhausted) in-council-path?)
                          (hubworld-all-installed state side)))
           (value cost))))
(defmethod handler :exhaust-council
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "card") " in your Council path to exhaust")
     :choices {:all true
               :max (value cost)
               :card #(and (installed? %)
                           (in-council-path? %)
                           (not= "Seeker" (:type %))
                           (if (= side :runner)
                             (runner? %)
                             (corp? %)))}
     :async true
     :effect (req (wait-for (exhaust state side targets {:suppress-checkpoint true
                                                         :no-msg true
                                                         :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "exhausts " (quantify (count async-result) " card")
                                              " (" (enumerate-str (map :title targets)) ")")
                               :paid/type :exhaust-council
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

;; exhaust any number of cards protected archives - this may target the source card (itself)
(defmethod value :exhaust-archives [cost] (:cost/amount cost))
(defmethod label :exhaust-archives [cost] (str "exhaust " (quantify (value cost) "card") " protecting Archives"))
(defmethod payable? :exhaust-archives
  [cost state side eid card]
  (<= 0 (- (count (filter (every-pred (complement :exhausted) in-archives-path?)
                          (hubworld-all-installed state side)))
           (value cost))))
(defmethod handler :exhaust-archives
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "card") " in your Archives path to exhaust")
     :choices {:all true
               :max (value cost)
               :card #(and (installed? %)
                           (in-archives-path? %)
                           (not= "Seeker" (:type %))
                           (if (= side :runner)
                             (runner? %)
                             (corp? %)))}
     :async true
     :effect (req (wait-for (exhaust state side targets {:suppress-checkpoint true
                                                         :no-msg true
                                                         :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "exhausts " (quantify (count async-result) " card")
                                              " (" (enumerate-str (map :title targets)) ")")
                               :paid/type :exhaust-archives
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

;; exhaust any number of cards protecting your front row - this may target the source card (itself)
(defmethod value :exhaust-front-row [cost] (:cost/amount cost))
(defmethod label :exhaust-front-row [cost] (str "exhaust " (quantify (value cost) "card") " protecting your front row"))
(defmethod payable? :exhaust-front-row
  [cost state side eid card]
  (<= 0 (- (count (filter (every-pred (complement :exhausted) in-front-row?)
                          (hubworld-all-installed state side)))
           (value cost))))
(defmethod handler :exhaust-front-row
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "card") " in your front row to exhaust")
     :choices {:all true
               :max (value cost)
               :card #(and (installed? %)
                           (in-front-row? %)
                           (not= "Seeker" (:type %))
                           (if (= side :runner)
                             (runner? %)
                             (corp? %)))}
     :async true
     :effect (req (wait-for (exhaust state side targets {:suppress-checkpoint true
                                                         :no-msg true
                                                         :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "exhausts " (quantify (count async-result) " card")
                                              " (" (enumerate-str (map :title targets)) ")")
                               :paid/type :exhaust-front-row
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

(defmethod value :exile-reaction [cost] (:cost/amount cost))
(defmethod label :exile-reaction [cost] "Exile this card")
(defmethod payable? :exile-reaction
  [cost state side eid card]
  (and (in-hand? (get-card state card))
       (= 1 (value cost))))
(defmethod handler :exile-reaction
  [cost state side eid card]
  (wait-for (exile state side card {:cause :ability-cost
                                    :seen true
                                    :unpreventable true
                                    :suppress-checkpoint true})
            (complete-with-result state side eid {:paid/msg (str "exiles " (:printed-title card))
                                                  :paid/type :exile-reaction
                                                  :paid/value 1
                                                  :paid/targets [card]})))

(defmethod value :exile-from-archives [cost] (:cost/amount cost))
(defmethod label :exile-from-archives [cost]
  (str "exile " (quantify (value cost) "card") " from your Archives"))
(defmethod payable? :exile-from-archives [cost state side eid card]
  (<= 0 (- (count (get-in @state [side :discard])) (value cost))))
(defmethod handler :exile-from-archives
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "card")
                  " in your Archives to Exile")
     :show-discard true
     :choices {:all true
               :max (value cost)
               :req (req (and (in-discard? target)
                              (same-side? (:side card) side)))}
     :async true
     :effect (req (wait-for (exile-cards state side targets {:cause :ability-cost
                                                             :seen true
                                                             :unpreventable true
                                                             :suppress-checkpoint true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "exiles " (quantify (value cost) "card")
                                              " from [their] archives ("
                                              (enumerate-str (map :title targets)) ")")
                               :paid/type :exile-from-archives
                               :paid/value (value cost)
                               :paid/targets targets})))}
    card nil))

(defmethod value :exile-total-shard-cost-from-council [cost] (:cost/amount cost))
(defmethod label :exile-total-shard-cost-from-council [cost]
  (str "exile cards from council with total shard cost of " (value cost) " [Credits] or more"))
(defmethod payable? :exile-total-shard-cost-from-council [cost state side eid card]
  (<= 0 (- (reduce + (map :cost (get-in @state [side :hand]))) (value cost))))
(defmethod handler :exile-total-shard-cost-from-council
  [cost state side eid card]
  (letfn [(choose-more [remaining to-exile]
            (let [exile-value (reduce + (map :cost to-exile))
                  amount-to-pay (max (- (value cost) exile-value) 0)]
              {:async true
               :prompt (str "Choose a card to exile (remaining: " amount-to-pay " [Credits])")
               :choices (if (zero? amount-to-pay) (conj (vec remaining) "Done") remaining)
               :effect (req (if (= "Done" target)
                              (wait-for (exile-cards state side to-exile {:cause :ability-cost
                                                                          :seen true
                                                                          :unpreventable true
                                                                          :suppress-checkpoint true})
                                        (complete-with-result
                                          state side eid
                                          {:paid/msg (str "exiles " (quantify (count to-exile) "card")
                                                          " from [their] council with total shard cost of "
                                                          exile-value " [Credits] (" (enumerate-str (map :title to-exile)) ")")
                                           :paid/type :exile-total-shard-cost-from-council
                                           :paid/value (value cost)
                                           :paid/targets to-exile}))
                              (continue-ability state side (choose-more
                                                             (set/difference (set remaining) (set [target]))
                                                             (conj to-exile target)) card nil)))
               }))]
    (continue-ability state side
      (choose-more (get-in @state [side :hand]) '()) card nil)))


;; Gain heat
(defmethod value :gain-heat [cost] (:cost/amount cost))
(defmethod label :gain-heat [cost] (str "gain " (value cost) " [heat]"))
(defmethod payable? :gain-heat
  [_ _ _ _ _] ;; probably fine
  true)
(defmethod handler :gain-heat
  [cost state side eid card]
  (wait-for (gain-heat state side (value cost) {:suppress-checkpoint true})
            (complete-with-result state side eid {:paid/msg (str "gains " (value cost) " [heat]")
                                                  :paid/type :gain-heat
                                                  :paid/value (value cost)})))

;; Steal heat
(defmethod value :steal-heat [cost] (:cost/amount cost))
(defmethod label :steal-heat [cost] (str "steal " (value cost) " [heat]"))
(defmethod payable? :steal-heat [cost state side eid card]
  (>= (value cost) (get-in @state [(other-side side) :heat :base] 0)))
(defmethod handler :steal-heat
  [cost state side eid card]
  (wait-for (lose-heat state (other-side side) (value cost) {:suppress-checkpoint true})
            (wait-for (gain-heat state side (value cost) {:suppress-checkpoint true})
                      (complete-with-result state side eid {:paid/msg (str "steals " (value cost) " [heat]")
                                                            :paid/type :steal-heat
                                                            :paid/value (value cost)}))))

;; TrashFromDeck
(defmethod value :archive-from-deck [cost] (:cost/amount cost))
(defmethod label :archive-from-deck [cost]
  (str "archive " (quantify (value cost) "card") " from the top of your Commons"))
(defmethod payable? :archive-from-deck
  [cost state side eid card]
  (<= 0 (- (count (get-in @state [side :deck])) (value cost))))
(defmethod handler :archive-from-deck
  [cost state side eid card]
  (wait-for (mill state side side (value cost) {:suppress-checkpoint true})
            (complete-with-result
              state side eid
              {:paid/msg (str "archives " (quantify (count async-result) "card")
                              " from the top of [their] Commons")
               :paid/type :archive-from-deck
               :paid/value (count async-result)
               :paid/targets async-result})))

(defmethod value :archive-from-council [cost] (:cost/amount cost))
(defmethod label :archive-from-council [cost]
  (str "archive " (quantify (value cost) "card") " from your council"))
(defmethod payable? :archive-from-council
  [cost state side eid card]
  (<= 0 (- (count (get-in @state [side :hand])) (value cost))))
(defmethod handler :archive-from-council
  [cost state side eid card]
  (let [select-fn #(and ((if (= :corp side) corp? runner?) %)
                        (in-hand? %))
        hand "[their] council"]
    (continue-ability
      state side
      {:prompt (str "Choose " (quantify (value cost) "card") " to archive")
       :choices {:all true
                 :max (value cost)
                 :card select-fn}
       :async true
       :effect (req (wait-for (trash-cards state side targets {:unpreventable true :seen false :cause :ability-cost :suppress-checkpoint true})
                              (complete-with-result
                                state side eid
                                {:paid/msg (str "archives " (quantify (count async-result) "card")
                                               (when (and (= :runner side)
                                                          (pos? (count async-result)))
                                                 (str " (" (enumerate-str (map #(card-str state %) targets)) ")"))
                                               " from " hand)
                                 :paid/type :trash-from-hand
                                 :paid/value (count async-result)
                                 :paid/targets async-result})))}
      nil nil)))

(defmethod value :swap-front-and-back-row-cards [cost] (:cost/amount cost))
(defmethod label :swap-front-and-back-row-cards [cost]
  "swap a card in your front and back row")
(defmethod payable? :swap-front-and-back-row-cards
  [cost state side eid card]
  (and (some in-front-row? (hubworld-all-installed state side))
       (some in-back-row? (hubworld-all-installed state side))))
(defmethod handler :swap-front-and-back-row-cards
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose cards in your front and back row to swap")
     :choices {:all true
               :max 2
               :req (req (and (installed? target)
                              (my-card? target)
                              (let [pre-selected (first (:cards (get-in @state [side :selected 0] [])))]
                                (or (and (not pre-selected)
                                         (or (in-front-row? target) (in-back-row? target)))
                                    (same-card? target pre-selected)
                                    (and (in-front-row? target) (in-back-row? pre-selected))
                                    (and (in-front-row? pre-selected) (in-back-row? target))))))}
     :async true
     :effect (req (swap-installed state side (first targets) (second targets))
                  (complete-with-result
                    state side eid
                    {:paid/msg (str "swaps " (hubworld-card-str state (first targets))
                                    " with " (hubworld-card-str state (second targets)))
                     :paid/type :swap-front-and-back-row-cards
                     :paid/value 1
                     :paid/targets targets}))}
    nil nil))

(defmethod value :reveal-agent-in-hand-or-discard [cost] (:cost/amount cost))
(defmethod label :reveal-agent-in-hand-or-discard [cost]
  (str "reveal " (quantify (value cost) "agent") " in your Council or Archives"))
(defmethod payable? :reveal-agent-in-hand-or-discard
  [cost state side eid card]
  (<= 0 (-
          (count
            (filter agent? (concat (get-in @state [side :hand]) (get-in @state [side :discard]))))
          (value cost))))
(defmethod handler :reveal-agent-in-hand-or-discard
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Reveal " (quantify (value cost) "agent") "in your Council or Archives")
     :choices {:all true
               :max (value cost)
               :req (req (and (agent? target)
                              (my-card? target)
                              (or (in-hand? target)
                                  (in-discard? target))))}
     :async true
     :effect (req (reveal-and-queue-event state side targets)
                  (complete-with-result
                    state side eid
                    {:paid/msg (str "reveals " (enumerate-str (map #(hubworld-card-str state %) targets)))
                     :paid/type :reveal-agent-in-hand-or-discard
                     :paid/value (count targets)
                     :paid/targets targets}))}
    nil nil))

(defmethod value :unforge [cost] (:cost/amount cost))
(defmethod label :unforge [cost]
  (str "unforge " (quantify (value cost) " installed card")))
(defmethod payable? :unforge
  [cost state side eid card]
  (<= 0 (- (count (filter (every-pred rezzed? (complement seeker?))
                          (hubworld-all-installed state side)))
           (value cost))))
(defmethod handler :unforge
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (value cost) " cards to unforge")
     :choices {:all true
               :max (value cost)
               :req (req (and (my-card? target)
                              (installed? target)
                              (rezzed? target)
                              (not (seeker? target))))}
     :async true
     :effect (req (doseq [t targets]
                    (derez state side t {:no-msg true}))
                  (complete-with-result
                    state side eid
                    {:paid/msg (str "unforges " (enumerate-str (map :title targets)))
                     :paid/type :unfroge
                     :paid/value (count targets)
                     :paid/targets targets}))}
    nil nil))

(defmethod value :shuffle-installed [cost] (:cost/amount cost))
(defmethod label :shuffle-installed [cost]
  (str "shuffle " (quantify (value cost) "card") " from your grid into your Commons"))
(defmethod payable? :shuffle-installed
  [cost state side eid card]
  (<= 0 (- (count (filter (complement seeker?) (hubworld-all-installed state side))) (value cost))))
(defmethod handler :shuffle-installed
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Shuffle " (quantify (value cost) "card") " from your grid into your Commons")
     :choices {:all true
               :max (value cost)
               :req (req (and (my-card? target)
                              (installed? target)
                              (not (seeker? target))))}
     :async true
     :effect (req (doseq [t targets]
                    (move state side t :deck))
                  (shuffle! state side :deck)
                  (complete-with-result
                    state side eid
                    {:paid/msg (str "shuffles " (enumerate-str (map #(hubworld-card-str state %) targets)) " into their Commons")
                     :paid/type :shuffle-installed
                     :paid/value (count targets)
                     :paid/targets targets}))}
    nil nil))
