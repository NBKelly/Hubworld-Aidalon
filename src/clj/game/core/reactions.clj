(ns game.core.reactions
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.board :refer [hubworld-all-active]]
   [game.core.bluffs :refer [bluffs]]
   [game.core.card :refer [get-card installed? resource? rezzed? same-card? in-hand?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.choose-one :refer [choose-one-helper]]
   [game.core.cost-fns :refer [card-ability-cost play-cost]]
   [game.core.eid :refer [complete-with-result effect-completed]]
   [game.core.effects :refer [any-effects get-effects]]
   [game.core.engine :refer [resolve-ability trigger-event-simult trigger-event-sync]]
   [game.core.flags :refer [can-trash? untrashable-while-resources? untrashable-while-rezzed?]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.prompts :refer [clear-wait-prompt show-bluff-prompt]]
   [game.core.say :refer [enforce-msg]]
   [game.core.to-string :refer [card-str]]
   [game.utils :refer [dissoc-in enumerate-str quantify]]
   [game.macros :refer [msg req wait-for]]
   [jinteki.utils :refer [other-side]]))

;; Sow how does this system work?
;;   * Cards may have a :reaction list in their cdef
;;   * This is a list of all reaction abilities on the card
;;   * A reaction ability looks like this:
;;
;;   :reaction [{:reaction :forge
;;               :type :ability
;;               :prompt "Lose 1 [heat]?"
;;               :ability {:async true
;;                         :max-uses 1
;;                         :req (req (pos? (count-heat state side)))
;;                         :msg "remove 1 [heat]"
;;                         :effect (req (lose-heat state side eid))}}]
;;
;;  KEYS:
;;    :reaction - key, what this reaction is for (ie :tag, :damage, :pre-damage, :jack-out)
;;    :type     - either :ability, or :event, for if this is a paid ability or a triggered event
;;    :prompt   - string - optional - a prompt to be displayed when you click the button
;;                if this isn't supplied, it will instead jump straight into the ability resolution
;;                otherwise, it will be an `:optional {:prompt prompt :yes-ability ability}`
;;    :label    - stribng - optional - the label for the button. If not supplied, it will just be the
;;                name of the card (ie "Decoy"). I recommend only using this for handlers that
;;                have ambiguity (ie feedback filter, dummy box, caldera, zaibatsu loyalty) as a way
;;                of adding additional contextual information for the user.
;;    :location - if this requires being installed, do not fill.
;;                  Otherwise, do one of:
;;                    :hand (if it triggers while in hand)
;;    :max-uses - int - optional - is there a maximum number of times this ability can trigger in one
;;                instance? See: prana, muresh, cleaners, etc. If blank,
;;                then this ability can be used any number of times so long as the req fn allows it
;;    :ability  - the ability that gets triggered when you chose the reaction.
;;                Write it like any other ability. It will have access to a context map which
;;                will typically have: (TBD)
;;
;;
;;  Note: The `:req` fn of the given :ability is used for computing if the button should
;;        show up, as well as wether or not you can pay the cost.

(defn- relevant-reaction-abilities
  "selects all reaction abilities which are:
   1) relevant to the context
   2) playable
   3) the player can afford to pay for
   4) haven't been used too many times"
  [state side eid key card]
  (let [abs (filter #(= (:reaction %) key) (:reaction (card-def card)))
        abs (filter #(case (:location %)
                       :hand (in-hand? card)
                       (installed? card))
                    abs)
        with-card (map #(assoc % :card card) abs)
        moment-cost? (fn [ab] (when (= :moment (:type ab))
                                [(->c :credit (play-cost state side card nil))]))
        playable? (filter #(let [cannot-play? (and (= (:type %) :ability)
                                                   (any-effects state side :prevent-paid-ability true? card [(:ability %) 0]))
                                 payable? (can-pay?
                                            state side eid card nil
                                            (seq (concat (card-ability-cost state side (:ability %) card [])
                                                         (moment-cost? %))))
                                 ;; todo - account for card being disabled
                                 not-used-too-many-times? (or (not (:max-uses %))
                                                              (not (get-in @state [:reaction key :uses (:cid card)]))
                                                              (< (get-in @state [:reaction key :uses (:cid card)]) (:max-uses %)))
                                 ability-req? (or (not (get-in % [:req]))
                                                  ((get-in % [:req]) state side eid card [(get-in @state [:reaction key])]))]
                             (and (not cannot-play?) payable? not-used-too-many-times? ability-req?))
                          abs)]
    (seq (map #(assoc % :card card) playable?))))

;; floating abilities probably don't exist... right?

(defn- gather-reaction-abilities
  [state side eid key]
  (mapcat #(relevant-reaction-abilities state side eid key %)
          (concat (hubworld-all-active state side)
                  (get-in @state [side :hand]))))

(defn- fetch-and-clear!
  "get the reaction map for a key and also dissoc it from the state"
  [state key]
  (let [res (get-in @state [:reaction key])]
    (if (seq (:reaction-stack @state))
      (do (swap! state assoc :reaction (first (:reaction-stack @state)))
          (swap! state update :reaction-stack rest))
      (swap! state dissoc :reaction))
    res))

(defn- push-reaction!
  [state key map]
  (let [map (update map :uses #(or % {}))]
    (when (:reaction @state)
      (swap! state assoc :reaction-stack (concat [(:reaction @state)] (:reaction-stack @state))))
    (swap! state assoc-in [:reaction key] map)))

(defn- trigger-reaction
  "Triggers an ability as having fired something"
  [state side eid key reaction]
  ;; this marks the player as having acted, so we can play the priority game
  ;; Note that this requires the following concession:
  ;;   * All abilities should either use the prompt system set up here
  ;;   * Or if they do not, clicking the ability MUST act
  ;; The consequence of ignoring this is the potential for a silly player to pretend to act, do nothing, and flip priority
  (let [abi {:async true
             :effect (req (swap! state assoc-in [:reaction key :priority-passes] 0)
                          (swap! state update-in [:reaction key :uses (->> reaction :card :cid)] (fnil inc 0))
                          (resolve-ability state side eid (:ability reaction) card [(get-in @state [:reaction key])]))}]
    (resolve-ability
      state side (assoc eid :source (:card reaction) :source-type :ability)
      (if (:prompt reaction)
        {:optional {:prompt (:prompt reaction)
                    :yes-ability abi}}
        abi)
      (:card reaction) nil)))

(defn- build-reaction-option
  "Builds a menu item for firing a reaction ability"
  [reaction key]
  {:option (or (:label reaction) (->> reaction :card :printed-title))
   :card (:card reaction)
   :ability {:async true
             :effect (req (trigger-reaction state side eid key reaction))}})

;;; ALL THE STUFF ABOVE HERE IS S O L I D

(defn- resolve-reaction-for-side
  [state side eid key {:keys [prompt waiting] :as args}]
  (if (get-in @state [:reaction key :passed])
    (do (swap! state dissoc-in [:reaction key :passed])
        (effect-completed state side eid))
    (let [reactions (gather-reaction-abilities state side eid key)]
      (if (empty? reactions)
        (if-let [bluff-fn (key bluffs)]
          (if (bluff-fn state side eid nil [(get-in @state [:reaction key])])
            (wait-for (show-bluff-prompt state side {:msg waiting})
                      (effect-completed state side eid))
            (effect-completed state side eid))
          (effect-completed state side eid))
        (wait-for (resolve-ability
                    state side
                    (choose-one-helper
                      {:prompt (str (side prompt) " - perform a reaction?")
                       :waiting-prompt waiting}
                      (concat (mapv #(build-reaction-option % key) reactions)
                              [(when-not (some :mandatory reactions)
                                 {:option "Pass priority"
                                  :ability {:effect (req (swap! state assoc-in [:reaction key :passed] true))}})]))
                    nil nil)
                  (resolve-reaction-for-side state side eid key args))))))

(defn resolve-reaction-effects-with-priority
  "Resolves reaction effects for a given key, automatically passing priority back and forth while doing so"
  [state side eid key reaction-fn {:keys [prompt waiting] :as args}]
  (let [side (or side
                 (and (:delve @state) (:active-player @state))
                 (-> @state :turn :first-player)
                 :corp)]
    (if (= 2 (get-in @state [:reaction key :priority-passes]))
      (complete-with-result state side eid (fetch-and-clear! state key))
      (wait-for (reaction-fn state side key args)
                (swap! state update-in [:reaction key :priority-passes] (fnil inc 0))
                (resolve-reaction-effects-with-priority state (other-side side) eid key reaction-fn args)))))

;; reaction types

(defn resolve-forge-reaction
  [state side eid {:keys [card] :as args}]
  (push-reaction! state :forge
                  {:card card :source-player side :priority-passes 0})
  (resolve-reaction-effects-with-priority
    state nil eid :forge resolve-reaction-for-side
    {:prompt  {side              (str "You forged " (:title card))
               (other-side side) (str "Your opponent forged " (:title card))}
     :waiting "your opponent to resolve on-forge reactions"}))

(defn resolve-complete-breach-reaction
  [state side eid {:keys [breach-server delver defender] :as args}]
  (push-reaction! state :complete-breach
                  {:breach-server breach-server
                   :delver delver
                   :defender defender})
  (resolve-reaction-effects-with-priority
    state delver eid :complete-breach resolve-reaction-for-side
    {:prompt {delver   (str "You finished breaching " (name breach-server))
              defender (str "Your opponent finished breaching " (name breach-server))}
     :waiting "your opponent to resolve end-of-breach reactions"}))
