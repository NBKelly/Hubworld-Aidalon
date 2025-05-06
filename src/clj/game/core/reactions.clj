(ns game.core.reactions
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.board :refer [hubworld-all-active]]
   [game.core.bluffs :refer [bluffs]]
   [game.core.card :refer [get-card installed? rezzed? seeker? same-card? in-hand?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.choose-one :refer [choose-one-helper]]
   [game.core.cost-fns :refer [card-ability-cost play-cost]]
   [game.core.eid :refer [complete-with-result effect-completed]]
   [game.core.effects :refer [any-effects]]
   [game.core.engine :refer [resolve-ability]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.prompts :refer [show-bluff-prompt]]
   [game.core.to-string :refer [hubworld-card-str]]
   [game.utils :refer [dissoc-in]]
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

(defn- tweak-ab-cost
  [state side eid card ab]
  (if (= (:type ab) :moment)
    (let [pc (play-cost state side card nil)
          ac (get-in ab [:ability :cost])]
      (cond
        (and pc ab (pos? pc))
        (assoc-in ab [:ability :cost] (concat ac [(->c :credit pc)]))
        (and pc (pos? pc))
        (assoc-in ab [:ability :cost] [(->c :credit pc)])
        :else ab))
    ab))

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
                       (or (installed? card) (seeker? card)))
                    abs)
        abs (map #(tweak-ab-cost state side eid card %) abs)
        with-card (map #(assoc % :card card) abs)
        playable? (filter #(let [cannot-play? (and (= (:type %) :ability)
                                                   (any-effects state side :prevent-paid-ability true? card [(:ability %) 0]))
                                 payable? (can-pay?
                                            state side eid card nil
                                            (seq (card-ability-cost state side (:ability %) card [])))
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
      (:card reaction) [(get-in @state [:reaction key])])))

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
                      {:prompt (str ((side prompt) (get-in @state [:reaction key]))
                                    " - perform a reaction?")
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
                 (-> @state :delve :delver)
                 (-> @state :turn :first-player)
                 :corp)]
    (if (= 2 (get-in @state [:reaction key :priority-passes]))
      (complete-with-result state side eid (fetch-and-clear! state key))
      (wait-for (reaction-fn state side key args)
                (swap! state update-in [:reaction key :priority-passes] (fnil inc 0))
                (resolve-reaction-effects-with-priority state (other-side side) eid key reaction-fn args)))))

;; ====== REACTION TYPES ======

;; FORGING CARDS

(defn forge-reaction
  [state side eid {:keys [card] :as args}]
  (push-reaction! state :forge (assoc args :source-player side))
  (resolve-reaction-effects-with-priority
    state nil eid :forge resolve-reaction-for-side
    {:prompt  {side              #(str "You forged " (-> % :card :title))
               (other-side side) #(str "Your opponent forged " (-> % :card :title))}
     :waiting "your opponent to resolve on-forge reactions"}))

;; DELVING SERVERS

(defn approach-district-reaction
  [state side eid {:keys [server delver defender] :as args}]
  (push-reaction! state :approach-district args)
  (resolve-reaction-effects-with-priority
    state delver eid :approach-district resolve-reaction-for-side
    {:prompt {delver   #(str "You are approaching your opponent's " (-> % :server name str/capitalize))
              defender #(str "Your opponent is approaching your " (-> % :server name str/capitalize))}
     :waiting "your opponent to resolve approach-district reactions"}))

(defn complete-breach-reaction
  [state side eid {:keys [breach-server delver defender] :as args}]
  (push-reaction! state :complete-breach args)
  (resolve-reaction-effects-with-priority
    state delver eid :complete-breach resolve-reaction-for-side
    {:prompt {delver   #(str "You finished breaching your opponent's " (str/capitalize (name (:breach-server %))))
              defender #(str "Your opponent finished breaching your " (str/capitalize (name (:breach-server %))))}
     :waiting "your opponent to resolve end-of-breach reactions"}))

(defn pre-discovery-reaction
  [state side eid {:keys [breach-server delver defender] :as args}]
  (push-reaction! state :pre-discovery args)
  (resolve-reaction-effects-with-priority
    state delver eid :pre-discovery resolve-reaction-for-side
    {:prompt {delver   #(str "You are breaching your opponent's " (str/capitalize (name (:breach-server %))))
              defender #(str "Your opponent is breaching your " (str/capitalize (name (:breach-server %))))}
     :waiting "your opponent to resolve pre-discovery reactions"}))

;; ENCOUNTERING CARDS

(defn pre-discover-reaction
  [state side eid {:keys [engaged-side card] :as args}]
  (push-reaction! state :pre-discover args)
  (resolve-reaction-effects-with-priority
    state nil eid :pre-discover resolve-reaction-for-side
    {:prompt {engaged-side              #(str "You are discovering " (:title (:card %)))
              (other-side engaged-side) #(str "Your opponent is discovering " (:title (:card %)))}
     :waiting "your opponent to resolve pre-discover reactions"}))

(defn pre-confrontation-reaction
  [state side eid {:keys [engaged-side card] :as args}]
  (push-reaction! state :pre-confrontation args)
  (resolve-reaction-effects-with-priority
    state nil eid :pre-confrontation resolve-reaction-for-side
    {:prompt {engaged-side              #(str "You are confronting " (:title (:card %)))
              (other-side engaged-side) #(str "Your opponent is confronting " (:title (:card %)))}
     :waiting "your opponent to resolve pre-confrontation reactions"}))

(defn pre-confrontation-ability-reaction
  [state side eid {:keys [defender card] :as args}]
  (push-reaction! state :pre-confrontation-ability args)
  (resolve-reaction-effects-with-priority
    state nil eid :pre-confrontation-ability resolve-reaction-for-side
    {:prompt {defender              (str "You are about to resolve a confrontation ability")
              (other-side defender) (str "your opponent is about to resolve a confrontation ability")}
     :waiting "your opponent to resolve pre-confrontation-ability reactions"}))

(defn pre-discover-ability-reaction
  [state side eid {:keys [defender card] :as args}]
  (push-reaction! state :pre-discover-ability args)
  (resolve-reaction-effects-with-priority
    state nil eid :pre-discover-ability resolve-reaction-for-side
    {:prompt {defender              (str "You are about to resolve a discover ability")
              (other-side defender) (str "your opponent is about to resolve a discover ability")}
     :waiting "your opponent to resolve pre-discover-ability reactions"}))

(defn post-discover-ability-reaction
  [state side eid {:keys [defender discovered-card] :as args}]
  (push-reaction! state :post-discover-ability args)
  (resolve-reaction-effects-with-priority
    state nil eid :post-discover-ability resolve-reaction-for-side
    {:prompt {defender              (fn [_] (str "You have resolved a discover ability"))
              (other-side defender) (fn [_] (str "your opponent has resolved a discover ability"))}
     :waiting "your opponent to resolve post-discover-ability reactions"}))

(defn encounter-ended-reaction
  [state side eid {:keys [delver encounter-card] :as args}]
  (if (get-card state encounter-card)
    (do
      (push-reaction! state :encounter-ended args)
      (resolve-reaction-effects-with-priority
        state nil eid :encounter-ended resolve-reaction-for-side
        {:prompt {delver              #(str "You finished an encounter with " (hubworld-card-str state (:encounter-card %)))
                  (other-side delver) #(str "Your opponent finished an encounter with " (:title (:encounter-card %)))}
         :waiting "your opponent to resolve post-encounter reactions"}))
    (effect-completed state side eid)))

(defn approach-slot-reaction
  [state side eid {:keys [defender approached-card] :as args}]
  (if-not (get-card state approached-card)
    (effect-completed state side eid)
    (do
      (push-reaction! state :approach-slot args)
      (resolve-reaction-effects-with-priority
        state nil eid :approach-slot resolve-reaction-for-side
        {:prompt {defender              #(str "Your opponent is approaching " (:title (:approached-card %)))
                  (other-side defender) #(str "You are approaching " (hubworld-card-str state (:approached-card %)))}
         :waiting "your opponent to resolve approach-slot reactions"}))))

;; CARDS MOVING ZONES / COSTS
(defn cards-exiled-reaction
  [state side eid args]
  (push-reaction! state :cards-exiled args)
  (resolve-reaction-effects-with-priority
    state nil eid :cards-exiled resolve-reaction-for-side
    {:prompt {:corp   "Cards were exiled"
              :runner "Cards were exiled"}
     :waiting "your opponent to resolve cards-exiled reactions"}))

(defn cards-archived-reaction
  [state side eid args]
  (push-reaction! state :cards-archived args)
  (resolve-reaction-effects-with-priority
    state nil eid :cards-archived resolve-reaction-for-side
    {:prompt {:corp   "Cards were archived"
              :runner "Cards were archived"}
     :waiting "your opponent to resolve cards-archived reactions"}))

;; REFRESH PHASE / TURN

(defn refresh-actions-reaction
  [state side eid]
  (push-reaction! state :refresh-actions {})
  (resolve-reaction-effects-with-priority
    state nil eid :refresh-actions resolve-reaction-for-side
    {:prompt {:corp   (fn [_] (str "Refresh phase - refill actions"))
              :runner (fn [_] (str "Refresh phase - refill actions"))}
     :waiting "your opponent to resolve refresh-actions reactions"}))

(defn round-begins-reaction
  [state side eid]
  (push-reaction! state :round-begins {})
  (resolve-reaction-effects-with-priority
    state nil eid :round-begins resolve-reaction-for-side
    {:prompt {:corp   (fn [_] (str "The round is beginning"))
              :runner (fn [_] (str "The round is beginning"))}
     :waiting "your opponent to resolve round-begins reactions"}))
