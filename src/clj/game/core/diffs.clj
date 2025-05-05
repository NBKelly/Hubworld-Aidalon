(ns game.core.diffs
  (:require
   [clojure.string :as string]
   [cond-plus.core :refer [cond+]]
   [differ.core :as differ]
   [game.core.board :refer [installable-servers]]
   [game.core.card :refer :all]
   [game.core.card-defs :refer [card-def]]
   [game.core.cost-fns :refer [card-ability-cost]]
   [game.core.delving :refer [card-for-current-slot]]
   [game.core.eid :refer [make-eid]]
   [game.core.engine :refer [can-trigger?]]
   [game.core.effects :refer [any-effects is-disabled-reg?]]
   [game.core.installing :refer [corp-can-pay-and-install?
                                 runner-can-pay-and-install?]]
   [game.core.payment :refer [can-pay? ->c]]
   [game.core.play-instants :refer [can-play-instant?]]
   [game.core.rezzing :refer [get-rez-cost]]
   [game.core.winning :refer [agenda-points-required-to-win]]
   [game.utils :refer [dissoc-in]]
   [jinteki.utils :refer [other-side select-non-nil-keys]]
   [medley.core :refer [update-existing]]))

(defn hubworld-playable? [card state side]
  (let [bac (get-in @state [side :basic-action-card])]
    (if (and ((if (= :corp side) corp? runner?) card)
             (in-hand? card)
             (= side (:active-player @state))
             (not (:delve @state))
             (cond
               (and (or (obstacle? card)
                        (agent? card)
                        (source? card))
                    ;; TODO delving, breaching, or discovering
                    (can-pay? state side {:source bac :source-type :stage} card nil (get-in bac [:abilities 3 :cost])))
               true
               (and (moment? card)
                    (when-let [on-play (:on-play (card-def card))]
                      (can-play-instant? state side {:source bac :source-type :play} card)))
               true))
      (assoc card :playable true)
      card)))

(defn forgeable [card state side]
  (if (and (installed? card)
           (not (rezzed? card))
           (can-pay? state side (make-eid state) card nil (or (get-rez-cost state side card nil) 0)))
    (assoc card :forgeable true)
    card))

(defn rushable [card state side]
  (if (and (in-hand? card)
           (:rush (card-def card))
           (can-pay? state side (make-eid state) card nil (or (get-rez-cost state side card nil) 0)))
    (assoc card :rushable true)
    card))

(defn flashable [card state side]
  (if (and (in-hand? card)
           (moment? card)
           (:flash (card-def card))
           (can-play-instant? state side {:source card :source-type :play} card {:flash true}))
    (assoc card :flashable true)
    card))

(defn ability-playable? [ability ability-idx state side card]
  (let [cost (card-ability-cost state side ability card)
        eid {:source card
             :source-type :ability
             :source-info {:ability-idx ability-idx}}]
    (if (and (or (active? card)
                 (:autoresolve ability))
             ;; using the reg instead of any-effect here because this function gets called
             ;; a LOT, and this is very close to linear time and should (I think)
             ;; always be correct when this fn is called.
             ;; If that's ever an issue, we can switch this to:
             ;; (is-disabled? state side card)
             ;; --n kelly, apr 2024
             (not (is-disabled-reg? state card))
             ;; actions cannot be used during runs
             (not (and (:action ability)
                       (or (:run @state)
                           (not= side (:active-player @state)))))
             (can-pay? state side eid card nil cost)
             (can-trigger? state side eid ability card nil))
      (assoc ability :playable true)
      ability)))

(def ability-keys
  [:cost-label
   :dynamic
   :index
   :keep-menu-open
   :label
   :msg
   :playable
   :source])

(defn ability-summary [state side card ab-idx ability]
  (-> ability
      (ability-playable? ab-idx state side card)
      (select-non-nil-keys ability-keys)))

(defn abilities-summary [abilities card state side]
  (some->> (seq abilities)
           (map-indexed (fn [ab-idx ab] (ability-summary state side card ab-idx ab)))
           (into [])))

(def subroutine-keys
  [:broken
   :fired
   :label
   :msg
   :resolve])

(defn subroutines-summary [subroutines]
  (when (seq subroutines)
    (mapv #(select-non-nil-keys % subroutine-keys) subroutines)))

(defn card-abilities-summary [card state side]
  (cond-> card
    (:abilities card) (update :abilities abilities-summary card state side)
    (:corp-abilities card) (update :corp-abilities abilities-summary card state side)
    (:runner-abilities card) (update :runner-abilities abilities-summary card state side)
    (:subroutines card) (update :subroutines subroutines-summary)))

(def card-keys
  [:abilities
   :advance-counter
   :advanceable
   :advancementcost
   :agendapoints
   :current-barrier
   :barrier
   :presence
   :current-presence
   :card-target
   :cid
   :code
   :corp-abilities
   :cost
   :counter
   :current-advancement-requirement
   :current-points
   :current-strength
   :disabled
   :extra-advance-counter
   :exhausted
   :face
   :faces
   :facedown
   :forgeable
   :flashable
   :host
   :hosted
   :icon
   :images
   :implementation
   :installed
   :new
   :normalizedtitle
   :playable
   :printed-title
   :rezzed
   :runner-abilities
   :rushable
   :seen
   :selected
   :side
   :strength
   :subroutines
   :subtype-target
   :poison
   :highlight-in-discard
   :subtypes
   :title
   :type
   :zone])

(def private-card-keys
  [:advance-counter
   :cid
   :counter
   :exhausted
   :extra-advance-counter
   :host
   :hosted
   :icon
   :new
   :side
   :zone])

(defn private-card
  "Returns only the public information of a given card when it's in a private state,
  for example, when it's facedown or in the hand"
  [card]
  (select-non-nil-keys card private-card-keys))

(declare cards-summary)

(defn card-summary [card state side]
  (if (is-public? card side)
    (-> (cond-> card
          (:host card) (-> (dissoc-in [:host :hosted])
                           (update :host card-summary state side))
          (:hosted card) (update :hosted cards-summary state side))
        (hubworld-playable? state side)
        (card-abilities-summary state side)
        (forgeable state side)
        (rushable state side)
        (flashable state side)
        (select-non-nil-keys card-keys))
    (-> (cond-> card
          (:host card) (-> (dissoc-in [:host :hosted])
                           (update :host card-summary state side))
          (:hosted card) (update :hosted cards-summary state side))
        (private-card))))

(defn cards-summary [cards state side]
  (when (seq cards)
    (mapv #(card-summary % state side) cards)))

(defn- path-summary [path state side]
  {:inner  (mapv #(card-summary % state side) (:inner path))
   :middle (mapv #(card-summary % state side) (:middle path))
   :outer  (mapv #(card-summary % state side) (:outer path))})

(defn paths-summary [player state side same-side?]
  (let [{:keys [commons council archives]} (:paths player)]
    {:commons  (path-summary commons state side)
     :council  (path-summary council state side)
     :archives (path-summary archives state side)}))

(def prompt-keys
  [:msg
   :choices
   :card
   :prompt-type
   :show-discard
   :show-exile
   :selectable
   ;; hubworld stuff
   :target-paths
   :other-side?
   ;; bluffs
   :start-time
   :end-time
   ;; traces
   :player
   :base
   :bonus
   :strength
   :unbeatable
   :beat-trace
   :link
   :corp-credits
   :runner-credits])

(defn prompt-summary
  [prompt same-side?]
  (when same-side?
    (-> prompt
        (update :card #(not-empty (select-non-nil-keys % card-keys)))
        (update :choices (fn [choices]
                           (if (sequential? choices)
                             (->> choices
                                  (mapv
                                   (fn [choice]
                                     (if (-> choice :value :cid)
                                       (update choice :value select-non-nil-keys card-keys)
                                       choice)))
                                  (not-empty))
                             choices)))
        (select-non-nil-keys prompt-keys)
        (not-empty))))

(defn toast-summary
  [toast same-side?]
  (when same-side?
    toast))

(def player-keys
  [:aid
   :user
   :identity
   :basic-action-card
   :deck
   :deck-id
   :hand
   :discard
   :scored
   :rfg
   :paths
   :heat
   :play-area
   :set-aside
   :click
   :credit
   :toast
   :hand-size
   :keep
   :quote
   :trash-like-cards
   :prompt-state
   :agenda-point
   :agenda-point-req])

(defn player-summary
  [player state side same-side?]
  (-> player
      (update :identity card-summary state side)
      (update :basic-action-card card-summary state side)
      (update :play-area cards-summary state side)
      (update :rfg cards-summary state side)
      (update :scored cards-summary state side)
      (update :set-aside cards-summary state side)
      (update :prompt-state prompt-summary same-side?)
      (update :toast toast-summary same-side?)
      (select-non-nil-keys player-keys)))

(defn prune-cards [cards]
  (mapv #(select-non-nil-keys % card-keys) cards))

(defn deck-summary
  "Is the player's deck publicly visible?"
  [deck same-side? player]
  (if (and same-side? (:view-deck player))
    (prune-cards deck)
    []))

(defn hand-summary
  "Is the player's hand publicly visible?"
  [hand state same-side? side player]
  (if (or same-side? (:openhand player))
    (cards-summary hand state side)
    []))

(defn discard-summary
  [discard state same-side? side player]
  (let [player-side (if same-side? side (other-side side))]
    (if (or same-side? (:openhand player))
      (cards-summary discard state player-side)
      (cards-summary discard state side))))

(defn hubworld-player-summary
  [player state side same-side?]
  (let [player-side (if same-side? side (other-side side))
        oppo-side (other-side player-side)]
    (-> (player-summary player state side same-side?)
        (update :deck deck-summary same-side? player)
        (update :hand hand-summary state same-side? player-side player)
        (update :discard discard-summary state same-side? side player)
        (assoc
          :deck-count (count (:deck player))
          :hand-count (count (:hand player))
          :paths (paths-summary player state side same-side?)))))

(def options-keys
  [:alt-arts
   :background
   :card-resolution
   :language
   :pronouns
   :show-alt-art])

(defn options-summary [options]
  (when (seq options)
    (select-non-nil-keys options options-keys)))

(def user-keys
  [:_id
   :username
   :emailhash
   :options
   :special])

(defn user-summary [user]
  (-> user
      (update-existing :options options-summary)
      (select-non-nil-keys user-keys)))

(def delve-keys
  [:approached-card
   :auto-pass-priority
   :delve-id
   :server
   :delver
   :defender
   :position
   :phase
   :no-action
   :cannot-end-delve
   :source-card])

(def run-keys
  [:server
   :position
   :corp-auto-no-action
   :cannot-jack-out
   :phase
   :next-phase
   :no-action
   :source-card
   :approached-ice-in-position?])

(defn delve-summary
  [state]
  (when-let [delve (:delve @state)]
    (-> delve
        ;; todo - if we ever need this, then we have to side correct it
        (assoc :cannot-end-delve (any-effects state (:delver delve) :cannot-end-delve true?))
        (assoc :approached-card (forgeable (card-for-current-slot state) state (:defender delve)))
        (select-non-nil-keys delve-keys))))

(defn run-summary
  [state]
  (when-let [run (:run @state)]
    (-> run
        (assoc :approached-ice-in-position? (when (= :approach-ice (:phase run))
                                              (some? (get-card state (:current-ice run)))))
        (assoc :cannot-jack-out (any-effects state :corp :cannot-jack-out true?))
        (select-non-nil-keys run-keys))))

(defn encounter-ice-summary
  [ice state]
  (when-let [ice (get-card state ice)]
    (card-summary ice state :corp)))

(def encounter-keys
  [:encounter-count
   :ice
   :no-action])

(defn encounters-summary
  [state]
  (let [encounters (:encounters @state)
        current-encounter (peek encounters)
        encounter-count (count encounters)]
    (when current-encounter
      (-> current-encounter
          (update :ice encounter-ice-summary state)
          (assoc :encounter-count encounter-count)
          (select-non-nil-keys encounter-keys)))))

(def state-keys
  [:active-player
   ;; :angel-arena-info
   :corp
   :corp-phase-12
   :decklists
   :delve
   :encounters
   :end-turn
   :gameid
   :last-revealed
   :log
   :mark
   :options
   :psi
   :reason
   :room
   :run
   :runner
   :runner-phase-12
   :sfx
   :sfx-current-id
   :start-date
   :stats
   :trace
   :turn
   :typing
   :winning-user
   :winner])

(defn strip-state
  [state]
  (-> @state
      (update-in [:corp :user] user-summary)
      (update-in [:runner :user] user-summary)
      (assoc :stats (when (:winner @state) (:stats @state)))
      (assoc :delve (delve-summary state))
      (assoc :encounters (encounters-summary state))
      (select-non-nil-keys state-keys)))

(defn state-summary
  [stripped-state state side]
  (-> stripped-state
      (update :corp hubworld-player-summary state side (= side :corp)) ;;         [player state side same-side?]   ;;corp-summary state side)
      (update :runner hubworld-player-summary state side (= side :runner))))

(defn strip-for-replay
  [stripped-state corp-player runner-player]
  (assoc stripped-state
         :corp (:corp corp-player)
         :runner (:runner runner-player)))

(defn strip-for-spectators
  [stripped-state corp-state runner-state]
  (let [spectator-hands? (-> stripped-state :options :spectatorhands)]
    (-> stripped-state
        (assoc :corp (if spectator-hands? (:corp corp-state) (:corp runner-state)))
        (assoc :runner (if spectator-hands? (:runner runner-state) (:runner corp-state))))))

(defn strip-for-corp-spect
  [stripped-state corp-state runner-state]
  (assoc stripped-state :corp (:corp corp-state) :runner (:runner corp-state)))

(defn strip-for-runner-spect
  [stripped-state corp-state runner-state]
  (assoc stripped-state :corp (:corp runner-state) :runner (:runner runner-state)))

(defn public-states
  "Generates privatized states for the Corp, Runner, any spectators, and the history from the base state.
  If `:spectatorhands` is on, all information is passed on to spectators as well.
  note that when joining or starting a game, all states are always generated.
  Otherwise when computing diffs, only the relevant states are needed, and we can skip computing the other ones."
  ([state] (public-states state true))
  ([state spectators?]
   (let [stripped-state (strip-state state)
         corp-state (state-summary stripped-state state :corp)
         runner-state (state-summary stripped-state state :runner)
         replay-state (strip-for-replay stripped-state corp-state runner-state)]
     ;; corp, runner, spectator, history
     {:corp-state corp-state
      :runner-state runner-state
      :spect-state (when spectators? (strip-for-spectators replay-state corp-state runner-state))
      :hist-state replay-state})))

(defn public-diffs [old-state new-state spectators?]
  (let [{old-corp :corp-state old-runner :runner-state
         old-spect :spect-state old-hist :hist-state}
        (when old-state (public-states (atom old-state) spectators?))
        {new-corp :corp-state new-runner :runner-state
         new-spect :spect-state new-hist :hist-state}
        (public-states new-state spectators?)]
    {:runner-diff (differ/diff old-runner new-runner)
     :corp-diff (differ/diff old-corp new-corp)
     :spect-diff (when spectators? (differ/diff old-spect new-spect))
     :hist-diff (differ/diff old-hist new-hist)}))

(defn message-diffs [old-state new-state]
  (let [old-messages (select-keys old-state [:log])
        new-messages (select-keys @new-state [:log])
        message-diff (differ/diff old-messages new-messages)]
    {:runner-diff message-diff
     :corp-diff message-diff
     :spect-diff message-diff
     :hist-diff message-diff}))
