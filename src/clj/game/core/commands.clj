(ns game.core.commands
  (:require
   [cljc.java-time.instant :as inst]
   [clojure.string :as string]
   [game.core.actions :refer [score]]
   [game.core.board :refer [all-installed server->zone]]
   [game.core.breaching :refer [discover-card]]
   [game.core.card :refer [agenda? can-be-advanced? corp? get-card seeker?
                           has-subtype? ice? in-hand? installed? rezzed? runner? side-fn stageable?]]
   [game.core.change-vals :refer [change]]
   [game.core.drawing :refer [draw]]
   [game.core.delving :refer [confront-card]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [resolve-ability trigger-event]]
   [game.core.flags :refer [is-scored?]]
   [game.core.identities :refer [disable-identity disable-card enable-card]]
   [game.core.initializing :refer [card-init deactivate make-card]]
   [game.core.moving :refer [move swap-installed trash exile]]
   [game.core.prompt-state :refer [remove-from-prompt-queue]]
   [game.core.prompts :refer [show-prompt show-stage-prompt show-bluff-prompt]]
   [game.core.props :refer [set-prop]]
   [game.core.say :refer [system-msg system-say unsafe-say]]
   [game.core.set-up :refer [build-card]]
   [game.core.staging :refer [stage-a-card]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [show-error-toast toast]]
   [game.core.update :refer [update!]]
   [game.core.winning :refer [clear-win]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer [dissoc-in enumerate-str quantify safe-split
                       same-card? same-side? server-card string->num]]
   [jinteki.utils :refer [other-side str->int]]))

(defmulti lobby-command :command)

(defn- constrain-value
  "Constrain value to [min-value max-value]"
  [value min-value max-value]
  (min max-value (max min-value value)))

(defn command-save-replay [state _]
  (swap! state assoc-in [:options :save-replay] true))

(defn command-bug-report [state side]
  (swap! state update :bug-reported (fnil inc -1))
  (let [title "[EDITME] Please give a short description of your bug here"
        body (str "Link to bug replay: https://hubteki.com/bug-report/" (:gameid @state)
                  "?b=" (:bug-reported @state) "\n\n"
                  "Description:\n\n"
                  "[EDITME] Please describe the steps to reproduce your bug and the resulting effect here.")]
    (unsafe-say state [:div.bugreport [:div.smallwarning "!"]
                       "Thanks for helping us make the game better! The replay was saved. "
                       "Please report a bug following "
                       [:a {:target "_blank"
                            :href (str "https://github.com/nbkelly/hubworld-aidalon/issues/new?title="
                                       (string/replace title #" " "%20")
                                       "&body="
                                       (string/replace (string/replace body #" " "%20") #"\n" "%0A"))}
                        "this link"]
                       " to GitHub."])))

(defn command-enable-api-access [state _]
  (swap! state assoc-in [:options :api-access] true))

(defn command-roll [state side value]
  (let [value (constrain-value value 1 1000)]
    (system-msg state side (str "rolls a " value " sided die and rolls a " (inc (rand-int value))))))

(defn command-undo-click
  "Resets the game state back to start of the click"
  [state side]
  (when-let [last-click-state (peek (:click-states @state))]
    (let [current-log (:log @state)
          current-history (:history @state)
          previous-click-states (pop (:click-states @state))
          turn-state (:turn-state @state)
          last-click-state (assoc last-click-state
                                  :log current-log
                                  :click-states previous-click-states
                                  :turn-state turn-state
                                  :history current-history
                                  :run nil)]
      (reset! state last-click-state))
    (system-say state side (str "[!] " (if (= side :corp) "Corp" "Runner") " uses the undo-click command"))
    (doseq [s [:runner :corp]]
      (toast state s "Game reset to start of click"))))

(defn clear-delve-state
  [state side]
  (when-let [delve (:delve @state)]
    (do (effect-completed state side (:delve-id delve))
        (effect-completed state side (:eid delve))
        (swap! state dissoc :delve))))

(defn command-undo-turn
  "Resets the entire game state to how it was at end-of-turn if both players agree"
  [state side]
  (when-let [turn-state (:turn-state @state)]
    (swap! state assoc-in [side :undo-turn] true)
    (when (and (-> @state :runner :undo-turn) (-> @state :corp :undo-turn))
      (let [current-log (:log @state)
            current-history (:history @state)
            original-turn-state (assoc turn-state
                                  :log current-log
                                  :history current-history
                                  :turn-state turn-state)]
        (reset! state original-turn-state))
      (doseq [s [:runner :corp]]
        (swap! state dissoc-in [s :turn-started])
        (toast state s "Game reset to start of turn")))))

(defn command-unique
  "Toggles :uniqueness of the selected card"
  [state side]
  (resolve-ability state side
                   {:effect (effect (set-prop target :uniqueness (not (:uniqueness target))))
                    :msg (msg "make " (card-str state target)
                              (when (:uniqueness target) " not") ;it was unique before
                              " unique")
                    :choices {:card (fn [t] (same-side? (:side t) side))}}
                   (make-card {:title "/unique command" :side side}) nil))

(defn command-close-prompt [state side]
  (when-let [prompt (-> @state side :prompt first)]
    (remove-from-prompt-queue state side prompt)
    (swap! state dissoc-in [side :selected])
    (effect-completed state side (:eid prompt))))

(defn command-peek
  [state side n]
  (show-prompt
    state side
    nil
    (str "The top " (quantify n "card")
         " of your deck " (if (< 1 n) "are" "is") " (top->bottom): "
         (->> (get-in @state [side :deck])
              (take n)
              (map :title)
              (enumerate-str)))
    ["Done"]
    identity))

(defn command-summon
  [state side args]
  (let [card-name (string/join " " args)]
    (try (let [s-card (server-card card-name)
               card (when s-card (build-card s-card (string/capitalize (name side))))]
           (when card (swap! state update-in [side :hand] #(concat % [(assoc card :zone [:hand])]))))
         (catch Exception ex
           (toast state side (str card-name " isn't a real card"))))))

(defn command-reload-id
  [state side]
  (let [card-name (:title (get-in @state [side :identity]))
        card-side (:side (get-in @state [side :identity]))]
    (try
      (let [s-card (server-card card-name)
            card (when s-card (build-card s-card (string/capitalize (name side))))]
        (if card
          (let [new-id (-> card :title server-card make-card (assoc :zone [:identity] :type "Seeker" :side card-side))]
            (disable-identity state side)
            (swap! state assoc-in [side :identity] new-id)
            (card-init state side new-id {:resolve-effect true :init-data true}))
          (toast state side (str card-name " isn't a valid card"))))
      (catch Exception ex
        (toast state side (str card-name " isn't a real card"))))))

(defn command-replace-id
  [state side args]
  (let [card-name (string/join " " args)]
    (try
      (let [s-card (server-card card-name)
            card (when (and s-card (same-side? (:side s-card) side))
                   (build-card s-card))]
        (if card
          (let [new-id (-> card :title server-card make-card (assoc :zone [:identity] :type "Identity"))]
            (disable-identity state side)
            (swap! state assoc-in [side :identity] new-id)
            (card-init state side new-id {:resolve-effect true :init-data true}))
          (toast state side (str card-name " isn't a valid card"))))
      (catch Exception ex
        (toast state side (str card-name " isn't a real card"))))))

(defn command-trash
  [state side]
  (let [f (side-fn side)]
    (resolve-ability
      state side
      {:prompt "Choose a card to trash"
       :choices {:card #(f %)}
       :async true
       :effect (effect (trash eid target {:unpreventable true}))}
      nil nil)))

(defn command-discover
  [state side]
  (resolve-ability
    state side
    {:prompt "Choose a card to discover"
     :choices {:card (complement seeker?)}
     :waiting-prompt true
     :async true
     :effect (req (discover-card state side eid target))}
    (make-card {:title "/discover command"}) nil))

(defn command-confront
  [state side]
  (resolve-ability
    state side
    {:prompt "Choose a card to confront"
     :choices {:card (every-pred installed? rezzed? (complement seeker?))}
     :waiting-prompt true
     :async true
     :effect (req (confront-card state side eid target))}
    (make-card {:title "/confront command"}) nil))

(defn command-stage
  [state side]
  (let [f (side-fn side)]
    (resolve-ability
      state side
      {:prompt "Choose a card to stage"
       :choices {:card (every-pred f in-hand? stageable?)}
       :async true
       :effect (req (stage-a-card state side eid (make-card {:title "/stage command"}) target))}
      nil nil)))

(defn command-swap-sides
  [state side]
  (swap! state dissoc-in [side :command-info :ignore-swap-sides])
  (if (get-in @state [(other-side side) :command-info :ignore-swap-sides])
    (toast state side "your opponent has indicated that they do not wish to swap sides")
    (resolve-ability
      state (other-side side)
      {:prompt "Your opponent wishes to swap sides"
       :waiting-prompt true
       :choices ["Accept" "Decline" "Don't ask me again"]
       :effect (req (cond
                      (= target "Decline")
                      (toast state (other-side side) "your opponent does not wish to swap sides at this time")
                      (= target "Don't ask me again")
                      (do (toast state (other-side side) "your opponent does not wish to swap sides")
                          (swap! state assoc-in [side :command-info :ignore-swap-sides] true))
                      (= target "Accept")
                      (do (system-msg state side "accepts the request to swap sides. Players swap sides")
                          (lobby-command {:command :swap-sides
                                          :gameid (:gameid @state)}))))}
      nil nil)))

(defn parse-command
  [state text]
  (let [[command & args] (safe-split text #" ")
        value (if-let [n (string->num (first args))] n 1)
        num   (if-let [n (-> args first (safe-split #"#") second string->num)] (dec n) 0)
        res
        (if (= (ffirst args) \#)
          (case command
            "/deck"       #(move %1 %2 (nth (get-in @%1 [%2 :hand]) num nil) :deck {:front true})
            "/discard"    #(move %1 %2 (nth (get-in @%1 [%2 :hand]) num nil) :discard)
            nil)
          (case command
            "/bug"        command-bug-report
            "/bluff"      #(show-bluff-prompt %1 %2 nil)
            "/card-info"  #(resolve-ability %1 %2
                                            {:effect (effect (system-msg (str "shows card-info of "
                                                                              (card-str state target)
                                                                              ": " (get-card state target))))
                                             :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                            (make-card {:title "/card-info command"}) nil)
            "/clear-delve-state"  clear-delve-state
            "/clear-win"  clear-win
            "/click"      #(swap! %1 assoc-in [%2 :click] (constrain-value value 0 1000))
            "/close-prompt" command-close-prompt
            "/credit"     #(swap! %1 assoc-in [%2 :credit] (constrain-value value 0 1000))
            "/deck"       #(toast %1 %2 "/deck number takes the format #n")
            "/disable-card" #(resolve-ability %1 %2
                                              {:prompt "Choose a card to disable"
                                               :effect (req (disable-card state side target))
                                               :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                              (make-card {:title "/disable-card command"}) nil)
            "/discard"    #(toast %1 %2 "/discard number takes the format #n")
            "/discard-random" #(move %1 %2 (rand-nth (get-in @%1 [%2 :hand])) :discard)
            "/draw"       #(draw %1 %2 (make-eid %1) (constrain-value value 0 1000))
            "/enable-card" #(resolve-ability %1 %2
                                             {:prompt "Choose a card to enable"
                                              :effect (req (enable-card state side target))
                                              :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                             (make-card {:title "/enable-card command"}) nil)
            "/enable-api-access" command-enable-api-access
            "/error"      show-error-toast
            "/exile"      #(resolve-ability %1 %2
                                          {:prompt "Choose a card"
                                           :async true
                                           :effect (req (exile %1 %2 (make-eid %1) target))
                                           :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                          (make-card {:title "/exile command"}) nil)
            "/handsize"   #(change %1 %2 {:key :hand-size
                                          :delta (- (constrain-value value -1000 1000)
                                                    (get-in @%1 [%2 :hand-size :total]))})
            "/move-bottom"  #(resolve-ability %1 %2
                                              {:prompt "Choose a card in hand to put on the bottom of your deck"
                                               :effect (effect (move target :deck))
                                               :choices {:card (fn [t] (and (same-side? (:side t) %2)
                                                                            (in-hand? t)))}}
                                              (make-card {:title "/move-bottom command"}) nil)
            "/move-deck"   #(resolve-ability %1 %2
                                             {:prompt "Choose a card to move to the top of your deck"
                                              :effect (req (let [c (deactivate %1 %2 target)]
                                                             (move %1 %2 c :deck {:front true})))
                                              :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                             (make-card {:title "/move-deck command"}) nil)
            "/move-hand"  #(resolve-ability %1 %2
                                            {:prompt "Choose a card to move to your hand"
                                             :effect (req (let [c (deactivate %1 %2 target)]
                                                            (move %1 %2 c :hand)))
                                             :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                            (make-card {:title "/move-hand command"}) nil)
            "/peek"       #(command-peek %1 %2 value)
            "/reload-id"  command-reload-id
            "/replace-id" #(command-replace-id %1 %2 args)
            "/rfg"        #(resolve-ability %1 %2
                                            {:prompt "Choose a card"
                                             :effect (req (let [c (deactivate %1 %2 target)]
                                                            (move %1 %2 c :rfg)))
                                             :choices {:card (fn [t] (same-side? (:side t) %2))}}
                                            (make-card {:title "/rfg command"}) nil)
            "/roll"       #(command-roll %1 %2 value)
            "/save-replay" command-save-replay
            "/show-hand" #(resolve-ability %1 %2
                                           {:effect (effect (system-msg (str
                                                                          (if (= :corp %2)
                                                                            "shows cards from HQ: "
                                                                            "shows cards from the grip: ")
                                                                          (enumerate-str (sort (map :title (:hand (if (= side :corp) corp runner))))))))}
                                           nil nil)
            "/stage"      command-stage
            "/summon"     #(command-summon %1 %2 args)
            "/swap-sides" #(command-swap-sides %1 %2)
            "/trash"         command-trash
            "/discover"      command-discover
            "/confront"      command-confront
            "/undo-click" #(command-undo-click %1 %2)
            "/undo-turn"  #(command-undo-turn %1 %2)
            "/unique"     #(command-unique %1 %2)
            nil))]
    (when res
      (swap! state update-in [:command-log] (fnil #(concat % [{:command command
                                                               :timestamp (inst/now)}])
                                                  [])))
    res))
