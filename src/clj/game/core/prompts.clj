(ns game.core.prompts
  (:require
   [clj-uuid :as uuid]
   [cljc.java-time.instant :as inst]
   [game.core.board :refer [get-all-cards]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.prompt-state :refer [add-to-prompt-queue remove-from-prompt-queue]]
   [game.core.toasts :refer [toast]]
   [game.macros :refer [when-let*]]
   [game.utils :refer [pluralize side-str]]
   [jinteki.utils :refer [other-side]]
   [medley.core :refer [find-first]]))

(defn choice-parser
  [choices]
  (if (or (map? choices) (keyword? choices))
    choices
    (into
      []
      (for [[idx choice] (map-indexed vector (keep identity choices))]
        {:value choice
         :uuid (uuid/v4)
         :idx idx}))))

(defn update-selectable
  [prev-selectable choices]
  (if (or (not choices) (string? choices) (keyword? choices) (not (sequential? choices)))
    prev-selectable
    (concat (or prev-selectable []) (filterv identity (map #(->> % :value :cid) choices)))))

(defn show-prompt
  "Engine-private method for displaying a prompt where a *function*, not a card ability, is invoked
  when the prompt is resolved. All prompts flow through this method."
  ([state side card message choices f] (show-prompt state side (make-eid state) card message choices f nil))
  ([state side card message choices f args] (show-prompt state side (make-eid state) card message choices f args))
  ([state side eid card message choices f
    {:keys [waiting-prompt prompt-type show-discard show-exile cancel-effect end-effect targets selectable]}]
   (let [prompt (if (string? message) message (message state side eid card targets))
         choices (choice-parser choices)
         selectable (update-selectable selectable choices)
         newitem ^:ignore-async-check
                 {:eid eid
                  :msg prompt
                  :choices choices
                  :effect f
                  :card card
                  :selectable selectable
                  :prompt-type (or prompt-type :other)
                  :show-discard show-discard
                  :show-exile show-exile
                  :cancel-effect cancel-effect
                  :end-effect end-effect}]
     (when (or (#{:waiting :run} prompt-type)
               (:number choices)
               (:card-title choices)
               (#{:credit :counter} choices)
               (pos? (count choices)))
       (when waiting-prompt
         (add-to-prompt-queue
           state (if (= :corp side) :runner :corp)
           {:eid (select-keys eid [:eid])
            :card card
            :prompt-type :waiting
            :msg (str "Waiting for "
                      (if (true? waiting-prompt)
                        "your opponent to make a decision"
                        waiting-prompt))}))
       (add-to-prompt-queue state side newitem)))))

(defn show-prompt-with-dice
  "Calls show-prompt normally, but appends a 'roll d6' button to choices.
  If user chooses to roll d6, reveal the result to user and re-display
  the prompt without the 'roll d6 button'."
  ([state side card message other-choices f]
   (show-prompt-with-dice state side card message other-choices f nil))
  ([state side card message other-choices f args]
   (let [dice-msg "Roll a d6",
         choices (conj other-choices dice-msg)]
     (show-prompt state side card message choices
                  #(if (not= (:value %) dice-msg)
                     (f %)
                     (show-prompt state side card
                                  (str message " (Dice result: " (inc (rand-int 6)) ")")
                                  other-choices f args))
                  args))))

(defn- clamp
  [a b x]
  (cond
    (< x a) a
    (> x b) b
    :else x))

(defn show-bluff-prompt
  "Specific function for showing a bluff prompt"
  ([state side args] (show-bluff-prompt state side (make-eid state) args))
  ([state side eid args]
   (let [low-end  (clamp 2 4 (get args :min-time 4))
         high-end (clamp 5 10 (get args :max-time 5))
         ;; this gives a random number of ms between high-low (ie 6500)
         timer (+ (* 1000 low-end) (rand-int (* (- high-end low-end) 1000)))
         now (inst/now)
         terminates (inst/plus-millis now timer)
         newitem {:eid eid
                  :msg "You have no triggers - press OK to continue"
                  :prompt-type :bluff
                  :start-time (inst/to-epoch-milli now)
                  :end-time (inst/to-epoch-milli terminates)}]
     (add-to-prompt-queue
       state (if (= :corp side) :runner :corp)
       {:eid (select-keys eid [:eid])
        :prompt-type :waiting
        :msg (str "Waiting for "
                  (or (:msg args)
                      (str (side-str side) " to make a decision")))})
     (add-to-prompt-queue state side newitem))))

(defn cancel-bluff
  [state side resolve-ability]
  (let [prompt (first (filter #(= :bluff (:prompt-type %)) (get-in @state [side :prompt])))]
    (when prompt
      (remove-from-prompt-queue state side prompt)
      (resolve-ability state side (:eid prompt) nil nil nil)
      ;; this is 1000% a hack...
      (cancel-bluff state side resolve-ability))))

(defn show-stage-prompt
  "Specific function for showing a staging prompt"
  ([state side card message ab args] (show-stage-prompt state side (make-eid state) card message ab args))
  ([state side eid card message ab {:keys [waiting-prompt targets req] :as args}]
   (let [prompt (if (string? message) message (message state side eid card targets))
         newitem {:eid eid
                  :msg prompt
                  :req req
                  :ability ab
                  :card card
                  :prompt-type :stage}]
     (when waiting-prompt
         (add-to-prompt-queue
           state (if (= :corp side) :runner :corp)
           {:eid (select-keys eid [:eid])
            :card card
            :prompt-type :waiting
            :msg (str "Waiting for "
                      (if (true? waiting-prompt)
                        (str (side-str side) " to make a decision")
                        waiting-prompt))}))
     (add-to-prompt-queue state side newitem))))

(defn show-shift-prompt
  "Specific function for showing a shift prompt"
  ([state side card zones message ab args] (show-shift-prompt state side (make-eid state) card zones message ab args))
  ([state side eid card zones message ab {:keys [waiting-prompt targets req other-side?] :as args}]
   (let [prompt (if (string? message) message (message state side eid card targets))
         newitem {:eid eid
                  :msg prompt
                  :req req
                  :ability ab
                  :target-paths zones
                  :other-side? other-side?
                  :card card
                  :prompt-type :shift}]
     (when waiting-prompt
         (add-to-prompt-queue
           state (if (= :corp side) :runner :corp)
           {:eid (select-keys eid [:eid])
            :card card
            :prompt-type :waiting
            :msg (str "Waiting for "
                      (if (true? waiting-prompt)
                        (str (side-str side) " to make a decision")
                        waiting-prompt))}))
     (add-to-prompt-queue state side newitem))))

(defn cancel-stage
  [state side card update! resolve-ability]
  (let [prompt (first (filter #(= :stage (:prompt-type %)) (get-in @state [side :prompt])))]
    (when prompt
      (remove-from-prompt-queue state side prompt)
      (resolve-ability state side (:eid prompt) (:cancel-ability prompt) card nil))))

(defn cancel-shift
  [state side card update! resolve-ability]
  (let [prompt (first (filter #(= :shift (:prompt-type %)) (get-in @state [side :prompt])))]
    (when prompt
      (remove-from-prompt-queue state side prompt)
      (resolve-ability state side (:eid prompt) (:cancel-ability prompt) card nil))))

(defn resolve-stage
  [state side card {:keys [server slot cancel] :as context} update! resolve-ability]
  (let [prompt (first (filter #(= :stage (:prompt-type %)) (get-in @state [side :prompt])))]
    (when prompt
      ;; todo - case for cancel/done
      (when (or (not (:req prompt))
                ((:req prompt) state side (make-eid state) card [context]))
        (remove-from-prompt-queue state side prompt)
        (resolve-ability state side (:eid prompt) (:ability prompt) card [context])))))

(defn resolve-shift
  [state side card {:keys [server slot cancel] :as context} update! resolve-ability]
  (let [prompt (first (filter #(= :shift (:prompt-type %)) (get-in @state [side :prompt])))]
    (when prompt
      ;; todo - case for cancel/done
      (when (or (not (:req prompt))
                ((:req prompt) state side (make-eid state) card [context]))
        (remove-from-prompt-queue state side prompt)
        (resolve-ability state side (:eid prompt) (:ability prompt) card [context])))))

(defn resolve-select
  "Resolves a selection prompt by invoking the prompt's ability with the targeted cards.
  Called when the user clicks 'Done' or selects the :max number of cards."
  [state side card args update! resolve-ability]
  (let [selected (get-in @state [side :selected 0])
        cards (map #(dissoc % :selected) (:cards selected))
        prompt (first (filter #(= :select (:prompt-type %)) (get-in @state [side :prompt])))]
    (swap! state update-in [side :selected] #(vec (rest %)))
    (when prompt
      (remove-from-prompt-queue state side prompt))
    (if (seq cards)
      (do (doseq [card cards]
            (update! state side card))
          (resolve-ability state side (:ability selected) card cards))
      (if-let [cancel-effect (:cancel-effect args)]
        (cancel-effect nil)
        (effect-completed state side (:eid (:ability selected)))))))

(defn- compute-selectable
  [state side card ability req-fn card-fn]
  (let [valid (filter #(not= (:zone %) [:deck]) (concat (get-all-cards state) [(get-in @state [:corp :identity]) (get-in @state [:runner :identity])]))
        valid (filter #(or (= nil card-fn) (card-fn %)) valid)
        valid (if (nil? req-fn) valid (filter #(req-fn state side (make-eid state) card [%]) valid))]
    (map :cid valid)))

(defn show-select
  "A select prompt uses a targeting cursor so the user can click their desired target of the ability.
  The preferred method for showing a select prompt is through resolve-ability."
  ([state side card ability update! resolve-ability args]
   ;; if :max or :min are a function, call them and assoc its return value as the new :max / :min number of cards
   ;; that can be selected.
   (letfn [(wrap-function [args kw]
             (let [f (kw args)] (if f (assoc args kw #(f state side (:eid ability) card [%])) args)))]
     (let [targets (:targets args)
           ability (update-in ability [:choices :max] #(if (fn? %) (% state side (make-eid state) card targets) %))
           ability (update-in ability [:choices :min] #(if (fn? %) (% state side (make-eid state) card targets) %))
           all (get-in ability [:choices :all])
           ability (if all (update-in ability [:choices] dissoc :min) ability) ; ignore :min if :all is set
           selectable-cards (compute-selectable state side card ability (get-in ability [:choices :req]) (get-in ability [:choices :card]))
           min-choices (get-in ability [:choices :min])
           max-choices (get-in ability [:choices :max])]
       (swap! state update-in [side :selected]
              #(conj (vec %) {:ability (-> ability
                                           (dissoc :choices :waiting-prompt)
                                           (assoc :card card))
                              :cards []
                              :card (get-in ability [:choices :card])
                              :req (get-in ability [:choices :req])
                              :not-self (when (get-in ability [:choices :not-self]) (:cid card))
                              :max max-choices
                              :all all}))
       (show-prompt state side (:eid ability)
                    card
                    (if-let [message (:prompt ability)]
                      message
                      (str
                        "Choose"
                        (when min-choices
                          (str " at least " min-choices))
                        (when (and min-choices max-choices)
                          " and")
                        (when max-choices
                          (str (if all "" " up to")
                               " " max-choices))
                        (if max-choices
                          (str " " (pluralize "target" max-choices))
                          (if min-choices
                            (str " " (pluralize "target" min-choices))
                            " a target"))
                        " for " (:title card)))
                    (if all ["Hide"] ["Done"])
                    (if all
                      (fn [_]
                        ; "Hide" was selected. Show toast and reapply select prompt. This allows players to access
                        ; prompts that lie "beneath" the current select prompt.
                        (toast state side (str "You must choose " max-choices " " (pluralize "card" max-choices)))
                        (show-select state side card ability update! resolve-ability args))
                      (fn [_]
                        (let [selected (get-in @state [side :selected 0] [])
                              cards (map #(dissoc % :selected) (:cards selected))]
                          ; check for :min. If not enough cards are selected, show toast and stay in select prompt
                          (if (and min-choices (< (count cards) min-choices))
                            (do
                              (toast state side (str "You must choose at least " min-choices " " (pluralize "card" min-choices)))
                              (show-select state side card ability update! resolve-ability args))
                            (resolve-select state side card
                                            (select-keys (wrap-function args :cancel-effect) [:cancel-effect])
                                            update! resolve-ability)))))
                    (-> args
                        (assoc :prompt-type :select
                               :selectable selectable-cards
                               :show-exile (:show-exile ability)
                               :show-discard (:show-discard ability))
                        (wrap-function :cancel-effect)))))))

(defn show-wait-prompt
  "Shows a 'Waiting for ...' prompt to the given side with the given message.
  The prompt cannot be closed except by a later call to clear-wait-prompt."
  ([state side message] (show-wait-prompt state side message nil))
  ([state side message {:keys [card]}]
   (show-prompt state side card (str "Waiting for " message) nil nil
                {:prompt-type :waiting})))

(defn clear-wait-prompt
  "Removes the first 'Waiting for...' prompt from the given side's prompt queue."
  [state side]
  (when-let [wait (find-first #(= :waiting (:prompt-type %)) (-> @state side :prompt))]
    (remove-from-prompt-queue state side wait)))

(defn show-delve-prompts
  "Adds a dummy prompt to both side's prompt queues.
   The prompt cannot be closed except by a later call to clear-delve-prompts."
  [state side msg card]
  (show-prompt state side card (str "You are " msg) nil nil {:prompt-type :delve})
  (show-prompt state (other-side side) card (str "Your opponent is " msg) nil nil {:prompt-type :delve}))

(defn clear-delve-prompts
  [state]
  (when-let* [runner-prompt (find-first #(= :delve (:prompt-type %)) (-> @state :runner :prompt))]
    (remove-from-prompt-queue state :runner runner-prompt))
  (when-let* [corp-prompt (find-first #(= :delve (:prompt-type %)) (-> @state :corp :prompt))]
    (remove-from-prompt-queue state :corp corp-prompt)))

(defn show-run-prompts
  "Adds a dummy prompt to both side's prompt queues.
   The prompt cannot be closed except by a later call to clear-run-prompts."
  [state msg card]
  (show-prompt state :runner card (str "You are " msg) nil nil {:prompt-type :run})
  (show-prompt state :corp card (str "The Runner is " msg) nil nil {:prompt-type :run}))

(defn clear-run-prompts
  [state]
  (when-let* [runner-prompt (find-first #(= :run (:prompt-type %)) (-> @state :runner :prompt))]
    (remove-from-prompt-queue state :runner runner-prompt))
  (when-let* [corp-prompt (find-first #(= :run (:prompt-type %)) (-> @state :corp :prompt))]
    (remove-from-prompt-queue state :corp corp-prompt)))

(defn cancellable
  "Wraps a vector of prompt choices with a final 'Cancel' option. Optionally sorts the vector alphabetically,
  with Cancel always last."
  ([choices] (cancellable choices false))
  ([choices sorted]
   (if sorted
     (conj (vec (sort-by :title choices)) "Cancel")
     (conj (vec choices) "Cancel"))))
