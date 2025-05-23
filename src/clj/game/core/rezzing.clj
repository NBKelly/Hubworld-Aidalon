(ns game.core.rezzing
  (:require
    [game.core.card :refer [asset? condition-counter? get-card ice? upgrade? source? obstacle? agent?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [rez-additional-cost-bonus rez-cost]]
    [game.core.delving :refer [reset-delve-continue!]]
    [game.core.effects :refer [is-disabled? unregister-static-abilities update-disabled-cards]]
    [game.core.eid :refer [complete-with-result effect-completed make-eid]]
    [game.core.engine :refer [register-pending-event queue-event checkpoint pay register-events resolve-ability trigger-event unregister-events]]
    [game.core.flags :refer [can-host? can-rez?]]
    [game.core.ice :refer [update-ice-strength]]
    [game.core.initializing :refer [card-init deactivate]]
    [game.core.moving :refer [trash-cards]]
    [game.core.payment :refer [build-spend-msg can-pay? merge-costs ->c]]
    [game.core.reactions :refer [forge-reaction]]
    [game.core.say :refer [play-sfx system-msg implementation-msg]]
    [game.core.toasts :refer [toast]]
    [game.core.to-string :refer [card-str]]
    [game.core.update :refer [update!]]
    [game.macros :refer [continue-ability effect wait-for]]
    [game.utils :refer [enumerate-str to-keyword]]
    [jinteki.utils :refer [other-side]]))

(defn get-rez-cost
  [state side card {:keys [ignore-cost alternative-cost cost-bonus]}]
  (merge-costs
    (cond
      (= :all-costs ignore-cost) [(->c :credit 0)]
      alternative-cost (when-not (is-disabled? state side card) alternative-cost)
      :else (let [cost (rez-cost state side card {:cost-bonus cost-bonus})
                  additional-costs (rez-additional-cost-bonus state side card (when ignore-cost #(not= :credit (:cost/type %))))]
              (concat
                (when-not ignore-cost
                  [(->c :credit cost)])
                (when (not (:disabled card))
                  additional-costs))))))

(defn trash-hosted-cards
  [state side eid card]
  (let [hosted-cards (seq (remove condition-counter? (:hosted card)))]
    (if (can-host? state card)
      (effect-completed state side eid)
      (wait-for (trash-cards state side hosted-cards {:unpreventable true :game-trash true})
                (when (pos? (count hosted-cards))
                  (system-msg state side (str "trashes " (enumerate-str (map #(card-str state %) hosted-cards))
                                              " because " (:title card)
                                              " cannot host cards")))
                (effect-completed state side eid)))))

(defn- complete-rez
  [state side eid
   {:keys [disabled] :as card}
   {:keys [alternative-cost ignore-cost no-warning no-msg press-continue] :as args}]
  (let [cdef (card-def card)
        costs (get-rez-cost state side card args)]
    (wait-for (pay state side (make-eid state eid) card costs)
              (let [{:keys [msg cost-paid]} async-result]
                (if-not msg
                  (effect-completed state side eid)
                  (let [_ (when (:unforged-events cdef)
                            (unregister-events state side card))
                        card (if disabled
                               (update! state side (assoc card :rezzed :this-turn :new true))
                               (card-init state side (assoc card :rezzed :this-turn :new true) {:resolve-effect false :init-data true}))]
                    (doseq [h (:hosted card)]
                      (update! state side (-> h
                                              (update-in [:zone] #(map to-keyword %))
                                              (update-in [:host :zone] #(map to-keyword %)))))
                    (when-not no-msg
                      (system-msg state side
                                  (str (build-spend-msg msg "forge" "forges")
                                       (:title card)
                                       (cond
                                         alternative-cost " by paying its alternative cost"
                                         ignore-cost " at no cost")))
                      (implementation-msg state card))
                    (let [rez-byte (:rez-sound (card-def card))]
                      ;; TODO - sounds for agent, source, obstacle, etc
                      (when-not (:silent args) (play-sfx state side (or rez-byte "rez-other"))))
                    (swap! state update-in [:stats side :cards :forged] (fnil inc 0))
                    (when-let [card-ability (:on-forge cdef)]
                      (register-pending-event state :forge card card-ability))
                    (queue-event state :forge {:card (get-card state card)
                                               :cost cost-paid})
                    (wait-for
                      (trash-hosted-cards state side (make-eid state eid) (get-card state card))
                      (wait-for
                        (checkpoint state nil (make-eid state eid) {:duration :forge})
                        (wait-for
                          (forge-reaction state side {:card (get-card state card)})
                          (reset-delve-continue! state (other-side side))
                          (complete-with-result state side eid {:card (get-card state card)}))))))))))

(defn can-pay-to-rez?
  ([state side eid card] (can-pay-to-rez? state side eid card nil))
  ([state side eid card args]
   (let [eid (assoc eid :source-type :rez)
         card (get-card state card)
         costs (or (get-rez-cost state side card args) 0)
         alternative-cost (when (and card
                                     (not (is-disabled? state side card)))
                            (:alternative-cost (card-def card)))]
     (or (and alternative-cost
              (can-pay? state side eid card nil alternative-cost))
         (can-pay? state side eid card nil costs)))))

(defn rez
  "Rez a corp card."
  ([state side eid card] (rez state side eid card nil))
  ([state side eid card
    {:keys [ignore-cost force declined-alternative-cost alternative-cost] :as args}]
   (let [eid (assoc eid :source-type :rez)
         card (get-card state card)
         alternative-cost (when (and card
                                     (not alternative-cost)
                                     (not (is-disabled? state side card))
                                     (not declined-alternative-cost))
                            (:alternative-cost (card-def card)))]
     (if (and card
              (or force
                  (can-rez? state side card))
              (or (source? card)
                  (agent? card)
                  (obstacle? card)
                  (:install-rezzed (card-def card))))
       (if (and alternative-cost
                (not ignore-cost)
                (can-pay? state side eid card nil alternative-cost))
         (continue-ability
           state side
           {:optional
            {:prompt "Pay the alternative Rez cost?"
             :yes-ability {:async true
                           :effect (effect (rez eid card (merge args {:ignore-cost true
                                                                      :alternative-cost alternative-cost})))}
             :no-ability {:async true
                          :effect (effect (rez eid card (merge args {:declined-alternative-cost true})))}}}
           card nil)
         (complete-rez state side eid card args))
       (effect-completed state side eid)))))

;; TODO: make async
(defn derez
  "Derez a corp card."
  ([state side card] (derez state side card nil))
  ([state side card {:keys [source-card no-msg] :as args}]
   (let [card (get-card state card)]
     (when-not no-msg
       (system-msg state side (str (if source-card
                                     (str "uses " (:title source-card) " to unforge ")
                                     "unforges ")
                                   (:title card))))
     (unregister-events state side card)
     (update! state side (deactivate state side card true))
     (let [cdef (card-def card)]
       (when-let [derez-effect (:unforge-effect cdef)]
         (resolve-ability state side derez-effect (get-card state card) nil))
       (when-let [derezzed-events (:unforged-events cdef)]
         (register-events state side card (map #(assoc % :condition :derezzed) derezzed-events))))
     (unregister-static-abilities state side card)
     (update-disabled-cards state)
     (trigger-event state side :unforge {:card card :side side}))))
