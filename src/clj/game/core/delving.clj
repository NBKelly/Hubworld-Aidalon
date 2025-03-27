(ns game.core.delving
  (:require
   [game.core.card :refer [agent? moment?
                           rezzed? in-discard?
                           get-card]]
   [game.core.card-defs :refer [card-def]]
   [game.core.choose-one :refer [choose-one-helper]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid]]
   [game.core.engine :refer [checkpoint queue-event register-default-events register-pending-event resolve-ability]]
   [game.core.effects :refer [register-static-abilities]]
   [game.core.flags :refer [card-flag?]]
   [game.core.moving :refer [move exile]]
   [game.core.payment :refer [->c merge-costs]]
   [game.core.presence :refer [get-presence]]
   [game.core.barrier :refer [get-barrier]]
   [game.core.say :refer [play-sfx system-msg]]
   [game.core.to-string :refer [card-str]]
   [game.macros :refer [req wait-for]]
   [game.utils :refer [to-keyword]]
   [jinteki.utils :refer [other-side]]
   [clojure.string :as str]))


;; things relating to delving on a path

(defn secure-agent
  "Moves a card to the players :scored area, triggering events from the completion of the steal."
  [state side eid card]
  (let [c (move state side (dissoc card :advance-counter :new :exhausted :installed :rezzed) :scored {:force true})
        _ (when (card-flag? c :has-events-when-secured true)
            (register-default-events state side c)
            (register-static-abilities state side c))
        c (get-card state c)]
    ;;(system-msg state side (str "secures " (:title c)))
    (swap! state update-in [side :register :secured-agent] #(+ (or % 0) 1))
    (play-sfx state side "agenda-steal")
    ;; (when (:breach @state)
    ;;   (swap! state assoc-in [:breach :did-steal] true))
    ;; (when (:run @state)
    ;;   (swap! state assoc-in [:run :did-steal] true))
    (when-let [on-secured (:on-secured (card-def c))]
      (register-pending-event state :agent-secured c on-secured))
    (queue-event state :on-secured {:card c})
    (checkpoint state nil eid {:duration :agent-secured})))
;;(access-end state side eid c {:stolen true}))))

;; DISCOVERY - facedown installed cards, and cards in centrals, can be discovered
;;             go through all the discover abilities top->bottom, then the player may dispatch the card

(defn discover-continue
  [state side eid discovered-card]
  (let [can-interact? (and (not (moment? discovered-card))
                           (not (in-discard? discovered-card)))
        should-secure? (agent? discovered-card)
        interact-cost? (merge-costs (concat (when-not (in-discard? discovered-card) [(->c :credit (get-presence discovered-card))])
                                            (:cipher (card-def discovered-card))))]
    (wait-for (resolve-ability
                state side
                (choose-one-helper
                  {:prompt (str "You are discovering " (:title discovered-card))}
                  [{:option "Exile"
                    :req (req (and (not should-secure?) can-interact?))
                    :cost (when (seq interact-cost?) interact-cost?)
                    :ability {:async true
                              :effect (req (let [cost-msg (:latest-payment-str eid)]
                                             (system-msg state side
                                                         (str (if cost-msg
                                                                (str cost-msg " to exile ")
                                                                "exiles ")
                                                              (:title discovered-card))))
                                           (exile state side eid discovered-card))}}
                   {:option "Secure"
                    :req (req (and should-secure? can-interact?))
                    :cost (when (seq interact-cost?) interact-cost?)
                    :ability {:async true
                              :effect (req (let [cost-msg (:latest-payment-str eid)]
                                             (system-msg state side
                                                         (str (if cost-msg
                                                                (str cost-msg " to secure ")
                                                                "secures ")
                                                              (:title discovered-card))))
                                           (secure-agent state side eid discovered-card))}}
                    {:option "End discovery"}])
                  nil nil))
              ;; TODO - if we're accessing, call access-end or something equivalent
              (effect-completed state side eid)))

;; discovery abilities are fired one at a time, from top to bottom
(defn- resolve-discover-abilities
  [state side eid card abs]
  ;; todo - see if we get access dissoc'd
  (if (seq abs)
    (let [ab (first abs)]
      (wait-for (resolve-ability state (other-side side) ab card nil)
                (resolve-discover-abilities state side eid card (rest abs))))
    (discover-continue state side eid card)))

(defn discover-card
  [state side eid card]
  (if-not (get-card state card)
    (effect-completed state side eid)
    (do (system-msg state side (str "discovers " (:title card)))
        (resolve-discover-abilities state side eid card (:discover-abilities (card-def card))))))

;; CONFRONTATION - faceup installed cards can be confronted. If the card is not exhausted, it *must* be confronted
;;                   1) Go through all the confront abilities top->bottom
;;                   2) The engaged player may pay the break cost of the card. If they do not, end the delve.
;;                   3) The engaged player may dispatch the card.

(defn confrontation-continue
  [state side eid discovered-card]
  (let [can-interact? (and (not (moment? discovered-card))
                           (not (in-discard? discovered-card)))
        should-secure? (agent? discovered-card)
        interact-cost? (merge-costs (concat (when-not (in-discard? discovered-card) [(->c :credit (get-presence discovered-card))])
                                            (:cipher (card-def discovered-card))))]
    (resolve-ability
      state side
      (choose-one-helper
        {:prompt (str "You are confronting " (:title discovered-card))}
        [{:option "Exile"
          :req (req (and (not should-secure?) can-interact?))
          :cost (when (seq interact-cost?) interact-cost?)
          :ability {:async true
                    :effect (req (let [cost-msg (:latest-payment-str eid)]
                                   (system-msg state side
                                               (str (if cost-msg
                                                      (str cost-msg " to exile ")
                                                      "exiles ")
                                                    (:title discovered-card))))
                                 (exile state side eid discovered-card))}}
         {:option "Secure"
          :req (req (and should-secure? can-interact?))
          :cost (when (seq interact-cost?) interact-cost?)
          :ability {:async true
                    :effect (req (let [cost-msg (:latest-payment-str eid)]
                                   (system-msg state side
                                               (str (if cost-msg
                                                      (str cost-msg " to secure ")
                                                      "secures ")
                                                    (:title discovered-card))))
                                 (secure-agent state side eid discovered-card))}}
         {:option "End confrontation"}])
      nil nil)))

(defn confrontation-resolve-barrier
  [state side eid confronted-card]
  (let [barrier-cost (max 0 (get-barrier confronted-card))]
    (if (pos? barrier-cost)
      (resolve-ability
        state side
        {:optional {:prompt (str "Pay " barrier-cost " [Credits] to break the barrier of " (:title confronted-card) "?")
                    :yes-ability {:cost [(->c :credit barrier-cost)]
                                  :async true
                                  :effect (req (system-msg state side (str (:latest-payment-str eid) " to break the barrier on " (:title confronted-card)))
                                               (confrontation-continue state side eid confronted-card))}
                    :no-ability {:effect (req (system-msg state (other-side side) " ends the delve! (todo)"))}}}
        nil nil)
      (confrontation-continue state side eid confronted-card))))

;; confrontation abilities are fired one at a time, from top to bottom
(defn- resolve-confrontation-abilities
  [state side eid card abs]
  ;; todo - see if we get access dissoc'd
  (if (seq abs)
    (let [ab (first abs)]
      (wait-for (resolve-ability state (other-side side) ab card nil)
                (resolve-confrontation-abilities state side eid card (rest abs))))
    (confrontation-resolve-barrier state side eid card)))

(defn confront-card
  [state side eid card]
  (if-not (and (get-card state card) (rezzed? card))
    (effect-completed state side eid)
    (do (system-msg state side (str "confronts " (:title card)))
        (resolve-confrontation-abilities state side eid card (:confront-abilities (card-def card))))))
