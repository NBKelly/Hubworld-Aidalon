(ns game.core.exhausting
  (:require
   [clojure.string :as str]
   [game.core.card :refer [get-card]]
   [game.core.card-defs :refer [card-def]]
   [game.core.eid :refer [complete-with-result effect-completed]]
   [game.core.engine :refer [checkpoint queue-event register-pending-event]]
   [game.core.say :refer [system-msg]]
   [game.core.to-string :refer [hubworld-card-str]]
   [game.core.update :refer [update!]]
   [game.macros :refer [wait-for]]
   [game.utils :refer [enumerate-str]]))

(defn exhaust
  "Exhaust one or multiple cards"
  ([state side eid cards] (exhaust state side eid cards nil))
  ([state side eid cards {:keys [no-event suppress-checkpoint no-msg] :as args}]
   (if-let [cards (seq (filter (complement :exhausted) (map #(get-card state %) (flatten [cards]))))]
     (do (when-not no-msg
           (system-msg state side (str "exhausts " (enumerate-str (map #(hubworld-card-str state %) cards)))))
         (doseq [card cards]
           (let [card (update! state side (assoc card :exhausted true))
                 cdef (card-def card)]
             (when-not no-event
               (swap! state update-in [:stats side :cards :exhausted] (fnil inc 0))
               (when-let [card-ability (:on-exhaust cdef)]
                 (register-pending-event state :exhaust card card-ability)))))
         (when-not no-event (queue-event state :exhaust {:cards cards}))
         (if-not suppress-checkpoint
           (wait-for
             (checkpoint state side {:duration :exhaust})
             (complete-with-result state side eid {:cards cards}))
           (complete-with-result state side eid {:cards cards})))
     (effect-completed state side eid))))

(defn unexhaust ;; aka "ready"
  "Un-Exhaust one or multiple cards"
  ([state side eid cards] (unexhaust state side eid cards nil))
  ([state side eid cards {:keys [no-event suppress-checkpoint no-msg] :as args}]
   (if-let [cards (seq (filter :exhausted (map #(get-card state %) (flatten [cards]))))]
     (do (when-not no-msg
           (system-msg state side (str "readies " (enumerate-str (map #(hubworld-card-str state %) cards)))))
         (doseq [card cards]
           (let [card (update! state side (assoc card :exhausted nil))
                 cdef (card-def card)]
             (when-not no-event
               (swap! state update-in [:stats side :cards :readied] (fnil inc 0))
               (when-let [card-ability (:on-ready cdef)]
                 (register-pending-event state :ready card card-ability)))))
         (when-not no-event (queue-event state :ready {:cards cards}))
         (if-not suppress-checkpoint
           (wait-for
             (checkpoint state side {:duration :ready})
             (complete-with-result state side eid {:cards cards}))
           (complete-with-result state side eid {:cards cards})))
     (effect-completed state side eid))))
