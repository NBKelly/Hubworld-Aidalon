(ns game.core.bluffs
  (:require
   [clojure.string :as str]
   [game.core.card :refer [get-card
                           in-hand?
                           rezzed?]]
   [game.core.payment :refer [->c can-pay?]]
   [game.utils :refer [to-keyword  same-card?]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [jinteki.utils :refer [other-side count-heat other-player-name]]))

(defn- known-copies
  "Copies of a card in the exile zone"
  [state side name]
  (count (filter #(= (:title %) name) (get-in @state [side :rfg]))))

(def bluffs
  {;; END BREACH SERVER:
   ;;   FUN RUN
   ;;   CORNERING THE MARKET
   :complete-breach (req (and (seq (get-in @state [side :hand]))
                              (or
                                (and ;; FUN RUN
                                     ;; CORNERING THE MARKET
                                  (= (:delver context) side)
                                  (= (:breach-server context) :commons)
                                  (or (< (known-copies state side "Fun Run") 2)
                                      (< (known-copies state side "Cornering the Market") 2))))))

   ;; BREACH SERVER
   ;;   INFILTRATE
   :breach-server (req (and (seq (get-in @state [side :hand]))
                            (or
                              (and ;; INFILTRATE
                                (= (:breach-server context) :council)
                                (= (:delver context) side)
                                (can-pay? state side eid card nil [(->c :credit 1)])
                                (< (known-copies state side "Infiltrate") 2)))))

   ;; ENCOUNTER ENDED
   ;;   LIKELY A TRAP
   :encounter-ended (req (and (seq (get-in @state [side :hand]))
                              (or
                                (and ;; LIKELY A TRAP
                                  (= (:defender context) side)
                                  (let [c (get-card state (:approached-card context))]
                                    (and c (not (rezzed? c))))
                                  (< (known-copies state side "Likely a Trap") 2)))))})
