(ns game.core.bluffs
  (:require
   [clojure.string :as str]
   [game.core.card :refer [get-card
                           obstacle?
                           in-hand? rezzed? installed?]]
   [game.core.payment :refer [->c can-pay?]]
   [game.utils :refer [to-keyword  same-card?]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [jinteki.utils :refer [other-side count-heat other-player-name]]))

(defn- known-copies
  "Copies of a card in the exile zone"
  [state side name]
  (count (filter #(= (:title %) name) (get-in @state [side :rfg]))))

(defn bluffs-enabled?
  "check bluff settings for each player"
  [state]
  ;; TODO - this
  ;; for now this is just for testing
  (not (get-in @state [:bluffs-disabled-for-testing])))

;; TODO - also adjust the known-copies thing to take into account known/possible influence,
;;   and rule out the cards which cannot possible be in the deck because of influence reasons

(def bluffs
  {;; END BREACH SERVER:
   ;;   FUN RUN
   ;;   CORNERING THE MARKET
   :complete-breach (req (and (seq (get-in @state [side :hand]))
                              (bluffs-enabled? state)
                              (or
                                (and
                                  ;; WHEN I DELVE
                                  (= (:delver context) side)
                                  (or
                                    ;; FUN RUN
                                    ;; CORNERING THE MARKET
                                    (and (= (:breach-server context) :commons)
                                         (or (< (known-copies state side "Fun Run") 2)
                                             (< (known-copies state side "Cornering the Market") 2)))
                                    ;; TURN UP THE HEAT
                                    (< (known-copies state side "Turn Up the Heat") 2))))))
   ;; BREACH SERVER
   ;;   INFILTRATE
   :pre-discovery (req (and (seq (get-in @state [side :hand]))
                            (bluffs-enabled? state)
                            (or
                              (and ;; INFILTRATE
                                (= (:breach-server context) :council)
                                (= (:delver context) side)
                                (can-pay? state side eid card nil [(->c :credit 1)])
                                (< (known-copies state side "Infiltrate") 2)))))

   ;; PRE-DISCOVER (A SINGLE CARD)
   ;;   TENACITY
   :pre-discover (req (and (seq (get-in @state [side :hand]))
                           (bluffs-enabled? state)
                           (or
                             (and ;; TENACITY
                               (not= side (:engaged-side context))
                               (installed? (:card context))
                               (< (known-copies state side "Tenacity") 2)))))

   ;; PRE-CONFRONT (A SINGLE CARD)
   ;;   TENACITY
   ;;   PROTECTING OUR INVESTMENT
   :pre-confrontation (req (and (seq (get-in @state [side :hand]))
                                (bluffs-enabled? state)
                                (or
                                  (and ;; TENACITY
                                    (not= side (:engaged-side context))
                                    (installed? (:card context))
                                    (obstacle? (:card context))
                                    (< (known-copies state side "Protecting Our Investment") 2))
                                  (and ;; TENACITY
                                    (not= side (:engaged-side context))
                                    (installed? (:card context))
                                    (< (known-copies state side "Tenacity") 2)))))

   ;; POST DISCOVER ABILITY
   ;;   TWICE AS BAD
   :post-discover-ability (req (and (seq (get-in @state [side :hand]))
                                    (bluffs-enabled? state)
                                    (or
                                      (and ;; TWICE AS BAD
                                        (and (= side (:defender context))
                                             (< (known-copies state side "Twice as Bad") 2)
                                             (:ability context)
                                             (get-card state (:discovered-card context))
                                             (let [r (or (-> context :ability :req)
                                                         (-> context :ability :optional :req))]
                                               (or (not r)
                                                   (r state side eid card targets))))))))

   :approach-slot (req (and (seq (get-in @state [side :hand]))
                            (bluffs-enabled? state)
                            (or
                              (and ;; FORCED LIQUIDATION
                                (= (:delver context) side)
                                (rezzed? (get-card state (:approached-card context)))
                                (can-pay? state side eid card nil [(->c :exhaust-forged-with-4-barrier 1)])))))

   ;; ENCOUNTER ENDED
   ;;   LIKELY A TRAP
   :encounter-ended (req (and (seq (get-in @state [side :hand]))
                              (bluffs-enabled? state)
                              (or
                                (and ;; LIKELY A TRAP
                                  (= (:defender context) side)
                                  (let [c (get-card state (:encounter-card context))]
                                    (and c (not (rezzed? c))))
                                  (< (known-copies state side "Likely a Trap") 2)))))})
