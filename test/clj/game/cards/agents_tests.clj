(ns game.cards.agents-tests
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.rezzing :as rezzing]
   [game.test-framework :refer :all]
   [game.core.payment :refer [->c]]))

;; (deftest auntie-ruth-draw-3
;;   (doseq [s [:corp :runner]]
;;     (do-game
;;       (new-game {:corp {:hand ["Auntie Ruth: Proprietor of the Hidden Tea House"]
;;                         :deck [(qty "Fun Run" 4)]}
;;                  :runner {:hand []
;;                           :deck [(qty "Fun Run" 10)]}})
;;       (play-from-hand state :corp "Auntie Ruth: Proprietor of the Hidden Tea House" :council :inner)
;;       (forge state :corp (pick-card state :corp :council :inner))
;;       (is (changed? [(count (get-hand state s)) 3]
;;             (click-prompt state :corp "Auntie Ruth: Proprietor of the Hidden Tea House")
;;             (click-card state :corp (get-id state s)))
;;           (str "side: " s " drew 3")))))

(deftest auntie-ruth-force-draw
  (doseq [[s a] [[:corp 1] [:runner 2]]]
    (do-game
      (new-game {:corp {:hand ["Auntie Ruth: Proprietor of the Hidden Tea House"] :deck ["Fun Run"]}})
      (play-from-hand state :corp "Auntie Ruth: Proprietor of the Hidden Tea House" :council :inner)
      (forge state :corp (pick-card state :corp :council :inner))
      (is (changed? [(count (get-hand state s)) 1]
            (card-ability state :corp (pick-card state :corp :council :inner) a))
          "Forced a draw for side"))))

(deftest auntie-ruth-collects
  (collects? {:name "Auntie Ruth: Proprietor of the Hidden Tea House"
              ;;:prompts ["Auntie Ruth: Proprietor of the Hidden Tea House" "Goldie Xin: Junk Collector"]
              :credits 1}))

(deftest auntie-ruth-cipher-lose-one-action
  (do-game
    (new-game {:runner {:hand ["Auntie Ruth: Proprietor of the Hidden Tea House"]}})
    (click-credit state :corp)
    (click-credit state :runner)
    (delve-empty-server state :corp :council {:give-heat? true})
    (is (changed? [(get-credits state :corp) -2
                   (get-clicks state :corp) -1]
          (click-prompt state :corp "Spend Lose [Click] and pay 2 [Credits]: Secure"))
        "Secured auntie ruth")
    (is (= 1 (count (get-scored state :corp))) "Ruth is in the score area")))

;; TODO: Big Varna Gorvis: Friends in Every District

;; TODO: Boss Bresloo: The Deal-Maker

(deftest coroner-goodman-discover-in-district
  (do-game
    (new-game {:corp {:hand ["Coroner Goodman: Slab Sleuth"]
                      :deck [(qty "Shardwinner" 10)]
                      :discard ["Infiltrate"]}})
    (play-from-hand state :corp "Coroner Goodman: Slab Sleuth" :council :inner)
    (forge state :corp (pick-card state :corp :council :inner))
    (click-credit state :runner)
    (click-credit state :corp)
    (delve-empty-server state :runner :commons {:give-heat? true})
    (click-prompts state :corp "Coroner Goodman: Slab Sleuth" "Yes")
    (click-card state :corp "Infiltrate")
    (is (changed? [(:credit (get-runner)) -5]
                  (click-prompt state :runner "Pay 5 [Credits]: Exile"))
        "Coroner Goodman increased presence from 2 -> 5")))

(deftest coroner-goodman-collects
  (collects? {:name "Coroner Goodman: Slab Sleuth"
              :credits 1}))

(deftest doctor-twilight-dream-surgeon-gain-3
  (do-game
    (new-game {:corp {:hand ["Doctor Twilight: Dream Surgeon"]
                      :discard ["Infiltrate"]}})
    (play-from-hand state :corp "Doctor Twilight: Dream Surgeon" :council :inner)
    (forge state :corp (pick-card state :corp :council :inner))
    (card-ability state :corp (pick-card state :corp :council :inner) 1)
    (is (changed? [(:credit (get-corp)) 3]
          (click-card state :corp "Infiltrate"))
        "Gained 3")))

(deftest doctor-twilight-collects
  (collects? {:name "Doctor Twilight: Dream Surgeon"
              :cards 1}))

(deftest doctor-twilight-discover-ability
  (do-game
    (new-game {:runner {:hand ["Doctor Twilight: Dream Surgeon"] :exile ["Shardwinner"]}})
    (click-credit state :corp)
    (click-credit state :runner)
    (delve-empty-server state :corp :council {:give-heat? true})
    (click-card state :runner "Shardwinner")
    (is-discard? state :runner ["Shardwinner"])))

(deftest gargala-larga-imperator-of-growth-unexhaust-seeker
  (do-game
    (new-game {:corp {:hand ["Gargala Larga: Imperator of Growth"]}})
    (play-from-hand state :corp "Gargala Larga: Imperator of Growth" :council :inner)
    (forge state :corp (pick-card state :corp :council :inner))
    (core/process-action "exhaust" state :corp {:card (get-id state :corp)})
    (is (exhausted? (get-id state :corp)) "ID is exhausted")
    (card-ability state :corp (pick-card state :corp :council :inner) 1)
    (is (not (exhausted? (get-id state :corp))) "ID is unexhausted")))

(deftest doctor-twilight-collects
  (collects? {:name "Gargala Larga: Imperator of Growth"
              :credits 1}))

(deftest gargala-larga-cipher-lose-one-action
  (do-game
    (new-game {:runner {:hand ["Gargala Larga: Imperator of Growth"]}})
    (click-credit state :corp)
    (click-credit state :runner)
    (delve-empty-server state :corp :council {:give-heat? true})
    (is (changed? [(get-credits state :corp) -1]
          (click-prompt state :corp "Pay 1 [Credits] and Exhaust your Seeker: Secure"))
        "Secured gargala larga")
    (is (= 1 (count (get-scored state :corp))) "Gargala Larga is in the score area")))

(deftest guildmaster-yanos-affable-gaffer
  (collects? {:name "Guildmaster Yanos: Affable Gaffer"
              :credits 1})
  (do-game
    (new-game {:corp {:hand ["Guildmaster Yanos: Affable Gaffer" "Shardwinner"]}})
    (play-from-hand state :corp "Guildmaster Yanos: Affable Gaffer" :council :inner)
    (click-credit state :runner)
    (play-from-hand state :corp "Shardwinner" :council :outer)
    (forge state :corp (pick-card state :corp :council :inner))
    (is (changed? [(:credit (get-corp)) -2]
          (forge state :corp (pick-card state :corp :council :outer)))
        "1c discount")))

(deftest kryzar-free-stage
  (do-game
    (new-game {:corp {:hand ["Kryzar the Rat: Navigator of the Cortex Maze"
                             "Eye Enforcers"]}})
    (play-from-hand state :corp "Kryzar the Rat: Navigator of the Cortex Maze" :council :inner)
    (click-credit state :runner)
    (forge state :corp (pick-card state :corp :council :inner))
    (delve-server state :corp :council)
    (delve-continue-to-approach state :corp)
    (click-prompts state :corp "Kryzar the Rat: Navigator of the Cortex Maze" "Yes" "Eye Enforcers")
    (stage-select state :corp :council :outer)
    (is (exhausted? (pick-card state :corp :council :inner)) "Exhausted kryzar")
    (is (= "Eye Enforcers" (:title (pick-card state :corp :council :outer))) "Staged EE")))

(deftest kryzar-the-rat-collects
  (collects? {:name "Kryzar the Rat: Navigator of the Cortex Maze"
              :credits 1}))

(deftest maestro-free-stage
  (do-game
    (new-game {:corp {:hand ["Maestro: The Bebop Boffin"
                             "Eye Enforcers"]}})
    (play-from-hand state :corp "Maestro: The Bebop Boffin" :council :inner)
    (click-credit state :runner)
    (forge state :corp (pick-card state :corp :council :inner))
    (delve-server state :corp :council)
    (delve-continue-to-approach state :corp)
    (click-prompts state :corp "Maestro: The Bebop Boffin" "Yes" "Eye Enforcers")
    (stage-select state :corp :council :outer)
    (is (exhausted? (pick-card state :corp :council :inner)) "Exhausted maestro")
    (is (= "Eye Enforcers" (:title (pick-card state :corp :council :outer))) "Staged EE")))

(deftest maestro-cipher-steal-1-heat
  (do-game
    (new-game {:runner {:hand ["Maestro: The Bebop Boffin"]}})
    (click-credit state :corp)
    (click-credit state :runner)
    (delve-empty-server state :corp :council {:give-heat? true})
    (is (changed? [(get-heat state :corp) 1
                   (get-heat state :runner) -1]
                  (click-prompt state :corp "Pay 1 [Credits] and steal 1 [heat]: Secure"))
        "Secured maestro")
    (is (= 1 (count (get-scored state :corp))) "Maestro is in the score area")))

(deftest maestro-collects
  (collects? {:name "Maestro: The Bebop Boffin"
              :credits 1}))

(deftest prime-treasurer-geel-munificent-financier
  (collects? {:name "Prime Treasurer Geel: Munificent Financier"
              :credits 1})
  ;; +1 barrier aura
  (do-game
    (new-game {:corp {:hand ["Prime Treasurer Geel: Munificent Financier" "Barbican Gate"]}})
    (play-from-hand state :corp "Barbican Gate" :council :outer)
    (click-credit state :runner)
    (play-from-hand state :corp "Prime Treasurer Geel: Munificent Financier" :council :middle)
    (is (changed? [(barrier state :corp :council :outer) 1
                   (barrier state :corp :council :middle) 0]
          (forge state :corp (pick-card state :corp :council :middle))
          (forge state :corp (pick-card state :corp :council :outer)))
        "Only buffs other cards"))
  ;; discover: +4 creds (while installed)
  (doseq [[opt c q] [["Yes" 4 "gained 4"] ["No" 0 "gained 0"]]]
    (do-game
      (new-game {:runner {:hand ["Prime Treasurer Geel: Munificent Financier"]}
                 :corp {:hand ["Capricious Informant"]}})
      (play-from-hand state :corp "Capricious Informant" :council :inner)
      (play-from-hand state :runner "Prime Treasurer Geel: Munificent Financier" :council :outer)
      (delve-server state :corp :council)
      (delve-discover-impl state :corp)
      (is (changed? [(:credit (get-runner)) c]
            (click-prompt state :runner opt))
          q))))

(deftest rory-and-bug-moves
  (do-game
    (new-game {:corp {:hand ["Rory & Bug: “We Fetch It, You Catch It!”"]}})
    (play-from-hand state :corp "Rory & Bug: “We Fetch It, You Catch It!”" :council :inner)
    (forge state :corp (pick-card state :corp :council :inner))
    (card-ability state :corp (pick-card state :corp :council :inner) 1)
    (stage-select state :corp :council :middle)
    (is (= "Rory & Bug: “We Fetch It, You Catch It!”"
           (:title (pick-card state :corp :council :middle)))
        "Shifted to middle")))

(deftest rory-and-bug-collects
  (collects? {:name "Rory & Bug: “We Fetch It, You Catch It!”"
              :credits 1}))

(deftest sergeant-cole-collects
  (collects? {:name "Sergeant Cole: Precinct 204, 3rd Level"
              :credits 1}))

(deftest sergeant-cole-cipher-exhaust-archives-path
  (do-game
    (new-game {:runner {:hand ["Sergeant Cole: Precinct 204, 3rd Level"]}
               :corp {:hand ["Eye Enforcers"]}})
    (play-from-hand state :corp "Eye Enforcers" :archives :inner)
    (click-credit state :runner)
    (delve-empty-server state :corp :council {:give-heat? true})
    (is (changed? [(get-credits state :corp) -1]
          (click-prompt state :corp "Pay 1 [Credits] and exhaust 1 card protecting Archives: Secure")
          (click-card state :corp (pick-card state :corp :archives :inner)))
        "Secured Sgt. Cole")
    (is (= 1 (count (get-scored state :corp))) "Ruth is in the score area")))

(deftest sergeant-cole-mills-2-cards
  (do-game
    (new-game {:corp {:hand ["Sergeant Cole: Precinct 204, 3rd Level"]}
               :runner {:deck [(qty "Fun Run" 10)]}})
    (play-from-hand state :corp "Sergeant Cole: Precinct 204, 3rd Level" :archives :inner)
    (forge state :corp (pick-card state :corp :archives :inner))
    (click-credit state :runner)
    (delve-empty-server state :corp :archives {:give-heat? true})
    (click-prompts state :corp "Sergeant Cole: Precinct 204, 3rd Level" "Yes")
    (is (= 2 (count (get-discard state :runner))) "Milled 2")))

(deftest spider-rebbek-dragons-hoard-pitboss
  (collects? {:name "“Spider” Rebbek: Dragon’s Hoard Pitboss"
              :cards 1})
  (do-game
    (new-game {:corp {:hand ["“Spider” Rebbek: Dragon’s Hoard Pitboss"]}
               :runner {:hand ["Capricious Informant"]}})
    (play-from-hand state :corp "“Spider” Rebbek: Dragon’s Hoard Pitboss" :council :outer)
    (play-from-hand state :runner "Capricious Informant" :council :outer)
    (forge state :runner (pick-card state :runner :council :outer))
    (click-credit state :corp)
    (delve-server state :runner :council)
    (delve-discover-impl state :runner)
    (click-card state :corp "Capricious Informant")
    (is (:exhausted (pick-card state :runner :council :outer)) "Exhausted it")))

(deftest ulin-marr-eccentric-architect
  (collects? {:name "Ulin Marr: Eccentric Architect"
              :cards 1})
  (do-game
    (new-game {:corp {:hand ["Ulin Marr: Eccentric Architect" "Capricious Informant" "Shardwinner" "Fun Run"]}})
    (play-from-hand state :corp "Ulin Marr: Eccentric Architect" :council :inner)
    (click-credit state :runner)
    (forge state :corp (pick-card state :corp :council :inner))
    (card-ability state :corp (pick-card state :corp :council :inner) 1)
    (click-card state :corp "Shardwinner")
    (stage-select state :corp :council :outer)
    (click-card state :corp "Capricious Informant")
    (stage-select state :corp :council :middle)
    (is (no-prompt? state :corp))))
