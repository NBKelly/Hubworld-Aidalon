(ns game.cards.obstacles-tests
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.rezzing :as rezzing]
   [game.test-framework :refer :all]
   [game.core.payment :refer [->c]]))

(deftest asset-protection-hub
  (do-game
    (new-game {:corp {:hand ["Asset Protection Hub" "Barbican Gate"]}})
    (play-from-hand state :corp "Asset Protection Hub" :council :inner)
    (click-credit state :corp)
    (play-from-hand state :corp "Barbican Gate" :council :middle)
    (forge state :corp (pick-card state :corp :council :middle))
    (is (changed? [(barrier (pick-card state :corp :council :middle)) 1]
          (forge state :corp (pick-card state :corp :council :inner))
          (core/fake-checkpoint state))
        "Gained 1 barrier")))

(deftest barbican-gate-discover-to-gain-1
  (doseq [[opt c q] [["Yes" 1 "gained 1"] ["No" 0 "gained 0"]]]
    (do-game
      (new-game {:runner {:hand ["Barbican Gate"]}
                 :corp {:hand ["Capricious Informant"]}})
      (play-from-hand state :corp "Capricious Informant" :council :inner)
      (play-from-hand state :runner "Barbican Gate" :council :outer)
      (delve-server state :corp :council)
      (delve-discover-impl state :corp)
      (is (changed? [(:credit (get-runner)) c]
            (click-prompt state :runner opt))
          q))))

(deftest barbican-gate-confront-gain-2
  (do-game
    (new-game {:runner {:hand ["Barbican Gate"]}
               :corp {:hand ["Capricious Informant"]}})
    (play-from-hand state :corp "Capricious Informant" :council :inner)
    (play-from-hand state :runner "Barbican Gate" :council :outer)
    (delve-server state :corp :council)
    (forge state :runner (pick-card state :runner :council :outer))
    (forge state :corp (pick-card state :corp :council :inner))
    (delve-confront-impl state :corp)
    (is (changed? [(:credit (get-runner)) 2]
          (click-prompt state :corp "Your opponent gains 2 [Credits]"))
        "Gained 2c")))

(deftest barbican-gate-confront-exhaust-card
  (do-game
    (new-game {:runner {:hand ["Barbican Gate"]}
               :corp {:hand ["Capricious Informant"]}})
    (play-from-hand state :corp "Capricious Informant" :council :inner)
    (play-from-hand state :runner "Barbican Gate" :council :outer)
    (delve-server state :corp :council)
    (forge state :runner (pick-card state :runner :council :outer))
    (forge state :corp (pick-card state :corp :council :inner))
    (delve-confront-impl state :corp)
    (is (changed? [(:credit (get-runner)) 0]
          (click-prompts state :corp "Exhaust 1 forged card" "Capricious Informant"))
        "Gained 0c")))

;; TODO: Canal Network
;; TODO: Emperor Drejj
;; TODO: Eye Enforcers

(deftest echofield-lockup
  (presence? {:name "Echofield Lockup" :presence-value 2 :discard [(qty "Fun Run" 2)]})
  (presence? {:name "Echofield Lockup" :presence-value 5 :discard [(qty "Fun Run" 3)]})
  (doseq [[opt c q] [["Yes" 3 "gained 3"] ["No" 0 "gained 0"]]
          discard [1 2 3 4 5 6]]
    (do-game
      (new-game {:runner {:hand ["Echofield Lockup"]
                          :discard [(qty "Fun Run" discard)]}
                 :corp {:hand ["Capricious Informant"]}})
      (play-from-hand state :corp "Capricious Informant" :council :inner)
      (play-from-hand state :runner "Echofield Lockup" :council :outer)
      (delve-server state :corp :council)
      (delve-discover-impl state :corp)
      (if (>= discard 3)
        (is (changed? [(:credit (get-runner)) c]
              (click-prompt state :runner opt))
            q)
        (click-prompt state :corp "No Action")))))

(deftest flooding-thoroughfare-test
  (do-game
    (new-game {:corp {:hand ["Flooding Thoroughfare"]}})
    (play-from-hand state :corp "Flooding Thoroughfare" :council :outer)
    (click-credit state :runner)
    (click-credit state :corp)
    (delve-server state :runner :commons)
    (forge state :corp (pick-card state :corp :council :outer))
    (click-prompt state :corp "Flooding Thoroughfare")
    (is (= 3 (barrier (pick-card state :corp :council :outer))) "Got +2 barrier")))

(deftest hired-lookout-test
  (do-game
    (new-game {:corp {:hand ["Hired Lookout"]}})
    (play-from-hand state :corp "Hired Lookout" :council :outer)
    (click-credit state :runner)
    (click-credit state :corp)
    (delve-server state :runner :council)
    (forge state :corp (pick-card state :corp :council :outer))
    (delve-confront-impl state :runner)
    (click-prompt state :corp "Yes")
    (click-prompt state :runner "The delve ends")
    (is (not (:delve @state)) "Not delve")))

(deftest orobo-plaza-test
  (doseq [opt [:confront :discover]]
    (do-game
      (new-game {:corp {:hand ["Orobo Plaza"] :heat 2}})
      (play-from-hand state :corp "Orobo Plaza" :council :outer)
      (click-credit state :runner)
      (click-credit state :corp)
      (delve-server state :runner :council)
      (if (= opt :confront)
        (do (forge state :corp (pick-card state :corp :council :outer))
            (delve-confront-impl state :runner))
        (delve-discover-impl state :runner))
      (is (changed? [(:credit (get-corp)) 1
                     (:credit (get-runner)) -1]
            (click-prompt state :corp "Yes"))
          "Drained 1c"))))

(deftest priority-expressway
  (do-game
    (new-game {:corp {:hand ["Priority Expressway"]}})
    (play-from-hand state :corp "Priority Expressway" :council :outer)
    (forge state :corp (pick-card state :corp :council :outer))
    (click-credit state :runner)
    (click-credit state :corp)
    (delve-server state :runner :council)
    (delve-confront-impl state :runner)
    (is (changed? [(:credit (get-corp)) 2
                   (get-counters (pick-card state :corp :council :outer) :credit) -1]
          (click-prompt state :corp "Yes"))
        "Took 1 credit")))

(deftest prized-gulk-gain-1
  (doseq [[opt c q] [["Yes" 1 "gained 1"] ["No" 0 "gained 0"]]
          f [delve-discover-impl delve-confront-impl]]
    (do-game
      (new-game {:runner {:hand ["Prized Gulk"]}
                 :corp {:hand ["Capricious Informant"]}})
      (play-from-hand state :corp "Capricious Informant" :council :inner)
      (play-from-hand state :runner "Prized Gulk" :council :outer)
      (delve-server state :corp :council)
      (when (= f delve-confront-impl)
        (forge state :runner (pick-card state :runner :council :outer)))
      (f state :corp)
      (is (changed? [(:credit (get-runner)) c]
            (click-prompt state :runner opt))
          q))))

(deftest probabilities-exhange-tests
  (do-game
    (new-game {:corp {:hand ["Probabilities Exchange"]}
               :runner {:hand ["Shardwinner" "Crispy Crawler"]}})
    (play-from-hand state :corp "Probabilities Exchange" :council :outer)
    (play-from-hand state :runner "Shardwinner" :council :outer)
    (click-credit state :corp)
    (play-from-hand state :runner "Crispy Crawler" :council :middle)
    (click-credit state :corp)
    (delve-server state :runner :council)
    (forge state :corp (pick-card state :corp :council :outer))
    (delve-continue-impl state :runner)
    (click-prompt state :corp "Yes")
    (click-card state :runner "Crispy Crawler")
    (click-prompts state :runner "Yes" "Pay 4 [Credits] and exhaust 1 card protecting your front row: Exile" "Shardwinner")))

(deftest pushy-salesperson-test
  (do-game
    (new-game {:corp {:hand ["Pushy Salesperson"]}})
    (play-from-hand state :corp "Pushy Salesperson" :council :outer)
    (click-credit state :runner)
    (click-credit state :corp)
    (delve-server state :runner :council)
    (delve-discover-impl state :runner)
    (click-prompts state :corp "Yes")
    (delve-confront-impl state :runner)
    (click-prompt state :corp "Yes")
    (click-prompt state :runner "The delve ends")
    (is (not (:delve @state)) "Not delve")))

;; TODO: Transit Station

(deftest tunnel-runners-test
  (do-game
    (new-game {:corp {:hand ["Tunnel Runners"]
                      :heat 1}})
    (play-from-hand state :corp "Tunnel Runners" :council :outer)
    (forge state :corp (pick-card state :corp :council :outer))
    (click-credit state :runner)
    (click-credit state :corp)
    (delve-server state :runner :council)
    (delve-confront-impl state :runner)
    (is (changed? [(:credit (get-corp)) -1
                   (get-heat state :corp) -1]
          (click-prompt state :corp "Yes"))
        "Lost 1 heat")))

(deftest silent-interrogator-test
  (do-game
    (new-game {:corp {:hand ["Silent Interrogator"]}
               :runner {:deck [(qty "Fun Run" 10)]}})
    (play-from-hand state :corp "Silent Interrogator" :council :outer)
    (click-credit state :runner)
    (click-credit state :corp)
    (delve-server state :runner :council)
    (delve-discover-impl state :runner)
    (is (changed? [(count (:deck (get-runner))) -4]
          (click-prompt state :corp "Yes"))
        "Milled 4")))

(deftest waterway-ferry-shift-on-forge
  (do-game
    (new-game {:corp {:hand ["Waterway Ferry"]}})
    (play-from-hand state :corp "Waterway Ferry" :council :outer)
    (forge state :corp (pick-card state :corp :council :outer))
    (click-prompt state :corp "Waterway Ferry")
    (click-prompt state :corp "Yes")
    (stage-select state :corp :council :middle)
    (is (= "Waterway Ferry" (:title (pick-card state :corp :council :middle))) "Shifted")))

(deftest yowling-tezu-discover-force-2-heat
  (do-game
    (new-game {:corp {:hand ["Yowling Tezu"]}})
    (play-from-hand state :corp "Yowling Tezu" :council :outer)
    (click-credit state :runner)
    (click-credit state :corp)
    (delve-server state :runner :council)
    (delve-discover-impl state :runner)
    (is (changed? [(get-heat state :runner) 2]
          (click-prompt state :corp "Yes"))
        "Gained 2 heat")))

(deftest yowling-encounter-heat-for-heat
  (do-game
    (new-game {:corp {:hand ["Yowling Tezu"]}})
    (play-from-hand state :corp "Yowling Tezu" :council :outer)
    (forge state :corp (pick-card state :corp :council :outer))
    (click-credit state :runner)
    (click-credit state :corp)
    (delve-server state :runner :council)
    (delve-confront-impl state :runner)
    (is (changed? [(get-heat state :runner) 1
                   (get-heat state :corp) 1]
          (click-prompt state :corp "Yes"))
        "Both gained 1 heat")))
