(ns game.cards.obstacles-tests
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.rezzing :as rezzing]
   [game.test-framework :refer :all]
   [game.core.payment :refer [->c]]))

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
