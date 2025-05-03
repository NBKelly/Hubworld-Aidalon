(ns game.cards.moments-tests
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.rezzing :as rezzing]
   [game.test-framework :refer :all]
   [game.core.payment :refer [->c]]))

(deftest calling-in-favors-test
  (do-game
    (new-game {:corp {:hand ["Calling in Favors"]
                      :deck [(qty "Fun Run" 3)]}})
    (is (changed? [(get-credits state :corp) 4
                   (get-clicks state :corp) -2
                   (count (get-hand state :corp)) 0]
          (play-from-hand state :corp "Calling in Favors"))
        "Gained a bunch of stuff")))

(deftest cooling-off
  (doseq [heat [0 1]]
    (do-game
      (new-game {:corp {:hand ["Cooling Off"]
                        :heat heat
                        :deck [(qty "Fun Run" 3)]}})
      (play-from-hand state :corp "Cooling Off")
      (click-card state :corp (get-id state :corp))
      (is-hand? state :corp ["Fun Run"])
      (is (zero? (get-heat state :corp)) "No heat"))))

(deftest cornering-the-market-test
  (do-game
    (new-game {:corp {:hand ["Cornering the Market"]}
               :runner {:hand [(qty "Cornering the Market" 5)]}})
    (click-credit state :corp)
    (click-credit state :runner)
    (delve-empty-server state :corp :commons)
    (is (changed? [(:credit (get-corp)) 3]
          (click-prompts state :corp "Cornering the Market" "Yes"))
        "Gained 3c")
    (is (no-prompt? state :corp))))

(deftest franchise-fees
  (do-game
    (new-game {:corp {:hand ["Franchise Fees" "Disagreeable Inspector"]}})
    (play-from-hand state :corp "Disagreeable Inspector" :council :outer)
    (click-credit state :runner)
    (is (changed? [(:credit (get-corp)) 4]
          (play-from-hand state :corp "Franchise Fees")
          (click-card state :corp "Disagreeable Inspector"))
        "Gained 4")
    (is (:exhausted (pick-card state :corp :council :outer)) "exhausted him")))

(deftest fun-run
  (do-game
    (new-game {:corp {:hand ["Fun Run"]}
               :runner {:hand [(qty "Fun Run" 5)]}})
    (click-credit state :corp)
    (click-credit state :runner)
    (delve-empty-server state :corp :commons)
    (is (changed? [(:credit (get-corp)) 3]
          (click-prompts state :corp "Fun Run" "Yes"))
        "Gained 3c")
    (is (no-prompt? state :corp))))

(deftest infiltrate
  (do-game
    (new-game {:corp {:hand ["Infiltrate"]}
               :runner {:hand [(qty "Fun Run" 5)]}})
    (click-credit state :corp)
    (click-credit state :runner)
    (delve-empty-server state :corp :council {:give-heat? true})
    (click-prompts state :corp "Infiltrate" "Yes" "No Action" "No Action" "No Action")
    (is (no-prompt? state :corp))))

(deftest smooth-handoff
  (doseq [heat [0 1]]
    (do-game
      (new-game {:corp {:hand ["Smooth Handoff"]
                        :heat heat
                        :deck [(qty "Fun Run" 3)]}})
      (play-from-hand state :corp "Smooth Handoff")
      (click-card state :corp (get-id state :corp))
      (is-hand? state :corp ["Fun Run"])
      (is (zero? (get-heat state :corp)) "No heat"))))

(deftest turn-up-the-heat
  (doseq [s [:archives :council :commons]]
    (do-game
      (new-game {:corp {:hand ["Turn Up the Heat"]}})
      (click-credit state :corp)
      (click-credit state :runner)
      (delve-empty-server state :corp :council {:give-heat? nil})
      (is (changed? [(:credit (get-corp)) -1
                     (get-heat state :runner) 1]
            (click-prompts state :corp "Turn Up the Heat" "Yes"))
          "Turned up the heat"))))

(deftest likely-a-trap
  (doseq [opt ["Yes" "No"]]
    (do-game
      (new-game {:corp {:hand ["Likely a Trap" "Capricious Informant"]}
                 :runner {:deck [(qty "Fun Run" 10)]}})
      (play-from-hand state :corp "Capricious Informant" :council :outer)
      (click-credit state :runner)
      (click-credit state :corp)
      (delve-server state :runner :council)
      (delve-bypass-impl state :runner)
      (click-prompt state :corp "Likely a Trap")
      (click-prompt state :corp "Yes")
      (click-prompt state :runner opt)
      (if (= opt "No")
        (is (= 2 (count (get-discard state :runner))))
        (click-prompt state :runner "No Action")))))

(deftest twice-as-bad-test
  (do-game
    (new-game {:corp {:hand ["Likely a Trap" "Twice as Bad" "Barbican Gate"]}
               :runner {:deck [(qty "Fun Run" 10)]}})
    (play-from-hand state :corp "Barbican Gate" :council :outer)
    (click-credit state :runner)
    (click-credit state :corp)
    (delve-server state :runner :council)
    (delve-bypass-impl state :runner)
    (click-prompt state :corp "Likely a Trap")
    (click-prompt state :corp "Yes")
    (click-prompt state :runner "Yes")
    (is (changed? [(:credit (get-corp)) 1] ;; we spent 1 on twice as bad...
          (click-prompts state :corp "Yes" "Twice as Bad" "Yes"))
        "Gained 2 for 2 encounters")))
