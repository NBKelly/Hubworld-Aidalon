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

(deftest cornering-the-market-test
  (do-game
    (new-game {:corp {:hand ["Cornering the Market"]}
               :runner {:hand [(qty "Cornering the Market" 5)]}})
    (click-credit state :corp)
    (click-credit state :runner)
    (delve-empty-server state :corp :commons)
    (is (changed? [(:credit (get-corp)) 3]
          (click-prompts state :corp "Yes"))
        "Gained 3c")
    (is (no-prompt? state :corp))))

(deftest infiltrate
  (do-game
    (new-game {:corp {:hand ["Infiltrate"]}
               :runner {:hand [(qty "Fun Run" 5)]}})
    (click-credit state :corp)
    (click-credit state :runner)
    (delve-empty-server state :corp :council {:give-heat? true})
    (click-prompts state :corp "Yes" "No Action" "No Action" "No Action")
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

;; TODO - likely a trap tests

(deftest fun-run
  (do-game
    (new-game {:corp {:hand ["Fun Run"]}
               :runner {:hand [(qty "Fun Run" 5)]}})
    (click-credit state :corp)
    (click-credit state :runner)
    (delve-empty-server state :corp :commons)
    (is (changed? [(:credit (get-corp)) 3]
          (click-prompts state :corp "Yes"))
        "Gained 3c")
    (is (no-prompt? state :corp))))
