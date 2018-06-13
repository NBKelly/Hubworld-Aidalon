(ns game-test.cards.agendas
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "agendas"))

(deftest ^{:card-title "15-minutes"}
  fifteen-minutes
  ;; 15 Minutes - check if it works correctly from both sides
  (do-game
    (new-game (default-corp ["15 Minutes"])
              (default-runner))
    (play-from-hand state :corp "15 Minutes" "New remote")
    (take-credits state :corp)
    ;; use 15 minutes to take it away from runner
    (run-empty-server state "Server 1")
    (prompt-choice :runner "Steal")
    (take-credits state :runner)
    (is (= 1 (:agenda-point (get-runner))))
    (is (= 1 (count (:scored (get-runner)))))
    (let [fifm (first (:scored (get-runner)))]
      (is (= 3 (:click (get-corp))))
      (is (= 1 (count (:abilities (refresh fifm)))))
      (card-ability state :corp (refresh fifm) 0)
      (is (zero? (:agenda-point (get-runner))))
      (is (zero? (count (:scored (get-runner))))))
    (is (= "15 Minutes" (:title (first (:deck (get-corp))))))
    ;; TODO: could also check for deck shuffle
    (is (= 2 (:click (get-corp))))
    ;; use 15 minutes to take it away from corp (hey, maybe some obscure case happens where corp would want that)
    (core/click-draw state :corp 1)
    (play-from-hand state :corp "15 Minutes" "New remote")
    (take-credits state :runner)
    (score-agenda state :corp (get-content state :remote2 0))
    (is (= 1 (:agenda-point (get-corp))))
    (is (= 1 (count (:scored (get-corp)))))
    (let [fifm (first (:scored (get-corp)))]
      (is (= 1 (count (:abilities (refresh fifm)))))
      (card-ability state :corp (refresh fifm) 0)
      (is (zero? (:agenda-point (get-corp))))
      (is (zero? (count (:scored (get-corp))))))
    (is (= "15 Minutes" (:title (first (:deck (get-corp))))))))

(deftest accelerated-beta-test
  ;; Accelerated Beta Test
  (do-game
    (new-game (default-corp ["Accelerated Beta Test" "Enigma" (qty "Hedge Fund" 2)])
              (default-runner))
    ;; Set up
    (starting-hand state :corp ["Accelerated Beta Test"])
    (play-and-score state "Accelerated Beta Test")
    (prompt-choice :corp "Yes")
    (prompt-select :corp (find-card "Enigma" (get-in @state [:corp :play-area])))
    (prompt-choice :corp "HQ")
    (is (some? (get-ice state :hq 0)))
    (is (= 2 (count (:discard (get-corp)))))
    (core/move state :corp (find-card "Accelerated Beta Test" (:scored (get-corp))) :hand)
    (core/move state :corp (find-card "Hedge Fund" (:discard (get-corp))) :deck)
    (core/move state :corp (find-card "Hedge Fund" (:discard (get-corp))) :deck)
    (play-and-score state "Accelerated Beta Test")
    (prompt-choice :corp "Yes")
    (prompt-choice :corp "I have no regrets")
    (is (= 2 (count (:discard (get-corp)))))))

(deftest advanced-concept-hopper
  ;; Advanced Concept Hopper
  (do-game
    (new-game (default-corp ["Advanced Concept Hopper" (qty "Hedge Fund" 4)])
              (default-runner))
    (starting-hand state :corp ["Advanced Concept Hopper"])
    (play-and-score state "Advanced Concept Hopper")
    (take-credits state :corp)
    (testing "Corp draws 1 card, only once per turn"
      (let [cards (count (:hand (get-corp)))]
        (is (= cards (count (:hand (get-corp)))) (str "Corp should have " cards " cards in hand"))
        (run-on state :archives)
        (prompt-choice :corp "Draw 1 card")
        (is (= (inc cards) (count (:hand (get-corp)))) (str "Corp should have " (inc cards) " card in hand"))
        (run-successful state)
        (run-on state :archives)
        (is (empty (:prompt (get-corp))) "No prompt as it's once per turn")))
    (take-credits state :runner)
    (take-credits state :corp)
    (testing "Corp gains 1 credit, only once per turn"
      (let [credits (:credit (get-corp))]
        (is (= credits (:credit (get-corp))) (str "Corp should have " credits " credits"))
        (run-on state :archives)
        (prompt-choice :corp "Gain 1 [Credits]")
        (is (= (inc credits) (:credit (get-corp))) (str "Corp should have " (inc credits) " credits"))
        (run-successful state)
        (run-on state :archives)
        (is (empty (:prompt (get-corp))) "No prompt as it's once per turn")))))

(deftest ancestral-imager
  ;; Ancestral Imager
  (do-game
    (new-game (default-corp [(qty "Ancestral Imager" 3)])
              (default-runner))
    (play-and-score state "Ancestral Imager")
    (take-credits state :corp)
    (let [grip (count (:hand (get-runner)))]
      (is (= grip (count (:hand (get-runner)))) (str "Runner has " grip " cards in hand"))
      (run-on state :hq)
      (run-jack-out state)
      (is (= (dec grip) (count (:hand (get-runner)))) "Runner took 1 net damage"))))

(deftest ar-enhanced-security
  ;; AR-Enhanced Security
  (do-game
    (new-game (default-corp ["AR-Enhanced Security" (qty "NGO Front" 3)])
              (default-runner))
    (testing "set up"
      (core/gain state :corp :click 10 :credit 10)
      (core/gain state :runner :credit 10)
      (dotimes [_ 3]
        (play-from-hand state :corp "NGO Front" "New remote"))
      (take-credits state :corp))
    (testing "don't take a tag from trashing normally"
      (run-on state :remote1)
      (run-successful state)
      (prompt-choice-partial :runner "Pay")
      (is (= 1 (count (:discard (get-corp)))) "trashed")
      (is (zero? (:tag (get-runner))) "Runner took 0 tags")
      (take-credits state :runner)
      (play-and-score state "AR-Enhanced Security")
      (take-credits state :corp))
    (testing "gain a tag from first trash"
      (run-on state :remote2)
      (run-successful state)
      (prompt-choice-partial :runner "Pay")
      (is (= 2 (count (:discard (get-corp)))) "trashed")
      (is (= 1 (:tag (get-runner))) "Runner took 1 tag"))
    (testing "don't gain a tag from second trash"
      (run-on state :remote3)
      (run-successful state)
      (prompt-choice-partial :runner "Pay")
      (is (= 3 (count (:discard (get-corp)))) "trashed")
      (is (= 1 (:tag (get-runner))) "Runner took 0 tags"))))

(deftest armed-intimidation
  ;; Armed Intimidation
  (do-game
    (new-game (default-corp [(qty "Armed Intimidation" 2)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 2)]))
    (play-and-score state "Armed Intimidation")
    (prompt-choice :runner "Take 2 tags")
    (is (= 2 (:tag (get-runner))) "Runner took 2 tags from Armed Intimidation tag choice")
    (play-and-score state "Armed Intimidation")
    (is (= 5 (count (:hand (get-runner)))) "Runner has 5 cards before Armed Intimidation meat damage")
    (prompt-choice :runner "Suffer 5 meat damage")
    (is (zero? (count (:hand (get-runner)))) "Runner has 0 cards after Armed Intimidation meat damage")))

(deftest armored-servers
  ;; Armored Servers
  (do-game
    (new-game (default-corp ["Armored Servers"])
              (default-runner))
    (play-and-score state "Armored Servers")
    (let [as-scored (get-scored state :corp 0)]
      (is (= 1 (get-counters (refresh as-scored) :agenda)) "Should start with 1 agenda counters")
      (take-credits state :corp)
      (run-on state "HQ")
      (card-ability state :corp as-scored 0)
      (is (last-log-contains? state "make the Runner trash") "Should only write to log"))))

(deftest astroscript-pilot-program
  ;; AstroScript token placement
  (do-game
    (new-game (default-corp [(qty "AstroScript Pilot Program" 3) (qty "Ice Wall" 2)])
              (default-runner))
    (core/gain state :corp :click 3)
    (letfn [(try-place [from to]
              (card-ability state :corp (refresh from) 0)
              (prompt-select :corp (refresh to)))
            (should-not-place [from to msg]
              (try-place from to)
              (prompt-choice :corp "Done")
              (is (= 1 (get-counters (refresh from) :agenda))
                  (str (:title from)" token was not used on " (:title to) msg))
              (is (zero? (get-counters (refresh to) :advancement))
                  (str "Advancement token not placed on " (:title to) msg)))
            (should-place [from to msg]
              (try-place from to)
              (is (zero? (get-counters (refresh from) :agenda))
                  (str (:title from) " token was used on " (:title to) msg))
              (is (= 1 (get-counters (refresh to) :advancement))
                  (str "Advancement token placed on " (:title to) msg)))]
      (play-and-score state "AstroScript Pilot Program")
      (play-from-hand state :corp "AstroScript Pilot Program" "New remote")
      (let [scored-astro (get-scored state :corp 0)
            installed-astro (get-content state :remote2 0)
            hand-astro (find-card "AstroScript Pilot Program" (:hand get-corp))]
        (should-not-place scored-astro hand-astro " in hand")
        (should-place scored-astro installed-astro " that is installed")
        (advance state installed-astro 2)
        (core/score state :corp {:card (refresh installed-astro)}))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (let [no-token-astro (get-scored state :corp 0)
            token-astro (get-scored state :corp 1)
            hand-ice-wall (find-card "Ice Wall" (:hand get-corp))
            installed-ice-wall (get-ice state :hq 0)]
        (should-not-place token-astro no-token-astro " that is scored")
        (should-not-place token-astro hand-ice-wall " in hand")
        (should-place token-astro installed-ice-wall " that is installed")))))

(deftest award-bait
  ;; Award Bait
  (do-game
    (new-game (default-corp [(qty "Award Bait" 2) "Ice Wall"])
              (default-runner))
    (core/move state :corp (find-card "Award Bait" (:hand (get-corp))) :deck)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [iw (get-ice state :hq 0)]
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement tokens")
      (play-from-hand state :corp "Award Bait" "New remote")
      (take-credits state :corp)
      (run-on state :remote1)
      (run-successful state)
      (prompt-choice :corp "2")
      (prompt-select :corp (refresh iw))
      (prompt-choice :runner "Steal")
      (is (= 2 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 advancement tokens")
      (run-on state :rd)
      (run-successful state)
      (prompt-choice :runner "Access")
      (prompt-choice :corp "2")
      (prompt-select :corp (refresh iw))
      (prompt-choice :runner "Steal")
      (is (= 4 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 advancement tokens"))))

(deftest bacterial-programming
  ;; Bacterial Programming
  (testing "Scoring should not cause a run to exist for runner."
    (do-game
      (new-game (default-corp ["Bacterial Programming" "Hedge Fund"])
                (default-runner))
      (starting-hand state :corp ["Bacterial Programming"])
      (play-and-score state "Bacterial Programming")
      (prompt-choice :corp "Yes")
      (prompt-choice :corp "Done")
      (prompt-choice :corp "Done")
      (prompt-card :corp (first (:deck (get-corp))))
      (prompt-choice :corp "Done")
      (is (empty (:prompt (get-corp))) "Bacterial Programming prompts finished")
      (is (not (:run @state)) "No run is active")))
  (testing "Removing all cards from R&D should not freeze for runner, nor give an extra access."
    (do-game
      (new-game (default-corp [(qty "Bacterial Programming" 8)])
                (default-runner)
                {:start-as :runner})
      (starting-hand state :corp [])
      (run-empty-server state :rd)
      (prompt-choice :runner "Steal")
      (prompt-choice :corp "Yes")
      ;; Move all 7 cards to trash
      (doseq [_ (range 7)
              ;; Get the first card listed in the prompt choice
              ;; TODO make this function
              :let [card (-> @state
                             (get-in [:corp :prompt])
                             first
                             (get-in [:choices 0]))]]
        (prompt-card :corp card))
      (prompt-choice :corp "Done")                          ; Finished with trashing
      (prompt-choice :corp "Done")                          ; Finished with move-to-hq (no cards to move)
      ;; Run and prompts should be over now
      (is (empty (:prompt (get-corp))) "Bacterial Programming prompts finished")
      (is (empty (:prompt (get-runner))) "Bacterial Programming prompts finished")
      (is (not (:run @state))))))

(deftest better-citizen-program
  ;; Better Citizen Program
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Better Citizen Program"])
                (default-runner [(qty "The Maker's Eye" 2)
                                 (qty "Wyrm" 2)]))
      (play-and-score state "Better Citizen Program")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (is (zero? (:tag (get-runner))) "Runner starts with 0 tags")
      (play-from-hand state :runner "The Maker's Eye")
      (prompt-choice :corp "Yes")
      (is (= 1 (:tag (get-runner))) "Runner takes 1 tag for playing a Run event")
      (run-successful state)
      (play-from-hand state :runner "Wyrm")
      (is (empty? (-> (get-corp) :prompt)) "Corp shouldn't get a prompt to use Better Citizen Program")
      (is (= 1 (:tag (get-runner))) "Runner doesn't gain a tag from installing an icebreaker after playing a Run event")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Wyrm")
      (prompt-choice :corp "Yes")
      (is (= 2 (:tag (get-runner))) "Runner gains 1 tag for installing an Icebreaker")
      (play-from-hand state :runner "The Maker's Eye")
      (is (empty? (-> (get-corp) :prompt)) "Corp shouldn't get a prompt to use Better Citizen Program")
      (is (= 2 (:tag (get-runner))) "Runner doesn't gain a tag from playing a Run event after installing an Icebreaker")
      (run-successful state)))
  (testing "Should only trigger on Run events. #3619"
    (do-game
      (new-game (default-corp ["Better Citizen Program"])
                (default-runner ["Mining Accident"]))
      (play-and-score state "Better Citizen Program")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Mining Accident")
      (prompt-choice-partial :corp "Pay")
      (is (empty? (-> (get-corp) :prompt)) "Corp shouldn't get a prompt to use Better Citizen Program")
      (is (zero? (:tag (get-runner))) "Runner should not gain a tag from playing a non-Run event"))))

(deftest bifrost-array
  ;; Bifrost Array
  (do-game
    (new-game (default-corp ["Bifrost Array" "Hostile Takeover"])
              (default-runner))
    (play-and-score state "Hostile Takeover")
    (is (= 12 (:credit (get-corp))) "Should gain 7 credits from 5 to 12")
    (is (= 1 (:bad-publicity (get-corp))) "Should gain 1 bad publicity")
    (let [ht-scored (get-scored state :corp 0)]
      (play-and-score state "Bifrost Array")
      (prompt-choice :corp "Yes")
      (prompt-select :corp (refresh ht-scored))
      (is (= 19 (:credit (get-corp))) "Should gain 7 credits from 12 to 19")
      (is (= 2 (:bad-publicity (get-corp))) "Should gain 1 bad publicity"))))

(deftest brain-rewiring
  ;; Brain Rewiring
  (do-game
    (new-game (default-corp ["Brain Rewiring"])
              (default-runner))
    (starting-hand state :runner ["Sure Gamble" "Sure Gamble"])
    (play-and-score state "Brain Rewiring")
    (prompt-choice :corp "Yes")
    (prompt-choice :corp 2)
    (is (= 1 (count (:hand (get-runner)))))))

(deftest braintrust
  ;; Braintrust
  (do-game
    (new-game (default-corp ["Braintrust" "Ichi 1.0"])
              (default-runner))
    (play-from-hand state :corp "Braintrust" "New remote")
    (let [bt (get-content state :remote1 0)]
      (core/add-prop state :corp bt :advance-counter 7)
      (core/score state :corp {:card (refresh bt)})
      (let [scored-bt (get-scored state :corp 0)]
        (is (= 2 (get-counters (refresh scored-bt) :agenda))
            "Scored w/ 4 over-advancements; 2 agenda counters")
        (play-from-hand state :corp "Ichi 1.0" "HQ")
        (core/rez state :corp (get-ice state :hq 0))
        (is (= 2 (:credit (get-corp))) "2c discount to rez Ichi")))))

(deftest breaking-news
  ;; Breaking News
  (do-game
    (new-game (default-corp [(qty "Breaking News" 3)])
              (default-runner))
    (play-and-score state "Breaking News")
    (is (= 2 (get-in @state [:runner :tag])) "Runner receives 2 tags from Breaking News")
    (take-credits state :corp)
    (is (zero? (get-in @state [:runner :tag]))) "Two tags removed at the end of the turn"))

(deftest cfc-excavation-contract
  ;; CFC Excavation Contract
  (dotimes [n 5]
    (do-game
      (new-game (default-corp ["CFC Excavation Contract" (qty "Eli 1.0" n)])
                (default-runner))
      (core/gain state :corp :click 10 :credit 10)
      (is (= 15 (:credit (get-corp))) "Should start with 5 credits")
      (dotimes [_ n]
        (play-from-hand state :corp "Eli 1.0" "New remote")
        (core/rez state :corp (get-ice state (keyword (str "remote" (:rid @state))) 0)))
      (let [credit (:credit (get-corp))]
        (play-and-score state "CFC Excavation Contract")
        (is (= (+ credit (* 2 n)) (:credit (get-corp)))
            (str "Should now have with " (+ credit (* 2 n)) " credits"))))))

(deftest character-assassination
  ;; Character Assassination
  (do-game
    (new-game (default-corp ["Character Assassination"])
              (default-runner ["Fall Guy" "Kati Jones"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (play-from-hand state :runner "Fall Guy")
    (take-credits state :runner)
    (play-and-score state "Character Assassination")
    (let [kati (get-resource state 0)]
      (prompt-select :corp kati)
      (is (empty? (:prompt (get-runner))) "Fall Guy prevention didn't occur")
      (is (= 1 (count (:discard (get-runner)))) "Kati Jones trashed"))))

(deftest chronos-project
  ;; Chronos Project
  (do-game
    (new-game (default-corp ["Chronos Project"])
              (default-runner))
    (dotimes [_ 3]
      (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :discard))
    (is (= 3 (count (:discard (get-runner)))) "Runner should have 3 cards in heap")
    (play-and-score state "Chronos Project")
    (is (zero? (count (:discard (get-runner)))) "Runner should have 0 cards in heap")))

(deftest city-works-project
  ;; City Works Project
  (do-game
    (new-game (default-corp ["City Works Project"])
              (default-runner [(qty "Sure Gamble" 4)]))
    (play-from-hand state :corp "City Works Project" "New remote")
    (let [cwp (get-content state :remote1 0)]
      (core/advance state :corp {:card (refresh cwp)})
      (core/advance state :corp {:card (refresh cwp)}))
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (prompt-choice :runner "Yes")
    (is (= 4 (count (:discard (get-runner)))) "Runner paid 4 meat damage")))

(deftest clone-retirement
  ;; Clone Retirement
  (do-game
    (new-game (default-corp [(qty "Clone Retirement" 2) "Hostile Takeover"])
              (default-runner))
    (play-and-score state "Hostile Takeover")
    (is (= 12 (:credit (get-corp))))
    (is (= 1 (:bad-publicity (get-corp))))
    (play-and-score state "Clone Retirement")
    (is (zero? (:bad-publicity (get-corp))))
    (play-from-hand state :corp "Clone Retirement" "New remote")
    (take-credits state :corp)
    (run-on state "Server 3")
    (run-successful state)
    (prompt-choice :runner "Steal")
    (is (= 1 (:bad-publicity (get-corp))))))

(deftest corporate-sales-team
  ;; Corporate Sales Team
  (do-game
    (new-game (default-corp [(qty "Corporate Sales Team" 2)])
              (default-runner))
    (is (= 5 (:credit (get-corp))))
    (play-and-score state "Corporate Sales Team")
    (let [scored-cst (get-scored state :corp 0)]
      (core/end-turn state :corp nil)
      (core/start-turn state :runner nil)
      (is (= 6 (:credit (get-corp))) "Increments at runner's start of turn")
      (is (= 9 (get-counters (refresh scored-cst) :credit)))
      (core/end-turn state :runner nil)
      (core/start-turn state :corp nil)
      (is (= 7 (:credit (get-corp))) "Increments at corp's start of turn")
      (is (= 8 (get-counters (refresh scored-cst) :credit))))))

(deftest corporate-war
  ;; Corporate War
  (do-game
    (new-game (default-corp [(qty "Corporate War" 2)])
              (default-runner))
    (is (= 5 (:credit (get-corp))))
    (play-and-score state "Corporate War")
    (is (zero? (:credit (get-corp))) "Lost all credits")
    (core/gain state :corp :credit 7)
    (play-and-score state "Corporate War")
    (is (= 14 (:credit (get-corp))) "Had 7 credits when scoring, gained another 7")))

(deftest crisis-management
  ;; Crisis Management
  (do-game
    (new-game (default-corp ["Crisis Management"])
              (default-runner))
    (play-and-score state "Crisis Management")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 3 (count (:hand (get-runner)))) "No damage done, Runner not tagged")
    (take-credits state :corp)
    (core/gain state :runner :tag 1)
    (take-credits state :runner)
    (is (= 2 (count (:hand (get-runner)))) "Crisis Management dealt 1 meat damage")))

(deftest dedicated-neural-net
  ;; Dedicated Neural Net
  (do-game
    (new-game (default-corp ["Dedicated Neural Net" (qty "Scorched Earth" 2)
                             "Hedge Fund" "Caprice Nisei"])
              (default-runner ["HQ Interface"]))
    (play-from-hand state :corp "Caprice Nisei" "HQ")
    (play-and-score state "Dedicated Neural Net")
    (take-credits state :corp)
    (run-empty-server state :hq)
    (prompt-choice :runner "0")
    (prompt-choice :corp "1")
    (is (-> @state :run :run-effect :replace-access) "Replace-access tiggered")
    (prompt-select :corp (find-card "Hedge Fund" (:hand (get-corp))))
    (prompt-choice :runner "Card from hand")
    (is (accessing state "Hedge Fund") "Runner accessing Hedge Fund")
    (prompt-choice :runner "No action")
    ;; test for #2376
    (prompt-choice :runner "Unrezzed upgrade in HQ")
    (is (accessing state "Caprice Nisei") "Runner accessing Caprice")
    (prompt-choice :runner "No")
    (is (not (:run @state)) "Run completed")
    (run-empty-server state :hq)
    (prompt-choice :runner "No action")
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "HQ Interface")
    (run-empty-server state :hq)
    (prompt-choice :runner "0")
    (prompt-choice :corp "1")
    (is (= 2 (-> (get-corp) :selected first :max)) "Corp chooses 2 cards for Runner to access")))

(deftest degree-mill
  ;; Degree Mill
  (testing "Basic behavior"
    (do-game
      (new-game (default-corp [(qty "Degree Mill" 2)])
                (default-runner ["Ice Analyzer" "All-nighter" "Hunting Grounds"]))
      (play-from-hand state :corp "Degree Mill" "New remote")
      (take-credits state :corp)
      (is (= 0 (count (:deck (get-runner)))) "Runner starts with empty deck")
      (run-on state "Server 1")
      (run-successful state)
      (prompt-choice :runner "No action")
      (is (= 0 (:agenda-point (get-runner))) "Runner stole Degree Mill with no installed cards")
      (play-from-hand state :runner "Ice Analyzer")
      (play-from-hand state :runner "All-nighter")
      (let [ia (get-resource state 0)
            an (get-resource state 1)]
        (run-on state "Server 1")
        (run-successful state)
        (prompt-choice-partial :runner "Pay")
        (prompt-select :runner ia)
        (prompt-select :runner an)
        (is (= 3 (:agenda-point (get-runner))) "Runner failed to steal Degree Mill")
        (is (empty? (get-in @state [:runner :rig :resource])) "Degree Mill didn't remove installed cards")
        (is (= 2 (count (:deck (get-runner)))) "Degree Mill didn't put cards back in deck"))
      (take-credits state :runner)
      ;; Checking if facedowns work as well
      (play-from-hand state :corp "Degree Mill" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Hunting Grounds")
      (let [hg (get-resource state 0)]
        (run-on state "Server 2")
        (run-successful state)
        (prompt-choice :runner "No action")
        (is (= 3 (:agenda-point (get-runner))) "Runner stole Degree Mill with single card")
        (card-ability state :runner hg 1)
        (is (= 2 (count (get-in (get-runner) [:rig :facedown]))) "Hunting Ground did not install cards facedown")
        (is (empty? (:deck (get-runner))) "Hunting Grounds did not remove cards from deck")
        (let [fd1 (get-runner-facedown state 0)
              fd2 (get-runner-facedown state 1)]
          (run-on state "Server 2")
          (run-successful state)
          (prompt-choice-partial :runner "Pay")
          (prompt-select :runner fd1)
          (prompt-select :runner fd2)
          (is (= 6 (:agenda-point (get-runner))) "Runner failed to steal Degree Mill with facedown cards")
          (is (empty? (get-in (get-runner)  [:rig :facedown])) "Degree Mill didn't remove facedown cards")
          (is (= 2 (count (:deck (get-runner)))) "Degree Mill didn't put cards back in deck")))))
  (testing "Multiple steal costs"
    (do-game
      (new-game (default-corp [(qty "Degree Mill" 1) (qty "Strongbox" 1)])
                (default-runner [(qty "Ice Analyzer" 3) (qty "All-nighter" 3)]))
      (play-from-hand state :corp "Degree Mill" "New remote")
      (play-from-hand state :corp "Strongbox" "Server 1")
      (let [dm (get-content state :remote1 0)
            sb (get-content state :remote1 1)]
        (core/rez state :corp sb)
        (take-credits state :corp)
        (play-from-hand state :runner "Ice Analyzer")
        (play-from-hand state :runner "All-nighter")
        (run-empty-server state :remote1)
        (prompt-select :runner (refresh dm))
        (prompt-choice-partial :runner "Pay")
        (is (= 1 (:click (get-runner))) "Runner should start with 1 remaining click")
        (prompt-choice-partial :runner "Click")
        (is (zero? (:click (get-runner))) "Runner should have spent a click")
        (is (= 2 (count (get-in @state [:runner :rig :resource]))) "Runner starts with 2 resources")
        (prompt-choice-partial :runner "shuffling")
        (prompt-select :runner (get-resource state 1))
        (prompt-select :runner (get-resource state 0))
        (is (empty? (get-resource state)) "Degree Mill removed installed cards")
        (is (not-empty (get-scored state :runner)) "Runner stole an agenda")))))

(deftest director-haas'-pet-project
  ;; Director Haas' Pet Project
  (do-game
    (new-game (default-corp ["Director Haas' Pet Project"
                             "Adonis Campaign"
                             "Strongbox"
                             "Eli 1.0"
                             (qty "Hedge Fund" 5)])
              (default-runner))
    (starting-hand state :corp ["Director Haas' Pet Project" "Adonis Campaign" "Strongbox"])
    (core/move state :corp (find-card "Eli 1.0" (:deck (get-corp))) :discard)
    (play-and-score state "Director Haas' Pet Project")
    (prompt-choice :corp "Yes")
    (prompt-select :corp (find-card "Adonis Campaign" (:hand (get-corp))))
    (prompt-select :corp (find-card "Strongbox" (:hand (get-corp))))
    (prompt-select :corp (find-card "Eli 1.0" (:discard (get-corp))))))

(deftest domestic-sleepers
  ;; Domestic Sleepers
  (do-game
    (new-game (default-corp ["Domestic Sleepers"])
              (default-runner))
    (play-and-score state "Domestic Sleepers")
    (core/gain state :corp :click 3)
    (let [ds_scored (get-scored state :corp 0)]
      (is (zero? (get-counters (refresh ds_scored) :agenda)) "Should start with 0 agenda counters")
      (is (zero? (:agenda-point (get-corp))) "Should provide 0 agenda points initially")
      (card-ability state :corp ds_scored 0)
      (is (= 1 (get-counters (refresh ds_scored) :agenda)) "Should gain 1 agenda counter")
      (is (= 1 (:agenda-point (get-corp))) "Should provide 1 agenda point after ability use"))))

(deftest eden-fragment
  ;; Test that Eden Fragment ignores the install cost of the first ice
  (do-game
    (new-game (default-corp [(qty "Eden Fragment" 3) (qty "Ice Wall" 3)])
              (default-runner))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-and-score state "Eden Fragment")
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (some? (get-ice state :hq 1)) "Corp has two ice installed on HQ")
    (is (= 6 (:credit (get-corp))) "Corp does not pay for installing the first ICE of the turn")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (some? (get-ice state :hq 2)) "Corp has three ice installed on HQ")
    (is (= 4 (:credit (get-corp))) "Corp pays for installing the second ICE of the turn")))

(deftest efficiency-committee
  ;; Efficiency Committee
  (do-game
    (new-game (default-corp [(qty "Efficiency Committee" 3) (qty "Shipment from SanSan" 2)
                             "Ice Wall"])
              (default-runner))
    (core/gain state :corp :click 4)
    (play-from-hand state :corp "Efficiency Committee" "New remote")
    (play-from-hand state :corp "Efficiency Committee" "New remote")
    (play-from-hand state :corp "Efficiency Committee" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [ec1 (get-content state :remote1 0)
          ec2 (get-content state :remote2 0)
          ec3 (get-content state :remote3 0)
          iw (get-ice state :hq 0)]
      (score-agenda state :corp ec1)
      (let [ec1_scored (get-scored state :corp 0)]
        (is (= 3 (get-counters (refresh ec1_scored) :agenda)))
        (is (= 2 (:agenda-point (get-corp))))
        ;; use token
        (is (= 3 (:click (get-corp))))
        (card-ability state :corp ec1_scored 0)
        (is (= 4 (:click (get-corp))))
        ;; try to advance Ice Wall
        (advance state iw)
        (is (= 4 (:click (get-corp))))
        (is (zero? (get-counters (refresh iw) :advancement)))
        ;; try to advance Efficiency Committee
        (advance state ec2)
        (is (= 4 (:click (get-corp))))
        (is (zero? (get-counters (refresh ec2) :advancement)))
        ;; advance with Shipment from SanSan
        (play-from-hand state :corp "Shipment from SanSan")
        (prompt-choice :corp "2")
        (prompt-select :corp ec2)
        (is (= 2 (get-counters (refresh ec2) :advancement)))
        (play-from-hand state :corp "Shipment from SanSan")
        (prompt-choice :corp "2")
        (prompt-select :corp ec2)
        (is (= 4 (get-counters (refresh ec2) :advancement)))
        (core/score state :corp {:card (refresh ec2)})
        (is (= 4 (:agenda-point (get-corp))))
        (take-credits state :corp)
        (take-credits state :runner)
        ;; can advance again
        (advance state iw)
        (is (= 1 (get-counters (refresh iw) :advancement)))
        (advance state ec3)
        (is (= 1 (get-counters (refresh ec3) :advancement)))))))

(deftest elective-upgrade
  ;; Elective Upgrade
  (do-game
    (new-game (default-corp ["Elective Upgrade"])
              (default-runner))
    (play-and-score state "Elective Upgrade")
    (let [eu-scored (get-scored state :corp 0)]
      (is (= 2 (get-counters (refresh eu-scored) :agenda)) "Should start with 2 agenda counters")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 3 (:click (get-corp))) "Should start with 4 clicks")
      (card-ability state :corp eu-scored 0)
      (card-ability state :corp eu-scored 0)
      (is (= 4 (:click (get-corp))) "Should gain 2 clicks, not 3")
      (is (= 1 (get-counters (refresh eu-scored) :agenda)) "Should still have 1 agenda counter"))))

(deftest encrypted-portals
  ;; Encrypted Portals
  (do-game
    (new-game (default-corp ["Encrypted Portals" "Lotus Field"])
              (default-runner))
    (play-from-hand state :corp "Lotus Field" "HQ")
    (let [lf (get-ice state :hq 0)]
      (core/rez state :corp lf)
      (is (= 4 (:current-strength (refresh lf))) "Should start with base strength of 4")
      (is (zero? (:credit (get-corp))) "Should have 0 credits after rez")
      (play-and-score state "Encrypted Portals")
      (is (= 5 (:current-strength (refresh lf))) "Should gain 1 strength from 4 to 5")
      (is (= 1 (:credit (get-corp))) "Should gain 1 credit for rezzed code gate"))))

(deftest escalate-vitriol
  ;; Escalate Vitriol
  (do-game
    (new-game (default-corp ["Escalate Vitriol"])
              (default-runner))
    (core/lose state :corp :credit 5)
    (play-and-score state "Escalate Vitriol")
    (let [ev-scored (get-scored state :corp 0)]
      (dotimes [tag 10]
        (is (zero? (:tag (get-runner))) "Should start with 0 tags")
        (is (zero? (:credit (get-corp))) "Should start with 0 credits")
        (core/gain state :runner :tag tag)
        (card-ability state :corp ev-scored 0)
        (is (= tag (:credit (get-corp))) (str "Should gain " tag " credits"))
        (take-credits state :corp)
        (take-credits state :runner)
        (core/lose state :corp :credit (:credit (get-corp)))
        (core/lose state :runner :tag tag)))))

(deftest executive-retreat
  ;; Executive Retreat
  (do-game
    (new-game (default-corp ["Executive Retreat" (qty "Hedge Fund" 5)])
              (default-runner))
    (starting-hand state :corp ["Executive Retreat" "Hedge Fund"])
    (is (= 2 (count (:hand (get-corp)))) "Corp should start with 1 card in HQ")
    (play-and-score state "Executive Retreat")
    (is (zero? (count (:hand (get-corp)))) "Corp should have 0 cards in HQ after shuffling HQ back into R&D")
    (let [er-scored (get-scored state :corp 0)]
      (card-ability state :corp er-scored 0)
      (is (= 5 (count (:hand (get-corp)))) "Corp should have 5 cards in hand")
      (is (zero? (get-counters (refresh er-scored) :agenda)) "Executive Retreat should have 0 agenda counters")))
  (testing "Overdraw"
    (do-game
      (new-game (default-corp ["Executive Retreat" (qty "Hedge Fund" 4)])
                (default-runner))
      (starting-hand state :corp ["Executive Retreat" "Hedge Fund"])
      (is (= 2 (count (:hand (get-corp)))) "Corp should start with 1 card in HQ")
      (play-and-score state "Executive Retreat")
      (is (zero? (count (:hand (get-corp)))) "Corp should have 0 cards in HQ after shuffling HQ back into R&D")
      (let [er-scored (get-scored state :corp 0)]
        (card-ability state :corp er-scored 0)
        (is (= 4 (count (:hand (get-corp)))) "Corp should have 5 cards in hand")
        (is (zero? (get-counters (refresh er-scored) :agenda)) "Executive Retreat should have 0 agenda counters")
        (is (= :runner (:winner @state)) "Runner wins")
        (is (= "Decked" (:reason @state)) "Win condition reports decked")))))

(deftest explode-a-palooza
  ;; Explode-a-palooza
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Explode-a-palooza"])
                (default-runner))
      (play-from-hand state :corp "Explode-a-palooza" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (prompt-choice :runner "Access")
      (prompt-choice :runner "Steal")
      (prompt-choice :corp "Yes")
      (is (= 12 (:credit (get-corp))) "Gained 5 credits")))
  (testing "Interaction with The Turning Wheel. Issue #1717."
    (do-game
      (new-game (default-corp [(qty "Explode-a-palooza" 3)])
                (default-runner ["The Turning Wheel"]))
      (starting-hand state :corp ["Explode-a-palooza" "Explode-a-palooza"])
      (play-from-hand state :corp "Explode-a-palooza" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "The Turning Wheel")
      (run-empty-server state :remote1)
      (prompt-choice :runner "Access")
      (prompt-choice :corp "Yes")
      (prompt-choice :runner "Steal")
      (let [ttw (get-resource state 0)]
        (is (zero? (get-counters (refresh ttw) :power)) "TTW did not gain counters")
        (is (= 1 (count (:scored (get-runner)))) "Runner stole Explodapalooza")
        (is (= 12 (:credit (get-corp))) "Gained 5 credits")
        (run-empty-server state :rd)
        (prompt-choice :runner "Access")
        (prompt-choice :corp "Yes")
        (prompt-choice :runner "Steal")
        (is (zero? (get-counters (refresh ttw) :power)) "TTW did not gain counters")
        (is (= 2 (count (:scored (get-runner)))) "Runner stole Explodapalooza")
        (is (= 17 (:credit (get-corp))) "Gained 5 credits")))))

(deftest false-lead
  ;; False Lead
  (do-game
    (new-game (default-corp ["False Lead"])
              (default-runner))
    (play-and-score state "False Lead")
    (is (= 1 (count (:scored (get-corp)))) "Corp should have 1 agenda point")
    (take-credits state :corp)
    (is (= 4 (:click (get-runner))) "Runner should start turn with 4 clicks")
    (card-ability state :corp (get-scored state :corp 0) 0)
    (is (= 2 (:click (get-runner))) "Runner should lose 2 clicks from False Lead")))

(deftest fetal-ai
  ;; Fetal AI
  (testing "basic test"
    (do-game
      (new-game (default-corp [(qty "Fetal AI" 3)])
                (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]))
      (play-from-hand state :corp "Fetal AI" "New remote")
      (take-credits state :corp 2)
      (run-empty-server state "Server 1")
      (prompt-choice-partial :runner "Pay")
      (is (= 3 (count (:hand (get-runner)))) "Runner took 2 net damage from Fetal AI")
      (is (= 3 (:credit (get-runner))) "Runner paid 2cr to steal Fetal AI")
      (is (= 1 (count (:scored (get-runner)))) "Runner stole Fetal AI"))
    (testing "can't afford to steal"
      (do-game
        (new-game (default-corp [(qty "Fetal AI" 3)])
                  (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]))
        (play-from-hand state :corp "Fetal AI" "New remote")
        (take-credits state :corp 2)
        (core/lose state :runner :credit 5)
        (run-empty-server state "Server 1")
        (prompt-choice :runner "Yes")
        (is (= 3 (count (:hand (get-runner)))) "Runner took 2 net damage from Fetal AI")
        (is (zero? (count (:scored (get-runner)))) "Runner could not steal Fetal AI")))))

(deftest firmware-updates
  ;; Firmware Updates
  (do-game
    (new-game (default-corp ["Firmware Updates"
                             "Ice Wall"])
              (default-runner))
    (play-and-score state "Firmware Updates")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [fu (get-scored state :corp 0)
          iw (get-ice state :hq 0)]
      (is (= 3 (get-counters (refresh fu) :agenda)) "Firmware Updates should start with 3 agenda counters")
      (core/rez state :corp iw)
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement tokens")
      (card-ability state :corp fu 0)
      (prompt-select :corp (refresh iw))
      (is (= 2 (get-counters (refresh fu) :agenda)) "Firmware Updates should now have 2 agenda counters")
      (is (= 1 (get-counters (refresh iw) :advancement)) "Ice Wall should have 1 advancement token"))))

(deftest geothermal-fracking
  ;; Geothermal Fracking
  (testing "basic test"
    (do-game
      (new-game (default-corp ["Geothermal Fracking"])
                (default-runner))
      (play-and-score state "Geothermal Fracking")
      (is (= 2 (:click (get-corp))) "Should have 2 clicks left")
      (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
      (is (zero? (:bad-publicity (get-corp))) "Should start with 0 bad publicity")
      (let [gf-scored (get-scored state :corp 0)]
        (is (= 2 (get-counters (refresh gf-scored) :agenda)) "Should start with 2 agenda counters")
        (card-ability state :corp gf-scored 0)
        (is (= 1 (:click (get-corp))) "Should have 1 click left")
        (is (= 12 (:credit (get-corp))) "Should gain 7 credits from 5 to 12")
        (is (= 1 (:bad-publicity (get-corp))) "Should gain 1 bad publicity"))))
  (testing "prevented bad publicity shouldn't block credit gain"
    (do-game
      (new-game (default-corp ["Geothermal Fracking" "Broadcast Square"])
                (default-runner))
      (play-and-score state "Geothermal Fracking")
      (is (= 2 (:click (get-corp))) "Should have 2 clicks left")
      (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
      (is (zero? (:bad-publicity (get-corp))) "Should start with 0 bad publicity")
      (play-from-hand state :corp "Broadcast Square" "New remote")
      (let [gf-scored (get-scored state :corp 0)
            bs (get-content state :remote2 0)]
        (core/rez state :corp bs)
        (is (= 2 (get-counters (refresh gf-scored) :agenda)) "Should start with 2 agenda counters")
        (card-ability state :corp gf-scored 0)
        (prompt-choice :corp 0)
        (prompt-choice :runner 0)
        (is (zero? (:click (get-corp))) "Should have 0 click left")
        (is (= 10 (:credit (get-corp))) "Should gain 7 credits from 3 to 10")
        (is (zero? (:bad-publicity (get-corp))) "Should gain 0 bad publicity from prevention")))))

(deftest genetic-resequencing
  ;; Genetic Resequencing
  (do-game
    (new-game (default-corp ["Genetic Resequencing" (qty "Braintrust" 2)])
              (default-runner))
    (play-from-hand state :corp "Braintrust" "New remote")
    (play-from-hand state :corp "Braintrust" "New remote")
    (play-from-hand state :corp "Genetic Resequencing" "New remote")
    (let [bt1 (get-content state :remote1 0)
          bt2 (get-content state :remote2 0)
          gr (get-content state :remote3 0)]
      (score-agenda state :corp bt1)
      (let [btscored (get-scored state :corp 0)]
        (is (zero? (get-counters (refresh btscored) :agenda)) "No agenda counters on scored Braintrust")
        (score-agenda state :corp gr)
        (prompt-select :corp bt2)
        (is (zero? (get-counters (refresh bt2) :agenda))
            "No agenda counters on installed Braintrust; not a valid target")
        (prompt-select :corp btscored)
        (is (= 1 (get-counters (refresh btscored) :agenda))
            "1 agenda counter placed on scored Braintrust")))))

(deftest gila-hands-arcology
  ;; Gila Hands Arcology
  (do-game
    (new-game (default-corp ["Gila Hands Arcology"])
              (default-runner))
    (play-and-score state "Gila Hands Arcology")
    (is (= 2 (:click (get-corp))) "Should have 2 clicks left")
    (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
    (core/gain state :corp :click 2)
    (let [gha-scored (get-scored state :corp 0)]
      (card-ability state :corp gha-scored 0)
      (is (= 2 (:click (get-corp))) "Should spend 2 clicks on Gila Hands")
      (is (= 8 (:credit (get-corp))) "Should gain 3 credits from 5 to 8")
      (card-ability state :corp gha-scored 0)
      (is (zero? (:click (get-corp))) "Should spend 2 clicks on Gila Hands")
      (is (= 11 (:credit (get-corp))) "Should gain 3 credits from 8 to 11"))))

(deftest glenn-station
  ;; Glenn Station
  (do-game
    (new-game (default-corp ["Glenn Station" "Ice Wall"])
              (default-runner))
    (play-and-score state "Glenn Station")
    (let [gs-scored (get-scored state :corp 0)]
      (card-ability state :corp gs-scored 0)
      (prompt-card :corp (find-card "Ice Wall" (:hand (get-corp))))
      (is (= 1 (count (:hosted (refresh gs-scored)))))
      (card-ability state :corp gs-scored 1)
      (prompt-card :corp (find-card "Ice Wall" (:hosted (refresh gs-scored))))
      (is (zero? (count (:hosted (refresh gs-scored))))))))

(deftest global-food-initiative
  ;; Global Food Initiative
  (do-game
    (new-game (default-corp [(qty "Global Food Initiative" 2)])
              (default-runner))
    (testing "Corp scores"
      (is (zero? (:agenda-point (get-runner))) "Runner should start with 0 agenda points")
      (is (zero? (:agenda-point (get-corp))) "Corp should start with 0 agenda points")
      (play-and-score state "Global Food Initiative")
      (is (= 3 (:agenda-point (get-corp))) "Corp should gain 3 agenda points"))
    (testing "Runner steals"
      (play-from-hand state :corp "Global Food Initiative" "New remote")
      (take-credits state :corp)
      (run-on state :remote2)
      (run-successful state)
      (prompt-choice :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Runner should gain 2 agenda points, not 3"))))

(deftest government-contracts
  ;; Government Contracts
  (do-game
    (new-game (default-corp ["Government Contracts"])
              (default-runner))
    (play-and-score state "Government Contracts")
    (is (= 2 (:click (get-corp))))
    (card-ability state :corp (get-scored state :corp 0) 0)
    (is (zero? (:click (get-corp))) "Spent 2 clicks")
    (is (= 9 (:credit (get-corp))) "Gained 4 credits")))

(deftest government-takeover
  ;; Government Takeover
  (do-game
    (new-game (default-corp ["Government Takeover"])
              (default-runner))
    (play-and-score state "Government Takeover")
    (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
    (let [gt-scored (get-scored state :corp 0)]
      (card-ability state :corp gt-scored 0)
      (is (= 8 (:credit (get-corp))) "Should gain 3 credits from 5 to 8"))))

(deftest graft
  ;; Graft
  (letfn [(graft-test [[number-of-picks deck-size]]
            (let [cards ["Ice Wall" "Fire Wall" "Orion"]]
              (do-game
                (new-game (default-corp ["Graft" "Ice Wall"
                                         "Fire Wall" "Orion"])
                          (default-runner))
                (starting-hand state :corp ["Graft"])
                (play-and-score state "Graft")
                (dotimes [current-pick number-of-picks]
                  (prompt-card :corp (find-card (nth cards current-pick) (:deck (get-corp)))))
                (is (= number-of-picks (count (:hand (get-corp)))))
                (is (= deck-size (count (:deck (get-corp))))))))]
    (doall (map graft-test
                [[0 3]
                 [1 2]
                 [2 1]
                 [3 0]]))))

(deftest hades-fragment
  ;; Hades Fragment
  (do-game
    (new-game (default-corp ["Hades Fragment" (qty "Hedge Fund" 2)])
              (default-runner))
    (starting-hand state :corp ["Hades Fragment"])
    (play-and-score state "Hades Fragment")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 1 (count (:hand (get-corp)))) "Corp should have no opportunity to use Hades Shard")
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :discard)
    (take-credits state :corp)
    (take-credits state :runner)
    (let [hf-scored (get-scored state :corp 0)]
      (card-ability state :corp hf-scored 0)
      (prompt-select :corp (find-card "Hedge Fund" (:discard (get-corp))))
      (is (= 2 (count (:deck (get-corp)))) "R&D should have 2 cards in it after Hades Fragment use"))))

(deftest helium-3-deposit
  ;; Helium-3 Deposit
  (do-game
    (new-game (default-corp ["Helium-3 Deposit"
                             "Chief Slee"
                             "Ice Wall"])
              (default-runner))
    (play-from-hand state :corp "Chief Slee" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (let [cs (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (is (zero? (get-counters (refresh cs) :power)) "Chief Slee should start with 0 power counters")
      (core/rez state :corp iw)
      (run-on state "HQ")
      (card-ability state :corp cs 0)
      (is (= 1 (get-counters (refresh cs) :power)) "Chief Slee should gain 1 power counter")
      (take-credits state :runner)
      (play-and-score state "Helium-3 Deposit")
      (prompt-choice :corp "2")
      (prompt-select :corp cs)
      (is (= 3 (get-counters (refresh cs) :power)) "Chief Slee should gain 2 power counters from 1 to 3"))))

(deftest high-risk-investment
  ;; High-Risk Investment
  (do-game
    (new-game (default-corp ["High-Risk Investment"])
              (default-runner))
    (play-and-score state "High-Risk Investment")
    (let [hri-scored (get-scored state :corp 0)]
      (is (= 1 (get-counters (refresh hri-scored) :agenda)) "Has 1 agenda counter")
      (take-credits state :corp)
      (is (= 7 (:credit (get-corp))))
      (take-credits state :runner)
      (is (= 9 (:credit (get-runner))))
      (card-ability state :corp hri-scored 0)
      (is (= 16 (:credit (get-corp))) "Gained 9 credits")
      (is (= 2 (:click (get-corp))) "Spent 1 click")
      (is (zero? (get-counters (refresh hri-scored) :agenda)) "Spent agenda counter"))))

(deftest hollywood-renovation
  ;; Hollywood Renovation
  (do-game
    (new-game (default-corp ["Hollywood Renovation" "Ice Wall"])
              (default-runner))
    (core/gain state :corp :click 10 :credit 10)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Hollywood Renovation" "New remote")
    (let [hr (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (is (zero? (get-counters (refresh hr) :advancement)) "Hollywood Renovation should start with 0 advancement tokens")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement tokens")
      (dotimes [n 5]
        (advance state (refresh hr))
        (prompt-select :corp (refresh iw)))
      (is (= 5 (get-counters (refresh hr) :advancement)) "Hollywood Renovation should gain 5 advancement tokens")
      (is (= 5 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 5 advancement tokens")
      (advance state (refresh hr))
      (prompt-select :corp (refresh iw))
      (is (= 6 (get-counters (refresh hr) :advancement)) "Hollywood Renovation should gain 1 from 5 to 6 advancement tokens")
      (is (= 7 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 from 5 to 7 advancement tokens"))))

(deftest hostile-takeover
  ;; Hostile Takeover
  (do-game
    (new-game (default-corp ["Hostile Takeover"])
              (default-runner))
    (play-and-score state "Hostile Takeover")
    (is (= 12 (:credit (get-corp))) "Gain 7 credits")
    (is (= 1 (:bad-publicity (get-corp))) "Take 1 bad publicity")))

(deftest house-of-knives
  ;; House of Knives
  (do-game
    (new-game (default-corp ["House of Knives"])
              (default-runner))
    (play-and-score state "House of Knives")
    (let [hok-scored (get-scored state :corp 0)]
      (is (= 3 (get-counters (refresh hok-scored) :agenda)) "House of Knives should start with 3 counters")
      (take-credits state :corp)
      (run-empty-server state "R&D")
      (run-phase-43 state)
      (card-ability state :corp hok-scored 0)
      (is (= 1 (count (:discard (get-runner)))) "Runner should pay 1 net damage")
      (run-empty-server state "R&D")
      (run-phase-43 state)
      (card-ability state :corp hok-scored 0)
      (card-ability state :corp hok-scored 0)
      (is (= 2 (count (:discard (get-runner)))) "Runner should pay 1 net damage"))))

(deftest ikawah-project
  ;; Ikawah Project
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Ikawah Project"])
                (default-runner))
      (play-from-hand state :corp "Ikawah Project" "New remote")
      (testing "No credits"
        (take-credits state :corp)
        (core/lose state :runner :credit (:credit (get-runner)) :click 3)
        (run-empty-server state :remote1)
        (prompt-choice :runner "No action")
        (is (zero? (:credit (get-runner))) "Runner couldn't afford to steal, so no credits spent")
        (is (zero? (count (:scored (get-runner)))) "Runner could not steal Ikawah Project"))
      (testing "No clicks"
        (take-credits state :runner)
        (take-credits state :corp)
        (core/lose state :runner :credit (:credit (get-runner)) :click 3)
        (run-empty-server state :remote1)
        (prompt-choice :runner "No action")
        (is (zero? (:click (get-runner))) "Runner couldn't afford to steal, so no clicks spent")
        (is (zero? (count (:scored (get-runner)))) "Runner could not steal Ikawah Project"))
      (testing "Enough of both"
        (take-credits state :runner)
        (take-credits state :corp)
        (core/lose state :runner :credit (:credit (get-runner)) :click (:click (get-runner)))
        (core/gain state :runner :credit 5 :click 4)
        (is (= 5 (:credit (get-runner))) "Runner should be reset to 5 credits")
        (is (= 4 (:click (get-runner))) "Runner should be reset to 4 clicks")
        (run-empty-server state :remote1)
        (prompt-choice-partial :runner "Pay")
        (prompt-choice :runner "[Click]")
        (prompt-choice :runner "2 [Credits]")
        (is (= 2 (:click (get-runner))) "Runner should lose 1 click to steal")
        (is (= 3 (:credit (get-runner))) "Runner should lose 2 credits to steal")
        (is (= 3 (:agenda-point (get-runner))))
        (is (= 1 (count (:scored (get-runner)))) "Runner should steal Ikawah Project"))))
  (testing "Not stealing"
    ;; do not reveal when the Runner does not steal from R&D
    (do-game
      (new-game (default-corp [(qty "Ikawah Project" 2)])
                (default-runner))
      (take-credits state :corp)
      (starting-hand state :corp ["Ikawah Project"])
      (run-empty-server state "R&D")
      (prompt-choice :runner "No action")
      (is (not (last-log-contains? state "Ikawah Project")) "Ikawah Project should not be mentioned")
      (run-empty-server state "HQ")
      (prompt-choice :runner "No action")
      (is (last-log-contains? state "Ikawah Project") "Ikawah Project should be mentioned"))))

(deftest illicit-sales
  ;; Illicit Sales
  (letfn [(illicit-sales-test [[starting-bp answer credits-gained]]
            (testing (str "starting with " starting-bp " and answering " answer " and gaining " credits-gained)
              (do-game
                (new-game (default-corp ["Illicit Sales"])
                          (default-runner))
                (let [credits (:credit (get-corp))]
                  (core/gain state :corp :bad-publicity starting-bp)
                  (play-and-score state "Illicit Sales")
                  (prompt-choice :corp answer)
                  (is (= (:credit (get-corp)) (+ credits credits-gained)))))))]
    (doall (map illicit-sales-test
                [[0 "No" 0]
                 [0 "Yes" 3]
                 [1 "No" 3]
                 [1 "Yes" 6]
                 [2 "No" 6]
                 [2 "Yes" 9]
                 [3 "No" 9]
                 [3 "Yes" 12]]))))

(deftest improved-protein-source
  ;; Improved Protein Source
  (do-game
    (new-game (default-corp [(qty "Improved Protein Source" 2)])
              (default-runner))
    (is (= 5 (:credit (get-runner))) "Runner starts with 5 credits")
    (play-and-score state "Improved Protein Source")
    (is (= 9 (:credit (get-runner))) "Runner should gain 4 credits from Corp scoring")
    (play-from-hand state :corp "Improved Protein Source" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote2)
    (prompt-choice :runner "Steal")
    (is (= 13 (:credit (get-runner))) "Runner should gain 4 credits from Corp scoring")))

(deftest improved-tracers
  ;; Improved Tracers
  (do-game
    (new-game (default-corp ["Improved Tracers" "News Hound" "Restructured Datapool"])
              (default-runner))
    (play-from-hand state :corp "News Hound" "HQ")
    (play-and-score state "Restructured Datapool")
    (let [nh (get-ice state :hq 0)
          rd (get-scored state :corp 0)]
      (core/rez state :corp nh)
      (is (= 4 (:current-strength (refresh nh))) "Should start with base strength of 4")
      (is (= 3 (:credit (get-corp))) "Should have 2 credits after rez")
      (play-and-score state "Improved Tracers")
      (is (= 5 (:current-strength (refresh nh))) "Should gain 1 strength from 4 to 5")
      (take-credits state :corp)
      (run-on state "HQ")
      (run-phase-43 state)
      (card-subroutine state :corp nh 0)
      (is (= 1 (-> (get-corp) :prompt first :bonus)) "Should gain 1 bonus trace strength")
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= 1 (:tag (get-runner))))
      (run-jack-out state)
      (run-on state "HQ")
      (run-phase-43 state)
      (card-subroutine state :corp nh 0)
      (is (= 1 (-> (get-corp) :prompt first :bonus))
          "Should gain only 1 bonus trace strength regardless of number of runs in a turn")
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= 2 (:tag (get-runner))))
      (take-credits state :runner)
      (card-ability state :corp rd 0)
      (is (zero? (-> (get-corp) :prompt first :bonus)) "Should gain 0 bonus trace strength"))))

(deftest labyrinthine-servers
  ;; Labyrinthine Servers
  (do-game
    (new-game (default-corp [(qty "Labyrinthine Servers" 2)])
              (default-runner))
    (play-and-score state "Labyrinthine Servers")
    (play-and-score state "Labyrinthine Servers")
    (take-credits state :corp)
    (let [ls1 (get-scored state :corp 0)
          ls2 (get-scored state :corp 1)]
      (is (= 2 (get-counters (refresh ls1) :power)))
      (is (= 2 (get-counters (refresh ls2) :power)))
      (testing "Don't use token"
        (run-on state "HQ")
        (run-jack-out state)
        (is (:run @state) "Jack out prevent prompt")
        (prompt-choice :corp "Done")
        (is (not (:run @state)) "Corp does not prevent the jack out, run ends"))
      (testing "Use token"
        (run-on state "HQ")
        (run-jack-out state)
        (card-ability state :corp ls1 0)
        (card-ability state :corp ls2 0)
        (card-ability state :corp ls1 0)
        (prompt-choice :corp "Done")
        (is (:run @state) "Jack out prevented, run is still ongoing")
        (is (true? (get-in @state [:run :cannot-jack-out])) "Cannot jack out flag is in effect")
        (run-successful state)
        (is (not (:run @state))))
      (testing "one Labyrinthine is empty but the other still has one token, ensure prompt still occurs"
        (is (zero? (get-counters (refresh ls1) :power)))
        (is (= 1 (get-counters (refresh ls2) :power)))
        (run-on state "HQ")
        (run-jack-out state)
        (is (:run @state))
        (card-ability state :corp ls2 0)
        (prompt-choice :corp "Done")
        (is (true? (get-in @state [:run :cannot-jack-out])))
        (run-successful state)
        (is (not (:run @state))))
      (testing "No more tokens"
        (run-on state "HQ")
        (run-jack-out state)
        (is (not (:run @state)) "No jack out prevent prompt")))))

(deftest license-acquisition
  ;; License Acquisition
  (do-game
    (new-game (default-corp [(qty "License Acquisition" 4)
                             "Adonis Campaign" "Eve Campaign"
                             "Strongbox" "Corporate Troubleshooter"])
              (default-runner))
    (testing "Set up"
      (starting-hand state :corp ["License Acquisition" "License Acquisition" "License Acquisition" "License Acquisition"
                                  "Adonis Campaign" "Strongbox"])
      (core/move state :corp (find-card "Eve Campaign" (:deck (get-corp))) :discard)
      (core/move state :corp (find-card "Corporate Troubleshooter" (:deck (get-corp))) :discard)
      (core/gain state :corp :click 4))
    (testing "Asset & HQ"
      (play-and-score state "License Acquisition")
      (prompt-select :corp (find-card "Adonis Campaign" (:hand (get-corp))))
      (prompt-choice :corp "New remote")
      (is (some? (get-content state :remote2 0))))
    (testing "Upgrade & HQ"
      (play-and-score state "License Acquisition")
      (prompt-select :corp (find-card "Strongbox" (:hand (get-corp))))
      (prompt-choice :corp "New remote")
      (is (some? (get-content state :remote4 0))))
    (testing "Asset & Archives"
      (play-and-score state "License Acquisition")
      (prompt-select :corp (find-card "Eve Campaign" (:discard (get-corp))))
      (prompt-choice :corp "New remote")
      (is (some? (get-content state :remote6 0))))
    (testing "Upgrade & Archives"
      (play-and-score state "License Acquisition")
      (prompt-select :corp (find-card "Corporate Troubleshooter" (:discard (get-corp))))
      (prompt-choice :corp "New remote")
      (is (some? (get-content state :remote8 0))))))

(deftest mandatory-seed-replacement
  ;; Mandatory Seed Replacement
  (do-game
    (new-game (default-corp ["Mandatory Seed Replacement"
                             "Ice Wall" "Fire Wall"
                             "Kakugo" "Chum"
                             "RSVP" "Sensei"])
              (default-runner))
    (core/click-draw state :corp 2)
    (core/gain state :corp :click 10 :credit 10)
    (play-from-hand state :corp "Ice Wall" "Archives")
    (play-from-hand state :corp "Fire Wall" "R&D")
    (play-from-hand state :corp "Kakugo" "HQ")
    (play-from-hand state :corp "Chum" "Archives")
    (play-from-hand state :corp "RSVP" "R&D")
    (play-from-hand state :corp "Sensei" "HQ")
    (let [iw (get-ice state :archives 0)
          fw (get-ice state :rd 0)
          kk (get-ice state :hq 0)
          ch (get-ice state :archives 1)
          rs (get-ice state :rd 1)
          sn (get-ice state :hq 1)]
      (core/rez state :corp iw)
      (core/rez state :corp fw)
      (core/rez state :corp kk)
      (core/rez state :corp ch)
      (core/rez state :corp rs)
      (core/rez state :corp sn)
      (play-and-score state "Mandatory Seed Replacement")
      (prompt-select :corp (refresh iw))
      (prompt-select :corp (refresh fw))
      (prompt-select :corp (refresh kk))
      (prompt-select :corp (refresh ch))
      (prompt-select :corp (refresh rs))
      (prompt-select :corp (refresh sn)))))

(deftest mandatory-upgrades
  ;; Mandatory Upgrades
  (testing "Gain an additional click"
    (do-game
      (new-game (default-corp ["Mandatory Upgrades"
                               "Melange Mining Corp."])
                (default-runner))
      (play-and-score state "Mandatory Upgrades")
      (is (= 2 (:agenda-point (get-corp))))
      (play-from-hand state :corp "Melange Mining Corp." "New remote")
      (let [mmc (get-content state :remote2 0)]
        (core/rez state :corp mmc)
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 4 (:click (get-corp))))
        (card-ability state :corp mmc 0)
        (is (= 1 (:click (get-corp)))))))
  (testing "Lose additional click if sacrificed"
    (do-game
      (new-game (default-corp ["Mandatory Upgrades"
                               "Archer"])
                (default-runner))
      (play-and-score state "Mandatory Upgrades")
      (is (= 2 (:agenda-point (get-corp))))
      (play-from-hand state :corp "Archer" "HQ")
      (take-credits state :corp)
      (take-credits state :runner)
      (let [arc (get-ice state :hq 0)
            mu (get-scored state :corp 0)]
        (is (= 4 (:click (get-corp))) "Corp should start turn with 4 clicks")
        (core/rez state :corp arc)
        (prompt-select :corp (refresh mu))
        (is (= 3 (:click (get-corp))) "Corp should lose 1 click on agenda sacrifice")))))

(deftest market-research
  ;; Market Research
  (do-game
    (new-game (default-corp [(qty "Market Research" 2)])
              (default-runner))
    (testing "Runner is not tagged"
      (play-and-score state "Market Research")
      (is (= 2 (:agenda-point (get-corp))) "Only 4 advancements: scored for standard 2 points"))
    (testing "Runner is tagged"
      (core/gain state :runner :tag 1)
      (play-and-score state "Market Research")
      (is (= 5 (:agenda-point (get-corp))) "5 advancements: scored for 3 points"))))

(deftest medical-breakthrough
  ;; Medical Breakthrough
  (do-game
    (new-game (default-corp [(qty "Medical Breakthrough" 3) (qty "Hedge Fund" 3)])
              (default-runner))
    (play-from-hand state :corp "Medical Breakthrough" "New remote")
    (play-from-hand state :corp "Medical Breakthrough" "New remote")
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (prompt-choice :runner "Steal")
    (take-credits state :runner)
    (let [mb2 (get-content state :remote2 0)]
      (advance state mb2 3)
      (core/score state :corp {:card (refresh mb2)})
      (is (= 2 (:agenda-point (get-corp))) "Only needed 3 advancements to score"))
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Medical Breakthrough" "New remote")
    (let [mb3 (get-content state :remote3 0)]
      (advance state mb3 2)
      (core/score state :corp {:card (refresh mb3)})
      (is (= 4 (:agenda-point (get-corp))) "Only needed 2 advancements to score"))))

(deftest merger
  ;; Merger
  (do-game
    (new-game (default-corp [(qty "Merger" 2)])
              (default-runner))
    (play-and-score state "Merger")
    (is (= 2 (:agenda-point (get-corp))) "Corp should score 2 points")
    (play-from-hand state :corp "Merger" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote2)
    (prompt-choice :runner "Steal")
    (is (= 3 (:agenda-point (get-runner))) "Runner should score 3 points")))

(deftest meteor-mining
  ;; Meteor Mining
  (testing "when Meteor Mining is stolen"
    (do-game
      (new-game (default-corp ["Meteor Mining"])
                (default-runner))
      (play-from-hand state :corp "Meteor Mining" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (prompt-choice :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Runner should score 2 points")))
  (testing "when Meteor Mining is scored"
    (letfn [(meteor-mining-test [[tags num-choices pick creds dmg]]
              (do-game
                (new-game (default-corp ["Meteor Mining"])
                          (default-runner [(qty "Sure Gamble" 7)]))
                (starting-hand state :runner (repeat 7 "Sure Gamble"))
                (let [credits (:credit (get-corp))
                      grip (count (:hand (get-runner)))]
                  (core/gain state :runner :tag tags)
                  (play-and-score state "Meteor Mining")
                  (is (= num-choices (count (:choices (first (get-in @state [:corp :prompt]))))))
                  (prompt-choice :corp pick)
                  (is (= (+ credits creds) (:credit (get-corp)))
                      (str "Corp should have " (+ credits creds) " credits"))
                  (is (= (- grip dmg) (count (:hand (get-runner))))
                      (str "Runner should have " (- grip dmg) " cards in hand")))))]
      (doall (map meteor-mining-test
                  [[0 2 "No action" 0 0]
                   [0 2 "Gain 7 [Credits]" 7 0]
                   [1 2 "No action" 0 0]
                   [1 2 "Gain 7 [Credits]" 7 0]
                   [2 3 "No action" 0 0]
                   [2 3 "Gain 7 [Credits]" 7 0]
                   [2 3 "Do 7 meat damage" 0 7]
                   [3 3 "No action" 0 0]
                   [3 3 "Gain 7 [Credits]" 7 0]
                   [3 3 "Do 7 meat damage" 0 7]])))))

(deftest napd-contract
  ;; NAPD Contract
  (testing "basic test"
    (do-game
      (new-game (default-corp ["NAPD Contract"])
                (default-runner))
      (play-from-hand state :corp "NAPD Contract" "New remote")
      (let [napd (get-content state :remote1 0)]
        (advance state napd 2)
        (take-credits state :corp)
        (core/lose state :runner :credit 2)
        (run-empty-server state "Server 1")
        (prompt-choice :runner "Yes")
        (is (zero? (count (:scored (get-runner)))) "Runner could not steal NAPD Contract")
        (is (= 3 (:credit (get-runner))) "Runner couldn't afford to steal, so no credits spent")
        (take-credits state :runner)
        (core/gain state :corp :bad-publicity 1)
        (advance state napd 2)
        (core/score state :corp {:card (refresh napd)})
        (is (some? (get-content state :remote1 0))
            "Corp can't score with 4 advancements because of BP")
        (advance state napd)
        (core/score state :corp {:card (refresh napd)})
        (is (= 2 (:agenda-point (get-corp))) "Scored NAPD for 2 points after 5 advancements"))))
  (testing "scoring requirement increases with bad publicity from Corporate Scandal"
    (do-game
      (new-game (default-corp ["NAPD Contract"])
                (default-runner ["Corporate Scandal"]))
      (play-from-hand state :corp "NAPD Contract" "New remote")
      (let [napd (get-content state :remote1 0)]
        (advance state napd 2)
        (take-credits state :corp)
        (play-from-hand state :runner "Corporate Scandal")
        (take-credits state :runner)
        (advance state napd 2)
        (core/score state :corp {:card (refresh napd)})
        (is (some? (get-content state :remote1 0))
            "Corp can't score with 4 advancements because of BP")
        (advance state napd)
        (core/score state :corp {:card (refresh napd)})
        (is (= 2 (:agenda-point (get-corp))) "Scored NAPD for 2 points after 5 advancements")))))

(deftest net-quarantine
  ;; Net Quarantine
  (do-game
    (new-game (default-corp ["Net Quarantine"])
              (default-runner))
    (core/gain state :runner :link 1)
    (core/gain state :corp :click 3)
    (play-and-score state "Net Quarantine")
    (let [credits (:credit (get-corp))]
      (is (= credits (:credit (get-corp))) (str "Corp has " credits " credits"))
      (is (= 1 (:link (get-runner))) "Runner has 1 link")
      (core/init-trace state :corp {:title "/trace command" :side :corp} {:base 1})
      (prompt-choice :corp 0)
      (is (zero? (-> (get-runner) :prompt first :link)) "Runner has 0 link during first trace")
      (prompt-choice :runner 3)
      (is (= (+ credits 1) (:credit (get-corp))) "Corp gained a credit from NQ")
      ; second trace of turn - no link reduction
      (core/init-trace state :corp {:title "/trace command" :side :corp} {:base 1})
      (prompt-choice :corp 0)
      (is (= 1 (-> (get-runner) :prompt first :link)) "Runner has 1 link during later traces")
      (prompt-choice :runner 2)
      (is (= (+ credits 2) (:credit (get-corp))) "Corp gained a credit from NQ"))))

(deftest new-construction
  ;; New Construction
  (do-game
    (new-game (default-corp ["New Construction" (qty "Commercial Bankers Group" 10)])
              (default-runner))
    (starting-hand state :corp (vec (cons "New Construction" (repeat 10 "Commercial Bankers Group"))))
    (core/gain state :corp :click 10 :credit 10)
    (play-from-hand state :corp "New Construction" "New remote")
    (let [nc (get-content state :remote1 0)]
      (is (zero? (get-counters (refresh nc) :advancement)))
      (dotimes [n 4]
        (advance state (refresh nc))
        (prompt-choice :corp "Yes")
        (prompt-select :corp (find-card "Commercial Bankers Group" (:hand (get-corp)))))
      (is (= 4 (get-counters (refresh nc) :advancement)))
      (is (not= :this-turn (:rezzed (get-content state :remote5 0))))
      (let [credits (:credit (get-corp))]
        (advance state (refresh nc))
        (prompt-choice :corp "Yes")
        (prompt-select :corp (find-card "Commercial Bankers Group" (:hand (get-corp))))
        (is (= 5 (get-counters (refresh nc) :advancement)))
        (is (= :this-turn (:rezzed (get-content state :remote6 0))))
        (is (= (dec credits) (:credit (get-corp))))))))

(deftest next-wave-2
  ;; NEXT Wave 2
  (do-game
    (new-game (default-corp [(qty "NEXT Wave 2" 2) "NEXT Bronze"])
              (default-runner))
    (is (zero? (:brain-damage (get-runner))) "Runner should start with 0 brain damage")
    (play-from-hand state :corp "NEXT Bronze" "HQ")
    (let [nxbr (get-ice state :hq 0)]
      (core/rez state :corp nxbr))
    (play-and-score state "NEXT Wave 2")
    (prompt-choice :corp "No")
    (is (zero? (:brain-damage (get-runner))) "Runner should stay at 0 brain damage")
    (play-and-score state "NEXT Wave 2")
    (prompt-choice :corp "Yes")
    (is (= 1 (:brain-damage (get-runner))) "Runner should gain 1 brain damage")))

(deftest nisei-mk-ii
  ;; Nisei MK II - Remove hosted counter to ETR, check this works in 4.3
  (do-game
    (new-game (default-corp ["Nisei MK II"])
              (default-runner))
    (play-and-score state "Nisei MK II")
    (let [scored-nisei (get-scored state :corp 0)]
      (is (= 1 (get-counters (refresh scored-nisei) :agenda)) "Scored Nisei has one counter")
      (take-credits state :corp)
      (run-on state "HQ")
      (run-phase-43 state)
      (card-ability state :corp (refresh scored-nisei) 0)
      (prompt-choice :corp "Done") ; close 4.3 corp
      (is (not (:run @state)) "Run ended by using Nisei counter")
      (is (zero? (get-counters (refresh scored-nisei) :agenda)) "Scored Nisei has no counters"))))

(deftest oaktown-renovation
  ;; Oaktown Renovation
  (do-game
    (new-game (default-corp ["Oaktown Renovation" "Shipment from SanSan"])
              (default-runner))
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (let [oak (get-content state :remote1 0)]
      (is (:rezzed (refresh oak)) "Oaktown installed face up")
      (advance state oak)
      (is (= 6 (:credit (get-corp))) "Spent 1 credit to advance, gained 2 credits from Oaktown")
      (play-from-hand state :corp "Shipment from SanSan")
      (prompt-choice :corp "2")
      (prompt-select :corp oak)
      (is (= 3 (get-counters (refresh oak) :advancement)))
      (is (= 6 (:credit (get-corp))) "No credits gained due to advancements being placed")
      (advance state oak)
      (is (= 7 (:credit (get-corp))) "Spent 1 credit to advance, gained 2 credits from Oaktown")
      (advance state oak)
      (is (= 5 (get-counters (refresh oak) :advancement)))
      (is (= 9 (:credit (get-corp)))
          "Spent 1 credit to advance, gained 3 credits from Oaktown"))))

(deftest obokata-protocol
  ;; Obotaka Protocol
  (do-game
    (new-game (make-deck "Jinteki: Personal Evolution" [(qty "Obokata Protocol" 10)])
              (default-runner [(qty "Sure Gamble" 4)]))
    (play-from-hand state :corp "Obokata Protocol" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :agenda-point 6)
    (run-empty-server state "Server 1")
    (prompt-choice-partial :runner "Pay")
    (is (= 4 (count (:discard (get-runner)))) "Runner paid 4 net damage")
    (is (= :runner (:winner @state)) "Runner wins")
    (is (= "Agenda" (:reason @state)) "Win condition reports agenda points")
    (is (last-log-contains? state "wins the game") "PE did not fire")))

(deftest paper-trail
  ;; Paper Trail
  (do-game
    (new-game (default-corp ["Paper Trail"])
              (default-runner ["Aeneas Informant" "Bank Job"
                               "Rosetta 2.0" "Magnum Opus"
                               "Astrolabe"]))
    (take-credits state :corp)
    (core/gain state :runner :click 10 :credit 10)
    (play-from-hand state :runner "Aeneas Informant")
    (play-from-hand state :runner "Bank Job")
    (play-from-hand state :runner "Rosetta 2.0")
    (play-from-hand state :runner "Magnum Opus")
    (play-from-hand state :runner "Astrolabe")
    (take-credits state :runner)
    (play-and-score state "Paper Trail")
    (prompt-choice :corp 0)
    (prompt-choice :runner 0)
    (is (= 2 (count (:discard (get-runner)))))
    (is (some? (get-resource state 0)))
    (is (= 1 (count (get-resource state))))
    (is (some? (get-program state 0)))
    (is (some? (get-hardware state 0)))))

(deftest personality-profiles
  ;; Personality Profiles
  (testing "basic test"
    (do-game
      (new-game (default-corp ["Personality Profiles"])
                (default-runner ["Self-modifying Code" "Clone Chip"
                                 "Corroder" (qty "Patron" 2)]))
      (starting-hand state :runner ["Self-modifying Code" "Clone Chip" "Patron" "Patron"])
      (play-and-score state "Personality Profiles")
      (take-credits state :corp)
      (play-from-hand state :runner "Self-modifying Code")
      (play-from-hand state :runner "Clone Chip")
      (let [smc (get-program state 0)]
        (card-ability state :runner smc 0)
        (prompt-card :runner (find-card "Corroder" (:deck (get-runner))))
        (is (= 2 (count (:discard (get-runner))))))
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (prompt-select :runner (find-card "Self-modifying Code" (:discard (get-runner))))
        (is (second-last-log-contains? state "Patron")
            "Personality Profiles trashed card name is in log")
        (is (= 3 (count (:discard (get-runner))))))))
  (testing "Ensure effects still fire with an empty hand, #1840"
    (do-game
      (new-game (default-corp ["Personality Profiles"])
                (default-runner ["Self-modifying Code" "Clone Chip"
                                 "Corroder"]))
      (starting-hand state :runner ["Self-modifying Code" "Clone Chip"])
      (play-and-score state "Personality Profiles")
      (take-credits state :corp)
      (play-from-hand state :runner "Self-modifying Code")
      (play-from-hand state :runner "Clone Chip")
      (let [smc (get-program state 0)]
        (card-ability state :runner smc 0)
        (prompt-card :runner (find-card "Corroder" (:deck (get-runner)))))
      (let [cor (get-program state 0)]
        (is (some? cor))
        (is (= (:title cor) "Corroder"))
        (is (= "Self-modifying Code" (:title (first (:discard (get-runner)))))))
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (prompt-select :runner (find-card "Self-modifying Code" (:discard (get-runner)))))
      (let [smc (get-program state 1)]
        (is (some? smc))
        (is (= (:title smc) "Self-modifying Code"))
        (is (= "Clone Chip" (:title (first (:discard (get-runner))))))))))

(deftest philotic-entanglement
  ;; Philotic Entanglement
  (do-game
    (new-game (default-corp ["Philotic Entanglement" (qty "House of Knives" 3)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Cache" 2)]))
    (play-from-hand state :corp "House of Knives" "New remote")
    (play-from-hand state :corp "House of Knives" "New remote")
    (play-from-hand state :corp "House of Knives" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (prompt-choice :runner "Steal")
    (run-empty-server state :remote2)
    (prompt-choice :runner "Steal")
    (run-empty-server state :remote3)
    (prompt-choice :runner "Steal")
    (is (= 3 (count (:scored (get-runner)))))
    (take-credits state :runner)
    (play-and-score state "Philotic Entanglement")
    (is (= 2 (:agenda-point (get-corp))))
    (is (= 3 (count (:discard (get-runner)))) "Dealt 3 net damage upon scoring")))

(deftest posted-bounty
  ;; Posted Bounty
  (testing "Forfeiting takes 1 bad publicity"
    (do-game
      (new-game (default-corp ["Posted Bounty"])
                (default-runner))
      (play-and-score state "Posted Bounty")
      (prompt-choice :corp "Yes")
      (is (zero? (:agenda-point (get-corp))) "Forfeiting Posted Bounty nullifies agenda points")
      (is (= 1 (:bad-publicity (get-corp))) "Forfeiting takes 1 bad publicity")
      (is (= 1 (:tag (get-runner))) "Runner receives 1 tag forfeiting Posted Bounty")))
  (testing "Choosing not to forfeit scores normally"
    (do-game
      (new-game (default-corp ["Posted Bounty"])
                (default-runner))
      (play-and-score state "Posted Bounty")
      (prompt-choice :corp "No")
      (is (= 1 (:agenda-point (get-corp))))
      (is (zero? (:bad-publicity (get-corp))))
      (is (zero? (:tag (get-runner)))))))

(deftest priority-requisition
  ;; Priority Requisition
  (do-game
    (new-game (default-corp ["Priority Requisition" "Archer"])
              (default-runner))
    (play-from-hand state :corp "Archer" "HQ")
    (let [arc (get-ice state :hq 0)]
      (play-and-score state "Priority Requisition")
      (prompt-select :corp arc)
      (is (:rezzed (refresh arc))))))

(deftest private-security-force
  ;; Private Security Force
  (do-game
    (new-game (default-corp [(qty "Private Security Force" 10)])
              (default-runner))
    (core/gain state :runner :tag 1)
    (play-and-score state "Private Security Force")
    (let [psf-scored (get-scored state :corp 0)]
      (card-ability state :corp psf-scored 0)
      (is (= 1 (count (:discard (get-runner)))))
      (take-credits state :runner)
      (dotimes [n 3]
        (card-ability state :corp psf-scored 0))
      (is (= 3 (count (:discard (get-runner)))))
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest profiteering
  ;; Profiteering
  (do-game
    (new-game (default-corp ["Profiteering"])
              (default-runner))
    (play-and-score state "Profiteering")
    (prompt-choice :corp "3")
    (is (= 1 (:agenda-point (get-corp))))
    (is (= 3 (:bad-publicity (get-corp))) "Took 3 bad publicity")
    (is (= 20 (:credit (get-corp))) "Gained 15 credits")))

(deftest project-ares
  ;; Project Ares
  (do-game
    (new-game (default-corp [(qty "Project Ares" 2)])
              (default-runner ["Clone Chip"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Clone Chip")
    (take-credits state :runner)
    (play-and-score state "Project Ares")
    (is (empty? (get-in @state [:runner :prompt])) "No prompt for Runner if scored with 4 advancement tokens")
    (core/gain state :corp :click 5)
    (play-from-hand state :corp "Project Ares" "New remote")
    (let [ares (get-content state :remote2 0)]
      (advance state ares 6)
      (is (= 6 (get-counters (refresh ares) :advancement)))
      (core/score state :corp {:card (refresh ares)})
      (is (prompt-is-card? :runner ares) "Runner has Ares prompt to trash installed cards"))
    (prompt-select :runner (find-card "Clone Chip" (:hardware (:rig (get-runner)))))
    (is (empty? (get-in @state [:runner :prompt])) "Runner must trash 2 cards but only has 1 card in rig, prompt ended")
    (is (= 1 (count (:discard (get-runner)))))
    (is (= 1 (:bad-publicity (get-corp))))))

(deftest project-atlas
  ;; Project Atlas
  (testing "basic test"
    (do-game
      (new-game (default-runner ["Project Atlas"
                                 "Beanstalk Royalties"])
                (default-runner))
      ;; Set up
      (starting-hand state :corp ["Project Atlas"])
      (is (= 1 (count (:hand (get-corp)))) "Corp should have 1 cards in hand")
      (core/gain state :corp :click 10 :credit 10)
      ;; Should gain 1 counter
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atlas (get-content state :remote1 0)]
        (advance state atlas 4)
        (is (= 4 (get-counters (refresh atlas) :advancement)) "Atlas should have 4 advancement tokens")
        (core/score state :corp {:card (refresh atlas)}))
      (let [atlas-scored (get-scored state :corp 0)]
        (is (= 1 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 1 agenda counter")
        (card-ability state :corp atlas-scored 0)
        (prompt-card :corp (find-card "Beanstalk Royalties" (:deck (get-corp))))
        (is (zero? (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 0 agenda counters")
        (is (= 1 (count (:hand (get-corp)))) "Corp should have 1 cards in hand"))))
  (testing "test with Titan"
    (do-game
      (new-game (make-deck "Titan Transnational: Investing In Your Future"
                           [(qty "Project Atlas" 2) "Beanstalk Royalties" "Hedge Fund"])
                (default-runner))
      ;; Set up
      (starting-hand state :corp ["Project Atlas" "Project Atlas"])
      (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 cards in hand")
      (core/gain state :corp :click 10 :credit 10)
      ;; Should gain 1 counter
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atlas (get-content state :remote1 0)]
        (advance state atlas 3)
        (is (= 3 (get-counters (refresh atlas) :advancement)) "Atlas should have 3 advancement tokens")
        (core/score state :corp {:card (refresh atlas)}))
      (let [atlas-scored (get-scored state :corp 0)]
        (is (= 1 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 1 agenda counter")
        (card-ability state :corp atlas-scored 0)
        (prompt-card :corp (find-card "Beanstalk Royalties" (:deck (get-corp))))
        (is (zero? (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 0 agenda counters")
        (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 card in hand"))
      ;; Should gain 2 counters
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atlas (get-content state :remote2 0)]
        (advance state atlas 4)
        (is (= 4 (get-counters (refresh atlas) :advancement)) "Atlas should have 4 advancement tokens")
        (core/score state :corp {:card (refresh atlas)}))
      (let [atlas-scored (get-scored state :corp 1)]
        (is (= 2 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 2 agenda counter")
        (card-ability state :corp atlas-scored 0)
        (prompt-card :corp (find-card "Hedge Fund" (:deck (get-corp))))
        (is (= 1 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 1 agenda counters")
        (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 cards in hand")))))

(deftest project-beale
  ;; Project Beale
  (do-game
    (new-game (default-corp [(qty "Project Beale" 2)])
              (default-runner))
    (core/gain state :corp :click 8 :credit 8)
    (play-from-hand state :corp "Project Beale" "New remote")
    (let [pb1 (get-content state :remote1 0)]
      (advance state pb1 4)
      (core/score state :corp {:card (refresh pb1)})
      (is (= 2 (:agenda-point (get-corp))) "Only 4 advancements: scored for standard 2 points")
      (play-from-hand state :corp "Project Beale" "New remote"))
    (let [pb2 (get-content state :remote2 0)]
      (advance state pb2 5)
      (core/score state :corp {:card (refresh pb2)})
      (is (= 5 (:agenda-point (get-corp))) "5 advancements: scored for 3 points"))))

(deftest project-kusanagi
  ;; Project Kusanagi
  (do-game
    (new-game (default-corp [(qty "Project Kusanagi" 2) "Ice Wall"])
              (default-runner))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (core/gain state :corp :click 10 :credit 10)
    (testing "Should gain 0 counters"
      (play-and-score state "Project Kusanagi")
      (let [pk-scored (get-scored state :corp 0)]
        (is (zero? (get-counters (refresh pk-scored) :agenda)) "Kusanagi should start with 0 agenda counters")))
    (testing "Should gain 1 counter"
      (play-from-hand state :corp "Project Kusanagi" "New remote")
      (let [pk (get-content state :remote2 0)]
        (advance state pk 3)
        (is (= 3 (get-counters (refresh pk) :advancement)) "Kusanagi should have 3 advancement tokens")
        (core/score state :corp {:card (refresh pk)}))
      (let [pk-scored (get-scored state :corp 1)]
        (is (= 1 (get-counters (refresh pk-scored) :agenda)) "Kusanagi should have 1 agenda counter")
        (run-empty-server state :hq)
        (card-ability state :corp pk-scored 0)
        (is (last-log-contains? state "Do 1 net damage"))
        (is (zero? (get-counters (refresh pk-scored) :agenda)) "Kusanagi should have 0 agenda counters")))))

(deftest project-vitruvius
  ;; Project Vitruvius
  (do-game
    (new-game (default-corp ["Project Vitruvius"
                             "Hedge Fund"])
              (default-runner))
    ;; Set up
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :discard)
    (is (= 1 (count (:discard (get-corp)))) "Corp should have 1 cards in hand")
    (is (= 1 (count (:hand (get-corp)))) "Corp should have 1 cards in hand")
    (core/gain state :corp :click 10 :credit 10)
    ;; Should gain 1 counter
    (play-from-hand state :corp "Project Vitruvius" "New remote")
    (let [vit (get-content state :remote1 0)]
      (advance state vit 4)
      (is (= 4 (get-counters (refresh vit) :advancement)) "Vitruvius should have 4 advancement tokens")
      (core/score state :corp {:card (refresh vit)}))
    (let [vit-scored (get-scored state :corp 0)]
      (is (= 1 (get-counters (refresh vit-scored) :agenda)) "Vitruvius should have 1 agenda counter")
      (card-ability state :corp vit-scored 0)
      (prompt-select :corp (find-card "Hedge Fund" (:discard (get-corp))))
      (is (zero? (get-counters (refresh vit-scored) :agenda)) "Vitruvius should have 0 agenda counters")
      (is (= 1 (count (:hand (get-corp)))) "Corp should have 1 cards in hand"))))

(deftest project-wotan
  ;; Project Wotan - Only checks if agenda counter is spent
  (do-game
    (new-game (default-corp ["Project Wotan"
                             "Eli 1.0"
                             (qty "Hedge Fund" 3)])
              (default-runner))
    (starting-hand state :corp ["Project Wotan" "Eli 1.0"])
    (play-from-hand state :corp "Eli 1.0" "HQ")
    (let [eli (get-ice state :hq 0)]
      (core/rez state :corp eli))
    (play-and-score state "Project Wotan")
    (take-credits state :corp)
    (let [wot-scored (get-scored state :corp 0)]
      (is (= 3 (get-counters (refresh wot-scored) :agenda)) "Wotan should start with 3 agenda counters")
      (run-on state "HQ")
      (card-ability state :corp wot-scored 0)
      (is (= 2 (get-counters (refresh wot-scored) :agenda))) "Wotan should only have 2 agenda counters")))

(deftest puppet-master
  ;; Puppet Master - game progresses if no valid targets. Issue #1661.
  (do-game
    (new-game (default-corp ["Puppet Master"])
              (default-runner))
    (play-and-score state "Puppet Master")
    (take-credits state :corp)
    (run-empty-server state :archives)
    (prompt-choice :corp "Done")
    (is (empty? (:prompt (get-runner))) "Runner's waiting prompt resolved")))

(deftest quantum-predictive-model
  ;; Quantum Predictive Model
  (do-game
    (new-game (default-corp [(qty "Quantum Predictive Model" 4)])
              (default-runner))
    (testing "Set up"
      (starting-hand state :corp ["Quantum Predictive Model" "Quantum Predictive Model"])
      (play-from-hand state :corp "Quantum Predictive Model" "New remote")
      (play-from-hand state :corp "Quantum Predictive Model" "New remote")
      (take-credits state :corp))
    (testing "Access installed with no tag"
      (run-on state :remote1)
      (run-successful state)
      (prompt-choice :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))) "Runner should steal"))
    (testing "Access R&D with no tag"
      (run-on state :rd)
      (run-successful state)
      (prompt-choice :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Runner should steal"))
    (core/gain state :runner :tag 1)
    (testing "Access intalled with tag"
      (run-on state :remote2)
      (run-successful state)
      (prompt-choice :runner "No action") ;; this is now a prompt that QPM was added to Corp score area
      (is (= 2 (:agenda-point (get-runner))) "Runner should not steal")
      (is (= 1 (:agenda-point (get-corp))) "Corp should score"))
    (testing "Access R&D with tag"
      (run-on state :rd)
      (run-successful state)
      (prompt-choice :runner "No action")
      (is (= 2 (:agenda-point (get-runner))) "Runner should not steal")
      (is (= 2 (:agenda-point (get-corp))) "Corp should score"))
    (is (zero? (count (:deck (get-corp)))))))

(deftest rebranding-team
  ;; Rebranding Team
  (do-game
    (new-game (default-corp ["Rebranding Team" "Launch Campaign" "City Surveillance"
                             "Jackson Howard" "Museum of History" "Advanced Assembly Lines"])
              (default-runner))
    (play-and-score state "Rebranding Team")
    (core/click-draw state :runner 1)
    (is (core/has-subtype? (find-card "Advanced Assembly Lines" (:hand (get-corp))) "Advertisement"))
    ; #2608 part 2 - retain Advertisement always
    (trash-from-hand state :corp "Advanced Assembly Lines")
    (is (core/has-subtype? (find-card "Advanced Assembly Lines" (:discard (get-corp))) "Advertisement"))
    (is (core/has-subtype? (find-card "Launch Campaign" (:hand (get-corp))) "Advertisement"))
    (is (core/has-subtype? (find-card "City Surveillance" (:hand (get-corp))) "Advertisement"))
    (is (core/has-subtype? (find-card "Jackson Howard" (:hand (get-corp))) "Advertisement"))
    (is (core/has-subtype? (find-card "Jackson Howard" (:hand (get-corp))) "Executive"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Advertisement"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Alliance"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Ritzy"))
    (core/move state :corp (find-card "Rebranding Team" (:scored (get-corp))) :deck)
    (is (core/has-subtype? (find-card "Launch Campaign" (:hand (get-corp))) "Advertisement"))
    (is (not (core/has-subtype? (find-card "Advanced Assembly Lines" (:discard (get-corp))) "Advertisement")))
    (is (not (core/has-subtype? (find-card "City Surveillance" (:hand (get-corp))) "Advertisement")))
    (is (not (core/has-subtype? (find-card "Jackson Howard" (:hand (get-corp))) "Advertisement")))
    (is (core/has-subtype? (find-card "Jackson Howard" (:hand (get-corp))) "Executive"))
    (is (not (core/has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Advertisement")))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Alliance"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Ritzy"))))

(deftest reeducation
  ;; Reeducation
  (testing "Simple test"
    (do-game
      (new-game (default-corp ["Reeducation" "Sweeps Week" "Hedge Fund"
                               "Jackson Howard" "Gutenberg"])
                (default-runner ["Self-modifying Code" "Clone Chip"
                                 "Corroder" "Sure Gamble" "Desperado"]))
      (starting-hand state :corp ["Reeducation" "Sweeps Week"])
      (starting-hand state :runner ["Self-modifying Code"])
      (play-and-score state "Reeducation")
      (is (prompt-is-type? :runner :waiting) "Runner has wait prompt")
      (is (= 1 (count (get-in @state [:corp :hand]))))
      (is (= 1 (count (get-in @state [:runner :hand]))))
      (prompt-card :corp (find-card "Sweeps Week" (:hand (get-corp)))) ; put Sweeps Week at bottom of R&D
      (prompt-choice :corp "Done") ; finished selecting cards
      (prompt-choice :corp "Done") ; corp prompt for Done/Start Over
      (is (= "Sweeps Week" (:title (last (:deck (get-corp))))))
      (is (= "Self-modifying Code" (:title (last (:deck (get-runner))))))
      (is (= 1 (count (get-in @state [:corp :hand]))))
      (is (zero? (count (get-in @state [:runner :hand]))))))
  (testing "Extra cards"
    ;; If Corp is adding more cards in HQ than Runner has in their Grip, Runner
    ;; is not 'able' to resolve the effect and doesn't have to add to bottom of Stack
    (do-game
      (new-game (default-corp ["Reeducation" "Sweeps Week" "Hedge Fund"
                               "Jackson Howard" "Gutenberg"])
                (default-runner ["Self-modifying Code" "Clone Chip"
                                 "Corroder" "Sure Gamble" "Desperado"]))
      (starting-hand state :corp ["Reeducation" "Sweeps Week" "Hedge Fund"])
      (starting-hand state :runner ["Self-modifying Code"])
      (play-and-score state "Reeducation")
      (is (prompt-is-type? :runner :waiting) "Runner has wait prompt")
      (is (= 2 (count (:hand (get-corp)))))
      (is (= 1 (count (:hand (get-runner)))))
      (prompt-card :corp (find-card "Sweeps Week" (:hand (get-corp))))
      (prompt-card :corp (find-card "Hedge Fund" (:hand (get-corp)))) ; this is the bottom card of R&D
      (prompt-choice :corp "Done") ; finished selecting cards
      (prompt-choice :corp "Done") ; corp prompt for Done/Start Over
      (is (= "Hedge Fund" (:title (last (:deck (get-corp))))))
      (is (= "Sweeps Week" (:title (last (butlast (:deck (get-corp)))))))
      (is (= "Self-modifying Code" (:title (first (:hand (get-runner))))))
      (is (= 2 (count (:hand (get-corp)))))
      (is (= 1 (count (:hand (get-runner))))))))

(deftest remote-data-farm
  ;; Remote Data Farm
  (do-game
    (new-game (default-corp ["Remote Data Farm"])
              (default-runner))
    (is (= 5 (get-hand-size :corp)))
    (play-and-score state "Remote Data Farm")
    (is (= 7 (get-hand-size :corp)))))

(deftest remote-enforcement
  ;; Remote Enforcement - Search R&D for a piece of ice and install it on a remote at no rez cost
  (do-game
   (new-game (default-corp [(qty "Remote Enforcement" 2)
                            "Archer"
                            "Chiyashi"])
             (make-deck "Reina Roja: Freedom Fighter" []))
   (starting-hand state :corp ["Remote Enforcement" "Remote Enforcement"])
   (is (= 2 (count (:deck (get-corp)))))
   (play-and-score state "Remote Enforcement")
   (let [N (:credit (get-corp))]
     (prompt-choice :corp "Yes")
     (prompt-choice :corp (find-card "Chiyashi" (:deck (get-corp))))
     (prompt-choice :corp "New remote")
     (is (core/rezzed? (get-ice state :remote2 0)) "Chiyashi was installed rezzed")
     (is (= N (:credit (get-corp))) "Rezzing Chiyashi was free"))
   (play-and-score state "Remote Enforcement")
   (let [N (:credit (get-corp))]
     (prompt-choice :corp "Yes")
     (prompt-card :corp (find-card "Archer" (:deck (get-corp))))
     (prompt-choice :corp "Server 2")
     (is (= (dec N) (:credit (get-corp))) "Installing Archer cost a credit")
     (is (not-empty (:prompt (get-corp))) "Corp prompted to forfeit an agenda for Archer")
     (is (= (dec N) (:credit (get-corp))) "Rezzing Archer didn't cost any credits"))))

(deftest research-grant
  ;; Research Grant
  (testing "Basic test"
    (do-game
      (new-game (default-corp [(qty "Research Grant" 2)])
                (default-runner))
      (play-from-hand state :corp "Research Grant" "New remote")
      (play-and-score state "Research Grant")
      (prompt-select :corp (get-content state :remote1 0))
      (is (= 2 (count (:scored (get-corp)))) "2 copies of Research Grant scored")))
  (testing "vs Leela"
    ;; Issue #3069
    (do-game
      (new-game (default-corp [(qty "Research Grant" 2) (qty "Ice Wall" 2)])
                (make-deck "Leela Patel: Trained Pragmatist" ["Sure Gamble"]))
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Research Grant" "New remote")
      (play-and-score state "Research Grant")
      (prompt-select :corp (get-content state :remote1 0))
      (is (= 2 (count (:scored (get-corp)))) "2 copies of Research Grant scored")
      (prompt-select :runner (get-ice state :hq 0))
      (prompt-select :runner (get-ice state :rd 0))
      (is (empty? (:effect-completed @state)) "All score and Leela effects resolved"))))

(deftest restructured-datapool
  ;; Restructured Datapool
  (do-game
    (new-game (default-corp ["Restructured Datapool"])
              (default-runner))
    (is (zero? (:tag (get-runner))) "Runner should start with no tags")
    (play-and-score state "Restructured Datapool")
    (let [rd-scored (get-scored state :corp 0)]
      (card-ability state :corp rd-scored 0)
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= 1 (:tag (get-runner))) "Runner should gain a tag from Restructured Datapool ability"))))

(deftest self-destruct-chips
  ;; Self-Destruct Chips
  (do-game
    (new-game (default-corp ["Self-Destruct Chips"])
              (default-runner))
    (is (= 5 (get-hand-size :runner)) "Runner's hand size starts at 5")
    (play-and-score state "Self-Destruct Chips")
    (is (= 4 (get-hand-size :runner)) "By scoring Self-Destruct Chips, Runner's hand size is reduced by 1")))

(deftest sensor-net-activation
  ;; Sensor Net Activation
  (do-game
    (new-game (default-corp [(qty "Sensor Net Activation" 2) "Enforcer 1.0" "Ash 2X3ZB9CY"])
              (default-runner))
    (play-from-hand state :corp "Enforcer 1.0" "HQ")
    (play-and-score state "Sensor Net Activation")
    (let [sna-scored (get-scored state :corp 0)
          enf (get-ice state :hq 0)]
      (is (= 1 (get-counters (refresh sna-scored) :agenda)) "Should start with 1 agenda counter")
      (is (not (:rezzed (refresh enf))) "Enforcer 1.0 should start derezzed")
      (card-ability state :corp (refresh sna-scored) 0)
      (prompt-select :corp enf)
      (is (:rezzed (refresh enf)) "Enforcer 1.0 should be rezzed")
      (is (= 1 (count (:scored (get-corp)))) "Enforcer 1.0 should be rezzed without forfeiting agenda")
      (take-credits state :corp)
      (is (not (:rezzed (refresh enf))) "Enforcer 1.0 should be derezzed"))
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Ash 2X3ZB9CY" "New remote")
    (play-and-score state "Sensor Net Activation")
    (let [sna-scored (get-scored state :corp 1)
          ash (get-content state :remote2 0)]
      (is (= 1 (get-counters (refresh sna-scored) :agenda)) "Should start with 1 agenda counter")
      (is (not (:rezzed (refresh ash))) "Ash should start derezzed")
      (card-ability state :corp (refresh sna-scored) 0)
      (prompt-select :corp ash)
      (is (:rezzed (refresh ash)) "Ash should be rezzed")
      (take-credits state :corp)
      (is (not (:rezzed (refresh ash))) "Ash should be derezzed"))))

(deftest sentinel-defense-program
  ;; Sentinel Defense Program - Doesn't fire if brain damage is prevented
  (do-game
    (new-game (default-corp ["Sentinel Defense Program" "Viktor 1.0"])
              (default-runner ["Feedback Filter" (qty "Sure Gamble" 3)]))
    (play-and-score state "Sentinel Defense Program")
    (play-from-hand state :corp "Viktor 1.0" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Feedback Filter")
    (let [viktor (get-ice state :hq 0)
          ff (get-hardware state 0)]
      (run-on state "HQ")
      (core/rez state :corp viktor)
      (card-subroutine state :corp viktor 0)
      (prompt-choice :runner "Done")  ;; Don't prevent the brain damage
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 1 (:brain-damage (get-runner))))
      (prompt-choice :runner "Done")  ;; So we take the net, but don't prevent it either
      (is (= 2 (count (:discard (get-runner)))))
      (card-subroutine state :corp viktor 0)
      (card-ability state :runner ff 1)  ;; Prevent the brain damage this time
      (prompt-choice :runner "Done")
      (is (= 3 (count (:discard (get-runner)))) "Feedback filter trashed, didn't take another net damage")
      (is (= 1 (:brain-damage (get-runner)))))))

(deftest show-of-force
  ;; Show of Force
  (do-game
    (new-game (default-corp ["Show of Force"])
              (default-runner))
    (is (= 3 (count (:hand (get-runner)))) "Runner should start with 3 cards in hand")
    (play-and-score state "Show of Force")
    (is (= 1 (count (:hand (get-runner)))) "Runner should have 1 card in hand")
    (is (= 2 (count (:discard (get-runner)))) "Runner should have discarded 2 cards")))

(deftest ssl-endorsement
  ;; SSL Endorsement
  (testing "gain credits when in corp score area before turn begins"
    (do-game
      (new-game (default-corp ["SSL Endorsement"])
                (default-runner))
      (play-and-score state "SSL Endorsement")
      (take-credits state :runner)
      (is (not-empty (:prompt (get-corp))) "Corp prompted to take credits")
      (is (= 5 (:credit (get-corp))) "Corp starts with 5 credits")
      (prompt-choice :corp "Yes")
      (is (= 8 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))) "Corp starts with 8 credits")
      (prompt-choice :corp "No")
      (is (= 8 (:credit (get-corp))) "Corp doesn't gain 3 credits")
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))) "Corp starts with 8 credits")
      (prompt-choice :corp "Yes")
      (is (= 11 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (= 11 (:credit (get-corp))) "Corp starts with 11 credits")
      (prompt-choice :corp "Yes")
      (is (= 14 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (empty? (:prompt (get-corp))) "Not prompted when out of money")))
  (testing "gain credits when in runner score area before turn begins"
    (do-game
      (new-game (default-corp ["SSL Endorsement"])
                (default-runner))
      (play-from-hand state :corp "SSL Endorsement" "New remote")
      (take-credits state :corp)
      (run-on state "Server 1")
      (run-successful state)
      (prompt-choice :runner "Steal")
      (take-credits state :runner)
      (is (not-empty (:prompt (get-corp))) "Corp prompted to take credits")
      (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
      (prompt-choice :corp "Yes")
      (is (= 10 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (= 10 (:credit (get-corp))) "Corp starts with 10 credits")
      (prompt-choice :corp "No")
      (is (= 10 (:credit (get-corp))) "Corp doesn't gain 3 credits")
      (take-credits state :runner)
      (is (= 10 (:credit (get-corp))) "Corp starts with 10 credits")
      (prompt-choice :corp "Yes")
      (is (= 13 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (= 13 (:credit (get-corp))) "Corp starts with 13 credits")
      (prompt-choice :corp "Yes")
      (is (= 16 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (empty? (:prompt (get-corp))) "Not prompted when out of money")))
  (testing "register event when agenda swapped with Turntable"
    ;; Regression test for #3114
    (do-game
      (new-game (default-corp ["SSL Endorsement" "Breaking News"])
                (default-runner ["Turntable"]))
      (play-from-hand state :corp "Breaking News" "New remote")
      (play-and-score state "SSL Endorsement")
      (take-credits state :corp)
      (play-from-hand state :runner "Turntable")
      (run-on state "Server 1")
      (run-successful state)
      (prompt-choice :runner "Steal")
      (prompt-choice :runner "Yes")
      (prompt-select :runner (find-card "SSL Endorsement" (:scored (get-corp))))  ;; Swap BN with SSL
      (take-credits state :runner)
      (is (not-empty (:prompt (get-corp))) "Corp prompted to take credits")
      (is (= 6 (:credit (get-corp))) "Corp starts with 7 credits")
      (prompt-choice :corp "Yes")
      (is (= 9 (:credit (get-corp))) "Corp gains 3 credits from Turntable'd SSL Endorsement")))
  (testing "don't double register event when agenda is swapped"
    (do-game
      (new-game (default-corp ["SSL Endorsement" "Breaking News"
                               "Exchange of Information"])
                (default-runner))
      (play-from-hand state :corp "SSL Endorsement" "New remote")
      (play-and-score state "Breaking News")
      (take-credits state :corp)
      (run-on state "Server 1")
      (run-successful state)
      (prompt-choice :runner "Steal")
      (take-credits state :runner)
      (is (not-empty (:prompt (get-corp))) "Corp prompted to take credits")
      (is (= 6 (:credit (get-corp))) "Corp starts with 6 credits")
      (prompt-choice :corp "Yes")
      (is (= 9 (:credit (get-corp))) "Corp gains 3 credits")
      (core/gain state :runner :tag 1)
      (play-from-hand state :corp "Exchange of Information")
      (prompt-select :corp (find-card "SSL Endorsement" (:scored (get-runner))))
      (prompt-select :corp (find-card "Breaking News" (:scored (get-corp))))
      (take-credits state :runner)
      (is (= 9 (:credit (get-corp))) "Corp starts with 9 credits")
      (prompt-choice :corp "No")
      (is (empty? (:prompt (get-corp))) "Not double prompted for credits")
      (is (= 9 (:credit (get-corp))) "Corp doesn't gain 3 credits")
      (take-credits state :runner)
      (is (= 9 (:credit (get-corp))) "Corp starts with 9 credits")
      (prompt-choice :corp "Yes")
      (is (= 12 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (= 12 (:credit (get-corp))) "Corp starts with 12 credits")
      (prompt-choice :corp "Yes")
      (is (= 15 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (empty? (:prompt (get-corp))) "Not prompted when out of money"))))

(deftest standoff
  ;; Standoff
  (testing "Runner declines first"
    (do-game
      (new-game (default-corp ["Standoff" "Ice Wall" "News Team"])
                (default-runner ["Cache"]))
      (starting-hand state :corp ["Standoff" "Ice Wall"])
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (take-credits state :runner)
      (play-and-score state "Standoff")
      (starting-hand state :corp [])
      (is (zero? (-> (get-runner) :discard count)) "Runner should have no cards in Heap")
      (prompt-select :runner (get-program state 0))
      (is (= 1 (-> (get-runner) :discard count)) "Runner should now have 1 card in Heap")
      (is (zero? (-> (get-corp) :discard count)) "Corp should have no cards in Archives")
      (prompt-select :corp (get-ice state :hq 0))
      (is (= 1 (-> (get-corp) :discard count)) "Corp should now have 1 card in Archives")
      (is (zero? (-> (get-corp) :hand count)) "Corp should have no cards in hand")
      (let [credits (:credit (get-corp))]
        (prompt-choice :runner "Done")
        (is (= (+ credits 5) (:credit (get-corp))) "Corp should gain 5 credits from Runner declining to trash an installed card")
        (is (= 1 (-> (get-corp) :hand count)) "Corp should draw a card from Runner declining to trash an installed card"))))
  (testing "Corp declines first"
    (do-game
      (new-game (default-corp ["Standoff" "Ice Wall" "News Team"])
                (default-runner ["Cache" "Cache"]))
      (starting-hand state :corp ["Standoff" "Ice Wall"])
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (take-credits state :runner)
      (play-and-score state "Standoff")
      (starting-hand state :corp [])
      (is (zero? (-> (get-runner) :discard count)) "Runner should have no cards in Heap")
      (prompt-select :runner (get-program state 0))
      (is (= 1 (-> (get-runner) :discard count)) "Runner should now have 1 card in Heap")
      (is (zero? (-> (get-corp) :discard count)) "Corp should have no cards in Archives")
      (prompt-select :corp (get-ice state :hq 0))
      (is (= 1 (-> (get-corp) :discard count)) "Corp should now have 1 card in Archives")
      (is (zero? (-> (get-corp) :hand count)) "Corp should have no cards in hand")
      (prompt-select :runner (get-program state 0))
      (is (= 2 (-> (get-runner) :discard count)) "Runner should now have 2 cards in Heap")
      (let [credits (:credit (get-corp))]
        (prompt-choice :corp "Done")
        (is (= credits (:credit (get-corp))) "Corp should gain no credits from declining to trash an installed card")
        (is (zero? (-> (get-corp) :hand count)) "Corp should draw no cards from declining to trash an installed card")))))

(deftest successful-field-test
  ;; Successful Field Test
  (do-game
    (new-game (default-corp ["Successful Field Test" (qty "Ice Wall" 10)])
              (default-runner))
    (starting-hand state :corp (vec (cons "Successful Field Test" (repeat 10 "Ice Wall"))))
    (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
    (play-and-score state "Successful Field Test")
    (dotimes [n 10]
      (prompt-select :corp (find-card "Ice Wall" (:hand (get-corp))))
      (prompt-choice :corp "HQ"))
    (is (= 5 (:credit (get-corp))) "Should still have 5 credits")
    (is (some? (get-ice state :hq 9)))))

(deftest superior-cyberwalls
  ;; Superior Cyberwalls
  (do-game
    (new-game (default-corp ["Superior Cyberwalls" "Ice Wall"])
              (default-runner))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [iw (get-ice state :hq 0)]
      (core/rez state :corp iw)
      (is (= 1 (:current-strength (refresh iw))) "Should start with base strength of 1")
      (is (= 4 (:credit (get-corp))) "Should have 4 credits after rez")
      (play-and-score state "Superior Cyberwalls")
      (is (= 2 (:current-strength (refresh iw))) "Should gain 1 strength from 1 to 2")
      (is (= 5 (:credit (get-corp))) "Should gain 1 credit for rezzed barrier"))))

(deftest tgtbt
  ;; TGTBT - Give the Runner 1 tag when they access
  ;; OHG still not working...
  (do-game
    (new-game (default-corp [(qty "TGTBT" 2) "Old Hollywood Grid"])
              (default-runner))
    (play-from-hand state :corp "TGTBT" "New remote")
    (play-from-hand state :corp "Old Hollywood Grid" "Server 1")
    (play-from-hand state :corp "TGTBT" "New remote")
    (take-credits state :corp)
    (let [tg1 (get-content state :remote1 0)
          ohg (get-content state :remote1 1)]
      (run-on state "Server 1")
      (core/rez state :corp ohg)
      (run-successful state)
      (prompt-select :runner tg1)
      ;; Accesses TGTBT but can't steal
      (is (= 1 (:tag (get-runner))) "Runner took 1 tag from accessing without stealing")
      (prompt-choice :runner "No action")
      (prompt-select :runner ohg))

    (prompt-choice :runner "Yes") ;; Trashes OHG
    (run-empty-server state "Server 2")
    ;; Accesses TGTBT and can steal
    (prompt-choice :runner "Steal")

    (is (= 2 (:tag (get-runner))) "Runner took 1 tag from accessing and stealing")))

(deftest the-cleaners
  ;; The Cleaners
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["The Cleaners" "Scorched Earth"])
                (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
      (play-and-score state "The Cleaners")
      (core/gain state :runner :tag 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (zero? (count (:hand (get-runner)))) "5 damage dealt to Runner")))
  (testing "No bonus damage when runner 'suffers' damage, ie Cybernetics"
    (do-game
      (new-game (default-corp ["The Cleaners"])
                (default-runner [(qty "Respirocytes" 3)]))
      (play-and-score state "The Cleaners")
      (take-credits state :corp)
      (play-from-hand state :runner "Respirocytes")
      (is (= 1 (count (:hand (get-runner)))) "Only 1 damage dealt to Runner from Cybernetics"))))

(deftest the-future-is-now
  ;; The Future is Now
  (testing "With at least one card in deck"
    (do-game
      (new-game (default-corp ["The Future is Now" "Ice Wall"])
                (default-runner))
      (starting-hand state :corp ["The Future is Now"])
      (is (= 1 (count (:hand (get-corp)))))
      (is (= 1 (count (:deck (get-corp)))))
      (play-and-score state "The Future is Now")
      (prompt-card :corp (find-card "Ice Wall" (:deck (get-corp))))
      (is (= 1 (count (:hand (get-corp)))))
      (is (zero? (count (:deck (get-corp)))))))
  (testing "With an empty deck"
    (do-game
      (new-game (default-corp ["The Future is Now"])
                (default-runner))
      (is (= 1 (count (:hand (get-corp)))))
      (is (zero? (count (:deck (get-corp)))))
      (play-and-score state "The Future is Now")
      (is (empty? (:prompt (get-corp))) "Ability shouldn't fire if deck is empty")
      (is (zero? (count (:hand (get-corp)))))
      (is (zero? (count (:deck (get-corp))))))))

(deftest the-future-perfect
  ;; The Future Perfect
  (do-game
    (new-game (default-corp [(qty "The Future Perfect" 2)])
              (default-runner))
    (play-from-hand state :corp "The Future Perfect" "New remote")
    (take-credits state :corp)
    (testing "No steal on not-equal Psi game"
      (run-empty-server state "HQ")
      (prompt-choice :corp "1 [Credits]")
      (prompt-choice :runner "0 [Credits]")
      ;; Cannot steal prompt
      (prompt-choice :runner "No action")
      (is (zero? (:agenda-point (get-runner))) "Runner did not steal TFP"))
    (testing "Successful steal on equal Psi game"
      (run-empty-server state "HQ")
      (prompt-choice :corp "1 [Credits]")
      (prompt-choice :runner "1 [Credits]")
      (prompt-choice :runner "Steal")
      (is (= 3 (:agenda-point (get-runner))) "Runner stole TFP"))
    (testing "No Psi game and successful steal when installed"
      (run-empty-server state "Server 1")
      (prompt-choice :runner "Steal")
      (is (= 6 (:agenda-point (get-runner))) "Runner stole TFP - no Psi game on installed TFP"))))

(deftest underway-renovation
  ;; Underway Renovation
  (do-game
    (new-game (default-corp ["Underway Renovation" "Shipment from SanSan"])
              (default-runner))
    (core/gain state :corp :click 2)
    (starting-hand state :runner [])
    (play-from-hand state :corp "Underway Renovation" "New remote")
    (let [ur (get-content state :remote1 0)]
      (advance state ur)
      (is (last-log-contains? state "Sure Gamble")
          "Underway Renovation trashed card name is in log")
      ; check for #2370
      (is (not (last-log-contains? state "Sure Gamble, Sure Gamble"))
          "Underway Renovation trashed card name is in log")
      (is (= 1 (count (:discard (get-runner)))) "1 card milled from Runner Stack")
      (play-from-hand state :corp "Shipment from SanSan")
      (prompt-choice :corp "2")
      (prompt-select :corp ur)
      (is (= 3 (get-counters (refresh ur) :advancement)))
      (is (= 1 (count (:discard (get-runner)))) "No Runner mills; advancements were placed")
      (advance state ur)
      (is (= 4 (get-counters (refresh ur) :advancement)))
      (is (last-log-contains? state "Sure Gamble, Sure Gamble")
          "Underway Renovation trashed card name is in log")
      (is (= 3 (count (:discard (get-runner)))) "2 cards milled from Runner Stack; 4+ advancements"))))

(deftest unorthodox-predictions
  ;; Unorthodox Predictions
  (do-game
    (new-game (default-corp ["Unorthodox Predictions"])
              (default-runner))
    (play-and-score state "Unorthodox Predictions")
    (prompt-choice :corp "Barrier")
    (is (last-log-contains? state "Barrier"))))

(deftest utopia-fragment
  ;; Utopia Fragment
  (do-game
    (new-game (default-corp ["Utopia Fragment"
                             "Hostile Takeover"])
              (default-runner))
    (play-and-score state "Utopia Fragment")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (advance state (get-content state :remote2 0))
    (take-credits state :corp)
    (run-on state :remote2)
    (run-successful state)
    (prompt-choice-partial :runner "Pay")
    (is (= 1 (:agenda-point (get-runner))))
    (is (= 3 (:credit (get-runner))))))

(deftest vanity-project
  ;; Vanity Project
  (do-game
    (new-game (default-corp ["Vanity Project"])
              (default-runner))
    (play-and-score state "Vanity Project")
    (is (= 4 (:agenda-point (get-corp))))))

(deftest veterans-program
  ;; Veterans Program
  (testing "Veterans Program basic test"
    (do-game
      (new-game (default-corp [(qty "Hostile Takeover" 2) "Veterans Program"])
                (default-runner))
      (play-and-score state "Hostile Takeover")
      (play-and-score state "Hostile Takeover")
      (is (= 19 (:credit (get-corp))) "Should gain 14 credits from 5 to 19")
      (is (= 2 (:bad-publicity (get-corp))) "Should gain 2 bad publicity")
      (play-and-score state "Veterans Program")
      (is (zero? (:bad-publicity (get-corp))) "Should lose 2 bad publicity")))
  (testing "Removes _up to 2_ bad publicity"
    (do-game
      (new-game (default-corp ["Hostile Takeover" "Veterans Program"])
                (default-runner))
      (play-and-score state "Hostile Takeover")
      (is (= 12 (:credit (get-corp))) "Should gain 7 credits from 5 to 12")
      (is (= 1 (:bad-publicity (get-corp))) "Should gain 1 bad publicity")
      (play-and-score state "Veterans Program")
      (is (zero? (:bad-publicity (get-corp))) "Should lose 1 bad publicity"))))

(deftest viral-weaponization
  ;; Viral Weaponization - at the end of turn scored, do 1 net damage for each card in grip
  (testing "Score on corp turn"
    (do-game
      (new-game (default-corp [(qty "Viral Weaponization" 2)])
                (default-runner [(qty "Sure Gamble" 3)]))
      (starting-hand state :runner ["Sure Gamble" "Sure Gamble"])
      (play-and-score state "Viral Weaponization")
      (is (= 2 (count (:hand (get-runner)))) "Runner doesn't take damage when scored")
      (take-credits state :corp)
      (is (zero? (count (:hand (get-runner)))) "Runner takes damage at end of turn")
      (core/click-draw state :runner 1)
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-runner)))) "Runner doesn't take damage in future turns")
      (play-from-hand state :runner "Sure Gamble")
      (take-credits state :runner)
      (is (zero? (count (:hand (get-runner)))) "Runner's hand is empty")
      (play-and-score state "Viral Weaponization")
      (take-credits state :corp)
      (is (zero? (count (:hand (get-runner)))) "Runner's hand is empty")))
  (testing "Score on runners turn"
    (do-game
      (new-game (default-corp ["Viral Weaponization" "Plan B"])
                (default-runner [(qty "Sure Gamble" 3)]))
      (starting-hand state :runner ["Sure Gamble" "Sure Gamble"])
      (play-from-hand state :corp "Plan B" "New remote")
      (core/add-prop state :corp (get-content state :remote1 0) :advance-counter 4)
      (take-credits state :corp)
      (run-on state "Server 1")
      (run-successful state)
      (prompt-choice :corp "Yes")
      (prompt-select :corp (find-card "Viral Weaponization" (:hand (get-corp))))
      (prompt-choice-partial :runner "No")
      (is (= 2 (count (:hand (get-runner)))) "Runner doesn't take damage when scored")
      (take-credits state :runner)
      (is (zero? (count (:hand (get-runner)))) "Runner takes damage at end of turn"))))

(deftest voting-machine-initiative
  ;; Voting Machine Initiative
  (testing "Voting Machine Initiative"
    (do-game
      (new-game (default-corp ["Voting Machine Initiative"])
                (default-runner))
      (letfn [(vmi-test [vmi choice counter]
                (let [diff (if (= "Yes" choice) 1 0)]
                  (is (= counter (get-counters (refresh vmi) :agenda)))
                  (is (= 4 (:click (get-runner))))
                  (prompt-choice :corp choice)
                  (is (= (- 4 diff) (:click (get-runner))))
                  (is (= (- counter diff) (get-counters (refresh vmi) :agenda)))
                  (take-credits state :runner)
                  (take-credits state :corp)))]
        (play-and-score state "Voting Machine Initiative")
        (take-credits state :corp)
        (let [vmi-scored (get-scored state :corp 0)]
          (vmi-test vmi-scored "Yes" 3)
          (vmi-test vmi-scored "No" 2)
          (vmi-test vmi-scored "Yes" 2)
          (vmi-test vmi-scored "Yes" 1)
          (is (empty (:prompt (get-corp))) "No prompt as there are no agenda counters left"))))))

(deftest vulcan-coverup
  ;; Vulcan Coverup
  (do-game
    (new-game (default-corp [(qty "Vulcan Coverup" 2)])
              (default-runner))
    (play-from-hand state :corp "Vulcan Coverup" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (prompt-choice :runner "Steal")
    (is (= 1 (:bad-publicity (get-corp))) "Took 1 bad pub from stolen agenda")
    (take-credits state :runner)
    (play-and-score state "Vulcan Coverup")
    (is (= 2 (count (:discard (get-runner)))) "Did 2 meat damage upon scoring")))

(deftest water-monopoly
  ;; Water Monopoly
  (do-game
    (new-game (default-corp ["Water Monopoly"])
              (default-runner ["Fan Site" "Levy Advanced Research Lab"]))
    (play-and-score state "Water Monopoly")
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))) "Runner should start with 5 credits")
    (play-from-hand state :runner "Fan Site")
    (is (= 5 (:credit (get-runner))) "Shouldn't lose any credits")
    (play-from-hand state :runner "Levy Advanced Research Lab")
    (is (zero? (:credit (get-runner))) "Should cost an extra credit to play")))
