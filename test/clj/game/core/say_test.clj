(ns game.core.say-test
  (:require
   [clojure.test :refer :all]
   [clojure.repl :as repl]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.commands :refer [parse-command]]
   [game.core.mark :refer :all]
   [game.test-framework :refer :all]
   [jinteki.utils :refer [command-info]]))

(deftest commands-are-documented-test
  (let [cmd-source (with-out-str (repl/source game.core.commands/parse-command))
        implemented-cmds (map str (re-seq #"(?<=\")\/[^ \"]*(?=\")" cmd-source))
        documented-cmds (map :name command-info)]
    (doseq [cmd implemented-cmds]
      (when-not (some #(= % cmd) documented-cmds)
        (is false (str "command '" cmd "' is undocumented"))))
    (doseq [cmd documented-cmds]
      (when-not (some #(= % cmd) implemented-cmds)
        (is false (str "command '" cmd "' is documented but not implemented"))))))

(deftest chat-commands

  (testing "/click"
    (let [user {:username "Corp"}]
      (do-game
        (new-game)
        (core/command-parser state :corp {:user user :text "/click 3"})
        (is (= 3 (:click (get-corp))) "Corp has 3 clicks")
        (core/command-parser state :corp {:user user :text "/click -5"})
        (is (= 0 (:click (get-corp))) "Corp has 0 clicks")
        (core/command-parser state :corp {:user user :text "/click 99999999999999999999999999999999999999999999"})
        (is (= 1000 (:click (get-corp))) "Corp has 1000 clicks"))))

  (testing "/credit"
    (let [user {:username "Corp"}]
      (do-game
        (new-game)
        (core/command-parser state :corp {:user user :text "/credit 3"})
        (is (= 3 (:credit (get-corp))) "Corp has 3 credits")
        (core/command-parser state :corp {:user user :text "/credit -5"})
        (is (= 0 (:credit (get-corp))) "Corp has 0 credits")
        (core/command-parser state :corp {:user user :text "/credit 99999999999999999999999999999999999999999999"})
        (is (= 1000 (:credit (get-corp))) "Corp has 1000 credits"))))

  (testing "/roll"
    (let [user {:username "Corp"}]
      (do-game
        (new-game)
        (core/command-parser state :runner {:user user :text "/roll 6"})
        (is (second-last-log-contains? state "rolls a 6 sided die") "Correct message, reasonable number")
        (core/command-parser state :runner {:user user :text "/roll -5"})
        (is (second-last-log-contains? state "rolls a 1 sided die") "Correct message, negative number")
        (core/command-parser state :runner {:user user :text "/roll 99999999999999999999999999999999999999999999"})
        (is (second-last-log-contains? state "rolls a 1000 sided die") "Correct message, very large number"))))

  (testing "/summon"
    (let [user {:username "Runner"}]
      (testing "Add card with short title"
        (do-game
          (new-game {:runner {:hand []}})
          (is (empty? (:hand (get-runner))) "Runner starts with empty grip")
          (core/command-parser state :runner {:user user :text "/summon Capricious Informant"})
          (is (= ["Capricious Informant"] (->> (get-runner) :hand (mapv :title))) "DDoS should now be added into hand")))

      (testing "Add non-existant card"
        (do-game
          (new-game {:runner {:hand []}})
          (is (empty? (:hand (get-runner))) "Runner starts with empty grip")
          (core/command-parser state :runner {:user user :text "/summon Probably Not A Real Card Name"})
          (is (empty? (:hand (get-runner))) "Runner still has an empty grip"))))))
