(ns drampa.matches-test
  (:require [clojure.test :refer :all]
            [drampa.matches :refer :all]))

(deftest get-initial-match-is-correct
  (let [{:keys [wall scores player-winds]} (get-initial-match)]
    (testing "Is the starting wall of a match initialied correctly?"
      (is (not (nil? wall)))
      (is (= (count wall)) 136))
    (testing "Are the starting scores of a match initialied correctly?"
      (is (not (nil? scores)))
      (is (= (count scores) 4))
      (is (every? #(= starting-score %) scores)))
    (testing "Are the seat winds of a match initialied correctly?"
      (is (= (count player-winds) 4))
      (is (distinct? player-winds))
      (is (some #{:east} player-winds))
      (is (some #{:south} player-winds))
      (is (some #{:west} player-winds))
      (is (some #{:north} player-winds)))))