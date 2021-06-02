(ns drampa.matches-test
  (:require [clojure.test :refer :all]
            [drampa.matches :refer :all]))

(deftest get-initial-match-is-correct
  (let [{:keys [wall scores]} (get-initial-match)]
    (testing "Is the starting wall of a match initialied correctly?"
      (is (not (nil? wall)))
      (is (= (count wall)) 136))
    (testing "Are the starting scores of a match initialied correctly?"
      (is (not (nil? scores)))
      (is (= (count scores) 4))
      (is (every? #(= starting-score %) scores)))))