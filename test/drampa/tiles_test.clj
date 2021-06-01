(ns drampa.tiles-test
  (:require [clojure.test :refer :all]
            [drampa.tiles :refer :all]))

(deftest there-are-the-correct-number-of-suits
  (testing "Are there 4 suits?"
    (is (= (count tile-suits) 4))))

(deftest there-are-the-correct-number-of-number-tiles
  (testing "Are there 108 number tiles?"
    (is (= (count number-tiles) 108))))

(deftest there-are-the-correct-number-of-red-fives
  (testing "Are there 3 red fives (Rank 0)?"
    (is (= (count (filter #(= (:rank %) 0) number-tiles)) 3))))

(deftest there-are-the-correct-number-of-honor-tiles
  (testing "Are there 28 honor tiles?"
    (is (= (count (filter #(= (:suit %) :zi) honor-tiles)) 28))))

(deftest there-are-the-correct-number-of-tiles-in-a-wall
  (testing "Are there 136 tiles in a wall?"
    (is (= (count sorted-wall) 136))))
