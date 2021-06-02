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
    (is (= (count initial-wall) 136))))

(deftest there-are-the-correct-tiles-in-a-wall
  (testing "Are the expected tiles in a wall?"
    (is (=
      (notation-from-tiles (sort compare-tiles initial-wall))
      (str
        "111122223333444455506666777788889999p111122223333444455506666777788889999s" "111122223333444455506666777788889999m1111222233334444555566667777z")))))

(deftest can-round-trip-tiles-from-notation
  (testing "Can we perform a round-trip through tiles-from-notation?"
    (let [original-notation "19p19s19m1234567z1m"
          tiles (tiles-from-notation original-notation)
          round-trip-notation (notation-from-tiles tiles)]
      (is (= original-notation round-trip-notation)))))

(deftest can-sort-tiles-using-compare-tiles
  (testing "Can tiles be sorted using compare-tiles?"
    (let [original-notation "12357z19p19s19m1m05p0m64z"
          tiles (sort compare-tiles (tiles-from-notation original-notation))
          round-trip-notation (notation-from-tiles tiles)]
      (is (= round-trip-notation "1509p19s1109m1234567z")))))

