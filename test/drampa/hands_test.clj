(ns drampa.hands-test
  (:require [clojure.test :refer :all]
            [drampa.hands :refer :all]
            [drampa.tiles :as d.tiles]))

(deftest is-chii-is-correct
  (testing "Are all valid chow considered chii?"
      (doseq [suit [:pin :man :sou]
              starting-rank (range 1 8)
              :let [ranks [starting-rank (inc starting-rank) (inc (inc starting-rank))]
                    tiles (mapv #(d.tiles/->Tile suit %) ranks)]]
        (is (= true (is-chii? (vec tiles))))))
  (testing "Are all valid chow with red fives considered chii?"
      (doseq [suit [:pin :man :sou]
              ranks [[3 4 0] [4 0 6] [0 6 7]]
              :let [tiles (mapv #(d.tiles/->Tile suit %) ranks)]]
        (is (= true (is-chii? tiles)))))
  (testing "Are honor sequences not considered chii?"
    (is (= false (is-chii? (d.tiles/tiles-from-notation "123z")))))
  (testing "Are different suits not considered chii?"
    (is (= false (is-chii? (d.tiles/tiles-from-notation "1p23m")))))
  (testing "Are non-sequential tiles not considered chii?"
    (is (= false (is-chii? (d.tiles/tiles-from-notation "147m")))))
  (testing "Are sequences greater than 4 not considered chii?"
    (is (= false (is-chii? (d.tiles/tiles-from-notation "5678m"))))))