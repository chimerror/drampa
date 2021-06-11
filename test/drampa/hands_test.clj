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
    (is (= false (is-chii? (d.tiles/tiles-from-notation "5678m")))))
  (testing "Are ordered pairs not considered chii?"
    (is (= false (is-chii? (d.tiles/tiles-from-notation "56s"))))))

(deftest is-pon-is-correct
  (testing "Are all valid number tile pung considered pon?"
      (doseq [suit [:pin :man :sou]
              rank (range 1 10)
              :let [tiles (mapv #(d.tiles/->Tile suit %) (repeat 3 rank))]]
        (is (= true (is-pon? tiles)))))
  (testing "Are all valid pung with red fives considered pon?"
      (doseq [suit [:pin :man :sou]
              ranks [[5 5 0] [5 0 5] [0 5 5]]
              :let [tiles (mapv #(d.tiles/->Tile suit %) ranks)]]
        (is (= true (is-pon? tiles)))))
  (testing "Are all valid honor tile pung considered pon?"
      (doseq [rank (range 1 8)
              :let [tiles (mapv #(d.tiles/->Tile :zi %) (repeat 3 rank))]]
        (is (= true (is-pon? tiles)))))
  (testing "Are sequences not considered pon?"
    (is (= false (is-pon? (d.tiles/tiles-from-notation "123z")))))
  (testing "Are different suits not considered pon?"
    (is (= false (is-pon? (d.tiles/tiles-from-notation "5p5s5m")))))
  (testing "Are quads not considered pon?"
    (is (= false (is-pon? (d.tiles/tiles-from-notation "5550m")))))
  (testing "Are pairs not considered pon?"
    (is (= false (is-pon? (d.tiles/tiles-from-notation "66z"))))))

(deftest is-kan-is-correct
  (testing "Are all valid number tile kong considered kan?"
      (doseq [suit [:pin :man :sou]
              rank (range 1 10)
              :let [tiles (mapv #(d.tiles/->Tile suit %) (repeat 4 rank))]]
        (is (= true (is-kan? tiles)))))
  (testing "Are all valid pung with red fives considered kan?"
      (doseq [suit [:pin :man :sou]
              ranks [[5 5 5 0] [5 5 0 5] [5 0 5 5] [0 5 5 5]]
              :let [tiles (mapv #(d.tiles/->Tile suit %) ranks)]]
        (is (= true (is-kan? tiles)))))
  (testing "Are all valid honor tile pung considered kan?"
      (doseq [rank (range 1 8)
              :let [tiles (mapv #(d.tiles/->Tile :zi %) (repeat 4 rank))]]
        (is (= true (is-kan? tiles)))))
  (testing "Are sequences not considered kan?"
    (is (= false (is-kan? (d.tiles/tiles-from-notation "1234z")))))
  (testing "Are different suits not considered kan?"
    (is (= false (is-kan? (d.tiles/tiles-from-notation "5p5s5m5z")))))
  (testing "Are triplets not considered kan?"
    (is (= false (is-kan? (d.tiles/tiles-from-notation "550m")))))
  (testing "Are pairs not considered kan?"
    (is (= false (is-kan? (d.tiles/tiles-from-notation "66z"))))))