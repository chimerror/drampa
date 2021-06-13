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

(def get-pung-melds-single-choice-test-cases
  [
    [(d.tiles/->Tile :pin 7) "777p3s11m66z" "777p" "7p3s11m66z"]
    [(d.tiles/->Tile :sou 4) "067p1234456s11m66z" "444s" "067p12356s11m66z"]
    [(d.tiles/->Tile :man 1) "2p3s11m66z" "111m" "2p3s66z"]
    [(d.tiles/->Tile :zi 5) "2p3s11m55z" "555z" "2p3s11m"]
    [(d.tiles/->Tile :man 0) "2p3s55m77z" "055m" "2p3s77z"]])

(def get-pung-melds-double-choice-test-case
  [(d.tiles/->Tile :sou 5) "067p12344550s11m66z" [["555s" "067p123440s11m66z"] ["505s" "067p123445s11m66z"]]])

(deftest get-pung-melds-is-correct
  (testing "Are valid single-choice pungs claimed?"
    (doseq [test-case get-pung-melds-single-choice-test-cases
            :let [
              [tile hand-notation pung-notation rest-notation] test-case
              hand (d.tiles/tiles-from-notation hand-notation)
              expected-pung (d.tiles/tiles-from-notation pung-notation)
              expected-rest (d.tiles/tiles-from-notation rest-notation)
              pung-melds (get-pung-melds tile hand)
              pung-meld (first pung-melds)
              [actual-pung actual-rest] pung-meld]]
      (is (not (nil? pung-melds)))
      (is (= 1 (count pung-melds)))
      (is (not (nil? pung-meld)))
      (is (= expected-pung actual-pung))
      (is (= expected-rest actual-rest))))
  (testing "Are valid double-choice pungs claimed?"
    (let [[tile hand-notation expected-melds] get-pung-melds-double-choice-test-case
          hand (d.tiles/tiles-from-notation hand-notation)
          expected-melds (mapv #(mapv d.tiles/tiles-from-notation %) expected-melds)
          actual-melds (get-pung-melds tile hand)]
      (is (= 2 (count actual-melds)))
      (doseq [expected-meld expected-melds]
        (is (some #(= expected-meld %) actual-melds))))))

(def get-chow-melds-test-cases
  [(d.tiles/->Tile :man 3) "1245m" [["123m" "45m"] ["234m" "15m"] ["345m" "12m"]]])

(deftest get-chow-melds-is-correct
  (testing "Are valid chows claimed?"
    (let [[tile hand-notation expected-melds] get-chow-melds-test-cases
          hand (d.tiles/tiles-from-notation hand-notation)
          expected-melds (mapv #(mapv d.tiles/tiles-from-notation %) expected-melds)
          actual-melds (get-chow-melds tile hand)]
      (is (= (count expected-melds) (count actual-melds)))
      (doseq [expected-meld expected-melds]
        (is (some #(= expected-meld %) actual-melds))))))
