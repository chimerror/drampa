#_{:clj-kondo/ignore [:refer-all]}
(ns drampa.hands-test
  (:require [clojure.test :refer :all]
            [drampa.hands :refer [
              is-chii?
              is-pon?
              is-kan?
              get-chow-melds
              get-pung-melds
              get-kong-melds
              separate-into-melds]]
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

(defn verify-get-melds [test-cases f meld-name]
  (doseq [test-case test-cases
          :let [
            [tile hand-notation expected-melds] test-case
            hand (d.tiles/tiles-from-notation hand-notation)
            expected-melds (when expected-melds (mapv #(mapv d.tiles/tiles-from-notation %) expected-melds))
            actual-melds (f tile hand)]]
    (if (nil? expected-melds)
      (testing (str "Are invalid " meld-name " melds NOT claimed?")
        (is (nil? actual-melds)))
      (testing (str "Are valid " meld-name " melds claimed?")
        (is (not (nil? actual-melds)))
        (is (= (count actual-melds) (count actual-melds)))
        (is (not-any? nil? actual-melds))
        (doseq [expected-meld expected-melds]
          (is (some #(= expected-meld %) actual-melds)))))))

(def get-chow-melds-test-cases
  [
    [(d.tiles/->Tile :zi 5) "2p3s11m34z" nil]
    [(d.tiles/->Tile :pin 7) "46p3s11m34z" nil]
    [(d.tiles/->Tile :man 3) "1245m" [["123m" "45m"] ["234m" "15m"] ["345m" "12m"]]]
    [(d.tiles/->Tile :sou 4) "34067s" [["340s" "467s"] ["406s" "347s"]]]
    [(d.tiles/->Tile :pin 0) "34567p" [["340p" "567p"] ["406p" "357p"] ["067p" "345p"]]]
    [(d.tiles/->Tile :sou 2) "13567s" [["123s" "567s"]]]
    [(d.tiles/->Tile :man 2) "134567m" [["123m" "4567m"] ["234m" "1567m"]]]
    [(d.tiles/->Tile :sou 2) "123p13567s123m" [["123s" "123p567s123m"]]]])

(deftest get-chow-melds-is-correct
  (verify-get-melds get-chow-melds-test-cases get-chow-melds "chow"))

(def get-pung-melds-test-cases
  [
    [(d.tiles/->Tile :zi 5) "2p3s11m5z" nil]
    [(d.tiles/->Tile :zi 5) "2p3s11m" nil]
    [(d.tiles/->Tile :pin 7) "777p3s11m66z" [["777p" "7p3s11m66z"]]]
    [(d.tiles/->Tile :sou 4) "067p1234456s11m66z" [["444s" "067p12356s11m66z"]]]
    [(d.tiles/->Tile :man 1) "2p3s11m66z" [["111m" "2p3s66z"]]]
    [(d.tiles/->Tile :zi 5) "2p3s11m55z" [["555z" "2p3s11m"]]]
    [(d.tiles/->Tile :man 0) "2p3s55m77z" [["055m" "2p3s77z"]]]
    [(d.tiles/->Tile :sou 5) "067p12344550s11m66z" [["555s" "067p123440s11m66z"] ["505s" "067p123445s11m66z"]]]])

(deftest get-pung-melds-is-correct
  (verify-get-melds get-pung-melds-test-cases get-pung-melds "pung"))

(def get-kong-melds-test-cases
  [
    [(d.tiles/->Tile :zi 5) "2p3s11m55z" nil]
    [(d.tiles/->Tile :zi 5) "2p3s11m5z" nil]
    [(d.tiles/->Tile :zi 5) "2p3s11m" nil]
    [(d.tiles/->Tile :pin 7) "777p3s11m66z" [["7777p" "3s11m66z"]]]
    [(d.tiles/->Tile :sou 4) "067p12344456s11m66z" [["4444s" "067p12356s11m66z"]]]
    [(d.tiles/->Tile :man 1) "2p3s111m66z" [["1111m" "2p3s66z"]]]
    [(d.tiles/->Tile :zi 5) "2p3s11m555z" [["5555z" "2p3s11m"]]]
    [(d.tiles/->Tile :man 0) "2p3s555m77z" [["0555m" "2p3s77z"]]]
    [(d.tiles/->Tile :man 5) "2p3s550m77z" [["5550m" "2p3s77z"]]]])

(deftest get-kong-melds-is-correct
  (verify-get-melds get-kong-melds-test-cases get-kong-melds "kong"))

(def get-separate-into-melds-test-cases
  [
    ["111p234s34m1z" ["234s" "111p"] "34m1z"]
  ])

(deftest separate-into-melds-is-correct
  (doseq [test-case get-separate-into-melds-test-cases
          :let [[hand-notation expected-melds expected-non-melds-notation] test-case
                hand (d.tiles/tiles-from-notation hand-notation)
                expected-melds (mapv #(d.tiles/sort-tiles (d.tiles/tiles-from-notation %)) expected-melds)
                expected-non-melds (d.tiles/sort-tiles (d.tiles/tiles-from-notation expected-non-melds-notation))
                [actual-melds actual-non-melds] (separate-into-melds hand)
                actual-melds (mapv d.tiles/sort-tiles actual-melds)
                actual-non-melds (d.tiles/sort-tiles actual-non-melds)]]
    (testing "Can a hand be separated into melds?"
      (is (= expected-melds actual-melds))
      (is (= expected-non-melds actual-non-melds)))))