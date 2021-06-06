(ns drampa.matches-test
  (:require [clojure.test :refer :all]
            [drampa.matches :refer :all]
            [drampa.tiles :as d.tiles]))

(deftest get-initial-match-is-correct
  (let [{:keys [wall dead-wall scores player-winds prevailing-wind dora ura-dora]} (get-initial-match)]
    (testing "Is the starting wall of a match initialized correctly?"
      (is (not (nil? wall)))
      (is (= (count wall)) 136))
    (testing "Are the starting scores of a match initialized correctly?"
      (is (not (nil? scores)))
      (is (= (count scores) 4))
      (is (every? #(= starting-score %) scores)))
    (testing "Are the seat winds of a match initialized correctly?"
      (is (= (count player-winds) 4))
      (is (distinct? player-winds))
      (is (some #{:east} player-winds))
      (is (some #{:south} player-winds))
      (is (some #{:west} player-winds))
      (is (some #{:north} player-winds)))
    (testing "Is the prevailing wind of a match initialized correctly?"
      (is (= prevailing-wind :east)))
    (testing "Is the wall broken correctly?"
      (is (= (count dead-wall) 14))
      (is (= (count wall) 122)))
    (testing "Has one dora been revealed?"
      (is (= (count dora) 1))
      (is (= (count ura-dora) 1)))))

(def break-wall-at-test-cases (sorted-map
  2 [(str
    "22223333444455506666777788889999m1111222233334444555566667777z111122223333444455506666777788889999p"
    "11112222333344445550666677s")
    "7788889999s1111m"]
  3 [(str
    "11222233334444555566667777z111122223333444455506666777788889999p111122223333444455506666777788889999s"
    "111122223333444455506666m")
    "777788889999m11z"]
  4 [(str
    "223333444455506666777788889999p111122223333444455506666777788889999s111122223333444455506666777788889999m"
    "11112222333344445555z")
    "66667777z111122p"]
  5 [(str
    "1122223333444455506666777788889999s111122223333444455506666777788889999m1111222233334444555566667777z"
    "111122223333444455506666p")
    "777788889999p11s"]
  6 [(str
    "9999s111122223333444455506666777788889999m1111222233334444555566667777z111122223333444455506666777788889999"
    "p111122223333444455s")
    "50666677778888s"]
  7 [(str
    "889999m1111222233334444555566667777z111122223333444455506666777788889999p111122223333444455506666777788889999s"
    "1111222233334444m")
    "55506666777788m"]
  8 [(str
    "77z111122223333444455506666777788889999p111122223333444455506666777788889999s"
    "111122223333444455506666777788889999m111122223333z")
    "44445555666677z"]
  9 [(str
    "889999p111122223333444455506666777788889999s111122223333444455506666777788889999m1111222233334444555566667777z"
    "1111222233334444p")
    "55506666777788p"]
  10 [(str
    "777788889999s111122223333444455506666777788889999m1111222233334444555566667777z"
    "111122223333444455506666777788889999p1111222233s")
    "33444455506666s"]
  11 [(str
    "66777788889999m1111222233334444555566667777z111122223333444455506666777788889999p"
    "111122223333444455506666777788889999s11112222m")
    "33334444555066m"]
  12 [(str
    "5566667777z111122223333444455506666777788889999p111122223333444455506666777788889999s"
    "111122223333444455506666777788889999m1111z")
    "22223333444455z"]))

(deftest break-wall-at-is-correct
  (let [break-test-wall (vec (d.tiles/sort-tiles d.tiles/initial-wall))
        test-cases
          (reduce-kv #(assoc %1 %2 (vec (map d.tiles/tiles-from-notation %3))) (sorted-map) break-wall-at-test-cases)]
    (testing "Is the wall broken correctly given a certain dice roll?"
      (doseq [[dice-roll expected-value] test-cases]
        (is (= (break-wall-at break-test-wall dice-roll) expected-value))))))

(def reveal-dora-test-cases (sorted-map
  1 {:dora (d.tiles/->Tile :zi 3) :ura-dora (d.tiles/->Tile :zi 5)}
  2 {:dora (d.tiles/->Tile :zi 4) :ura-dora (d.tiles/->Tile :zi 4)}
  3 {:dora (d.tiles/->Tile :zi 5) :ura-dora (d.tiles/->Tile :zi 3)}
  4 {:dora (d.tiles/->Tile :zi 6) :ura-dora (d.tiles/->Tile :zi 2)}))

(defn- reveal-dora-test-case-reducer [test-cases key {:keys [dora ura-dora]}]
  (let [{last-dora :dora last-ura-dora :ura-dora} (get test-cases (dec key))]
    (-> test-cases
      (assoc-in [key :dora] (conj last-dora dora))
      (assoc-in [key :ura-dora] (conj last-ura-dora ura-dora)))))

(defn- reveal-dora-multiple-times [match times]
  (reduce
    (fn [acc-match _] (reveal-dora acc-match))
    match
    (range times)))

(deftest reveal-dora-is-correct
  (let [test-dead-wall (vec (d.tiles/tiles-from-notation "17263544536271z"))
        test-cases (reduce-kv reveal-dora-test-case-reducer {0 {:dora [] :ura-dora []}} reveal-dora-test-cases)
        test-cases (dissoc test-cases 0)
        match (->Match nil test-dead-wall nil nil nil [] [])]
    (testing "Are dora tiles revealed correctly multiple times?"
      (doseq [[reveal-count {expected-dora :dora expected-ura-dora :ura-dora}] test-cases
              :let [{actual-dora :dora actual-ura-dora :ura-dora} (reveal-dora-multiple-times match reveal-count)]]
          (testing (str "Are they correctly revealed after " reveal-count " time(s)?")
            (is (= expected-dora actual-dora))
            (is (= expected-ura-dora actual-ura-dora)))))))
