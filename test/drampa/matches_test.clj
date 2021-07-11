(ns drampa.matches-test
  (:require [clojure.test :refer :all]
            [drampa.matches :refer :all]
            [drampa.claims :as d.claims]
            [drampa.tiles :as d.tiles]
            [drampa.players :as d.players]))

(deftest get-initial-match-is-correct
  (let [{:keys [wall dead-wall players prevailing-wind dora ura-dora]} (get-initial-match)]
    (testing "Is the starting wall of a match initialized correctly?"
      (is (not (nil? wall)))
      (is (= (count wall)) 136))
    (testing "Are the players of a match initialized correctly?"
      (is (not (nil? players)))
      (is (= (count players) 4))
      (testing "Are the seat winds of a match initialized correctly?"
        (let [player-winds (map :wind players)]
          (is (not (nil? player-winds)))
          (is (= (count player-winds) 4))
          (is (distinct? player-winds))
          (is (some #{:east} player-winds))
          (is (some #{:south} player-winds))
          (is (some #{:west} player-winds))
          (is (some #{:north} player-winds))))
      (testing "Are the starting scores of a match initialized correctly?"
        (let [player-scores (map :score players)]
          (is (not (nil? player-scores)))
          (is (= (count player-scores) 4))
          (is (every? #(= starting-score %) player-scores))))
      (testing "Have the hands been dealt correctly?"
        (doseq [{:keys [wind hand]} players]
          (is (= (count hand) (if (= :east wind) 14 13))))))
    (testing "Is the prevailing wind of a match initialized correctly?"
      (is (= prevailing-wind :east)))
    (testing "Is the wall broken correctly?"
      (is (not (nil? dead-wall)))
      (is (= (count dead-wall) 14))
      (is (not (nil? wall)))
      (is (= (count wall) 69)))
    (testing "Has one dora been revealed?"
      (is (not (nil? dora)))
      (is (= (count dora) 1))
      (is (not (nil? dora)))
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
          (reduce-kv #(assoc %1 %2 (map d.tiles/tiles-from-notation %3)) (sorted-map) break-wall-at-test-cases)]
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
  (let [test-dead-wall (d.tiles/tiles-from-notation "17263544536271z")
        test-cases (reduce-kv reveal-dora-test-case-reducer {0 {:dora [] :ura-dora []}} reveal-dora-test-cases)
        test-cases (dissoc test-cases 0)
        match (->Match nil test-dead-wall nil nil nil [] [])]
    (testing "Are dora tiles revealed correctly multiple times?"
      (doseq [[reveal-count {expected-dora :dora expected-ura-dora :ura-dora}] test-cases
              :let [{actual-dora :dora actual-ura-dora :ura-dora} (reveal-dora-multiple-times match reveal-count)]]
          (testing (str "Are they correctly revealed after " reveal-count " time(s)?")
            (is (= expected-dora actual-dora))
            (is (= expected-ura-dora actual-ura-dora)))))))

(deftest get-dora-from-indicator-is-correct
  (testing "Can the dora be retrieved from the indicator?"
    (testing "If the tile is a red five?"
      (is (= (get-dora-from-indicator (d.tiles/->Tile :pin 0)) (d.tiles/->Tile :pin 6)))
      (is (= (get-dora-from-indicator (d.tiles/->Tile :sou 0)) (d.tiles/->Tile :sou 6)))
      (is (= (get-dora-from-indicator (d.tiles/->Tile :man 0)) (d.tiles/->Tile :man 6))))
    (testing "If the tile is a nine?"
      (is (= (get-dora-from-indicator (d.tiles/->Tile :pin 9)) (d.tiles/->Tile :pin 1)))
      (is (= (get-dora-from-indicator (d.tiles/->Tile :sou 9)) (d.tiles/->Tile :sou 1)))
      (is (= (get-dora-from-indicator (d.tiles/->Tile :man 9)) (d.tiles/->Tile :man 1))))
    (testing "If the tile is a different number tile?"
      (doseq [suit [:pin :sou :man] rank [1 2 3 4 5 6 7 8]]
        (is (= (get-dora-from-indicator (d.tiles/->Tile suit rank)) (d.tiles/->Tile suit (inc rank))))))
    (testing "If the tile is a North Wind?"
      (is (= (get-dora-from-indicator (d.tiles/->Tile :zi 4)) (d.tiles/->Tile :zi 1))))
    (testing "If the tile is a Red Dragon?"
      (is (= (get-dora-from-indicator (d.tiles/->Tile :zi 7)) (d.tiles/->Tile :zi 5))))
    (testing "If the tile is a different honor?"
      (doseq [rank [1 2 3 5 6]]
        (is (= (get-dora-from-indicator (d.tiles/->Tile :zi rank)) (d.tiles/->Tile :zi (inc rank))))))))

(def deal-initial-hands-test-wall
  (d.tiles/tiles-from-notation "9p9s9m7z5p4z4m4s4p3333z3333m3333s3333p2222z2222m2222s2222p1111z1111m1111s1111p"))

(def deal-initial-hands-expected-hands
  {:east "11112222333345p" :south "1111222233334s" :west "1111222233334m" :north "1111222233334z"})

(defn load-deal-initial-hands-expected-hands []
  (reduce-kv #(assoc %1 %2 (d.tiles/tiles-from-notation %3)) {} deal-initial-hands-expected-hands))

(deftest deal-initial-hands-is-correct
  (testing "Are hands dealt correctly from the wall?"
    (let [test-wall deal-initial-hands-test-wall
          match (->Match test-wall nil (fill-players 0) nil nil nil nil)
          {:keys [wall players]} (deal-initial-hands match)
          expected-hands (load-deal-initial-hands-expected-hands)]
      (testing "Is the remaining wall correct?"
        (is (= wall (d.tiles/tiles-from-notation "9p9s9m7z"))))
      (testing "Are the hands correct?"
        (doseq [{:keys [wind hand]} players
                :let [expected-hand (expected-hands wind)]]
          (testing (str "Is the " wind " hand correct?")
            (is (= expected-hand (d.tiles/sort-tiles hand)))))))))

(def sentinel-tile (d.tiles/->Tile :zi 8))

(def sentinel-tile-dead-wall (d.tiles/->Tile :zi 9))

(defn test-match-from-starting-hands [hands]
  (let [[east-hand south-hand west-hand north-hand] hands
        hand-tiles (apply concat hands)
        live-wall-beginning
          (concat
            (subvec east-hand 0 4)
            (subvec south-hand 0 4)
            (subvec west-hand 0 4)
            (subvec north-hand 0 4)
            (subvec east-hand 4 8)
            (subvec south-hand 4 8)
            (subvec west-hand 4 8)
            (subvec north-hand 4 8)
            (subvec east-hand 8 12)
            (subvec south-hand 8 12)
            (subvec west-hand 8 12)
            (subvec north-hand 8 12)
            [(get east-hand 12) (get south-hand 12) (get west-hand 12) (get north-hand 12) (get east-hand 13)])
        wall
          (vec (concat
            (repeat 6 sentinel-tile-dead-wall)
            (repeat 69 sentinel-tile)
            (reverse live-wall-beginning)
            (repeat 8 sentinel-tile-dead-wall)))
        players (fill-players 0 :always-last :always-claim)]
    (get-initial-match wall 4 players)))

(def get-claims-test-cases [
  [
    ["1111222233334z2p" "444z11112222m13p" "555566667777z1s" "3333444455506m"]
    [nil {:claiming-wind :south :claim-type :chii :choice "123p" :discarding-wind :east} nil nil]]
  [
    ["1111222233334z0p" "444z11112222m46p" "555566667777z1s" "3333444455506m"]
    [nil {:claiming-wind :south :claim-type :chii :choice "406p" :discarding-wind :east} nil nil]]
  [
    ["1111222233334p2z" "444z11112222m13z" "555566667777p1s" "3333444455506m"]
    [nil nil nil nil]]
  [
    ["1111222233334z2p" "555566667777z1s" "444z11112222m13p" "3333444455506m"]
    [nil nil nil nil]]
  [
    ["1111222233334z2p" "444z11112222m13p" "55556666777z22p" "3333444455506m"]
    [
      nil
      {:claiming-wind :south :claim-type :chii :choice "123p" :discarding-wind :east}
      {:claiming-wind :west :claim-type :pon :choice "222p" :discarding-wind :east}
      nil]]
  [
    ["1111222233334z2p" "444z11112222m13s" "3333444455506m" "55556666777z22p"]
    [nil nil nil {:claiming-wind :north :claim-type :pon :choice "222p" :discarding-wind :east}]]
    ])

(defn- load-get-claims-expected-claim [claim-map]
  (if (nil? claim-map)
      nil
      (d.claims/map->Claim (update claim-map :choice d.tiles/tiles-from-notation))))

(deftest get-claims-is-correct
  (testing "Are claims correctly offered and taken from known walls?"
    (doseq [[starting-hands-notation expected-claims-maps] get-claims-test-cases
            :let [starting-hands (mapv d.tiles/tiles-from-notation starting-hands-notation)
                  expected-claims (mapv load-get-claims-expected-claim expected-claims-maps)]]
      (let [match (-> (test-match-from-starting-hands starting-hands)
                      (perform-draw))
            actual-claims (get-claims match)]
        (is (= expected-claims actual-claims))))))