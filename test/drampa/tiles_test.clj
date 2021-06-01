(ns drampa.tiles-test
  (:require [clojure.test :refer :all]
            [drampa.tiles :refer :all]))

(deftest there-are-four-suits
  (testing "Are there 4 suits"
    (is (= (count tile-suits) 4))))
