(ns drampa.claims)

(defrecord Claim [claiming-wind claim-type choice discarding-wind])

(def claim-types [:chii :pon :kan :tsumo :ron])

(defn compare-claims [x y]
  (compare (.indexOf claim-types (:claim-type x)) (.indexOf claim-types (:claim-type y))))
