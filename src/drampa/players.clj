(ns drampa.players
  (:require [drampa.claims :as d.claims]
            [drampa.tiles :as d.tiles]
            [drampa.utils :refer [first-where]]))

(defrecord Player [score wind hand melds discards discard-logic claim-logic])

(defn print-friendly-player [{:keys [hand melds discards] :as player}]
  (-> player
      (assoc :hand (d.tiles/notation-from-tiles (d.tiles/sort-tiles hand)))
      (assoc :melds (map d.tiles/notation-from-tiles melds))
      (assoc :discards (d.tiles/notation-from-tiles discards))))

(defn get-player-by-wind [players wind]
  (first-where #(= wind (:wind %)) players))

(defmulti make-discard (fn [{:keys [active-player-wind] :as match} _]
  (:discard-logic (get-player-by-wind (:players match) active-player-wind))))

(defmethod make-discard :random [match discard-context]
  (let [{:keys [players active-player-wind]} match
        {:keys [hand]} (get-player-by-wind players active-player-wind)
        legal-hand (filterv #(not (some #{%} (:illegal discard-context))) hand)
        index-to-discard (rand-int (count legal-hand))]
    (get legal-hand index-to-discard)))

(defmethod make-discard :always-last [match discard-context]
  (let [{:keys [players active-player-wind]} match
        {:keys [hand]} (get-player-by-wind players active-player-wind)
        legal-hand (filterv #(not (some #{%} (:illegal discard-context))) hand)]
    (last legal-hand)))

(defmulti make-claim (fn [{:keys [players]} claiming-wind _]
  (:claim-logic (get-player-by-wind players claiming-wind))))

(defmethod make-claim :wins-only [{:keys [active-player-wind]} claiming-wind legal-calls]
  (cond (:ron legal-calls) (d.claims/->Claim claiming-wind :ron nil active-player-wind)
        (:tsumo legal-calls) (d.claims/->Claim claiming-wind :tsumo nil active-player-wind)
        :else nil))

(defmethod make-claim :always-claim [{:keys [active-player-wind]} claiming-wind legal-calls]
  (cond (:ron legal-calls) (d.claims/->Claim claiming-wind :ron nil active-player-wind)
        (:tsumo legal-calls) (d.claims/->Claim claiming-wind :tsumo nil active-player-wind)
        (:kan legal-calls) (d.claims/->Claim claiming-wind :kan nil active-player-wind)
        (:pon legal-calls) (d.claims/->Claim claiming-wind :pon (first (:pon legal-calls)) active-player-wind)
        (:chii legal-calls) (d.claims/->Claim claiming-wind :chii (first (:chii legal-calls)) active-player-wind)
        :else nil))

(defmethod make-claim :random [{:keys [active-player-wind]} claiming-wind legal-calls]
  (cond (:ron legal-calls) (d.claims/->Claim claiming-wind :ron nil active-player-wind)
        (:tsumo legal-calls) (d.claims/->Claim claiming-wind :tsumo nil active-player-wind)
        (and (:kan legal-calls) (<= (rand) 0.75)) (d.claims/->Claim claiming-wind :kan nil active-player-wind)
        (and (:pon legal-calls) (<= (rand) 0.50))
          (let [pon-options (:pon legal-calls)
                option-index (rand-int (count pon-options))]
            (d.claims/->Claim claiming-wind :pon (get pon-options option-index) active-player-wind))
        (and (:chii legal-calls) (<= (rand) 0.25))
          (let [chii-options (:chii legal-calls)
                option-index (rand-int (count chii-options))]
            (d.claims/->Claim claiming-wind :chii (get chii-options option-index) active-player-wind))
        :else nil))
