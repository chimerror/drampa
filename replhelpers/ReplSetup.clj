(require
  '[drampa.claims :refer :all]
  '[drampa.hands :refer :all]
  '[drampa.matches :refer :all]
  '[drampa.players :refer :all]
  '[drampa.tiles :refer :all]
  '[drampa.utils :refer :all])

(def foo (get-initial-match))
(def pfm print-friendly-match)
(def pfp print-friendly-player)
(def pt notation-from-tiles)
(def pts #(notation-from-tiles (sort-tiles %)))