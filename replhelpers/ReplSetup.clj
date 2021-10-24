#_{:clj-kondo/ignore [:refer-all]}
(require
  '[drampa.claims :refer :all]
  '[drampa.hands :refer :all]
  '[drampa.matches :refer :all]
  '[drampa.players :refer :all]
  '[drampa.tiles :refer :all]
  '[drampa.utils :refer :all])

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def foo (drampa.matches/get-initial-match))
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pfm drampa.matches/print-friendly-match)
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pfp drampa.players/print-friendly-player)
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pt drampa.tiles/notation-from-tiles)
(def tt drampa.tiles/tiles-from-notation)
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def t #(last (tt %)))
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pts #(drampa.tiles/notation-from-tiles (drampa.tiles/sort-tiles %)))