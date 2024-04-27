(ns filter.strip-todo
  (:require [lib.pandoc :as pandoc :refer [deffilter]]))

(deffilter pandoc/header?
  "Remove TODOs from a Header.
   Removes only those TODOs which get read in as Spans."
  [el]
  {:if (pandoc/span? (first (pandoc/inlines el)))}
  (let [drop? (fn [x] (or (pandoc/span? x) (pandoc/space? x)))]
    (pandoc/update-inlines el #(drop-while drop? %))))
