(ns filter.reveal.animate
  (:require [lib.pandoc :as pandoc]))

(defn animate [el]
  (if (and (pandoc/header? el)
           (some #{"animate"} (pandoc/classes el)))
    (pandoc/assoc-attributes el "auto-animate" "true")
    el))
