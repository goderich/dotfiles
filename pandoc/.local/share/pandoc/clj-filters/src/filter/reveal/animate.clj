(ns filter.reveal.animate
  (:require [lib.pandoc :as pandoc :refer [deffilter]]))

(deffilter pandoc/header? [el]
  {:if (some #{"animate"} (pandoc/classes el))}
  (pandoc/assoc-attributes el "auto-animate" "true"))
