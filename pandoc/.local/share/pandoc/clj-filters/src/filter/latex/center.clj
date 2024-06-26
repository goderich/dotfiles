(ns filter.latex.center
  (:require [lib.pandoc :as pandoc :refer [deffilter]]))

(defn- center? [el]
  (let [[_ classes attributes] (pandoc/attrs el)
        keys (map first attributes)
        commands #{"center" "centered" "centering"}]
    (or
     (some commands classes)
     (some commands keys))))

(defn- centering-latex
  "Insert raw LaTeX centering command around the input element."
  [el]
  (vec
   (concat
    [(pandoc/raw-inline "latex" "\\centering{")]
    el
    [(pandoc/raw-inline "latex" "}")])))

(deffilter pandoc/header? [el]
  {:if (center? el)}
  (pandoc/update-inlines el centering-latex))
