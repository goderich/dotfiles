(ns filter.latex.center
  (:require [lib.pandoc :as pandoc]))

(defn- center? [el]
  (let [[_ classes attributes] (pandoc/attrs el)
        keys (map first attributes)]
    (or
     (some #{"center" "centered" "centering"} classes)
     (some #{"center" "centered" "centering"} keys))))

(defn- centering-latex
  "Insert raw LaTeX centering command around the input element."
  [el]
  (vec
   (concat
    [(pandoc/raw-inline "latex" "\\centering{")]
    el
    [(pandoc/raw-inline "latex" "}")])))

(defn center-header [el]
  (if (and (pandoc/header? el) (center? el))
    (pandoc/update-inlines el centering-latex)
    el))
