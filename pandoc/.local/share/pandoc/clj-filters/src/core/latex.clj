#!/usr/bin/bb
(ns core.latex
  (:require [cheshire.core :as json]
            [clojure.walk :refer [prewalk]]
            [lib.pandoc :as pandoc]))

(def transform-pandoc-ast
  (comp center-header))

(defn -main [& _args]
  (->> (json/decode (slurp *in*) true)
       (prewalk transform-pandoc-ast)
       (json/encode)
       print))
