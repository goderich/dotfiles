#!/usr/bin/bb
(ns core.latex
  (:require [cheshire.core :as json]
            [clojure.walk :refer [prewalk]]
            [filter.classes :as classes]
            [filter.latex.center :as center]))

(def filters
  (comp center/filter classes/header-parse-tags))

(->> (json/decode (slurp *in*) true)
     (prewalk filters)
     (json/encode)
     print)
