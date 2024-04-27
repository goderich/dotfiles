#!/usr/bin/bb
(ns core.latex
  (:require [cheshire.core :as json]
            [clojure.walk :as walk]
            [filter.classes :as classes]
            [filter.latex.center :as center]
            [filter.strip-todo :as strip-todo]))

(def filters
  (comp center/filter classes/filter strip-todo/filter))

(->> (json/decode (slurp *in*) true)
     (walk/prewalk filters)
     (json/encode)
     print)
