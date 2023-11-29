#!/usr/bin/bb
(ns core.reveal
  (:require [cheshire.core :as json]
            [clojure.walk :refer [prewalk]]
            [filter.classes :as classes]
            [filter.reveal.bg-img :as bg-img]
            [filter.reveal.stretch :as stretch]
            [filter.reveal.animate :as animate]))

(def filters
  (comp animate/filter
        stretch/filter
        bg-img/filter
        classes/header-parse-tags))

(->> (json/decode (slurp *in*) true)
     (prewalk filters)
     (json/encode)
     print)
