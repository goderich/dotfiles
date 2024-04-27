#!/usr/bin/bb
(ns core.reveal
  (:require [cheshire.core :as json]
            [clojure.walk :as walk]
            [filter.classes :as classes]
            [filter.reveal.bg-img :as bg-img]
            [filter.reveal.stretch :as stretch]
            [filter.reveal.animate :as animate]
            [filter.strip-todo :as strip-todo]))

(def filters
  (comp animate/filter
        stretch/filter
        classes/filter
        strip-todo/filter
        bg-img/filter
        ))

(->> (json/decode (slurp *in*) true)
     (walk/prewalk filters)
     (json/encode)
     print)
