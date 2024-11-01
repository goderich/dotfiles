#!/usr/bin/bb
(ns core.reveal
  (:require [cheshire.core :as json]
            [lib.pandoc :as pandoc]
            [filter.classes :as classes]
            [filter.reveal.bg-img :as bg-img]
            [filter.reveal.stretch :as stretch]
            [filter.reveal.animate :as animate]
            [filter.strip-todo :as strip-todo]))

(def ^:private filters
  (comp animate/filter
        stretch/filter
        classes/filter
        strip-todo/filter
        bg-img/filter))

(defn- filter-nohandout
  "Remove headers tagged :nohandout: and all their contents."
  [input]
  (loop [acc []
         tail input]
    (cond
      (empty? tail) acc

      (let [el (first tail)]
        (and (pandoc/header? el) (some #{"nohandout"} (pandoc/classes el))))
      (let [lvl (-> tail first pandoc/header-level)]
        (recur acc (drop-while #(or (not (pandoc/header? %)) (< lvl (pandoc/header-level %))) (rest tail))))

      :else (recur (conj acc (first tail)) (rest tail)))))

(let [input (json/decode (slurp *in*) true)
      handout? (-> input :meta :handout :c)]
  (-> input
      (update :blocks #(map filters %))
      ;; Removing :nohandout: parts affects more than
      ;; just individual elements, so it cannot be done with `map`.
      ((fn [x] (if handout?
                 (update x :blocks filter-nohandout)
                 x)))
      (json/encode)
      (print)))
