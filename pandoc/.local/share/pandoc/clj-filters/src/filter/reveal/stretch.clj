(ns filter.reveal.stretch
  (:require [lib.pandoc :as pandoc]))

(defn- nostretch? [el]
  (->> (pandoc/attributes el)
       (map first)
       (some #{"nostretch"})
       some?))

(defn- dissoc-attribute [attributes name]
  (as-> attributes it
    (into {} it)
    (dissoc it name)
    (into [] it)))

(defn- stretch-para-image [para]
  (let [image (-> para pandoc/inlines first)]
    (if (nostretch? image)
      (pandoc/para (pandoc/update-attributes image dissoc-attribute "nostretch"))
      (pandoc/update-classes image conj "r-stretch"))))

(defn stretch [el]
  (if (and (pandoc/para? el)
           (-> el pandoc/inlines first pandoc/image?))
    (stretch-para-image el)
    el))
