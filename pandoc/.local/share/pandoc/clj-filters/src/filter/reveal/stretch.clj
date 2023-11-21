(ns filter.reveal.stretch
  (:require [lib.pandoc :as pandoc]))

(defn- nostretch?
  "Does EL have a `nostretch` attribute?"
  [el]
  (->> (pandoc/attributes el)
       (map first)
       (some #{"nostretch"})
       some?))

(defn- dissoc-attribute [attributes name]
  (as-> attributes it
    (into {} it)
    (dissoc it name)
    (into [] it)))

(defn- stretch-image [image]
  (if (nostretch? image)
    (pandoc/update-attributes image dissoc-attribute "nostretch")
    (pandoc/update-classes image conj "r-stretch")))

(defn stretch
  " If you don't want a given image to be stretched,
  add the :nostretch attribute to it.
  Org syntax:

  #+ATTR_HTML: :nostretch

  (The actual value of the attribute is ignored,
  so it can be anything.)"
  [el]
  (if (and (pandoc/para? el)
           (-> el pandoc/inlines first pandoc/image?))
    (let [img (-> el pandoc/inlines first stretch-image)]
      (pandoc/assoc-inlines el [0] img))
    el))
