(ns filter.reveal.stretch
  (:require [lib.pandoc :as pandoc :refer [deffilter]]))

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

(deffilter pandoc/para?
  "If you don't want a given image to be stretched,
  add the :nostretch attribute to it.
  Org syntax:

  #+ATTR_HTML: :nostretch

  (The actual value of the attribute is ignored,
  so it can be anything.)"
  [el]
  {:if (-> el pandoc/inlines first pandoc/image?)}
  (-> el pandoc/inlines first stretch-image))
