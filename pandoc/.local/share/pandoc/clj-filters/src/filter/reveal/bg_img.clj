(ns filter.reveal.bg-img
  (:require [lib.pandoc :as pandoc]))

(defn- bg-image-attrs
  "Update (Header) Attrs with Image source and styling.
  The styling makes sure that the image is fullscreen.
  Takes two arguments: Header Attributes and Image Target,
  deconstructing them."
  [attrs target]
  (let [[header-id classes kvs] attrs
        [src img-id] target
        id (if (seq img-id) img-id header-id)
        img-attrs {"background-image" src
                   "background-size" "contain"
                   "background-repeat" "no-repeat"
                   "background-position" "contain"}]
    [id classes (into kvs img-attrs)]))

(defn- insert-img-attrs
  "Insert Image Attrs into Header, making it into a full-screen image."
  [h]
  (let [target (-> h pandoc/inlines first pandoc/target)
        attrs (bg-image-attrs (pandoc/attributes h) target)
        inlines (-> h pandoc/inlines rest vec)]
    (-> (pandoc/assoc-attributes h attrs)
        (pandoc/assoc-inlines inlines))))

(defn bg-image
  "Transform a Header with an image link into a full-screen image."
  [el]
  (if (and (pandoc/header? el)
           (pandoc/image? (first (pandoc/inlines el))))
    (insert-img-attrs el)
    el))
