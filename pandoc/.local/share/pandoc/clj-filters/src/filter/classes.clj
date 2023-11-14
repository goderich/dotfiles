(ns filter.classes
  (:require [lib.pandoc :as pandoc]))

(defn- span->tag
  "Extract a tag name from a Span pandoc object.
  A typical Span object looks like this:
  {:t \"Span\",
   :c [[\"\" [\"tag\"] [[\"tag-name\" \"center\"]]]             ;; Attributes
       [{:t \"SmallCaps\", :c [{:t \"Str\", :c \"center\"}]}]]} ;; Inlines
  The Haskel type signature is: Span Attr [Inline]

  We can extract the tag name from the Attributes or the Inlines,
  it does not seem to matter which.
  There is just the one key-value pair per tag.
  Here I am going with the Attributes."
  [el]
  (let [[_ _ [[_ tag]]] (pandoc/attributes el)]
    tag))

(defn- extract-tags
  "Return a seq of :tags: from an org Header."
  [h]
  (->> (pandoc/inlines h)
       (filter pandoc/span?)
       (map span->tag)))

(defn- remove-spans
  "Remove Span objects at the end of an Inlines in a Header.
  The last object before the first Span is always a Space,
  so we remove it as well."
  [inlines]
  (->> inlines
       (take-while (complement pandoc/span?))
       butlast
       vec))

(defn header-parse-tags
  "Parse the :tags: in an org header into Pandoc classes in the Header object."
  [el]
  (if (pandoc/header? el)
    (let [tags (extract-tags el)
          add-tags (fn [classes] (into classes tags))]
      (-> el
          (pandoc/update-inlines remove-spans)
          (pandoc/update-classes add-tags)))
    el))
