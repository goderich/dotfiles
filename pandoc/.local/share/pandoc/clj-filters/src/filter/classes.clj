(ns filter.classes
  (:require [lib.pandoc :as pandoc :refer [deffilter]]))

(defn- span->tag
  "Extract a tag name from a Span pandoc object.
  A typical Span object looks like this:
  {:t \"Span\",
   :c [[\"\" [\"tag\"] [[\"tag-name\" \"center\"]]]             ;; Attrs
       [{:t \"SmallCaps\", :c [{:t \"Str\", :c \"center\"}]}]]} ;; Inlines
  The Haskell type signature is: Span Attr [Inline]

  We can extract the tag name from the Attributes or the Inlines,
  it does not seem to matter which.
  There is just the one key-value pair per tag.
  Here I am going with the Attributes."
  [el]
  (let [[[_ tag]] (pandoc/attributes el)]
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

(deffilter pandoc/header?
  "Parse the :tags: in an org header into Pandoc classes in the Header object.
  The :tags: are encoded as Spans at the end of a Header.
  However, TODOs are also encoded as Spans, so we need to be careful here.
  Right now, the solution is to get rid of TODOs first."
  [el]
  {:if (some pandoc/span? (pandoc/inlines el))}
  (let [tags (extract-tags el)
        add-tags (fn [classes] (into classes tags))]
    (-> el
        (pandoc/update-inlines remove-spans)
        (pandoc/update-classes add-tags))))
