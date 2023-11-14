(ns filter.latex.classes
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

(defn- extract-title
  "Return the title of an org Header (minus tags) as a JSON object.
  The last object before the first tag (Span) is always a Space,
  so we remove it."
  [h]
  (->> (pandoc/inlines h)
       (take-while (complement pandoc/span?))
       butlast
       vec))

(defn header-parse-tags
  "Parse the :tags: in an org header into Pandoc classes in the Header object."
  [h]
  (let [title (extract-title h)
        tags (extract-tags h)
        attrs (-> (pandoc/attributes h)
                  (update-in [1] (fn [classes] (into classes tags))))]
    (-> h
        (pandoc/assoc-inlines title)
        (pandoc/assoc-attributes attrs))))
