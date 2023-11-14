(ns lib.pandoc)

(defn attributes
  "Return the Attributes based on type.
  According to the docstring on the Attr type in the pandoc source code,
  attributes are [identifier, classes, key-value pairs].
  This is how they get returned, as a vector, in that order.
  Use destructuring to get the values you need."
  [el]
  (case (:t el)
    "Header" (get-in el [:c 1])))

(defn inlines-dispatch
  "Get the correct location of the Inlines based on type.
  Used for get, assoc, and update functions."
  [el]
  (case (:t el)
    "Header" [:c 2]))

(defn inlines [el]
  (get-in el (inlines-dispatch el)))

(defn update-inlines [el f]
  (update-in el (inlines-dispatch el) f))

(defn assoc-inlines [el val]
  (assoc-in el (inlines-dispatch el) val))

(defn header? [el]
  (= (:t el) "Header"))

(defn span? [el]
  (= (:t el) "Span"))

(defn raw-inline [format text]
  {:t "RawInline" :c [format text]})
