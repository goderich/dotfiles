(ns lib.pandoc)

;; Attrs [_, Classes, Attributes]

(defn- attrs-dispatch
  "Get the correct Attrs location based on type.
  According to the docstring on the Attr type in the pandoc source code,
  Attrs are [identifier, classes, attribute key-value pairs].
  This is how they get returned, as a vector, in that order.
  Use destructuring to get the values you need."
  [el]
  (case (:t el)
    "Header" [:c 1]
    "Span" [:c 0]
    "Image" [:c 0]))

(defn attrs [el]
  (get-in el (attrs-dispatch el)))

(defn update-attrs [el f]
  (update-in el (attrs-dispatch el) f))

(defn assoc-attrs [el val]
  (assoc-in el (attrs-dispatch el) val))

(defn classes [el]
  (get-in el (conj (attrs-dispatch el) 1)))

(defn update-classes [el f & args]
   (apply update-in el (conj (attrs-dispatch el) 1) f args))

(defn assoc-classes [el val]
  (assoc-in el (conj (attrs-dispatch el) 1) val))

(defn attributes [el]
  (get-in el (conj (attrs-dispatch el) 2)))

(defn update-attributes [el f & args]
  (apply update-in el (conj (attrs-dispatch el) 2) f args))

(defn assoc-attributes
  ([el val]
   (assoc-in el (conj (attrs-dispatch el) 2) val))
  ([el key val]
   (let [attrs (as-> (attributes el) it
                 (into {} it)
                 (assoc it key val)
                 (into [] it))]
     (assoc-attributes el attrs))))

;; Inlines

(defn- inlines-dispatch
  "Get the correct location of the Inlines based on type.
  Used for get, assoc, and update functions."
  [el]
  (case (:t el)
    "Header" [:c 2]
    "Para" [:c]))

(defn inlines [el]
  (get-in el (inlines-dispatch el)))

(defn update-inlines [el f]
  (update-in el (inlines-dispatch el) f))

(defn assoc-inlines
  ([el val]
   (assoc-inlines el [] val))
  ([el [& keys] val]
   (assoc-in el (into (inlines-dispatch el) keys) val)))

;; Targets

(defn- target-dispatch
  "Get the correct location of the Target based on type.
  Used with Links and Images.
  Note that Target is a vector of [URL, title]."
  [el]
  (case (:t el)
    "Image" [:c 2]))

(defn target [el]
  (get-in el (target-dispatch el)))

;; Predicates

(defn header? [el]
  (= (:t el) "Header"))

(defn span? [el]
  (= (:t el) "Span"))

(defn image? [el]
  (= (:t el) "Image"))

(defn para? [el]
  (= (:t el) "Para"))

;; Constructors

(defn raw-inline [format text]
  {:t "RawInline" :c [format text]})

(defn para [& inlines]
  {:t "Para" :c (vec inlines)})
