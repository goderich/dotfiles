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

(defn str? [el]
  (= (:t el) "Str"))

(defn space? [el]
  (= (:t el) "Space"))

;; Constructors

(defn raw-inline [format text]
  {:t "RawInline" :c [format text]})

(defn para [& inlines]
  {:t "Para" :c (vec inlines)})

;; Defining filters

(defn- expand-filter-conds [attr-map]
  (when-let [conds (:if attr-map)]
    (if (vector? conds)
      conds
      (vector conds))))

(defmacro deffilter
  "Syntactic sugar for creating pandoc filters.
  All filters are called `filter`, since they
  are expected to be called from different namespaces.

  TYPE is a pandoc predicate, choosing which elements
  the filter will run on.

  ARG is a binding to be used in conditions and in RESULT.

  ATTR-MAP is a map of various attributes passed to the macro,
  the main being `:if`, which takes either a single s-exp or
  a vector of s-exp as a value.
  Each s-exp is evaluated with the element in place of ARG,
  and if all conditions are true, the element is replaced
  with the output of running RESULT.

  Note that due to arity constraints, if a docstring is present,
  then an ATTR-MAP *must* be supplied (even an empty map).

  Example usage:
  (deffilter header?
     \"Docstr\"
     [el]
     {:if (some #{\"animate\"} (classes el))}
     (assoc-attributes el \"auto-animate\" \"true\"))
  "
  {:clj-kondo/ignore [:unresolved-symbol]}
  ([type [arg] result]
   `(deffilter ~type [~arg] {} ~result))
  ([type ^String docstring [arg] attr-map result]
   `(deffilter ~type [~arg] ~(assoc attr-map :doc docstring) ~result))
  ([type [arg] attr-map result]
   (assert (map? attr-map))
   `(defn ~(with-meta 'filter (select-keys attr-map [:doc]))
      [~arg]
      (if (and (~type ~arg)
               ~@(expand-filter-conds attr-map))
        ~result
        ~arg))))
