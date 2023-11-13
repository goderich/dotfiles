(ns filter.latex.classes)


(comment

  (defn header-tags [header]
    (let [inlines (get-in header [:c 2])
          tag? (fn [el] (= "tag" (get-in el [:c 0 1 0])))]
      (->> inlines
           (filterv tag?)
           (mapv #(get-in % [:c 0 2 0 1])))))

  (def data (json/decode (slurp "test.json") true))

  (defn- extract-title
    "Return the title of an org Header (minus tags) as a JSON object."
    [h]
    (->> (get-in h [:c 2])
         (take-while (fn [el] (not= "Span" (:t el))))
         butlast
         vec))

  (defn- extract-tags
    "Return a seq of :tags: from an org Header."
    [h]
    (->> (get-in h [:c 2])
         (filter (fn [el] (= "Span" (:t el))))
         (map (fn [el] (get-in el [:c 0 2 0 1])))))


  (let [h (get-in data [:blocks 0])
        title (extract-title h)
        tags (extract-tags h)]
    (-> h
        (assoc-in [:c 2] title)
        (update-in [:c 1 1] (fn [classes] (into classes tags))))
    )
  ;; => {:t "Header",
  ;;     :c
  ;;     [1
  ;;      ["introduction-with-four-words" ["animate" "moar"] [["center" "t"]]]
  ;;      [{:t "Str", :c "Introduction"}
  ;;       {:t "Space"}
  ;;       {:t "Str", :c "With"}
  ;;       {:t "Space"}
  ;;       {:t "Str", :c "Four"}
  ;;       {:t "Space"}
  ;;       {:t "Str", :c "Words"}]]}

  )
