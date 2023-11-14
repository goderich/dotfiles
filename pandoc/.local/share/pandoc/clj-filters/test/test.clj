(ns test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [cheshire.core :as json]
            [clojure.walk :refer [prewalk]]
            [lib.pandoc :as pandoc]
            [filter.classes :as classes]
            [filter.latex.center :as center]))

(deftest parse-test
  (testing "Basic tag parsing"
    (let [h
          {:t "Header",
           :c [1 ["head" [] []]
               [{:t "Str", :c "Head"}
                {:t "Space"}
                {:t "Span",
                 :c [["" ["tag"] [["tag-name" "center"]]]
                     [{:t "SmallCaps", :c [{:t "Str", :c "center"}]}]]}]]}]
      (is (=
           (classes/header-parse-tags h)
           {:t "Header", :c [1 ["head" ["center"] []] [{:t "Str", :c "Head"}]]})))
    ))

(deftest center-test
   (testing "LaTeX centering on headers with attrs."
     (let [h
           {:t "Header",
            :c [1 ["head" [] [["centering" "t"]]] [{:t "Str", :c "Head"}]]}]
       (is (=
            (center/center-header h)
            {:t "Header",
             :c [1 ["head" [] [["centering" "t"]]]
              [{:t "RawInline", :c ["latex" "\\centering{"]}
               {:t "Str", :c "Head"}
               {:t "RawInline", :c ["latex" "}"]}]]}))))

  (testing "LaTeX centering on headers with tags."
    (let [h
          {:t "Header",
           :c [1 ["head" [] []]
               [{:t "Str", :c "Head"}
                {:t "Space"}
                {:t "Span",
                 :c [["" ["tag"] [["tag-name" "center"]]]
                     [{:t "SmallCaps", :c [{:t "Str", :c "center"}]}]]}]]}]
      (is (=
           (center/center-header (classes/header-parse-tags h))
           {:t "Header",
            :c [1 ["head" ["center"] []]
                [{:t "RawInline", :c ["latex" "\\centering{"]}
                 {:t "Str", :c "Head"}
                 {:t "RawInline", :c ["latex" "}"]}]]})))
   ))
