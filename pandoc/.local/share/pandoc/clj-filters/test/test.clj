(ns test
  (:require [clojure.test :as t :refer [deftest is testing]]
            ))

(deftest testname
  (testing "Basic functionality"
    (is
     (= :A
        :B))))
