(ns holling.core-test
  (:use clojure.test
        holling.core))

(def test-file "pricefile.txt")
(def new-file "pricefile2.txt")

(deftest test-diff-file
  (testing "Is the new-file equal to the test-file"
    (is (= (slurp new-file) (slurp test-file)))))