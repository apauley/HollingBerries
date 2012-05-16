(ns berry.core-test
  (:use clojure.test
        berry.second))

(def test-file "../../pricefile.txt")
(def out-file "pricefile2.txt")

(deftest test-diff-file
  (testing "Is the out-file equal to the test-file"
    (is (= (slurp out-file) (slurp test-file)))))
