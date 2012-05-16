(ns berry.core-test
  (:use clojure.test
        berry.core))

(def test-file "../../pricefile.txt")

(deftest test-diff-file
  (testing "Is the out-file equal to the test-file"
    (is (= (slurp out-file) (slurp test-file)))))
