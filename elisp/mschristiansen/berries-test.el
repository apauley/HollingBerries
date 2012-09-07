;;; berries-test.el -- tests for berries.el

;; No copyright

;; Author: Mikkel S. Christiansen
;; Version: 1.0
;; Package-Requires: nil
;; Keywords: text parsing

;;; Commentary:

(require 'berries)

(ert-deftest berries-test-label ()
  "Test the label function in berries.el"
  (should (string=
           "R   21.072012/02/29Apples 1kg Golden Delicious. Th\n"
           (format label 21.07 "2012/02/29"
                   "Apples 1kg Golden Delicious. The sweetest Apples!"))))

(ert-deftest test-sell-by ()
  (should (string= "2012/02/29" (sell-by 1101 "15" "2012/02/15"))))

(ert-deftest test-price ()
  (should (equal 21.07 (price 1101 "15" 15.05))))

(ert-deftest test-create-labels ()
  (should (string= "R   21.072012/02/29Apples 1kg Golden Delicious. Th\n"
                   (create-labels '("15" "1101" "Apples 1kg Golden Delicious. The sweetest Apples! Always a favourite. Love, Mrs. Hollingberry" "2012/02/15" "1505" "1")))))
