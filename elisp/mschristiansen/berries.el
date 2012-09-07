;;; berries.el --- Helping Mr. and Mrs. Holling Berries

;; No copyright

;; Author: Mikkel S. Christiansen
;; Version: 1.0
;; Package-Requires: nil
;; Keywords: text parsing

;;; Commentary:

;; This program was written for the HollingBerries Challenge
;; Details here: https://github.com/apauley/HollingBerries

(defconst label "R%8.2f%s%.31s\n" "Label format")
(defconst poor-suppliers '("32" "101") "Susan, Togetherness")
(defconst premium-suppliers '("219" "204") "Promise, Karel")

(defun fruit (code)
  (cond
   ((betweenp code 1100 1199) 'apples)
   ((betweenp code 1200 1299) 'bananas)
   ((betweenp code 1300 1399) 'berries)))

(defun betweenp (x low high)
  (and (>= x low) (<= x high)))

(defun lookup (code alist default)
  "Lookup value based on key and return default value if no match."
  (or (cdr (assq (fruit code) alist)) default))

(defun markup (code)
  "Return cost to price multiplier"
  (lookup code '((berries . 1.55) (bananas . 1.35) (apples . 1.40)) 1.50))

(defun lasts (code)
  "Return how long a fruit lasts in days"
  (lookup code '((bananas . 5) (apples . 14)) 7))

(defun price (code supplier cost)
  (cond
   ((member supplier premium-suppliers) (ceiling (* cost (markup code) 1.1)))
   ((member supplier poor-suppliers) (- (* cost (markup code)) 2))
   (t (* cost (markup code)))))

(defun date-only-to-time (date)
  "Convert from date (YYYY/MM/DD) to Elisp time"
  (date-to-time (concat (replace-regexp-in-string "/" "-" date) " 00:00:00")))

(defun date-add (date days)
  "Add the given number of days to the date (YYYY/MM/DD)"
  (let ((result (time-add
                 (date-only-to-time date)
                 (seconds-to-time (* days 24 60 60)))))
    (format-time-string "%Y/%m/%d" result)))

(defun sell-by (code supplier date)
  "Return the sell-by date for a fruit"
  (if (member supplier poor-suppliers)
      (date-add date (- (lasts code) 3))
    (date-add date (lasts code))))

(defun create-labels (line)
  (let* ((supplier (first line))
         (code (string-to-int (second line)))
         (desc (third line))
         (delivered (fourth line))
         (sell-by (sell-by code supplier delivered))
         (cost (/ (string-to-int (fifth line)) 100.0))
         (price (price code supplier cost))
         (amount (string-to-int (sixth line))))
    (print (list supplier code desc delivered sell-by cost price amount))
    (apply 'concat
           (make-list amount
                      (format label price sell-by desc)))))

(defun get-product ()
  (split-string-and-unquote
   (buffer-substring (line-beginning-position)
                     (line-end-position)) "[,\n]"))

(defun berries-pricefile (filename)
  "Can be called interactively e.g. `M-x berries-pricelist'
   and will prompt for the `produce.csv' file then create
   `pricelist.txt' in the same directory."
  (interactive "fproduce.csv file: ")
  (message "Generating labels ...")
  (save-excursion
    (let ((out-file "pricefile.txt"))
      (find-file out-file)
      (erase-buffer)
      (with-temp-buffer
        (insert-file-contents filename)
        (setq in-file (current-buffer))
        (while (zerop (forward-line 1))
          (setq product (get-product))
          (set-buffer out-file)
          (when product
            (insert (create-labels product)))
          (set-buffer in-file)))))
  (message "Labels created!"))

(provide 'berries)
