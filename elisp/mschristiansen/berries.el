;;; berries.el --- Helping Mr. and Mrs. Holling Berries

;; No copyright

;; Author: Mikkel S. Christiansen
;; Version: 1.0
;; Package-Requires: nil
;; Keywords: text parsing

;;; Commentary:

;; This program converts a file in csv format into labels.

(defconst in "produce.csv" "Location for the data file")
(defconst out "pricefile.txt" "Location for the label file")

(defconst poor '("32" "101") "Poor supplier")
(defconst premium '("219" "204") "Premium suppliers")

(defun lookup (key alist default)
  "Lookup value based on key and return default value if no match."
  (or (cdr (assq key alist)) default))

(defun markup (fruit)
  "Return cost to price multiplier"
  (lookup fruit '((berries . 1.55) (bananas . 1.35) (apples . 1.40)) 1.50))

(defun lasts (fruit)
  "Return how long a fruit lasts in days"
  (lookup fruit '((bananas . 5) (apples . 14)) 7))

(defun read-lines (file)
  "Return a list of lines in a file"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))


(defun transform (inputFile outputFile)
  (let ((moreLines t))
    (find-file outputFile)
    (erase-buffer)
    (find-file inputFile)
    (goto-char 1)
    (while moreLines
      
      (find-file outputFile)
      (insert "mikkel\n")
      (save-buffer)
      (setq moreLines (zerop (forward-line 1)))
      )))

;; (search-forward "(")

;; (setq splitPos (1- (point)))
;; (beginning-of-line)
;; (setq fName (buffer-substring-no-properties (point) splitPos))

;; (end-of-line)
;; (setq restLine (buffer-substring-no-properties splitPos (point) ))

;; ;; create the file
;; (find-file fName)
;; (insert "# --\n")
;; (insert fName restLine "\n{\n$0\n}" )
;; (save-buffer)
;; (kill-buffer (current-buffer))

;; (setq moreLines (= 0 (forward-line 1)))


;(mapcar (lambda (line) (split-string line ",")) *data*)

(defconst *data* (read-lines in))


(defun berries-pricelist (filename)
  "Can be called interactively e.g. `M-x berries-pricelist'
   and will prompt for the `produce.csv' file then create
   `pricelist.txt' in the same directory."
  (interactive "fproduce.csv file: ")
  (transform filename out))

(ert-deftest diff-test ()
    (should t))

