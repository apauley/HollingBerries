(ns berry.core
  (:use [clojure.java.io :only [reader]]
        [clojure.data.csv :only [read-csv]]
        [clj-time.core :only [plus days]]
        [clj-time.format :only [formatter parse unparse]]))

;;; Define file locations and parse the file
;;; 
(def in-file "../../produce.csv")
(def out-file "pricefile.txt")

(def items
  (drop 1  ;Leave headers out.
        (with-open [file (reader in-file)]
            (doall
             (read-csv file)))))


(defn sell-by-date-for
  "Return the sell-by-date based on category keyword"
  [category]
  (condp = category
    :apples (days 14)
    :bananas (days 5)
    (days 7)))

(defn markup-for
  "Return markup based on category keyword"
  [category]
  (condp = category
    :apples 1.40
    :bananas 1.35
    :berries 1.55
    1.50))

(defn lookup-number
  "Helper function to lookup a number in the item sequence."
  [item index]
  (Integer/parseInt (nth item index)))

(defn get-product-category
  "Return category keyword based on product code."
  [item]
  (let [product-code (lookup-number item 1)]
    (condp < product-code
      1300 :berries
      1200 :bananas
      1100 :apples
      1000 :fruit)))

;;; Getting the sales price
;;; 
(defn calculate-price
  "Calculate price based on category and format result"
  [item]
  (let [category (get-product-category item)
        cost (lookup-number item 4)]
    (str "R"
         (format "%8.2f"
                 (* (markup-for category) (/ cost 100))))))

;;; Getting the date
;;; 
(def date-format (formatter "yyyy/MM/dd"))

(defn lookup-date
  "Helper function to lookup and format the date from an item."
  [item]
  (parse date-format (nth item 3)))

(defn poor-supplier
  "Check if supplier no. 32 and allow for 3 extra days."
  [item]
  (let [supplier (lookup-number item 0)]
    (if (= supplier 32) (days -3) (days 0))))

(defn calculate-sell-by-date
  "Calculate the sell-by-date based on category, delivery date and
  supplier quality"
  [item]
  (let [category (get-product-category item)
        category-days (sell-by-date-for category)
        delivery-date (lookup-date item)
        special-case (poor-supplier item)]
    (unparse date-format
             (plus delivery-date category-days special-case))))

;;; Writing to the file in the correct format
;;;

(defn include-description
  "Take substring of length 31 from item description"
  [item]
  (subs (nth item 2) 0 31))

(defn create-tags
  "Concatenate the strings together and add end-of-line."
  [item]
  (let [tags (lookup-number item 5)]
    (repeat tags
            (str (calculate-price item)
                 (calculate-sell-by-date item)
                 (include-description item)
                 "\n"))))

(defn -main []
  (spit out-file (doall (reduce str (mapcat create-tags items)))))
