(ns berry.second
  (:use [clojure.java.io :only [reader]]
        [clojure.data.csv :only [read-csv]]
        [clj-time.core :only [plus days]]
        [clj-time.format :only [formatter parse unparse]]))

(def markup
  {:apples 0.4
   :bananas 0.35
   :berries 0.55
   :default 0.5})

(def durations
  {:apples (days 14)
   :bananas (days 5)
   :default (days 7)})

(def categories
  {:fruit [1000 1099]
   :apples [1100 1199]
   :bananas [1200 1299]
   :berries [1300 1399]})

(def poor-suppliers
  #{"32"})

(def date-format (formatter "yyyy/MM/dd"))

(defrecord Product [supplier code desc delivery cost amount])

(def products
  (map #(apply ->Product %)
       (drop 1 (with-open [file (reader "../../produce.csv")]
                 (doall (read-csv file))))))

(defn parse-int [key product]
  (Integer/parseInt (key product)))

(defn between? [n min max]
  (and (> n min) (< n max)))

(defn get-category [product]
  (let [code (parse-int :code product)]
    (some #(when (apply between? (cons code (val %))) (key %)) categories)))

(defn format-price [cents]
  (str "R" (format "%8.2f" (/ cents 100))))

(defmulti price
  (fn [product] (get-category product)))

(defn price-methods [category markup]
  (defmethod price category
    [product] (format-price (* (inc markup) (parse-int :cost product)))))

(doall (map price-methods (keys markup) (vals markup)))

(defmulti sell-by
  (fn [product] (get-category product)))

(defn sell-by-methods [category duration]
  (defmethod sell-by category
    [{delivery :delivery supplier :supplier}]
    (unparse date-format
             (plus (parse date-format delivery)
                   duration
                   (when (poor-suppliers supplier) (days -3))))))

(doall (map sell-by-methods (keys durations) (vals durations)))

(defn description [{desc :desc}] (subs desc 0 31))

(defn tag [product]
  (apply str
          (repeat (parse-int :amount product)
                  (str (price product)
                       (sell-by product)
                       (description product) "\n"))))

(defn -main []
  (spit "pricefile2.txt" (doall (apply str (map tag products)))))
