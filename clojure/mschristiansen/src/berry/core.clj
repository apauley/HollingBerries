(ns berry.core
  (:use [clojure.data.csv :only [read-csv]]
        [clj-time.core :only [plus days]]
        [clj-time.format :only [formatter parse unparse]]))

(defn lookup [code attr]
  (attr (condp < code
          1300 {:markup 1.55 :lasts (days 7)}  ; Berries
          1200 {:markup 1.35 :lasts (days 5)}  ; Bananas
          1100 {:markup 1.40 :lasts (days 14)} ; Apples
          {:markup 1.50 :lasts (days 7)})))    ; Default

(def trouble #{"32" "101"})             ; Susan, Togetherness
(def premium #{"219" "204"})            ; Promise, Karel

(defn price [cost code supplier]
  (let [price (* cost (lookup code :markup))]
    (cond (neg? (- price 2)) 0.0
          (trouble supplier) (- price 2)
          (premium supplier) (Math/ceil (+ price (* cost 0.10)))
          :else price)))

(defn sell-by [delivery code supplier]
  (let [fmt (formatter "yyyy/MM/dd")
        date (plus (parse fmt delivery) (lookup code :lasts))]
    (unparse fmt (if (trouble supplier) (plus date (days -3)) date))))

(defn create-tags [s [supplier code desc delivery cost amount]]
  (let [p (price (* (Integer. cost) 0.01) (Integer. code) supplier)
        d (sell-by delivery (Integer. code) supplier)]
    (apply str s (repeat (Integer. amount)
                         (format "R%8.2f%s%.31s\n" p d desc)))))

(defn -main []
  (with-open [file (clojure.java.io/reader "../../produce.csv")]
    (spit "pricefile.txt" (reduce create-tags "" (rest (read-csv file))))))
