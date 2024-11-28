(ns db
  (:require [clojure.string :as str]))

(defn load-file [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (map #(str/split % #"\|") (remove empty? (line-seq rdr))))))

(def customers (load-file "cust.txt"))
(def products (load-file "prod.txt"))
(def sales (load-file "sales.txt"))

(defn get-customers []
  (into {} (map (fn [[id name address phone]] [(Integer. id) [name address phone]]) customers)))

(defn get-products []
  (into {} (map (fn [[id desc cost]] [(Integer. id) [desc (Double. cost)]]) products)))

(defn get-sales []
  (map (fn [[id cust prod count]] [(Integer. id) (Integer. cust) (Integer. prod) (Integer. count)]) sales))

(defn total-sales-by-customer []
  (let [sales (get-sales)
        customer-sales (group-by second sales)]
    (into {} (map (fn [[cust-id sales]]
                    [cust-id (reduce + (map (fn [[_ _ prod-id count]]
                                              (* count (second (get (get-products) prod-id))))
                                            sales))])
                  customer-sales))))

(defn get-customer-id-by-name [name]
  (let [customers (get-customers)]
    (some (fn [[id [cust-name _ _]]]
            (when (= cust-name name) id))
          customers)))

(defn get-customer-sales [cust-id]
  (let [sales (get-sales)
        customer-sales (filter #(= (second %) cust-id) sales)
        total-sales (reduce + (map (fn [[_ _ prod-id count]]
                                     (* count (second (get (get-products) prod-id))))
                                   customer-sales))]
    {:customer-id cust-id
     :sales customer-sales
     :total-sales total-sales}))

(defn get-customer-details [cust-id]
  (get (get-customers) cust-id))

(defn get-product-id-by-name [name]
  (let [products (get-products)]
    (some (fn [[id [prod-name _]]]
            (when (= prod-name name) id))
          products)))

(defn total-units-sold-by-product [prod-id]
  (reduce + (map #(nth % 3) (filter #(= (nth % 2) prod-id) (get-sales)))))
