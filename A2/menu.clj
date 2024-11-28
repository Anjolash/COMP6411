(ns menu
  (:require [db :as db]
            [clojure.string :as str]))

(defn display-customers []
  (println (str/join "\n" 
                     (map (fn [[id info]] (str id ":" info)) 
                          (sort-by key (db/get-customers))))))

(defn display-product-table []
  (println (str/join "\n" 
                     (map (fn [[id info]] (str id ":" info)) 
                          (sort-by key (db/get-products))))))

(defn display-sales-table []
  (let [customers (db/get-customers)
        products (db/get-products)
        sales (sort-by first (db/get-sales))]
    (->> sales
         (map (fn [[id cust-id prod-id count]]
                (let [customer-name (first (customers cust-id))
                      product-desc (first (products prod-id))]
                  (str id ": [" customer-name " " product-desc " " count "]"))))
         (clojure.string/join "\n")
         println)))


(defn display-total-sales-by-customer []
  (println (str/join "\n" 
                     (map (fn [[cust-id total-sales]] 
                            (str "Customer ID: " cust-id " Total Sales: " total-sales)) 
                          (db/total-sales-by-customer)))))

(defn display-sales-for-customer [customer-name]
  (let [cust-id (db/get-customer-id-by-name customer-name)
        customer-details (db/get-customer-details cust-id)
        customer-sales (db/get-customer-sales cust-id)]
    (if cust-id
      (println (str (first customer-details)
                    ": " (:total-sales customer-sales)))
      (println "Customer not found."))))

(defn display-units-sold-for-product [product-name]
  (let [prod-id (db/get-product-id-by-name product-name)
        total-units-sold (db/total-units-sold-by-product prod-id)]
    (if prod-id
      (println (str product-name ": " total-units-sold))
      (println "Product not found."))))

(defn pause []
  (println "Press Enter to continue...")
  (flush)
  (read-line))

(defn -main []
  (loop []
    (print "\u001b[2J")
    (println "Sales Order Application")
    (println "1. Display Customers")
    (println "2. Display Product Table")
    (println "3. Display Sales Table")
    (println "4. Total Sales by Customer")
    (println "5. Total Count fot Product")
    (println "6. Exit")
    (print "Enter your choice: ")
    (flush)
    (let [choice (read-line)]
      (case choice
        "1" (do (display-customers) (pause))
        "2" (do (display-product-table) (pause))
        "3" (do (display-sales-table) (pause))    
        "4" (do (print "Enter customer name: ")
                (flush)
                (let [customer-name (read-line)]
                  (display-sales-for-customer customer-name)) (pause))
        "5" (do (print "Enter product name: ")
                (flush)
                (let [product-name (read-line)]
                  (display-units-sold-for-product product-name)) (pause))
        "6" (do (println "Good Bye.") (System/exit 0))
        (do (println "Invalid choice")(pause)))
        (recur))))

(-main)
