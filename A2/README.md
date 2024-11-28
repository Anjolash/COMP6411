# Sales Order Application - README

## Overview
The **Sales Order Application** is a Clojure-based system designed to manage and analyze sales data. It processes customer, product, and sales information from text files and provides various features, such as displaying data tables, calculating total sales, and showing total units sold for specific products.

## Features
1. **Display Customers**  
   View a sorted list of all customers with their details.

2. **Display Product Table**  
   View a sorted list of all products with their descriptions and prices.

3. **Display Sales Table**  
   View a sorted list of all sales transactions, including customer and product details.

4. **Total Sales by Customer**  
   Calculate and display the total sales amount for a specific customer by name.

5. **Total Count for Product**  
   Calculate and display the total number of units sold for a specific product by name.

6. **Exit**  
   Exit the application.

---

## Installation and Setup

1. **Prerequisites**
   - Clojure runtime environment installed.
   - Text files: 
     - `cust.txt`: Contains customer data.
     - `prod.txt`: Contains product data.
     - `sales.txt`: Contains sales transaction data.

2. **File Format Requirements**
   - **Customers (`cust.txt`)**  
     Each line should follow the format:  
     `CustomerID|Name|Address|Phone`
   - **Products (`prod.txt`)**  
     Each line should follow the format:  
     `ProductID|Description|Price`
   - **Sales (`sales.txt`)**  
     Each line should follow the format:  
     `SaleID|CustomerID|ProductID|Quantity`

3. **Run the Application**
   - Save the `db` namespace code and the `menu` namespace code in separate files (`db.clj` and `menu.clj` respectively).
   - Ensure the files are located in the same project directory.
   - Use the command:  
     ```bash
     clj -M -m menu
     ```

---

## Usage

### Main Menu Options
1. **Display Customers**  
   Outputs a sorted list of all customers in the format:  
   ```
   CustomerID: [Name, Address, Phone]
   ```

2. **Display Product Table**  
   Outputs a sorted list of all products in the format:  
   ```
   ProductID: [Description, Price]
   ```

3. **Display Sales Table**  
   Outputs a sorted list of all sales transactions in the format:  
   ```
   SaleID: [Customer Name, Product Description, Quantity]
   ```

4. **Total Sales by Customer**  
   - Prompts for a customer name.
   - Outputs the total sales amount for the customer if found; otherwise, displays "Customer not found."

5. **Total Count for Product**  
   - Prompts for a product name.
   - Outputs the total units sold for the product if found; otherwise, displays "Product not found."

6. **Exit**  
   Safely terminates the program.

---

## Examples

### Sample Input Files

#### `cust.txt`
```
1|Alice|123 Elm St|123-456-7890
2|Bob|456 Oak St|987-654-3210
```

#### `prod.txt`
```
1|Laptop|999.99
2|Mouse|19.99
```

#### `sales.txt`
```
1|1|1|2
2|2|2|5
```

### Sample Commands

- Display Customers:  
  ```
  1
  ```
  Output:  
  ```
  1: [Alice, 123 Elm St, 123-456-7890]
  2: [Bob, 456 Oak St, 987-654-3210]
  ```

- Total Sales by Customer:  
  ```
  4
  Enter customer name: Alice
  ```
  Output:  
  ```
  Alice: 1999.98
  ```

---

## Notes
- Ensure the input files have no empty lines.
- The program is case-sensitive for customer and product names.
- Sales calculations rely on consistent formatting of the input files.

---

## License
This project is released under the MIT License. Use and modify it freely for educational or professional purposes.
