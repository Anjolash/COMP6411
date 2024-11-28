# Customer Management System

This repository implements a customer management system using a TCP server-client architecture. The server manages customer data, including operations like adding, finding, updating, and deleting customer records. The client provides an interactive menu for users to interact with the server.

---

## Features

### Server
- **Find Customer**: Retrieve details of a customer by their first name.
- **Add Customer**: Add a new customer record to the database.
- **Delete Customer**: Remove a customer record from the database.
- **Update Customer Information**:
  - Update age
  - Update address
  - Update phone number
- **Print Report**: Print all records in the database.
- **Data Validation**: Ensures data integrity with validation checks for name, age, address, and phone number.
- **Persistent Database Loading**: Reads customer records from a `data.txt` file at startup.

### Client
- Interactive menu to perform CRUD operations on customer records.
- Sends requests to the server and displays server responses.
- Includes input validation and user prompts for ease of use.

---

## Installation and Setup

### Prerequisites
- Python 3.8 or higher
- `data.txt` file containing initial database records in the format:  
  ```
  first_name|age|address|phone_number
  ```

### Server Setup
1. Clone this repository.
2. Ensure a valid `data.txt` file exists in the same directory as the server script.
3. Start the server by running:
   ```bash
   python server.py
   ```
   The server will start at `localhost:9999`.

### Client Setup
1. Start the client by running:
   ```bash
   python client.py
   ```
2. Use the interactive menu to perform operations.

---

## Usage

### Client Menu Options
1. **Find Customer**: Search for a customer by their first name.
2. **Add Customer**: Add a new record by providing:
   - First name
   - Age
   - Address
   - Phone number
3. **Delete Customer**: Remove a customer by their first name.
4. **Update Customer Age**: Update the age of an existing customer.
5. **Update Customer Address**: Update the address of an existing customer.
6. **Update Customer Phone**: Update the phone number of an existing customer.
7. **Print Report**: Display all customer records in the database.
8. **Exit**: Quit the client application.

### Data Validation Rules
- **Name**: Alphabetic characters only.
- **Age**: Numeric, between 1 and 120.
- **Address**: Alphanumeric characters, spaces, periods, and hyphens.
- **Phone Number**: Must follow formats `XXX XXX-XXXX` or `XXX-XXXX`.

---

## File Structure
```
.
├── server.py          # TCP server script
├── client.py          # TCP client script with interactive menu
├── data.txt           # Database file containing initial records
├── README.md          # Documentation
```

---

## Sample `data.txt`
```plaintext
John|25|123 Elm Street|123 456-7890
Alice|30|456 Oak Avenue|987 654-3210
Bob|40|789 Pine Road|555-1234
```

---

## Error Handling
- **Server-Side**:
  - Validates input fields before processing requests.
  - Skips invalid records during database loading and logs errors.
- **Client-Side**:
  - Displays meaningful error messages for invalid operations.
  - Ensures smooth interaction via input prompts.

---

## Future Improvements
- Add authentication for client requests.
- Enhance database persistence with a robust database system (e.g., SQLite).
- Implement logging for server and client interactions.

---

## License
This project is licensed under the MIT License. See `LICENSE` for details.

---

Happy coding! 😊
