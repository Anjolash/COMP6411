import socketserver
import re

class MyTCPHandler(socketserver.BaseRequestHandler):
    """
    The request handler class for our server.

    It is instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.
    """

    database = {}

    @classmethod
    def load_database(cls):
        with open('data.txt', 'r') as file:
                  for line in file:
                      fields = line.strip().split('|')
                      is_valid, msg = cls.validate_record(fields)
                      if is_valid:
                          first_name, age, address, phone_number = fields
                          cls.database[first_name.lower()] = (first_name.lower(), age, address, phone_number)
                      else:
                          print(f"Record skipped: {line.strip()} - {msg}")

    @staticmethod
    def validate_record(fields):
        if len(fields) != 4:
            return False, "missing fields"
            
        first_name, age, address, phone_number = fields
        if first_name in MyTCPHandler.database:
            return False,"Customer already exists"
        
        if not first_name.isalpha():
            return False, "invalid name field"
        if age and(not age.isdigit() or not(1 <= int(age) <= 120)):
            return False, "invalid age field"
        if address and not re.match(r'[\w\s\.\-]*$', address):
            return False, "invald address field"
        if phone_number and not re.match(r'^(\d{3} \d{3}-\d{4}|\d{3}-\d{4})$', phone_number):
            return False, "invalid phone number field"
        return True, ""

    @staticmethod
    def validate_record_customerexists(fields):
        if len(fields) != 4:
            return False, "missing fields"
            
        first_name, age, address, phone_number = fields
        
        if not first_name.isalpha():
            return False, "invalid name field"
        if age and(not age.isdigit() or not(1 <= int(age) <= 120)):
            return False, "invalid age field"
        if address and not re.match(r'[\w\s\.\-]*$', address):
            return False, "invald address field"
        if phone_number and not re.match(r'^(\d{3} \d{3}-\d{4}|\d{3}-\d{4})$', phone_number):
            return False, "invalid phone number field"
        return True, ""

    def handle(self):
        # self.request is the TCP socket connected to the client
        self.data = self.request.recv(1024).strip().decode('utf-8')
        print("Received from {}: {}".format(self.client_address[0], self.data))
        # just send back the same data, but upper-cased

        command, *params = self.data.split('|')
        response = ""

        if command == "FIND":
            response = self.get_record(params[0])
        elif command == "ADD":
            response = self.add_record(params)
        elif command == "DELETE":
            response = self.delete_record(params[0])
        elif command == "UPDATE AGE":
            response = self.update_age(*params)
        elif command == "UPDATE ADDRESS":
            response = self.update_address(*params)
        elif command == "UPDATE PHONE":
            response = self.update_phone(*params)
        elif command == "PRINT":
            response = self.print_report()
        
            
        self.request.sendall(response.encode('utf-8'))

    def get_record(self, name):
        first_name = name.lower()
        if first_name in self.database:
            return '|'.join(self.database[first_name])
        else:
            return 'Record not found'
        
    def add_record(self, fields):
        first_name = fields[0].lower()
        if first_name in self.database:
            return "Customer already exists"

        is_valid, msg = self.validate_record(fields)
        if is_valid:    
            self.database[first_name.lower()] = (first_name.lower(), fields[1], fields[2], fields[3])
            return 'Customer added successfully'
        else:
            return f"Invalid data: {msg}"

    def delete_record(self, name):
        first_name = name.lower()
        if first_name in self.database:
            del self.database[first_name]
            return 'Customer deleted successfully'
        else:
            return 'Customer does not exist'

    def update_age(self,name, newage):
        first_name = name.lower()
        if first_name in self.database:
            fields = list(self.database[first_name])
            field_idx = ["first_name", "age", "address", "phone_number"].index("age")
            fields[field_idx] = newage
            is_valid, msg = self.validate_record_customerexists(fields)
            if is_valid:
                self.database[first_name] = tuple(fields)
                return "Customer age updated successfully"
            else:
                return f"Invalid data: {msg}"
        return "Customer not found"
    
    def update_address(self,name, newaddress):
        first_name = name.lower()
        if first_name in self.database:
            fields = list(self.database[first_name])
            field_idx = ["first_name", "age", "address", "phone_number"].index("address")
            fields[field_idx] = newaddress
            is_valid, msg = self.validate_record_customerexists(fields)
            if is_valid:
                self.database[first_name] = tuple(fields)
                return "Customer address updated successfully"
            else:
                return f"Invalid data: {msg}"
        return "Customer not found"

    def update_phone(self,name, newphone):
        first_name = name.lower()
        if first_name in self.database:
            fields = list(self.database[first_name])
            field_idx = ["first_name", "age", "address", "phone_number"].index("phone_number")
            fields[field_idx] = newphone
            is_valid, msg = self.validate_record_customerexists(fields)
            if is_valid:
                self.database[first_name] = tuple(fields)
                return "Customer phone number updated successfully"
            else:
                return f"Invalid data: {msg}"
        return "Customer not found"

    def print_report(self):
        report = "Database contents: \n"
        for key in self.database:
            report += '|'.join(self.database[key]) + '\n'
        return report.strip()

                                             

if __name__ == "__main__":
    HOST, PORT = "localhost", 9999

    MyTCPHandler.load_database()

    # Create the server, binding to localhost on port 9999
    with socketserver.TCPServer((HOST, PORT), MyTCPHandler) as server:
        # Activate the server; this will keep running until you
        # interrupt the program with Ctrl-C
        print(f"Server started at {HOST}:{PORT}")
        server.serve_forever()
