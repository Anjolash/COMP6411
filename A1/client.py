import socket
import sys
import os

HOST, PORT = "localhost", 9999


def send_request(request):
    # Create a socket (SOCK_STREAM means a TCP socket)
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        # Connect to server and send data
        sock.connect((HOST, PORT))
        sock.sendall(request.encode('utf-8'))
        response = sock.recv(1024).decode('utf-8')
        return response
        
def clear_screen():
    os.system('clear')

def find_customer():
    name = input("Enter the first name of the customer: ")
    request = f"FIND|{name}"
    response = send_request(request)
    print(f"Server response: {response}")

def add_customer():
    first_name = input("Enter the first name of the customer: ")
    age = input("Enter the customer's age: ")
    address = input("Enter the customer's address: ")
    phone_number = input("Enter the customer's phone number: ")
    request = f"ADD|{first_name}|{age}|{address}|{phone_number}"
    response = send_request(request)
    print(f"Server response: {(response)}")

def delete_customer():
    name = input("Enter the first name of the customer: ")
    request = f"DELETE|{name}"
    response = send_request(request)
    print(f"Server response: {response}")

def update_age():
    name = input("Enter the first name of the customer: ")
    age = input("Enter the new customer's age: ")
    request = f"UPDATE AGE|{name}|{age}"
    response = send_request(request)
    print(f"Server response: {response}")

def update_address():
    name = input("Enter the first name of the customer: ")
    address = input("Enter the customer's address: ")
    request = f"UPDATE ADDRESS|{name}|{address}"
    response = send_request(request)
    print(f"Server response: {response}")

def update_phone():
    name = input("Enter the first name of the customer: ")
    phone = input("Enter the phone number of the customer: ")
    request = f"UPDATE PHONE|{name}|{phone}"
    response = send_request(request)
    print(f"Server response: {response}")

def print_report():
    request = f"PRINT"
    response = send_request(request)
    print(f"Server response: {response}")

def exit_kini():
    print("Goodbye")
    sys.exit()

def main():
    while True:
        clear_screen()
        print("\nCustomer Management Menu")
        print("1. Find customer")
        print("2. Add customer")
        print("3. Delete customer")
        print("4. Update customer age")
        print("5. Update customer address")
        print("6. Update customer phone")
        print("7. Print report")
        print("8. Exit")
        choice = input("Select: ")

        if choice == "1":
            find_customer()
        elif choice == "2":
            add_customer()
        elif choice == "3":
            delete_customer()
        elif choice == "4":
            update_age()
        elif choice == "5":
            update_address()
        elif choice == "6":
            update_phone()
        elif choice == "7":
            print_report()
        elif choice == "8":
            exit_kini()
        else:
            print("Invalid choice. please try again.")

        input("Press any key to continue...  ")


    
if __name__ == "__main__":
    main()
