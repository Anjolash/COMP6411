# COMP 6411: Comparative Study of Programming Languages  

## Overview  
This repository contains projects and assignments completed as part of the **COMP 6411: Comparative Study of Programming Languages** course. The course focuses on exploring different programming paradigms, analyzing their strengths and weaknesses, and applying them to solve real-world problems. The projects provide practical experience in implementing concepts such as concurrency, message-passing, and functional programming, with a particular focus on Erlang.  

## Contents  

### 1. **Game Simulation with Erlang**  
A multi-process simulation of a Rock-Paper-Scissors tournament where players compete until one remains with tokens. Key features include:  
- Concurrency via lightweight processes.  
- Communication using message-passing.  
- Dynamic updates to player statistics (tokens and wins).  
- Game termination when one player remains.  

### 2. **Random Message Passing in Erlang**  
A simulation of a message-passing system using Erlang. Players exchange random messages in a distributed environment. Highlights:  
- Demonstrates the actor model of concurrency.  
- Implements random selection and interaction logic.  
- Highlights fault-tolerance with timeouts and termination.

### 3. **Erlang Concurrency Examples**  
Assignments exploring various concurrency patterns in Erlang, focusing on:  
- Process spawning and lifecycle management.  
- Communication and synchronization using message-passing.  
- Handling edge cases and ensuring graceful process termination.  

## How to Run  
1. **Setup Erlang Environment**  
   - Install Erlang/OTP (compatible version recommended for the project).  

2. **Compile Modules**  
   - Navigate to the project directory and compile the `.erl` files:  
     ```shell
     erl -make
     ```

3. **Run the Applications**  
   - Start the Erlang shell and execute the desired module:  
     ```shell
     erl -noshell -run game start players.txt -s init stop
     ```

4. **Player Input File**  
   - Ensure the `players.txt` file exists in the working directory, listing player names and tokens in the format:  
     ```erlang
     {player_name, token_count}.
     ```

## Key Learning Outcomes  
- **Understanding of Erlang**: Gained hands-on experience in Erlang programming with practical applications of its functional and concurrent features.  
- **Comparative Analysis**: Explored how Erlang's design contrasts with other programming languages, especially in concurrent and distributed computing.  
- **Problem Solving**: Implemented real-world systems emphasizing fault-tolerance, scalability, and maintainability.  

## Acknowledgments  
Special thanks to the COMP 6411 instructors for their guidance and insights, and to fellow students for their collaboration and support during the course.
