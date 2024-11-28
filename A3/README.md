# **Game Module Readme**

This project is an Erlang-based multi-process game simulation where players engage in a Rock-Paper-Scissors game. Players lose tokens upon losing a game, and the system determines a winner once only one player remains with tokens.

---

## **Modules Overview**

### **`game` Module**
The `game` module is responsible for orchestrating the game lifecycle. It initializes the game, handles message passing between players, determines game outcomes, and manages the game flow.

#### **Exported Functions**
1. **`start/1`**
   - **Purpose**: Initializes the game with player information.
   - **Parameters**: 
     - `PlayerFile`: Path to a file containing player names and their token counts in the format `{Name, Tokens}`.
   - **Example**:
     ```erlang
     game:start("players.txt").
     ```

2. **`messagePass/5`**
   - **Purpose**: Handles communication between players during a game session.
   - **Parameters**: 
     - `From`: Master process.
     - `Sender`: Player initiating the message.
     - `Receiver`: Opponent(s).
     - `GameId`: Unique identifier for the game.
     - `Time`: Timestamp for the message.

3. **`send_message/4`**
   - **Purpose**: Facilitates game moves and handles game completion logic.

4. **`time_stamp/4`**
   - **Purpose**: Records timestamps for each interaction.

5. **`determine_winner/3`**
   - **Purpose**: Determines the winner based on the moves made by players.

---

### **`player` Module**
The `player` module manages the player processes, including generating opponents, managing tokens, and checking for game completion.

#### **Exported Functions**
1. **`game_loop/4`**
   - **Purpose**: Core loop that processes player interactions and handles game state updates.

2. **`generate_opponents/5`**
   - **Purpose**: Selects valid opponents for a player and spawns a game process.

3. **`post_feedback/5`**
   - **Purpose**: Processes game results and checks game completion conditions.

---

## **Key Features**

### **Game Mechanics**
- Players engage in Rock-Paper-Scissors.
- The winner is determined by standard rules (e.g., rock beats scissors).
- If a player loses, they lose one token.

### **Game Termination**
- The game ends when only one player has tokens remaining.
- Final results (win counts and remaining tokens) are printed.

### **Token Management**
- Players with zero tokens cannot participate further.
- Tokens are deducted from losing players.

---

## **File Format**

The input file should list players and their initial token counts in the following format:
```erlang
{player1, 5}.
{player2, 3}.
{player3, 2}.
```

---

## **Execution**

To run the game:
1. Prepare the input file (e.g., `players.txt`).
2. Start the game using:
   ```bash
   erl -noshell -run game start players.txt -s init stop
   ```

---

## **Example Output**

```plaintext
 *** Game Started ***
player1: Tokens: 5
player2: Tokens: 3
player3: Tokens: 2
+ [1] New game for player1 -> player2
$ Game result (ID: 1): player1 made move rock, player2 made move paper. Winner: player2
+ [2] New game for player1 -> player3
$ Game result (ID: 2): player1 made move scissors, player3 made move rock. Winner: player3
...
Game over. We have a winner!
Total games: 15
Winner: player3
```

---

## **Customizations**
- **Modify Game Rules**: Update the `determine_winner/3` function for different rules.
- **Adjust Timeouts**: Edit the `after` clauses in `send_message/4` and `post_feedback/5` to change timeout durations.

---

## **Limitations**
- Tokens must be integers.
- Players with zero tokens are skipped but not explicitly removed from the player list.

---

This README serves as a guide to understanding and using the game modules effectively. Happy coding!
