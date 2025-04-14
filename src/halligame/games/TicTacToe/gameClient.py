# gameClient.py

# Tic-Tac-Toe Game Module for Halligame Testing
# Written on 2025-03-22 by Cole Glotfelty
# Last edited by: Cole Glotfelty 2025-03-29

# Potential Framework structure for python
# each game has a Game class and a Player class
# The game class implements the rules and game specific data
# The Player class stores player specific data 
# (potentially part of the Game constructor)

from halligame.utils.screen import Screen
from halligame.utils.gameState import GameState
import threading

import time

class Client():
    # comms is the function to call when you want to send a message to the server
    def __init__(self, comms):
        """
        Memeber Variables:
            screen
            stateLock
            comms
            state
            playerID
            myTurn
            done
        """
        self.__screen = Screen(self.userInput, width=120, height=30)
        self.__stateLock = threading.Lock()
        self.__comms = comms
        self.__state: GameState = GameState()
        self.__playerID = None
        self.__myTurn = True

    def play(self):
        pass

    def updateState(self, state):
        """
        Takes in a message that contains the new state (this message is sent 
        from the server side of the game class) and updates the internal 
        state, potentially updating/refreshing the display
        """
        with self.__stateLock:
            self.__state.deserialize(state)

            for row in self.__state.getValue("board"):
                self.__screen.print("-------------")
                self.__screen.print("|   |   |   |")
                self.__screen.print(f"| {row[0]} | {row[1]} | {row[2]} |")
                self.__screen.print("|   |   |   |")
            self.__screen.print("-------------\n")

            self.__myTurn = (self.__state.getValue("currentPlayer") == self.__playerID)


            if self.__state.getValue("gameOver") != "":
                self.__screen.print(self.__state.getValue("gameOver"))
                self.__screen.print("Type 'q' to quit")
            elif (self.__myTurn):
                self.__screen.print("Select a square [1..9]: ")
        
            self.__screen.refresh()

    def gotMessage(self, msg):
        self.__screen.print("That Square is Occupied!")

    def confirmedJoin(self, Msg):
        (playerID, state) = Msg
        self.__playerID = playerID
        self.__screen.write(10, 10, "hello!")
        self.updateState(state)
    
    def otherMessage(self, Msg):
        raise ValueError("Client Received Unknown Message" + str(Msg))

    def userInput(self, input):
        """
        called when the screen receives user input (a char). For now, I'm just 
        forwarding input to the server side for the server side to handle 
        (but you can obviously do more things like have client side checking 
        to see if it's a number 1-9 before sending to server as an event)
        Input comes from the curses module, which defines some constants for
        recognizing things like down arrows, etc.
        see https://docs.python.org/3/library/curses.html and search for 
        "curses.KEY_"...
        """
        with self.__stateLock:
            self.__screen.print(input)
            self.__screen.refresh()

            if (input == "q"):
                self.__comms.shutdown()
                self.__screen.shutdown()
            elif self.__myTurn:
                playerInput = -1
                while not (1 <= playerInput and playerInput <= 9):
                    try:
                        playerInput = int(input)
                        self.__comms.sendMessage((self.__playerID, playerInput - 1))
                    except: # 
                        print("Please enter a number between 1 and 9!")
