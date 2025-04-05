#!/usr/bin/env python

# Tic-Tac-Toe Game Module for Halligame Testing
# Written on 2025-03-22 by Cole Glotfelty
# Last edited by: Cole Glotfelty 2025-03-29

# Potential Framework structure for python
# each game has a Game class and a Player class
# The game class implements the rules and game specific data
# The Player class stores player specific data (potentially part of the Game constructor)

from halligame.utils.screen import Screen
from halligame.utils.gameState import GameState
import threading

import time

class Client():
    # comms is the function to call when you want to send a message to the server
    def __init__(self, comms: callable, playerID):
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
        self.__screen = Screen(self.userInput) # create new instance of the ncurses module
        self.__stateLock = threading.Lock()
        self.__comms = comms
        self.__state: GameState = GameState()
        self.__playerID = playerID
        self.__myTurn = True

    # Takes in a message that contains the new state (this message is sent from 
    # from the server side of the game class) and updates the internal 
    # state, potentially updating/refreshing the display
    def updateState(self, msg : list[tuple]):
        with self.__stateLock:
            self.__state.deserialize(msg)
            # print(f"Client has state {self.__state.objects}")

            # print('DEBUG: Printing board:')
            # print('{self.__state.objects["board"]}')
            # print('DEBUG: End board')

            self.__screen.refresh()
            for row in self.__state.objects["board"]:
                self.__screen.print("-------------")
                self.__screen.print("|   |   |   |")
                self.__screen.print(f"| {row[0]} | {row[1]} | {row[2]} |")
                self.__screen.print("|   |   |   |")
                self.__screen.print("-------------\n")
            self.__screen.refresh()
            self.__myTurn = (self.__state.objects["currentPlayer"] == self.__playerID)


            if self.__state.objects["gameOver"] != "":
                self.__screen.print(self.__state.objects["gameOver"])
                self.__screen.print("Type 'q' to quit")
                self.__screen.refresh()
            elif (self.__myTurn):
                self.__screen.print("Select a square [1..9]: ")
                self.__screen.refresh()

    def gotReply(self, msg):
        self.__screen.print("That Square is Occupied!")
        self.__screen.refresh()

    # called when the screen receives user input (a char). For now, I'm just 
    # forwarding input to the server side for the server side to handle 
    # (but you can obviously do more things like have client side checking 
    # to see if it's a number 1-9 before sending to server as an event)
    # Input comes from the curses module, which defines some constants for
    # recognizing things like down arrows, etc.
    # see https://docs.python.org/3/library/curses.html and search for 
    # "curses.KEY_"...
    def userInput(self, input):
        with self.__stateLock:
            if (input == "q"):
                self.shutdown()
            elif self.__myTurn:
                playerInput = int(input)
                if 1 <= playerInput and playerInput <= 9:
                    self.__comms((self.__playerID, playerInput - 1))
                else:
                    self.__screen.print("Please enter a number between 1 and 9!")
                    self.__screen.refresh()
    
    def shutdown(self):
        self.__screen.shutdown()
        self.__comms("close")
