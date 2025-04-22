# Example Game Client
# Written by Cole Glotfelty <2025-04-14>

# A barebones game client explaining what needs to be implemented and what's
# available for use when creating halligame games. This also includes some
# typical patterns

from halligame.utils.gameClientTemplate import ClientSuper
from halligame.utils.gameState import GameState
from halligame.utils.screen import Screen
from typing import Any


class Client(ClientSuper):
    def __init__(self, comms: callable) -> None:
        """
        Constructor for the Client (you should initalize stuff here)

        One thing to note: `comms` needs to be assigned like so:

            self.__comms = comms

        We also have assorted utilities in halligame.utils. Here we're using
        the GameState class which also must be used for storing the state of
        your game. (We do this for serialization and server communication)
        """
        self.__comms = comms
        self.__playerID = None
        self.__ships = [["  " for y in range(9)] for x in range(9)]
        self.__guesses = [["  " for y in range(9)] for x in range(9)]

        # Screen Printing
        self.__screen = Screen(self.userInput, self.mouseInput)
        self.__view = "select"  # could be 'select' 'hits' or 'ships'
        self.__top = 5
        self.__left = 10

    def updateState(self, state: bytes) -> None:
        """
        Callback function triggered by `broadcastState`. This is where you
        should modify the TUI screen for the user. The state should then be
        copied to the local state from the message sent by the server.
        """
        self.__state.deserialize(state)
        pass

    def gotServerMessage(self, msg) -> None:
        """
        Callback function for when a message is received from the server. One
        could deconstruct the msg like below and then discriminate based on
        the status or message text.
        """
        (status, message) = msg
        if status == "error":
            print(message)
            self.__comms.shutdown()

    # FIX: screen prints when room full just the error
    def joinConfirmed(self, msg) -> None:
        """
        Callback function for when the player joins the server. This message
        can be set in GameServer::addClient().
        """
        self.__playerID = msg
        self.__drawScreen()

    def userInput(self, input):
        if input == "q":
            self.__screen.shutdown()
            self.__comms.shutdown()

    def mouseInput(self, row, col, region, mouseEventType):
        pass

    def __drawScreen(self):
        self.__drawGrid()
        self.__screen.write(self.__top, 100, "YOUR TURN")
        self.__drawButton(40, 100, "HITS | SHIPS")
        self.__screen.refresh()

    def __drawGrid(self, scale: int = 2):
        # 19, 37
        for row in range((18 * scale) + 1):
            for col in range((36 * scale) + 1):
                if row % (2 * scale) == 0:
                    self.__screen.write(self.__top + row, self.__left + col, "-")
                else:
                    if col % (4 * scale) == 0:
                        self.__screen.write(self.__top + row, self.__left + col, "|")

    def __drawButton(self, row: int, col: int, toPrint: str):
        for i in range(len(toPrint)):
            self.__screen.write(row - 1, col + i, "-")
            self.__screen.write(row + 1, col + i, "-")

        # Print Corners
        self.__screen.write(row + 1, col + len(toPrint), "-+")
        self.__screen.write(row - 1, col + len(toPrint), "-+")
        self.__screen.write(row + 1, col - 2, "+-")
        self.__screen.write(row - 1, col - 2, "+-")

        # Print edges
        self.__screen.write(row, col - 2, "|")
        self.__screen.write(row, col + len(toPrint) + 1, "|")

        self.__screen.write(row, col, toPrint)
