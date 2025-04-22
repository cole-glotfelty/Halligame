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
        self.__ships = [["" for y in range(9)] for x in range(9)]
        self.__guesses = [["" for y in range(9)] for x in range(9)]

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
        if mouseEventType == "left_click":
            match region:
                case ('button', "HITS"):
                    self.__view = "hits"
                case ('button', "CONTINUE"):
                    self.__view = "hits"
                case ('button', "SHIPS"):
                    self.__view = "ships"
                # FIX: add something to indicate hit
                case ('grid', index):
                    if self.__view == "hits":
                        self.__guesses[index // 9][index % 9] = "X"
                        self.__comms.sendMessage(('gridMove', self.__playerID, index))
                    else:
                        return
                case _:
                    return

        # screen drawing after mouse clicks
        self.__drawScreen()

    def gotServerMessage(self, msg):
        self.__screen.write(10, 200, "test")
        match msg:
            case ('gridMove', playerID, move):
                if playerID != self.__playerID:
                    self.__ships[move // 9][move % 9] = "X"
                    self.__drawScreen()


        
    ## Screen Drawing ##
    def __drawScreen(self):
        self.__screen.clearClickableRegions()
        self.__screen.clearScreen()

        if self.__view == "select":
            self.__drawGrid()
            self.__screen.write(self.__top + 2, 100, "PLACE YOUR SHIPS")
            self.__drawButton(40, 100, "CONTINUE")
        elif self.__view == "hits":
            self.__drawGrid()
            self.__screen.write(self.__top + 2, 100, "YOUR TURN")
            self.__drawButton(40, 100, "HITS")
            self.__drawButton(40, 110, "ships")
        elif self.__view == "ships":
            self.__drawGrid()
            self.__screen.write(self.__top + 2, 100, "YOUR TURN")
            self.__drawButton(40, 100, "hits")
            self.__drawButton(40, 110, "SHIPS")

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

        for row in range(9):
            for col in range(9):
                center_row = self.__top + (row * 2 * scale) + scale
                center_col = self.__left + (col * 4 * scale) + (2 * scale)
                cell_value = ""
                
                if self.__view == "hits":
                    cell_value = self.__guesses[row][col]
                elif self.__view == "ships":
                    cell_value = self.__ships[row][col]
                
                self.__screen.write(center_row, center_col, cell_value)

        self.__defineGridClickableRegions()

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

        self.__screen.addClickableRegion(
            row - 1, col - 2, 3, len(toPrint) + 4, ("button", toPrint.upper())
        )

    ## Mouse Regions ##
    def __defineGridClickableRegions(self, scale: int = 2):
        for row in range(9):
            for col in range(9):
                # Calculate starting positions based on scale
                start_row = self.__top + (row * 2 * scale) + 1
                start_col = self.__left + (col * 4 * scale) + 1

                # Define the region with appropriate size
                self.__screen.addClickableRegion(
                    start_row,
                    start_col,
                    (2 * scale) - 1,
                    (4 * scale) - 1,
                    (
                        "grid",
                        (row * 9) + col,
                    ),
                )
