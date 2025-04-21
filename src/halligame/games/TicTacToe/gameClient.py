# gameClient.py

# Tic-Tac-Toe Game Module for Halligame Testing
# Written on 2025-03-22 by Cole Glotfelty
# Last edited by: Cole Glotfelty 2025-03-29

# Potential Framework structure for python
# each game has a Game class and a Player class
# The game class implements the rules and game specific data
# The Player class stores player specific data
# (potentially part of the Game constructor)

import threading
import time
from typing import Any

import pyfiglet

from halligame.utils.gameClientTemplate import ClientSuper
from halligame.utils.gameState import GameState
from halligame.utils.screen import Screen


class Client(ClientSuper):
    # comms is an instance of halligame.utils.ClientCommunicate
    def __init__(self, comms) -> None:  # noqa: ANN001
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
        self.__screen = Screen(self.userInput, self.mouseInput)
        self.__stateLock = threading.Lock()
        self.__comms = comms
        self.__state: GameState = GameState()
        self.__playerID = None
        self.__myTurn = True

        self.__formatter = pyfiglet.Figlet(font="georgia11")
        self.__topRow = 5

        self.initializeScreenColors()

    def gotServerMessage(self, msg: tuple[str, Any]) -> None:
        with self.__stateLock:
            toPrint = self.__formatter.renderText(msg[0])
            self.__screen.clearScreen()
            self.__screen.write(0, 15, toPrint)
            self.__screen.refresh()

            time.sleep(1.5)

            self.__updateState(msg[1])

    def joinConfirmed(self, msg: Any) -> None:
        (playerID, state) = msg
        self.__playerID = playerID
        self.updateState(state)
        self.defineClickableRegions()

    def initializeScreenColors(self) -> None:
        self.__screen.addColor(44, 29, 219, "O")
        self.__screen.addColor(219, 33, 61, "X")
        self.__screen.addColor(209, 107, 177, "background")

        # define the color palette for printing X and O
        self.__screen.addColorPair("O", "background", "O")
        self.__screen.addColorPair("X", "background", "X")
        self.__screen.addColorPair("white", "background", " ")

        self.__screen.addColorPair("black", "background", "terminal")
        self.__screen.setStyle("terminal")
        # self.__screen.setStyle("white_random")

    def defineClickableRegions(self) -> None:
        # Getting the dimensions of the rendered 'X'
        letter = self.__formatter.renderText("X")
        letterHeight = len(letter.split("\n"))
        letterWidth = len(letter.split("\n")[0])

        for i in range(3):
            for j in range(3):
                verticalOffset = (letterHeight + 1) * i
                horizontalOffset = (letterWidth + 2) * j
                self.__screen.addClickableRegion(
                    self.__topRow + verticalOffset,
                    horizontalOffset,
                    letterHeight,
                    letterWidth,
                    (3 * i) + j,
                )

    def userInput(self, input: int | str) -> None:
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
            if input == "q":
                self.__screen.shutdown()
                self.__comms.shutdown()

            elif self.__myTurn:
                try:
                    playerInput = int(input)
                    if playerInput >= 1 and playerInput <= 9:
                        self.__comms.sendMessage(
                            (self.__playerID, playerInput - 1)
                        )
                except Exception:  # didn't ent
                    pass

    def mouseInput(
        self, row: int, col: int, region: int | None, mouseEventType: str
    ) -> None:
        with self.__stateLock:  # draw it so it appears instantaneously
            if (
                region is not None
                and self.__myTurn
                and self.__state.getValue("gameOver") == ""
                and mouseEventType == "left_click"
            ):
                newBoard = self.__state.getValue("board")
                playerSymbol = "X" if self.__playerID == 0 else "O"
                newBoard[region // 3][region % 3] = playerSymbol
                self.__state.setValue("board", newBoard)
                self.__drawGame()

                self.__comms.sendMessage((self.__playerID, region))

    def updateState(self, newState: bytes) -> None:
        """
        Takes in a message that contains the new state (this message is sent
        from the server side of the game class) and updates the internal
        state, potentially updating/refreshing the display
        """
        with self.__stateLock:
            self.__updateState(newState)

    def __updateState(self, newState: bytes) -> None:
        """
        Backend for update state that does not use the statelock. Meant to be
        called by other functions that have already acquired the statelock
        """
        self.__state.deserialize(newState)

        if self.__state.getValue("gameOver") != "":
            self.__drawGameOver()
        else:
            self.__drawGame()

        self.__myTurn = (
            self.__state.getValue("currentPlayer") == self.__playerID
        )

    def __drawGameOver(self) -> None:
        Message = self.__formatter.renderText(self.__state.getValue("gameOver"))
        self.__screen.clearScreen()
        self.__screen.write(15, 15, Message)
        self.__screen.refresh()

        time.sleep(3)
        self.__screen.clearScreen()
        self.__drawBoard()
        self.__screen.refresh()

    def __drawGame(self) -> None:
        self.__screen.clearScreen()
        self.__drawBoard()
        self.__drawGameInfo()
        self.__screen.refresh()

    def __drawBoard(self) -> None:
        letter = self.__formatter.renderText("X")
        letterHeight = len(letter.split("\n"))
        letterWidth = len(letter.split("\n")[0])

        for i in range(3):
            for j in range(3):
                character = self.__state.getValue("board")[i][j]

                letter = self.__formatter.renderText(character)

                verticalOffset = (letterHeight + 1) * i
                horizontalOffset = (letterWidth + 2) * j

                self.__screen.write(
                    row=self.__topRow + verticalOffset,
                    col=horizontalOffset,
                    toPrint=letter,
                    colorPairId=character,
                )

        for i in range(letterHeight * 3 + 4):
            self.__screen.write(self.__topRow + i, letterWidth, "||")
            self.__screen.write(self.__topRow + i, (letterWidth * 2) + 2, "||")

        for i in range(letterWidth * 3 + 2):
            self.__screen.write(self.__topRow + letterHeight, i, "=")
            self.__screen.write(
                self.__topRow + (letterHeight * 2) + 2, i, "="
            )
    
    def __drawGameInfo(self):
        letter = self.__formatter.renderText("X")
        letterWidth = len(letter.split("\n")[0])

        boardFarRightCol = letterWidth * 3 + 4
        boardTopRow = self.__topRow



        infoList = []
        infoList.append("You Are Symbol " + ("X" if self.__playerID == 0 else "O"))
        infoList.append("Your Opponent is " + self.__state.getValue("playerNames")[(self.__playerID + 1) % 2])
        
        if (self.__state.getValue("gameOver") != ""):
            infoList.append(self.__state.getValue("gameOver"))
        else:
            infoList.append(f"It's {self.__state.getValue("playerNames")[self.__state.getValue("currentPlayer")]}'s Turn")

        for i, info in enumerate(infoList):
            self.__screen.write(boardTopRow + 6 + (i * 2), boardFarRightCol + 7, info)
