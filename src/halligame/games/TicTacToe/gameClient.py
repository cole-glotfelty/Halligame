"""Our TicTacToe game client.

Written on 2025-03-22 by Cole Glotfelty
Last edited by: Michael Daniels 2025-04-22
"""

import time
from threading import Lock
from typing import Any

from pyfiglet import Figlet

from halligame.utils.gameClientTemplate import ClientSuper
from halligame.utils.gameState import GameState
from halligame.utils.screen import Screen


class Client(ClientSuper):
    """Represents our game's client."""

    def __init__(self, comms) -> None:  # noqa: ANN001
        """Initialize this client.

        Args:
            comms: is an instance of halligame.utils.ClientCommunicate
                   (It can't be type-annotated due to circular imports.)
        """
        #: The screen we'll use.
        self.__screen: Screen = Screen(self.userInput, self.mouseInput)
        #: Protects internal state.
        self.__stateLock: Lock = Lock()
        #: Our ClientCommunicate instance.
        self.__comms = comms
        #: Our game state.
        self.__state: GameState = GameState()
        #: Which player are we? 0 for X, 1 for O.
        self.__playerID: int = -1
        #: Is it my turn?
        self.__myTurn: bool = True
        #: TODO: doc
        self.__formatter: Figlet = Figlet(font="georgia11")
        #: TODO: doc
        self.__topRow: int = 5

        self.initializeScreenColors()

    def gotServerMessage(self, msg: tuple[str, Any]) -> None:
        """Process messages from the server.

        The received message should be a tuple (str, state).
        The string is displayed, and the state is updated.
        """
        with self.__stateLock:
            toPrint = self.__formatter.renderText(msg[0])
            self.__screen.displayFullScreenMessage(toPrint)
            time.sleep(1.5)

            self.__updateState(msg[1])

    def joinConfirmed(self, msg: Any) -> None:
        """Set our player ID and game state."""
        (playerID, state) = msg
        self.__playerID = playerID
        self.updateState(state)
        self.defineClickableRegions()

    def initializeScreenColors(self) -> None:
        """Initialize the screen's colors."""
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
        """TODO: doc."""
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
        """Receive user input.

        If the input is the letter "q", quit.
        If it's our turn, process the command.
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
                except Exception:  # didn't enter a number
                    pass

    def mouseInput(
        self, row: int, col: int, region: int | None, mouseEventType: str
    ) -> None:
        """TODO: doc."""
        with self.__stateLock:  # draw it so it appears instantaneously
            if self.__state.getValue("gameOver") != "":
                message = self.__formatter.renderText("Game Over")
                self.__screen.displayFullScreenMessage(message)
                time.sleep(1)
                self.__drawGame()
            elif not self.__myTurn:
                message = self.__formatter.renderText("Not Your Turn")
                self.__screen.displayFullScreenMessage(message)
                time.sleep(1.5)
                self.__drawGame()
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
        """Update the game's state and display."""
        with self.__stateLock:
            self.__updateState(newState)

    def __updateState(self, newState: bytes) -> None:
        """Update the game's state and display without using the lock.

        Meant to be called by other functions that have already acquired
        the statelock.
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
        """Draw our "Game Over" screen."""
        Message = self.__formatter.renderText(self.__state.getValue("gameOver"))
        self.__screen.displayFullScreenMessage(Message)
        time.sleep(3)

        self.__drawGame()

    def __drawGame(self) -> None:
        """Draw the game as dictated by the current state."""
        self.__screen.clearScreen()
        self.__drawBoard()
        self.__drawGameInfo()
        self.__screen.refresh()

    def __drawBoard(self) -> None:
        """Draw the tic-tac-toe board."""
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
            self.__screen.write(self.__topRow + (letterHeight * 2) + 2, i, "=")

    def __drawGameInfo(self) -> None:
        """Draw some ancillary info."""
        letter = self.__formatter.renderText("X")
        letterWidth = len(letter.split("\n")[0])

        boardFarRightCol = letterWidth * 3 + 4
        boardTopRow = self.__topRow

        infoList = []
        infoList.append(
            "You Are Symbol " + ("X" if self.__playerID == 0 else "O")
        )
        infoList.append(
            "Your Opponent is "
            + self.__state.getValue("playerNames")[(self.__playerID + 1) % 2]
        )

        if self.__state.getValue("gameOver") != "":
            infoList.append(self.__state.getValue("gameOver"))
        else:
            playerNames = self.__state.getValue("playerNames")
            currentPlayer = self.__state.getValue("currentPlayer")
            infoList.append(f"It's {playerNames[currentPlayer]}'s Turn")

        for i, info in enumerate(infoList):
            self.__screen.write(
                boardTopRow + 6 + (i * 2), boardFarRightCol + 7, info
            )
