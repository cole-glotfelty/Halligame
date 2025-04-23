"""Our Canvas client.

Created:     Will Cordray,    2025-04-15
Last edited: Michael Daniels, 2025-04-22
"""

import threading
from typing import Any

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
        self.__screen = Screen(self.userInput, self.mouseInput)
        """The screen we'll use."""
        self.__stateLock = threading.Lock()
        """Protects self.__state.
        
        TODO: GameState already uses a lock... Needed?
        """
        self.__state: GameState = GameState()
        """Our game state."""
        self.__comms = comms
        """Our ClientCommunicate instance."""

        self.__boardHeight = 30
        self.__boardWidth = 40

        # TODO: doc these two
        self.__boardVOffset = 5
        self.__boardHOffset = 5

        # TODO: doc
        self.__colorBoxHeight = 3
        self.__colorBoxWidth = 5
        self.__colorBoxVSeparator = 1

        # TODO: any restrictions on this?
        self.__currColor: str = "blue"

        self.__screen.clearScreen()

        self.__initializeColors()
        self.__defineMouseRegions()
        self.__drawBlankBoard()

        self.__screen.refresh()

    def __initializeColors(self) -> None:
        """Initialize the screen's colors."""
        self.__colors = [
            "black",
            "blue",
            "cyan",
            "green",
            "magenta",
            "red",
            "white",
            "yellow",
        ]
        for color in self.__colors:
            self.__screen.addColorPair("white", color, color)

        self.__screen.addColor(209, 107, 177, "colorSwatchBackground")
        self.__screen.addColorPair(
            "white", "colorSwatchBackground", "colorSwatchBackground"
        )

        self.__screen.addColor(242, 182, 61, "currColorHighlight")
        self.__screen.addColorPair(
            "white", "currColorHighlight", "currColorHighlight"
        )

        self.__screen.setStyle("black")  # set background to black

    def __defineMouseRegions(self) -> None:
        """TODO: doc."""
        self.__screen.addClickableRegion(
            self.__boardVOffset,
            self.__boardHOffset,
            self.__boardHeight,
            self.__boardWidth,
            "board",
        )

        colorBoxHeight = self.__colorBoxHeight
        colorBoxWidth = self.__colorBoxWidth
        colorBoxVSeparator = self.__colorBoxVSeparator

        colorBoxCol = self.__boardHOffset + self.__boardWidth + 8

        colorBoxDraw = ((" " * colorBoxWidth) + "\n") * colorBoxHeight
        swatchBoxDraw = ((" " * (colorBoxWidth + 4)) + "\n") * (
            (colorBoxHeight * len(self.__colors))
            + (colorBoxVSeparator * (len(self.__colors) - 1))
            + 2
        )

        self.__screen.write(
            self.__boardVOffset - 1,
            colorBoxCol - 2,
            swatchBoxDraw,
            "colorSwatchBackground",
        )

        for i in range(len(self.__colors)):
            colorBoxRow = self.__boardVOffset + (
                i * (colorBoxHeight + colorBoxVSeparator)
            )
            self.__screen.addClickableRegion(
                colorBoxRow,
                colorBoxCol,
                colorBoxHeight,
                colorBoxWidth,
                self.__colors[i],
            )
            self.__screen.write(
                colorBoxRow, colorBoxCol, colorBoxDraw, self.__colors[i]
            )

        currColorBox = (" " * (colorBoxWidth + 4) + "\n") * (colorBoxHeight + 2)
        self.__currColorTopRow = (
            self.__boardVOffset
            + (len(self.__colors) * (colorBoxHeight + colorBoxVSeparator))
            + 6
        )
        self.__currColorLeftCol = colorBoxCol
        self.__screen.write(
            self.__currColorTopRow - 1,
            self.__currColorLeftCol - 2,
            currColorBox,
            "currColorHighlight",
        )

    def __drawBlankBoard(self) -> None:
        """TODO: doc."""
        boardDraw = ((" " * self.__boardWidth) + "\n") * self.__boardHeight
        self.__screen.write(
            self.__boardVOffset, self.__boardHOffset, boardDraw, "white"
        )
        self.__updateCurrColor(self.__currColor)

        instructions = "Left-Click to select color and draw\n\n"
        instructions += "Right-Click to erase"

        self.__screen.write(
            self.__boardVOffset + self.__boardHeight + 5,
            self.__boardHOffset,
            instructions,
        )

    def userInput(self, input: str | int) -> None:
        """Process user input.

        The only input we care about is "q", which quits the game.
        """
        if input == "q":
            self.__screen.shutdown()
            self.__comms.shutdown()

    def mouseInput(
        self, row: int, col: int, region: str, mouseEventType: str
    ) -> None:
        """TODO: doc."""
        with self.__stateLock:
            if mouseEventType == "left_click":
                if region == "board":
                    pixelToDraw = (
                        row - self.__boardVOffset,
                        col - self.__boardHOffset,
                        self.__currColor,
                    )
                    self.__drawPixel(pixelToDraw)
                    self.__comms.sendMessage(pixelToDraw)
                elif region is not None:
                    # the region is equal to the color name, so update it
                    self.__updateCurrColor(region)
            elif mouseEventType == "right_click" and region == "board":
                pixelToDraw = (
                    row - self.__boardVOffset,
                    col - self.__boardHOffset,
                    "white",
                )
                self.__drawPixel(pixelToDraw)
                self.__comms.sendMessage(pixelToDraw)

    def __updateCurrColor(self, color: str) -> None:
        """TODO: doc."""
        colorBoxDraw = (
            (" " * self.__colorBoxWidth) + "\n"
        ) * self.__colorBoxHeight
        self.__screen.write(
            self.__currColorTopRow, self.__currColorLeftCol, colorBoxDraw, color
        )
        self.__screen.refresh()

        self.__currColor = color

    def joinConfirmed(self, newState: bytes) -> None:
        """Set our game state."""
        with self.__stateLock:
            self.__state.deserialize(newState)
            self.__drawBoard()

    def gotServerMessage(self, msg: Any) -> None:
        """Process messages from the server.

        If the online players change, we update the screen.
        If the state changes, we update the screen as well.
        """
        if msg[0] == "state_diff":
            with self.__stateLock:
                self.__drawPixel(msg[1])
        elif msg[0] == "players":
            with self.__stateLock:
                self.__drawPlayers(msg[1])
                self.__screen.refresh()

    def __drawBoard(self) -> None:
        """Draw the board."""
        board = self.__state.getValue("board")
        for row in range(len(board)):
            for col in range(len(board[row])):
                self.__screen.write(
                    row + self.__boardVOffset,
                    col + self.__boardHOffset,
                    " ",
                    board[row][col],
                )
        self.__screen.refresh()

    def __drawPixel(self, pixelToDraw: tuple[int, int, str]) -> None:
        """Draw a pixel.

        Args:
            pixelToDraw: tuple (row, col, color)
        """
        (row, col, color) = pixelToDraw
        self.__screen.write(
            row + self.__boardVOffset, col + self.__boardHOffset, " ", color
        )
        self.__screen.refresh()

    # TODO: type annotation for players
    def __drawPlayers(self, players) -> None:
        """Draw the currently active players' names."""
        col = self.__boardHOffset + self.__boardWidth + 20
        players.sort()

        self.__screen.write(self.__boardVOffset + 2, col, "Active Players:")
        for i, player in enumerate(players):
            self.__screen.write(
                self.__boardVOffset + 3 + (i * 2), col + 2, player
            )
