# gameClient.py

import threading

from halligame.utils.gameClientTemplate import ClientSuper
from halligame.utils.gameState import GameState
from halligame.utils.screen import Screen


class Client(ClientSuper):
    # comms is an instance of halligame.utils.ClientCommunicate
    def __init__(self, comms):
        self.__screen = Screen(
            self.userInput, self.mouseInput, width=50, height=25
        )
        self.__stateLock = threading.Lock()
        self.__comms = comms
        self.__state: GameState = GameState()

        self.__boardHeight = 30
        self.__boardWidth = 40

        self.__boardVOffset = 5
        self.__boardHOffset = 5

        self.__colorBoxHeight = 3
        self.__colorBoxWidth = 5
        self.__colorBoxVSeparator = 1

        self.__currColor = "blue"

        self.__screen.clearScreen()

        self.__initializeColors()
        self.__defineMouseRegions()
        self.__drawBlankBoard()

        self.__screen.refresh()

    def __initializeColors(self):
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

    def __defineMouseRegions(self):
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

    def __drawBlankBoard(self):
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

    def userInput(self, input):
        if input == "q":
            self.__screen.shutdown()
            self.__comms.shutdown()

    def mouseInput(self, row, col, region, mouseEventType):
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

    def __updateCurrColor(self, color):
        colorBoxDraw = (
            (" " * self.__colorBoxWidth) + "\n"
        ) * self.__colorBoxHeight
        self.__screen.write(
            self.__currColorTopRow, self.__currColorLeftCol, colorBoxDraw, color
        )
        self.__screen.refresh()

        self.__currColor = color

    def __drawPixel(self, pixelToDraw):
        (row, col, color) = pixelToDraw
        self.__screen.write(
            row + self.__boardVOffset, col + self.__boardHOffset, " ", color
        )
        self.__screen.refresh()

    def joinConfirmed(self, newState):
        with self.__stateLock:
            self.__state.deserialize(newState)
            self.__drawBoard()

    def gotServerMessage(self, msg):
        if msg[0] == "state_diff":
            with self.__stateLock:
                self.__drawPixel(msg[1])

    def __drawBoard(self):
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
