"""Our Uno game client.

Written by:  Will Cordray, 2024-04-19
Last edited: Will Cordray, 2025-04-28
"""

import threading
import time
from typing import Any

import pyfiglet

from halligame.utils.gameClientTemplate import ClientSuper
from halligame.utils.screen import Screen

from .Uno import Card, Deck, Uno


class Client(ClientSuper):
    """Represents our game's client."""

    def __init__(self, comms) -> None:  # noqa: ANN001
        """Initialize this instance.

        Args:
            comms: halligame.utils.ClientCommunicate
        """
        self.__screen = Screen(self.userInput, self.mouseInput)
        self.__stateLock = threading.Lock()
        self.__comms = comms
        self.__game = Uno()

        self.__currUsersTurn = 0  # the current users turn
        self.__myTurn = False  # whether it's my turn or not
        self.__gameOver = False
        self.__waitingCard: Card = "blank"

        self.__unoPossibility = False
        self.__topCard: Card
        self.__deck: Deck

        self.__screen.clearScreen()

        self.__initColors()

        self.__screen.refresh()

    def __initColors(self) -> None:
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
            self.__screen.addColorPair(color, "black", color)

        # redefine blue to be cyan for nicer printing
        self.__screen.addColorPair("cyan", "black", "blue")

        self.__screen.addColorPair("white", "black", "background")
        self.__screen.setStyle("background")  # set background to black

    def userInput(self, input: Any) -> None:
        """Process user input.

        The only input we care about is "q", which quits the game.
        """
        if input == "q":
            self.__screen.shutdown()
            self.__comms.shutdown()

    def mouseInput(
        self, row: int, col: int, region: Any, mouseEventType: str
    ) -> None:
        """Handles mouse input from the user."""
        with self.__stateLock:
            if mouseEventType == "left_click":
                if self.__gameOver:
                    # don't allow moves if the game is over
                    self.__screen.displayFullScreenMessage(
                        "GAME OVER", font="roman"
                    )
                    time.sleep(1.5)
                    self.__drawScreen()
                elif region == "uno":
                    # they clicked the uno button
                    self.__comms.sendMessage(("uno", self.__playerNum))
                elif not self.__myTurn:
                    # not their turn, so tell them that and ignore
                    self.__screen.displayFullScreenMessage(
                        "NOT YOUR\nTURN", font="roman"
                    )
                    time.sleep(1.5)
                    self.__drawScreen()
                elif type(region) is int:
                    # They clicked on one of the cards, so verify it's
                    # placable then send to server (or popopen the color
                    # choice menu if it's a while or +4)
                    if not self.__game.cardPlacable(
                        self.__topCard, self.__deck[region]
                    ):
                        self.__screen.displayFullScreenMessage(
                            "INVALID CARD", font="roman"
                        )
                        time.sleep(1.5)
                        self.__drawScreen()
                    elif self.__game.type(self.__deck[region]) in [
                        "wild",
                        "+4",
                    ]:
                        self.__waitingCard = self.__deck.pop(region)
                        self.colorPicker()
                    else:
                        card = self.__deck.pop(region)

                        self.__myTurn = False

                        # do client insta update
                        self.__topCard = card
                        self.__unoPossibility = len(self.__deck) == 1
                        self.__drawScreen()

                        self.__comms.sendMessage(
                            ("placeCard", self.__playerNum, card, self.__deck)
                        )

                        # prevent screen refreshes to give them a chance to
                        # click uno
                        if self.__unoPossibility:
                            time.sleep(3)
                elif region == "dealCard":
                    # they clicked the deal card button, so request a card
                    # from the server
                    self.__comms.sendMessage(("dealCard", self.__playerNum))
                elif region in [
                    "red",
                    "yellow",
                    "green",
                    "blue",
                ]:  # click response to color picker
                    if self.__waitingCard != "blank":
                        card = self.__game.setColor(self.__waitingCard, region)

                        self.__myTurn = False
                        self.__unoPossibility = len(self.__deck) == 1
                        self.__drawScreen()

                        self.__comms.sendMessage(
                            ("placeCard", self.__playerNum, card, self.__deck)
                        )

                        self.__waitingCard = "blank"

                        # do insta update
                        self.__topCard = card
                        self.__drawScreen()

                        # prevent screen refreshes to give them a chance to
                        # click uno
                        if self.__unoPossibility:
                            time.sleep(3)

    def colorPicker(self) -> None:
        """Display color choice menu.

        Displays a menu that lets the user choose what color they want when
        they choose a wild or +4.
        """
        numRows = self.__screen.terminalHeight()
        self.__screen.clearScreen()

        # purposefully designed to be length 4
        menuTexts = [
            pyfiglet.figlet_format("PICK", font="finalass"),
            pyfiglet.figlet_format(
                self.__game.type(self.__waitingCard).upper(), font="finalass"
            ),
            pyfiglet.figlet_format("CARD", font="finalass"),
            pyfiglet.figlet_format("COLOR", font="finalass"),
        ]

        colors = ["RED", "YELLOW", "GREEN", "BLUE"]
        colorTexts = []
        for color in colors:
            colorTexts.append(pyfiglet.figlet_format(color, font="finalass"))

        textHeight = len(colorTexts[0].split("\n"))
        textGap = max(1, (numRows - (4 * textHeight)) // 5)
        for i in range(len(colors)):
            colorText = colorTexts[i]
            menuText = menuTexts[i]
            color = colors[i]

            row = (i * textHeight) + ((i + 1) * textGap)

            self.__screen.write(row, 15, menuText)

            self.__screen.write(row, 100, colorText, color.lower())
            textWidth = len(colorText.split("\n")[0])
            self.__screen.addClickableRegion(
                row, 100, textHeight, textWidth, color.lower()
            )

        self.__screen.refresh()

    def joinConfirmed(self, joinMsg: str | tuple[int, list]) -> None:
        """Set our game state."""
        with self.__stateLock:
            if type(joinMsg) is str:
                self.__screen.displayFullScreenMessage(
                    joinMsg + "\n\nJOINING AS VIEWER", font="roman"
                )
                time.sleep(3)
                # joining as viewer
                self.__playerNum = -1
                self.__deck = []
            elif type(joinMsg) is tuple:
                (self.__playerNum, self.__deck) = joinMsg

    def gotServerMessage(self, msg: tuple) -> None:
        """Process messages from the server."""
        with self.__stateLock:
            if msg[0] == "Game Over":
                self.__gameOver = True
                self.__winner = msg[1]

                self.__screen.displayFullScreenMessage(
                    f"Game Over\n{self.__winner} Won!", font="roman"
                )
                time.sleep(3)
                self.__updateState(msg[2][1])
            elif msg[0] == "state":
                # got a new game state
                self.__updateState(msg[1])
            elif msg[0] == "newCard":
                # server is sending us a new card (either we drew a card, or
                # got hit with a +2/+4)
                self.__deck.append(msg[1])
                self.__drawScreen()
            elif msg[0] == "uno_loss":  # you lost the uno race
                self.__screen.displayFullScreenMessage(
                    "YOU DIDN'T\nSAY UNO!", font="roman"
                )
                time.sleep(1.5)
                self.__drawScreen()

    def __updateState(self, state: tuple) -> None:
        """Update the game state."""
        # unpack state
        (
            self.__topCard,
            self.__opponentCardCounts,
            self.__playerUTLNs,
            self.__currUsersTurn,
            unoPlayerNum,
        ) = state

        if unoPlayerNum is not None:
            self.__unoPossibility = True
        else:
            self.__unoPossibility = False

        if not self.__gameOver:
            # just became your turn
            if not self.__myTurn and self.__currUsersTurn == self.__playerNum:
                self.__screen.displayFullScreenMessage(
                    "YOUR TURN", font="roman"
                )
                time.sleep(0.5)

            self.__myTurn = self.__currUsersTurn == self.__playerNum

        self.__drawScreen()

    def __drawScreen(self) -> None:
        """Draws the entire screen for the Game."""
        self.__screen.clearScreen()
        self.__screen.clearClickableRegions()

        self.__drawOpponentCards()
        self.__drawGameInfo()
        self.__drawButtons()
        self.__drawCardPile()
        self.__drawHand()

        self.__screen.refresh()

    def __drawOpponentCards(self) -> None:
        """Draws the opponents info cards along the top of the screen."""
        cardHeight = self.__game.cardHeight()
        cardWidth = self.__game.cardWidth()

        col = 1
        for playerNum in range(len(self.__opponentCardCounts)):
            count = self.__opponentCardCounts[playerNum]
            if count == -1:
                continue

            self.__game.drawCard(0, col, "blank", self.__screen)

            # highlight the current user
            if playerNum == self.__currUsersTurn:
                colorPairId = "yellow"
            else:
                colorPairId = None

            self.__screen.write(
                cardHeight + 2,
                col + 1,
                f"Player: {self.__playerUTLNs[playerNum]}",
                colorPairId=colorPairId,
            )
            self.__screen.write(
                cardHeight + 3,
                col + 1,
                f"Count: {count}",
                colorPairId=colorPairId,
            )

            col += cardWidth + 2

    def __drawCardPile(self) -> None:
        """Draws the discard pile in the center of the screen."""
        centeredRow = (self.__screen.terminalHeight() // 2) - (
            self.__game.cardHeight() // 2
        )
        centeredCol = (self.__screen.terminalWidth() // 2) - (
            self.__game.cardWidth() // 2
        )

        self.__game.drawCard(
            centeredRow, centeredCol, self.__topCard, self.__screen
        )

    def __drawGameInfo(self) -> None:
        """Draws the game info for the game on the middle-left of the screen."""
        gameInfo = []
        if self.__gameOver:
            gameInfo.append(f"Game Over: {self.__winner} Won!")
        else:
            gameInfo.append(
                f"Current Player: {self.__playerUTLNs[self.__currUsersTurn]}"
            )
            if self.__myTurn:
                gameInfo.append("It's Your Turn")

        printableInfo = "\n\n".join(gameInfo)
        centeredRow = self.__screen.getCenteredRow(printableInfo)
        self.__screen.write(centeredRow, 3, printableInfo)

    def __drawHand(self) -> None:
        """Draws the cards in your current hand on the botton of the screen."""
        if len(self.__deck) == 0:  # nothing to draw
            return

        numCols = self.__screen.terminalWidth()
        numRows = self.__screen.terminalHeight()

        cardTopRow = numRows - self.__game.cardHeight() - 1
        cardCol = 1

        widthDiff = numCols // len(self.__deck)
        widthDiff = min(self.__game.cardWidth() + 8, widthDiff)

        # Make sure within bound
        while (
            widthDiff * (len(self.__deck) - 1)
            + self.__game.cardWidth()
            + cardCol
            >= numCols
        ):
            widthDiff -= 1

        widthDiff = max(3, widthDiff)

        for i, card in enumerate(self.__deck):
            self.__game.drawCard(cardTopRow, cardCol, card, self.__screen)

            self.__screen.addClickableRegion(
                cardTopRow,
                cardCol,
                self.__game.cardHeight(),
                self.__game.cardWidth(),
                i,
            )

            cardCol += widthDiff

    def __drawButtons(self) -> None:
        """Draws clickable buttons for the game.

        Draws on the middle right of the screen. The possible buttons are draw
        are the draw button and the uno button if there is an uno possiblity.
        """
        if self.__playerNum == -1:  # viewer
            return

        buttons: list[tuple[pyfiglet.FigletString, str]] = []
        buttons.append(
            (pyfiglet.figlet_format("DRAW", font="finalass"), "dealCard")
        )

        self.__defineAndDrawButtons(buttons)

    # format of buttons is [(messageToDisplay, regionId), ...]
    def __defineAndDrawButtons(
        self, buttons: list[tuple[pyfiglet.FigletString, str]]
    ) -> None:
        """Helper for actually drawing and defining the button stack."""
        startingDrawRow = (
            self.__screen.terminalHeight() - self.__game.cardHeight() - 1
        )
        startingDrawCol = self.__screen.terminalWidth()
        for button, regionId in buttons:
            drawButtonHeight = len(button.split("\n"))
            drawButtonWidth = len(button.split("\n")[0])

            drawRow = startingDrawRow - drawButtonHeight
            drawCol = startingDrawCol - (drawButtonWidth + 2)

            border = (("*" * (drawButtonWidth + 2)) + "\n") * (
                drawButtonHeight + 1
            )

            self.__screen.write(drawRow - 1, drawCol - 1, border)
            self.__screen.write(drawRow, drawCol, button)

            self.__screen.addClickableRegion(
                drawRow - 1,
                drawCol - 1,
                drawButtonHeight + 2,
                drawButtonWidth + 2,
                regionId,
            )

            startingDrawRow = drawRow - 3
