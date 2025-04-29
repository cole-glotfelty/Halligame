"""Handles the cards of a game of Uno (including  deck and discard pile).

Note: not designed to be thread safe

Written by:  Will Cordray, 2024-04-19
Last edited: Will Cordray, 2025-04-28
"""

import random

import pyfiglet

from halligame.utils.screen import Screen

BLANK_CARD = [
    "  _____________  ",
    " /             \\ ",
    "|               |",
    "|               |",
    "|               |",
    "|               |",
    "|               |",
    "|               |",
    "|               |",
    "|               |",
    "|               |",
    " \\_____________/ ",
]

type Card = str | tuple[str | int, str | None]
type Deck = list[Card]


class Uno:
    """Handles the cards of a game of Uno (including  deck and discard pile)."""

    def __init__(self) -> None:
        """Initialize a deck of uno cards."""
        self.__colors = ["red", "yellow", "green", "blue"]
        self.__deck: Deck = self.__createDeck()
        self.__discards: Deck = []
        self.__topCard: Card = self.dealCard()

        # want a number to be the top card
        while type(self.type(self.__topCard)) != int:
            self.placeCard(self.__topCard)
            self.__topCard = self.dealCard()

        self.__valueformatter = pyfiglet.Figlet(font="future_7")

    def __createDeck(self) -> list[Card]:
        """Creates a new uno deck with the appropiate card ratios."""
        deck: list[Card] = []

        normalCards = [(i, color) for i in range(10) for color in self.__colors]
        skips = [("skip", color) for color in self.__colors]
        reverses = [("reverse", color) for color in self.__colors]
        plusTwos = [("+2", color) for color in self.__colors]
        wilds = [("wild", None)] * 4
        plusFours = [("+4", None)] * 4

        # two sets of normal cards
        deck += normalCards
        deck += normalCards
        deck += skips
        deck += reverses
        deck += plusTwos
        deck += wilds
        deck += plusFours

        random.shuffle(deck)

        return deck

    def cardPlacable(self, onPile: Card, toPlace: Card) -> bool:
        """Returns whether toPlace is placable on onPile."""
        return bool(
            onPile == "blank"
            or self.type(toPlace) in ["wild", "+4"]
            or self.type(toPlace) == self.type(onPile)
            or self.color(toPlace) == self.color(onPile)
        )

    def dealCard(self) -> Card:
        """Draw a card from the deck, shuffling discards if needed."""
        if len(self.__deck) == 0:
            self.__reshuffleDeck()

        card = self.__deck[0]
        self.__deck = self.__deck[1:]
        return card

    def __reshuffleDeck(self):
        if len(self.__discards) > 0: # available discard
            random.shuffle(self.__discards)
            self.__deck = self.__discards
            for i in range(len(self.__deck)):
                if self.type(self.__deck[i]) in ["wild", "+4"]:
                    self.__deck[i] = self.setColor(self.__deck[i], None)
            self.__discards = []
        else:
            self.__deck = self.__createDeck()

    def placeCard(self, card: Card) -> None:
        """Places a card on the discard pile, updating the top card."""
        self.__discards.append(self.__topCard)

        self.__topCard = card

    def getTopCard(self) -> Card:
        """Returns the top card of the discard pile."""
        return self.__topCard

    def type(self, card: Card) -> str | int:
        """Returns the type of the card."""
        if card == "blank":
            return "blank"
        else:
            return card[0]

    def color(self, card: Card) -> str | None:
        """Returns the color of the card."""
        if card == "blank":
            return None
        else:
            return card[1]

    def setColor(self, card: Card, newColor: str) -> Card:
        """Sets the color of the card to the new color (must be wild or +4)."""
        if self.type(card) not in ["wild", "+4"]:
            return card

        return (self.type(card), newColor)

    def drawCard(
        self, topLeftRow: int, topLeftCol: int, card: Card, Screen: Screen
    ) -> None:
        """Draws the top card from the deck."""
        Screen.write(
            topLeftRow, topLeftCol, "\n".join(BLANK_CARD), self.color(card)
        )
        cardColor = self.color(card)
        if cardColor is not None:
            colorMarker = "\n" + cardColor[0].upper()
        elif self.type(card) != "blank":
            self.drawEmptyRainbowCard(topLeftRow, topLeftCol, Screen)
            colorMarker = ""

        if type(self.type(card)) == int:
            cardValue = self.__valueformatter.renderText(str(self.type(card)))
            Screen.write(
                topLeftRow + 3, topLeftCol + 5, cardValue, self.color(card)
            )

            self.cornerCardDraw(
                topLeftRow,
                topLeftCol,
                str(self.type(card)) + colorMarker,
                Screen,
                self.color(card),
            )
        elif self.type(card) == "skip":
            self.cornerCardDraw(
                topLeftRow,
                topLeftCol,
                "S\nK\nI\nP\n" + colorMarker,
                Screen,
                self.color(card),
            )
        elif self.type(card) == "reverse":
            self.cornerCardDraw(
                topLeftRow,
                topLeftCol,
                "->\n<-\n" + colorMarker,
                Screen,
                self.color(card),
            )
        elif self.type(card) == "+2":
            self.cornerCardDraw(
                topLeftRow,
                topLeftCol,
                "+2" + colorMarker,
                Screen,
                self.color(card),
            )
        elif self.type(card) == "wild":
            self.cornerCardDraw(
                topLeftRow,
                topLeftCol,
                "W\nI\nL\nD\n" + colorMarker,
                Screen,
                self.color(card),
            )
        elif self.type(card) == "+4":
            self.cornerCardDraw(
                topLeftRow,
                topLeftCol,
                "+4" + colorMarker,
                Screen,
                self.color(card),
            )
        elif self.type(card) == "blank":
            Screen.write(
                topLeftRow + 5, topLeftCol + 8, "U\nN\nO", self.color(card)
            )
        else:
            Screen.write(
                topLeftRow + 20,
                topLeftCol + 20,
                "Unknown Card" + str(card),
                self.color(card),
            )

    def cornerCardDraw(
        self,
        cardTopLeftRow: int,
        cardTopLeftCol: int,
        toWrite: str,
        Screen: Screen,
        color: str | None,
    ) -> None:
        """Write the extra info found in the corners of the cards (e.g. +2G)."""
        textHeight = len(toWrite.split("\n"))
        textWidth = max(map(lambda x: len(x), toWrite.split("\n")))
        Screen.write(cardTopLeftRow + 2, cardTopLeftCol + 2, toWrite, color)
        Screen.write(
            cardTopLeftRow + 11 - textHeight,
            cardTopLeftCol + 15 - textWidth,
            toWrite,
            color,
        )

    def drawEmptyRainbowCard(
        self, cardTopLeftRow: int, cardTopLeftCol: int, Screen: Screen
    ) -> None:
        """Draws a blank card with a rainbow border."""
        for i in range(len(BLANK_CARD)):
            color = self.__colors[(i // 3) % 4]
            Screen.write(
                cardTopLeftRow + i, cardTopLeftCol, BLANK_CARD[i], color
            )

    def cardHeight(self) -> int:
        """Returns the height of the ascii representation of a card."""
        return len(BLANK_CARD)

    def cardWidth(self) -> int:
        """Returns the width of the ascii representation of a card."""
        return len(BLANK_CARD[0])
