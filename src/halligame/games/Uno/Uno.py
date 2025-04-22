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


class Uno:
    def __init__(self):
        self.__colors = ["red", "yellow", "green", "blue"]
        self.__deck = self.__createDeck()
        self.__discards = []

        self.__topCard = self.dealCard()
        # want a number to be the top card
        while type(self.type(self.__topCard)) != int:
            self.placeCard(self.__topCard)
            self.__topCard = self.dealCard()

        self.__valueformatter = pyfiglet.Figlet(font="future_7")

    def __createDeck(self):
        deck = []
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

    def cardPlacable(self, onPile, toPlace) -> bool:
        if onPile == "blank":
            return True
        elif self.type(toPlace) in ["wild", "+4"]:  # always placeable
            return True
        elif self.type(toPlace) == self.type(onPile):
            return True
        elif self.color(toPlace) == self.color(onPile):
            return True
        else:
            return False

    def dealCard(self):
        if len(self.__deck) == 0:
            if len(self.__discards) > 0:
                random.shuffle(self.__discards)
                self.__deck = self.__discards
                self.__discards = []
            else:
                self.__deck = self.__createDeck()

        card = self.__deck[0]
        self.__deck = self.__deck[1:]
        return card

    def placeCard(self, card):
        self.__discards.append(self.__topCard)

        self.__topCard = card

    def getTopCard(self):
        return self.__topCard

    def type(self, card):
        if card == "blank":
            return "blank"
        else:
            return card[0]

    def color(self, card):
        if card == "blank":
            return None
        else:
            return card[1]

    def setColor(self, card, newColor):
        if self.type(card) not in ["wild", "+4"]:
            return

        return (self.type(card), newColor)

    def drawCard(self, topLeftRow, topLeftCol, card, Screen: Screen):
        Screen.write(
            topLeftRow, topLeftCol, "\n".join(BLANK_CARD), self.color(card)
        )

        if self.color(card) != None:
            colorMarker = "\n" + self.color(card)[0].upper()
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
        self, cardTopLeftRow, cardTopLeftCol, toWrite, Screen, color
    ):
        textHeight = len(toWrite.split("\n"))
        textWidth = max(map(lambda x: len(x), toWrite.split("\n")))
        Screen.write(cardTopLeftRow + 2, cardTopLeftCol + 2, toWrite, color)
        Screen.write(
            cardTopLeftRow + 11 - textHeight,
            cardTopLeftCol + 15 - textWidth,
            toWrite,
            color,
        )

    def drawEmptyRainbowCard(self, cardTopLeftRow, cardTopLeftCol, Screen):
        for i in range(len(BLANK_CARD)):
            color = self.__colors[(i // 3) % 4]
            Screen.write(
                cardTopLeftRow + i, cardTopLeftCol, BLANK_CARD[i], color
            )

    def cardHeight(self):
        return len(BLANK_CARD)

    def cardWidth(self):
        return len(BLANK_CARD[0])
