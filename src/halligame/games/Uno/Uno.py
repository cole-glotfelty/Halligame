import random
import pyfiglet
from halligame.utils.screen import Screen

BLANK_CARD = [  '  ____________  ',
                ' /            \\ ',
                '|              |',
                '|              |',
                '|              |',
                '|              |',
                '|              |',
                '|              |',
                '|              |',
                '|              |',
                '|              |',
              ' \\____________/ '
             ]

class Uno():
    def __init__(self):
        self.__colors = ["red", "yellow", "green", "blue"]
        self.__deck = self.__createDeck()
        self.__discards = []
        self.__topCard = self.dealCard()

        self.__valueformatter = pyfiglet.Figlet(font='future_7')
    
    def __createDeck(self):
        deck = []
        normalCards = [(i, color) for i in range(10) 
                                      for color in self.__colors]

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
        if (onPile == "blank"):
            return True
        elif (self.type(toPlace) in ["wild", "+4"]): # always placeable
            return True
        elif (self.type(toPlace) == self.type(onPile)):
            return True
        elif (self.color(toPlace) == self.color(onPile)):
            return True
        else:
            return False

    def dealCard(self):
        if len(self.__deck) == 0:
            self.__deck = random.shuffle(self.__discards)
            self.__discards = []
        
        card = self.__deck[0]
        self.__deck = self.__deck[1:]
        return card

    def placeCard(self, card):
        if (self.__topCard != "blank"):
            self.__discards.append(self.__topCard)
        
        self.__topCard = card
    
    def getTopCard(self):
        return self.__topCard

    def type(self, card):
        if (card == "blank"):
            return "blank"
        else:
            return card[0]

    def color(self, card):
        if (card == "blank"):
            return None
        else:
            return card[1]
    
    def setColor(self, card, newColor):
        if (self.type(card) not in ["wild", "+4"]):
            return

        return (self.type(card), newColor)

    def drawCard(self, topLeftRow, topLeftCol, card, Screen: Screen):
        Screen.write(topLeftRow, topLeftCol, "\n".join(BLANK_CARD), self.color(card))
        if (type(self.type(card)) == int):
            cardValue = self.__valueformatter.renderText(str(self.type(card)))
            Screen.write(topLeftRow + 3, topLeftCol + 5, cardValue, self.color(card))
        elif (self.type(card) == "skip"):
            self.cornerCardDraw(topLeftRow, topLeftCol, "S\nK\nI\nP", Screen, self.color(card))
        elif (self.type(card) == "reverse"):
            self.cornerCardDraw(topLeftRow, topLeftCol, "->\n<-", Screen, self.color(card))
            Screen.write(topLeftRow + 9, topLeftCol + 12, "->\n<-", self.color(card))
        elif (self.type(card) == "+2"):
            self.cornerCardDraw(topLeftRow, topLeftCol, "+2", Screen, self.color(card))
        elif (self.type(card) == "wild"):
            self.cornerCardDraw(topLeftRow, topLeftCol, "WILD", Screen, self.color(card))
        elif (self.type(card) == "+4"):
            self.cornerCardDraw(topLeftRow, topLeftCol, "+4", Screen, self.color(card))
        elif (self.type(card) == "blank"):
            Screen.write(topLeftRow + 5, topLeftCol + 8, "U\nN\nO", self.color(card))
        else:
            Screen.write(topLeftRow + 20, topLeftCol + 20, "Unknown Card" + str(card), self.color(card))

    def cornerCardDraw(self, cardTopLeftRow, cardTopLeftCol, toWrite, Screen, color):
            Screen.write(cardTopLeftRow + 2, cardTopLeftCol + 4, toWrite, color)

    def cardHeight(self):
        return len(BLANK_CARD)

    def cardWidth(self):
        return len(BLANK_CARD[0])
