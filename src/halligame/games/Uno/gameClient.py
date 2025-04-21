# gameClient.py

from halligame.utils.screen import Screen
from halligame.utils.gameState import GameState
from halligame.utils.gameClientTemplate import ClientSuper

import threading
from .Uno import Uno

import time
import pyfiglet

class Client(ClientSuper):
    # comms is an instance of halligame.utils.ClientCommunicate 
    def __init__(self, comms):
        self.__screen = Screen(self.userInput, self.mouseInput)
        self.__stateLock = threading.Lock()
        self.__comms = comms
        self.__game = Uno()

        self.__currUsersTurn = 0 # the current users turn
        self.__myTurn = False # whether it's my turn or not
        self.__waitingCard = None

        self.__screen.clearScreen()

        self.__initColors()

        self.__screen.refresh()

    def __initColors(self):
        self.__colors = ["black", "blue", "cyan", "green", "magenta", "red", 
                         "white", "yellow"]
        for color in self.__colors:
            self.__screen.addColorPair(color, "white", color)

        self.__screen.addColorPair("black", "white", "background")
        self.__screen.setStyle("background") # set background to black

    def userInput(self, input):
        if (input == "q"):
            self.__screen.shutdown()
            self.__comms.shutdown()

    def mouseInput(self, row, col, region, mouseEventType):
        with self.__stateLock:
            if mouseEventType == "left_click" and self.__myTurn:
                if type(region) == int and self.__game.cardPlacable(self.__topCard, self.__deck[region]):
                    # need to pick a card before it's a valid choice
                    if (self.__game.type(self.__deck[region]) in ["wild", "+4"]):
                        self.__waitingCard = self.__deck.pop(region)
                        self.colorPicker()
                    else:
                        self.__comms.sendMessage(("placeCard", self.__userId, self.__deck.pop(region)))
                if region == "dealCard":
                    self.__comms.sendMessage(("dealCard", self.__userId))
                elif (region in ["red", "yellow", "green", "blue"]): # respond to color picker
                    if (self.__waitingCard != None):
                        card = self.__game.setColor(self.__waitingCard, region)

                        self.__comms.sendMessage(("placeCard", self.__userId, card))

                        self.__waitingCard = None

    def colorPicker(self):
        menuText = pyfiglet.figlet_format(f"COLOR", font="finalass")
        numRows = self.__screen.terminalHeight()
        numCols = self.__screen.terminalWidth()

        self.__screen.clearScreen()
        self.__screen.write(int(numRows * 0.2), 15, menuText)

        colors = ["RED", "YELLOW", "GREEN", "BLUE"]
        colorTexts = []
        for color in colors:
            colorTexts.append(pyfiglet.figlet_format(color.lower(), font="finalass"))

        textHeight = len(colors[0].split("\n"))
        textGap = max(1, (numRows - (4 * textHeight)) // 5)
        for i in range(len(colors)):
            colorText = colorTexts[i]
            color = colors[i]

            row = i * textHeight + ((i + 1) * textGap)
            col = 100

            self.__screen.write(row, col, colorText, color.lower())

            textWidth = colorText.split("\n")[0]
            self.__screen.addClickableRegion(row, col, textHeight, textWidth, color.lower())

        self.__screen.refresh()

    def joinConfirmed(self, joinMsg):
        with self.__stateLock:
            if (joinMsg == "Game Full"):
                # joining as viewer
                self.__userId = -1
                self.__deck = []
            else:
                (self.__userId, self.__deck) = joinMsg
                self.__game.drawCard(0, 0, self.__deck[0], self.__screen)
                self.__screen.refresh()

    def gotServerMessage(self, msg):
        with self.__stateLock:
            if (msg[0] == "Game Over"):
                self.__screen.clearScreen()
                text = pyfiglet.figlet_format(f"Game Over, Player {msg[1]} Won", font="red_phoenix")
                self.__screen.write(15, 15, text) # TODO: don't use constants
                self.__screen.refresh()
            elif (msg[0] == "state"):
                self.__screen.clearScreen()
                (self.__topCard, opponentCardCounts, self.__playerUTLNs, self.__currUsersTurn) = msg[1]

                self.__myTurn = self.__currUsersTurn == self.__userId

                self.__drawScreen(self.__topCard, opponentCardCounts)
            elif (msg[0] == "newCard"):
                self.__deck.append(msg[1])
                self.__drawScreen(self.__topCard, opponentCardCounts)

    def __drawScreen(self, topCard, opponentCardCounts):
        self.__screen.clearScreen()
        self.__screen.clearClickableRegions()

        self.__drawOpponentCards(opponentCardCounts)
        self.__drawCardPile(topCard)
        self.__drawGameInfo()
        self.__drawHand()
        self.__drawButtons()

        self.__screen.refresh()

    def __drawOpponentCards(self, opponentCardCounts):
        cardHeight = self.__game.cardHeight()
        cardWidth = self.__game.cardWidth()

        col = 0
        for (userId, Count) in opponentCardCounts:
            self.__game.drawCard(0, col, "blank", self.__screen)

            # highlight the current user
            if (userId == self.__currUsersTurn):
                colorPairId = "yellow"
            else:
                colorPairId = None

            self.__screen.write(cardHeight + 2, col + 3, f"Player: {userId}", colorPairId=colorPairId)
            self.__screen.write(cardHeight + 3, col + 3, f"Count: {Count}", colorPairId=colorPairId)
    
            col += cardWidth

    def __drawCardPile(self, topCard):
        row = (self.__screen.terminalHeight() // 2) - (self.__game.cardHeight() // 2)
        col = (self.__screen.terminalWidth() // 2) - (self.__game.cardWidth() // 2)

        if (topCard == None):
            topCard = "blank"

        self.__game.drawCard(row, col, topCard, self.__screen)

    def __drawGameInfo(self):
        gameInfo = []
        gameInfo.append(f"Current Player: {self.__currUsersTurn}")
        gameInfo.append(f"Your Player Name: {self.__userId}")

        numRows = self.__screen.terminalHeight()

        row = (numRows // 2) - (len(gameInfo) // 2)
        for infoPiece in gameInfo:
            self.__screen.write(row, 0, infoPiece)
            row += 1

    def __drawHand(self):
        numCols = self.__screen.terminalWidth()
        numRows = self.__screen.terminalHeight()

        cardTopRow = numRows - self.__game.cardHeight() - 1

        widthDiff = max(4, numCols // len(self.__deck))

        # reset previous regions
        self.__screen.addClickableRegion(cardTopRow, 0, self.__game.cardHeight(), numCols, None)

        col = 0
        for i, card in enumerate(self.__deck):
            self.__game.drawCard(cardTopRow, col, card, self.__screen)

            self.__screen.addClickableRegion(cardTopRow, col, self.__game.cardHeight(), self.__game.cardWidth(), i)

            col += widthDiff

    def __drawButtons(self):
        self.__defineDrawCardButton()

    def __defineDrawCardButton(self):
        drawButton = pyfiglet.figlet_format(f"DRAW", font="finalass")
        drawButtonHeight = len(drawButton.split("\n"))
        drawButtonWidth = len(drawButton.split("\n")[0])

        drawRow = self.__screen.terminalHeight() - self.__game.cardHeight() - (drawButtonHeight + 2)
        drawCol = self.__screen.terminalWidth() - (drawButtonWidth + 4)

        drawBorder = (("*" * (drawButtonWidth + 2)) + "\n")  * (drawButtonHeight + 1)
        self.__screen.write(drawRow, drawCol, drawBorder)
        self.__screen.write(drawRow + 1, drawCol + 1, drawButton)

        self.__screen.addClickableRegion(drawRow, drawCol, drawButtonHeight + 2, drawButtonWidth + 2, "dealCard")
