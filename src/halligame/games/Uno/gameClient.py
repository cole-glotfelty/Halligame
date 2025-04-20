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
        self.__screen = Screen(self.userInput, self.mouseInput, width=50, height=25)
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
                if type(region) == int and self.__game.cardPlacable(self.__topCard, self.__deck[region]): # this is a deck index - TODO: add a confirm button
                    # need to pick a card before it's a valid choice
                    if (self.__game.type(self.__deck[region]) in ["wild", "+4"]):
                        self.__waitingCard = self.__deck.pop(region)
                        self.colorPicker()
                    else:
                        self.__comms.sendMessage(("placeCard", (self.__userId, self.__deck.pop(region))))
                if region == "dealCard":
                    self.__comms.sendMessage(("dealCard", self.__userId))
                elif (region in ["red", "yellow", "green", "blue"]):
                    if (self.__waitingCard != None):
                        card = self.__game.setColor(self.__waitingCard, region)

                        self.__comms.sendMessage(("placeCard", (self.__userId, card)))

                        self.__waitingCard = None

    def colorPicker(self):
        menuText = pyfiglet.figlet_format(f"Pick a Color", font="finalass")
        numRows = self.__screen.terminalHeight()
        numCols = self.__screen.terminalWidth()

        self.__screen.clearScreen()
        self.__screen.write(int(numRows * 0.2), 15, menuText)

        red = pyfiglet.figlet_format(f"RED", font="finalass")
        yellow = pyfiglet.figlet_format(f"YELLOW", font="finalass")
        green = pyfiglet.figlet_format(f"GREEN", font="finalass")
        blue = pyfiglet.figlet_format(f"BLUE", font="finalass")

        self.__screen.write(5, 100, red, "red")
        self.__screen.write(15, 100, yellow, "yellow")
        self.__screen.write(25, 100, green, "green")
        self.__screen.write(35, 100, blue, "blue")

        self.__screen.addClickableRegion(5, 100, len(red.split("\n")), len(red.split("\n")[0]), "red")
        self.__screen.addClickableRegion(15, 100, len(yellow.split("\n")), len(yellow.split("\n")[0]), "yellow")
        self.__screen.addClickableRegion(25, 100, len(green.split("\n")), len(green.split("\n")[0]), "green")
        self.__screen.addClickableRegion(35, 100, len(blue.split("\n")), len(blue.split("\n")[0]), "blue")

        self.__screen.refresh()

    def joinConfirmed(self, joinMsg):
        with self.__stateLock:
            if (joinMsg == "Game Full"):
                todo: crash
            else:
                (self.__userId, self.__deck) = joinMsg
                self.__game.drawCard(0, 0, self.__deck[0], self.__screen)
                self.__screen.refresh()

    def gotServerMessage(self, msg):
        with self.__stateLock:
            if (msg[0] == "Game Over"):
                self.__screen.clearScreen()
                text = pyfiglet.figlet_format(f"Game Over, Player {msg[1]} Won", font="red_phoenix")
                self.__screen.write(15, 15, text, "white") # TODO: don't use constants
                self.__screen.refresh()
            elif (msg[0] == "state"):
                self.__screen.clearScreen()
                (self.__topCard, opponentCardCounts, self.__currUsersTurn) = msg[1]

                self.__myTurn = self.__currUsersTurn == self.__userId

                self.__screen.clearScreen()
                self.__drawScreen(self.__topCard, opponentCardCounts)
                self.__screen.refresh()
            elif (msg[0] == "newCard"):
                self.__deck.append(msg[1])

    def __drawScreen(self, topCard, opponentCardCounts):
        self.__drawOpponentCards(opponentCardCounts)
        self.__drawCardPile(topCard)
        self.__drawGameInfo()
        self.__drawHand()
        self.__drawButtons()

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
