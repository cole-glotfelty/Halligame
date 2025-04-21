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
            self.__screen.addColorPair(color, "black", color)
        
        # redefine blue to be cyan for printing
        self.__screen.addColorPair("cyan", "black", "blue")

        self.__screen.addColorPair("white", "black", "background")
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
                        self.__comms.sendMessage(("placeCard", self.__playerNum, self.__deck.pop(region), self.__deck))
                if region == "dealCard":
                    self.__comms.sendMessage(("dealCard", self.__playerNum))
                elif (region in ["red", "yellow", "green", "blue"]): # respond to color picker
                    if (self.__waitingCard != None):
                        card = self.__game.setColor(self.__waitingCard, region)

                        self.__comms.sendMessage(("placeCard", self.__playerNum, card, self.__deck))

                        self.__waitingCard = None

    def colorPicker(self):
        numRows = self.__screen.terminalHeight()
        numCols = self.__screen.terminalWidth()
        self.__screen.clearScreen()

        # purposefully designed to be length 4
        menuTexts = [pyfiglet.figlet_format(f"PICK", font="finalass"),
                     pyfiglet.figlet_format(self.__game.type(self.__waitingCard).upper(), font="finalass"),
                     pyfiglet.figlet_format(f"CARD", font="finalass"),
                     pyfiglet.figlet_format(f"COLOR", font="finalass")]

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
            self.__screen.addClickableRegion(row, 100, textHeight, textWidth, color.lower())

        self.__screen.refresh()

    def joinConfirmed(self, joinMsg):
        with self.__stateLock:
            if (joinMsg == "Game Full"):
                # joining as viewer
                self.__playerNum = -1
                self.__deck = []
            else:
                (self.__playerNum, self.__deck) = joinMsg

    def gotServerMessage(self, msg):
        with self.__stateLock:
            if (msg[0] == "Game Over"):
                self.__screen.clearScreen()
                text = pyfiglet.figlet_format(f"Game Over, Player {msg[1]} Won", font="red_phoenix")
                self.__screen.write(self.__screen.getCenteredRow(text), self.__screen.getCenteredCol(text), text)
                self.__screen.refresh()
            elif (msg[0] == "state"):
                # unpack state
                (self.__topCard, self.__opponentCardCounts, self.__playerUTLNs, self.__currUsersTurn) = msg[1]

                self.__myTurn = self.__currUsersTurn == self.__playerNum

                self.__drawScreen()
            elif (msg[0] == "newCard"):
                self.__deck.append(msg[1])
                self.__drawScreen()

    def __drawScreen(self):
        self.__screen.clearScreen()
        self.__screen.clearClickableRegions()

        self.__drawOpponentCards()
        self.__drawGameInfo()
        self.__drawButtons()
        self.__drawCardPile()
        self.__drawHand()

        self.__screen.refresh()

    def __drawOpponentCards(self):
        cardHeight = self.__game.cardHeight()
        cardWidth = self.__game.cardWidth()

        col = 0
        for playerNum in range(len(self.__opponentCardCounts)):
            count = self.__opponentCardCounts[playerNum]
            if (count == -1):
                continue


            self.__game.drawCard(0, col, "blank", self.__screen)

            # highlight the current user
            if (playerNum == self.__currUsersTurn):
                colorPairId = "yellow"
            else:
                colorPairId = None

            self.__screen.write(cardHeight + 2, col + 1, f"Player: {self.__playerUTLNs[playerNum]}", colorPairId=colorPairId)
            self.__screen.write(cardHeight + 3, col + 1, f"Count: {count}", colorPairId=colorPairId)
    
            col += cardWidth + 4

    def __drawCardPile(self):
        centeredRow = (self.__screen.terminalHeight() // 2) - (self.__game.cardHeight() // 2)
        centeredCol = (self.__screen.terminalWidth() // 2) - (self.__game.cardWidth() // 2)

        self.__game.drawCard(centeredRow, centeredCol, self.__topCard, self.__screen)

    def __drawGameInfo(self):
        gameInfo = []
        gameInfo.append(f"Current Player: {self.__playerUTLNs[self.__currUsersTurn]}")

        numRows = self.__screen.terminalHeight()

        printableInfo = "\n".join(gameInfo)
        centeredRow = self.__screen.getCenteredRow(printableInfo)
        self.__screen.write(centeredRow, 0, printableInfo)

    def __drawHand(self):
        numCols = self.__screen.terminalWidth()
        numRows = self.__screen.terminalHeight()

        cardTopRow = numRows - self.__game.cardHeight() - 1

        widthDiff = numCols // len(self.__deck)
        widthDiff = max(6, widthDiff)
        widthDiff = min(self.__game.cardWidth() + 8, widthDiff)

        col = 0
        for i, card in enumerate(self.__deck):
            self.__game.drawCard(cardTopRow, col, card, self.__screen)

            self.__screen.addClickableRegion(cardTopRow, col, self.__game.cardHeight(), self.__game.cardWidth(), i)

            col += widthDiff

    def __drawButtons(self):
        buttons = []
        buttons.append(pyfiglet.figlet_format(f"DRAW", font="finalass"))

        self.__defineAndDrawButtons(buttons)

    def __defineAndDrawButtons(self, buttons):
        startingDrawRow = self.__screen.terminalHeight() - self.__game.cardHeight() - 1
        startingDrawCol = self.__screen.terminalWidth()
        for button in buttons:
            drawButtonHeight = len(button.split("\n"))
            drawButtonWidth = len(button.split("\n")[0])

            drawRow = startingDrawRow - drawButtonHeight
            drawCol = startingDrawCol - (drawButtonWidth + 2)

            border = (("*" * (drawButtonWidth + 2)) + "\n")  * (drawButtonHeight + 1)

            self.__screen.write(drawRow - 1, drawCol - 1, border)
            self.__screen.write(drawRow, drawCol, button)

            self.__screen.addClickableRegion(drawRow - 1, drawCol - 1, drawButtonHeight + 2, drawButtonWidth + 2, "dealCard")

            startingDrawRow = drawRow
