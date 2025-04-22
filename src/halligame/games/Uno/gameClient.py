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
        self.__gameOver = False
        self.__waitingCard = None

        self.__unoPossibility = False

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
            if mouseEventType == "left_click":
                if (self.__gameOver):
                    self.__screen.displayFullScreenMessage("GAME OVER", font="roman")
                    time.sleep(1.5)
                    self.__drawScreen()
                elif region == "uno":
                    self.__comms.sendMessage(("uno", self.__playerNum))
                elif (not self.__myTurn):
                    self.__screen.displayFullScreenMessage("NOT YOUR\nTURN", font="roman")
                    time.sleep(1.5)
                    self.__drawScreen()
                elif type(region) == int:
                    # need to pick a card before it's a valid choice
                    if (not self.__game.cardPlacable(self.__topCard, self.__deck[region])):
                        self.__screen.displayFullScreenMessage("INVALID CARD", font="roman")
                        time.sleep(1.5)
                        self.__drawScreen()
                    elif (self.__game.type(self.__deck[region]) in ["wild", "+4"]):
                        self.__waitingCard = self.__deck.pop(region)
                        self.colorPicker()
                    else:
                        card = self.__deck.pop(region)

                        self.__myTurn = False

                        # do client insta update
                        self.__topCard = card
                        self.__unoPossibility = len(self.__deck) == 1
                        self.__drawScreen()

                        self.__comms.sendMessage(("placeCard", self.__playerNum, card, self.__deck))

                        # prevent screen refreshes to give them a chance to 
                        # click uno
                        if (self.__unoPossibility):
                            time.sleep(3)
                elif region == "dealCard":
                    self.__comms.sendMessage(("dealCard", self.__playerNum))
                elif (region in ["red", "yellow", "green", "blue"]): # respond to color picker
                    if (self.__waitingCard != None):
                        card = self.__game.setColor(self.__waitingCard, region)

                        self.__myTurn = False
                        self.__unoPossibility = len(self.__deck) == 1
                        self.__drawScreen()

                        self.__comms.sendMessage(("placeCard", self.__playerNum, card, self.__deck))

                        self.__waitingCard = None

                        # do insta update
                        self.__topCard = card
                        self.__drawScreen()

                        # prevent screen refreshes to give them a chance to 
                        # click uno
                        if (self.__unoPossibility):
                            time.sleep(3)

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
            if (joinMsg == "GAME FULL" or joinMsg == "GAME STARTED"):
                self.__screen.displayFullScreenMessage(joinMsg + "\n\nJOINING AS VIEWER", font="roman")
                time.sleep(3)
                # joining as viewer
                self.__playerNum = -1
                self.__deck = []
            else:
                (self.__playerNum, self.__deck) = joinMsg

    def gotServerMessage(self, msg):
        with self.__stateLock:
            if (msg[0] == "Game Over"):
                self.__gameOver = True
                self.__winner = msg[1]

                self.__screen.displayFullScreenMessage(f"Game Over\n{self.__winner} Won!", font="roman")
                time.sleep(3)
                self.__updateState(msg[2][1])
            elif (msg[0] == "state"):
                self.__updateState(msg[1])
            elif (msg[0] == "newCard"):
                self.__deck.append(msg[1])
                self.__drawScreen()
            elif (msg[0] == "uno_loss"): # you lost the uno race
                self.__screen.displayFullScreenMessage(f"YOU DIDN'T\nSAY UNO!", font="roman")
                time.sleep(1.5)
                self.__drawScreen()

    def __updateState(self, state):
        # unpack state
        (self.__topCard, self.__opponentCardCounts, self.__playerUTLNs, self.__currUsersTurn, unoPlayerNum) = state

        if (unoPlayerNum != None):
            self.__unoPossibility = True
        else:
            self.__unoPossibility = False

        if (not self.__gameOver):
            # just became your turn
            if (not self.__myTurn and self.__currUsersTurn == self.__playerNum):
                self.__screen.displayFullScreenMessage("YOUR TURN", font="roman")
                time.sleep(0.5)

            self.__myTurn = self.__currUsersTurn == self.__playerNum

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

        col = 1
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
    
            col += cardWidth + 2

    def __drawCardPile(self):
        centeredRow = (self.__screen.terminalHeight() // 2) - (self.__game.cardHeight() // 2)
        centeredCol = (self.__screen.terminalWidth() // 2) - (self.__game.cardWidth() // 2)

        self.__game.drawCard(centeredRow, centeredCol, self.__topCard, self.__screen)

    def __drawGameInfo(self):
        gameInfo = []
        if (self.__gameOver):
            gameInfo.append(f"Game Over: {self.__winner} Won!")
        else:
            gameInfo.append(f"Current Player: {self.__playerUTLNs[self.__currUsersTurn]}")
            if (self.__myTurn):
                gameInfo.append(f"It's Your Turn")

        printableInfo = "\n\n".join(gameInfo)
        centeredRow = self.__screen.getCenteredRow(printableInfo)
        self.__screen.write(centeredRow, 3, printableInfo)

    def __drawHand(self):
        if (len(self.__deck) == 0): # nothing to draw
            return

        numCols = self.__screen.terminalWidth()
        numRows = self.__screen.terminalHeight()

        cardTopRow = numRows - self.__game.cardHeight() - 1
        cardCol = 1

        widthDiff = numCols // len(self.__deck)
        widthDiff = min(self.__game.cardWidth() + 8, widthDiff)

        # Make sure within bound
        while widthDiff  * (len(self.__deck) - 1) + self.__game.cardWidth() + cardCol >= numCols:
            widthDiff -= 1

        widthDiff = max(3, widthDiff)

        for i, card in enumerate(self.__deck):
            self.__game.drawCard(cardTopRow, cardCol, card, self.__screen)

            self.__screen.addClickableRegion(cardTopRow, cardCol, self.__game.cardHeight(), self.__game.cardWidth(), i)

            cardCol += widthDiff

    def __drawButtons(self):
        if (self.__playerNum == -1): # viewer
            return

        buttons = []
        buttons.append((pyfiglet.figlet_format(f"DRAW", font="finalass"), "dealCard"))
        # if (self.__unoPossibility):
            # buttons.append((pyfiglet.figlet_format(f"UNO", font="finalass"), "uno"))

        self.__defineAndDrawButtons(buttons)

    # format of buttons is [(messageToDisplay, regionId), ...]
    def __defineAndDrawButtons(self, buttons):
        startingDrawRow = self.__screen.terminalHeight() - self.__game.cardHeight() - 1
        startingDrawCol = self.__screen.terminalWidth()
        for button, regionId in buttons:
            drawButtonHeight = len(button.split("\n"))
            drawButtonWidth = len(button.split("\n")[0])

            drawRow = startingDrawRow - drawButtonHeight
            drawCol = startingDrawCol - (drawButtonWidth + 2)

            border = (("*" * (drawButtonWidth + 2)) + "\n")  * (drawButtonHeight + 1)

            self.__screen.write(drawRow - 1, drawCol - 1, border)
            self.__screen.write(drawRow, drawCol, button)

            self.__screen.addClickableRegion(drawRow - 1, drawCol - 1, drawButtonHeight + 2, drawButtonWidth + 2, regionId)

            startingDrawRow = drawRow - 3
