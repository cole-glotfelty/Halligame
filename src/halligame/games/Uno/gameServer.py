# GameServer.py

from halligame.utils.ServerComms import ServerCommunicate
from halligame.utils.gameState import GameState
from halligame.utils.gameServerTemplate import ServerSuper

import threading
from .Uno import Uno

class Server(ServerSuper):
    def __init__(self, comms: ServerCommunicate) -> None:
        self.__comms = comms
        self.__stateLock = threading.Lock()

        self.__game = Uno()

        self.__numJoined = 0
        self.__playerClientPids = [-1] * 10
        self.__playerUTLNs = [-1] * 10
        self.__userCardCounts = [-1] * 10
        self.__clientDecks = [-1] * 10

        self.__currUsersTurn = 0

        self.__turnChangeDelta = 1 # deal with reverse

    def gotClientMessage(self, ClientPid, message):
        with self.__stateLock:
            messageType = message[0]
            playerNum = message[1]
            if (messageType == "dealCard"):
                if (playerNum == self.__currUsersTurn):
                    
                    self.__dealPlayerCard(playerNum)
            elif (messageType == "placeCard"):
                card = message[2]
                newDeck = message[3]
                if (playerNum == self.__currUsersTurn):
                    self.__clientDecks[playerNum] = newDeck
                    self.__evaluatePlaceCard(playerNum, card)

    def __evaluatePlaceCard(self, playerNum, card):
        self.__game.placeCard(card)

        self.__userCardCounts[playerNum] -= 1 # take away a card

        # decide the next player
        if (self.__game.type(card) in ["skip", "+2", "+4"]):
            playerChange = 2
        else:
            playerChange = 1

        if self.__game.type(card) == "reverse":
            self.__turnChangeDelta *= -1

        playerChange *= self.__turnChangeDelta

        self.__currUsersTurn = (self.__currUsersTurn + playerChange) % self.__numJoined

        # deal with dealing cards for +2 and +4
        if (self.__game.type(card) == "+2"):
            for i in range(2):
                self.__dealPlayerCard(self.__currUsersTurn)
        elif (self.__game.type(card) == "+4"):
            for i in range(4):
                self.__dealPlayerCard(self.__currUsersTurn)

        if (self.__userCardCounts[playerNum] == 0):
            self.__comms.broadcastMessage(self.serializeState())
            self.__comms.broadcastMessage((f"Game Over: {self.__playerUTLNs[playerNum]} Won!"))
        else:
            self.__comms.broadcastMessage(self.serializeState())


    def __dealPlayerCard(self, playerNum):
        newCard = self.__game.dealCard()
        self.__comms.sendClientMessage(self.__playerClientPids[playerNum], 
                                       ("newCard", newCard))
        self.__clientDecks[playerNum].append(newCard)

        self.__userCardCounts[playerNum] += 1 # add 1 to the user cart counts

        self.__comms.broadcastMessage(self.serializeState())

    def addClient(self, clientPid, username):
        with self.__stateLock:
            if (self.__numJoined >= 10):
                self.__comms.confirmJoin(clientPid, username, "Game Full")
                self.__comms.sendClientMessage(clientPid, self.serializeState())
            else:
                if (username not in self.__playerUTLNs or username == "wcordr01"):
                    playerNum = self.__numJoined
                    self.__playerClientPids[playerNum] = clientPid
                    self.__playerUTLNs[playerNum] = username

                    self.__clientDecks[playerNum] = [self.__game.dealCard() for i in range(7)]

                    self.__userCardCounts[playerNum] = 7 # this user has 7 cards
                    self.__comms.confirmJoin(clientPid, username, (playerNum, self.__clientDecks[playerNum]))
                    self.__comms.broadcastMessage(self.serializeState())

                    self.__numJoined += 1
                else:
                    playerNum = self.__playerUTLNs.index(username)
                    self.__playerClientPids[playerNum] = clientPid

                    self.__comms.confirmJoin(clientPid, username, (playerNum, self.__clientDecks[playerNum]))
                    self.__comms.sendClientMessage(clientPid, self.serializeState())
                    # self.__comms.broadcastMessage(self.serializeState())

    def serializeState(self):
        return ("state", (self.__game.getTopCard(), self.__userCardCounts, self.__playerUTLNs, self.__currUsersTurn))
