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

        self.__numConnected = 0
        self.__clientPidsToClientIds = {}
        self.__clientIdsToClientPids = {}
        self.__userCardCounts = []

        self.__currUsersTurn = 0

        self.__turnChangeDelta = 1 # deal with reverse

    def gotClientMessage(self, ClientPid, message):
        with self.__stateLock:
            if (message[0] == "dealCard"):
                if (message[1] == self.__currUsersTurn):
                    self.__comms.sendClientMessage(ClientPid, ("newCard", self.__game.dealCard()))
                    self.__userCardCounts[self.__clientPidsToClientIds[ClientPid]][1] += 1 # add 1 to the user cart counts
                    self.__comms.broadcastMessage(self.serializeState())
            elif (message[0] == "placeCard"):
                (userId, card) = message[1]
                if (self.__currUsersTurn == userId):
                    self.__evaluatePlaceCard(userId, card)

    def __evaluatePlaceCard(self, userId, card):
        self.__game.placeCard(card)

        self.__userCardCounts[userId][1] -= 1 # take away a card

        if (self.__game.type(card) == "skip"):
            playerChange = 2
        else:
            playerChange = 1

        if self.__game.type(card) == "reverse":
            self.__turnChangeDelta *= -1

        playerChange *= self.__turnChangeDelta

        self.__currUsersTurn = (self.__currUsersTurn + playerChange) % self.__numConnected

        nextTurnPlayersClientId = self.__clientIdsToClientPids[self.__currUsersTurn]
        if (self.__game.type(card) == "+2"):
            self.__userCardCounts[self.__currUsersTurn][1] += 2
            for i in range(2):
                self.__comms.sendClientMessage(nextTurnPlayersClientId, ("newCard", self.__game.dealCard()))
        elif (self.__game.type(card) == "+4"):
            self.__userCardCounts[self.__currUsersTurn][1] += 4
            for i in range(4):
                self.__comms.sendClientMessage(nextTurnPlayersClientId, ("newCard", self.__game.dealCard()))

        if (self.__userCardCounts[userId][1] == 0):
            self.__comms.broadcastMessage(("Game Over", str(userId)))
        else:
            self.__comms.broadcastMessage(self.serializeState())

    def addClient(self, clientPid, username):
        with self.__stateLock:
            if (self.__numConnected >= 10):
                self.__comms.confirmJoin(clientPid, username, "Game Full")
            else:
                clientDeck = [self.__game.dealCard() for i in range(7)]
                print(clientDeck)
                self.__clientPidsToClientIds[clientPid] = self.__numConnected
                self.__clientIdsToClientPids[self.__numConnected] = clientPid
                self.__userCardCounts.append([self.__numConnected, 7]) # this user has 7 cards
                self.__comms.confirmJoin(clientPid, username, (self.__numConnected, clientDeck))
                self.__comms.broadcastMessage(self.serializeState())

                self.__numConnected += 1

    def serializeState(self):
        return ("state", (self.__game.getTopCard(), self.__userCardCounts, self.__currUsersTurn))
