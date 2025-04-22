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
        self.__numOnline = 0
        self.__playerClientPids = [-1] * 10
        self.__playerUTLNs = [-1] * 10
        self.__userCardCounts = [-1] * 10
        self.__clientDecks = [-1] * 10

        self.__currUsersTurn = 0
        self.__gameStarted = False
        self.__gameOver = False

        # used when there is an uno race
        self.__unoPlayerNum = None

        self.__turnChangeDelta = 1  # deal with reverse

    def gotClientMessage(self, ClientPid, message):
        with self.__stateLock:
            messageType = message[0]
            playerNum = message[1]
            if messageType == "uno":
                if playerNum != self.__unoPlayerNum:  # someone else won ;)
                    self.__comms.sendClientMessage(
                        self.__playerClientPids[self.__unoPlayerNum],
                        ("uno_loss",),
                    )
                    for i in range(2):
                        self.__dealPlayerCard(self.__unoPlayerNum)

                self.__unoPlayerNum = None

                # removes the uno button
                self.__comms.broadcastMessage(self.serializeState())
            elif playerNum == self.__currUsersTurn:
                self.__gameStarted = True

                if messageType == "dealCard":
                    self.__dealPlayerCard(playerNum)
                elif messageType == "placeCard":
                    card = message[2]
                    newDeck = message[3]

                    self.__unoPlayerNum = None  # uno chance is gone

                    self.__clientDecks[playerNum] = newDeck
                    self.__evaluatePlaceCard(playerNum, card)

    def __evaluatePlaceCard(self, playerNum, card):
        self.__game.placeCard(card)
        self.__userCardCounts[playerNum] -= 1  # take away a card

        # decide the next player
        if self.__game.type(card) == "skip":
            self.__currUsersTurn = self.__nextPlayer()
        elif self.__game.type(card) == "reverse":
            self.__turnChangeDelta *= -1
        elif self.__game.type(card) == "+2":
            for i in range(2):
                self.__dealPlayerCard(self.__nextPlayer())
            self.__currUsersTurn = self.__nextPlayer()
        elif self.__game.type(card) == "+4":
            for i in range(4):
                self.__dealPlayerCard(self.__nextPlayer())
            self.__currUsersTurn = self.__nextPlayer()

        self.__currUsersTurn = self.__nextPlayer()

        if self.__userCardCounts[playerNum] == 0:
            self.__comms.broadcastMessage(
                (
                    "Game Over",
                    self.__playerUTLNs[playerNum],
                    self.serializeState(),
                )
            )
            self.__gameOver = True
        elif self.__userCardCounts[playerNum] == 1:  # UNO
            self.__unoPlayerNum = playerNum
            self.__comms.broadcastMessage(self.serializeState())
        else:
            self.__comms.broadcastMessage(self.serializeState())

    def __nextPlayer(self):
        return (
            self.__currUsersTurn + self.__turnChangeDelta
        ) % self.__numJoined

    def __dealPlayerCard(self, playerNum):
        newCard = self.__game.dealCard()
        self.__comms.sendClientMessage(
            self.__playerClientPids[playerNum], ("newCard", newCard)
        )
        self.__clientDecks[playerNum].append(newCard)

        self.__userCardCounts[playerNum] += 1  # add 1 to the user cart counts

        self.__comms.broadcastMessage(self.serializeState())

    def addClient(self, clientPid, username):
        with self.__stateLock:
            self.__numOnline += 1
            if username not in self.__playerUTLNs and (
                self.__numJoined >= 10 or self.__gameStarted
            ):
                if self.__gameStarted:
                    message = "GAME STARTED"
                else:
                    message = "GAME FULL"

                self.__comms.confirmJoin(clientPid, username, message)
                self.__comms.sendClientMessage(clientPid, self.serializeState())
            else:
                if username not in self.__playerUTLNs:
                    playerNum = self.__numJoined
                    self.__playerClientPids[playerNum] = clientPid
                    self.__playerUTLNs[playerNum] = username

                    self.__clientDecks[playerNum] = [
                        self.__game.dealCard() for i in range(7)
                    ]

                    self.__userCardCounts[playerNum] = (
                        7  # this user has 7 cards
                    )
                    self.__comms.confirmJoin(
                        clientPid,
                        username,
                        (playerNum, self.__clientDecks[playerNum]),
                    )
                    self.__comms.broadcastMessage(self.serializeState())

                    self.__numJoined += 1
                else:
                    playerNum = self.__playerUTLNs.index(username)
                    self.__playerClientPids[playerNum] = clientPid

                    self.__comms.confirmJoin(
                        clientPid,
                        username,
                        (playerNum, self.__clientDecks[playerNum]),
                    )
                    self.__comms.sendClientMessage(
                        clientPid, self.serializeState()
                    )
                    # self.__comms.broadcastMessage(self.serializeState())

    def removeClient(self, clientPID, username: str):
        self.__numOnline -= 1
        if self.__gameOver and self.__numOnline == 0:
            self.__comms.shutdown()  # the game is over, so shut down

    def serializeState(self):
        return (
            "state",
            (
                self.__game.getTopCard(),
                self.__userCardCounts,
                self.__playerUTLNs,
                self.__currUsersTurn,
                self.__unoPlayerNum,
            ),
        )
