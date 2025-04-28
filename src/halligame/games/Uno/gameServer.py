"""Our Uno game server.

Written by:  Will Cordray, 2024-04-19
Last edited: Will Cordray, 2025-04-28
"""

import threading
from typing import Any

from term import Pid

from halligame.utils.gameServerTemplate import ServerSuper
from halligame.utils.ServerComms import ServerCommunicate

from .Uno import Card, Uno


class Server(ServerSuper):
    """Represents our game's server."""

    def __init__(self, comms: ServerCommunicate) -> None:
        """Initialize this instance."""
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

    def gotClientMessage(self, clientPid: Pid, message: Any) -> None:
        """Process messages from clients."""
        with self.__stateLock:
            messageType = message[0]
            playerNum = message[1]
            if messageType == "uno":
                # one of the users clicked the uno button
                if playerNum != self.__unoPlayerNum and self.__unoPlayerNum is not None:  # someone else won ;)
                    self.__comms.sendClientMessage(
                        self.__playerClientPids[self.__unoPlayerNum],
                        ("uno_loss",),
                    )
                    for _ in range(2):
                        self.__dealPlayerCard(self.__unoPlayerNum)

                self.__unoPlayerNum = None

                # removes the uno button
                self.__comms.broadcastMessage(self.serializeState())
            elif playerNum == self.__currUsersTurn:
                self.__gameStarted = True

                if messageType == "dealCard":
                    # player requested a card
                    self.__dealPlayerCard(playerNum)
                elif messageType == "placeCard":
                    # player placed a card
                    card = message[2]
                    newDeck = message[3]

                    self.__unoPlayerNum = None  # uno chance is gone

                    self.__clientDecks[playerNum] = newDeck
                    self.__evaluatePlaceCard(playerNum, card)

    def __evaluatePlaceCard(self, playerNum: int, card: Card) -> None:
        """Evaluate placing a card.

        Includes advancing the current player, as well as handling skips and
        +2/+4s among others.
        """
        self.__game.placeCard(card)
        self.__userCardCounts[playerNum] -= 1  # take away a card

        # decide the next player
        if self.__game.type(card) == "skip":
            self.__currUsersTurn = self.__nextPlayer()
        elif self.__game.type(card) == "reverse":
            self.__turnChangeDelta *= -1
        elif self.__game.type(card) == "+2":
            for _ in range(2):
                self.__dealPlayerCard(self.__nextPlayer())
            self.__currUsersTurn = self.__nextPlayer()
        elif self.__game.type(card) == "+4":
            for _ in range(4):
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

    def __nextPlayer(self) -> int:
        """Returns the ID of the player who should go next."""
        return (
            self.__currUsersTurn + self.__turnChangeDelta
        ) % self.__numJoined

    def __dealPlayerCard(self, playerNum: int) -> None:
        """Deal the given player a card."""
        newCard = self.__game.dealCard()
        self.__comms.sendClientMessage(
            self.__playerClientPids[playerNum], ("newCard", newCard)
        )
        self.__clientDecks[playerNum].append(newCard)

        self.__userCardCounts[playerNum] += 1  # add 1 to the user cart counts

        self.__comms.broadcastMessage(self.serializeState())

    def addClient(self, clientPid: Pid, username: str) -> None:
        """Add a new client to this game."""
        with self.__stateLock:
            self.__numOnline += 1
            if username not in self.__playerUTLNs and (
                self.__numJoined >= 10 or self.__gameStarted
            ):
                message = "GAME STARTED" if self.__gameStarted else "GAME FULL"

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

    def removeClient(self, clientPID: Pid, username: str) -> None:
        """Remove a client from this game."""
        self.__numOnline -= 1
        if self.__gameOver and self.__numOnline == 0:
            self.__comms.shutdown()  # the game is over, so shut down

    def serializeState(self) -> tuple[str,]:
        """Serialize our game state to send to clients."""
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
