# GameServer.py

# Game logic and turn validation for Tic Tic Toe
# Written by Cole Glotfelty and Will Cordray <2025-03-29>
# Last edited by: Cole Glotfelty 2025-03-29

from typing import Any

from term import Pid

from halligame.utils.gameServerTemplate import ServerSuper
from halligame.utils.gameState import GameState
from halligame.utils.ServerComms import ServerCommunicate


class Server(ServerSuper):
    def __init__(self, comms: ServerCommunicate) -> None:
        self.__comms = comms
        self.__numConnected = 0

        # "Public/Client Facing Members - For GameClient.py"
        self.__state: GameState = GameState()
        self.__state.setValue(
            "board", [[" " for _ in range(3)] for _ in range(3)]
        )
        self.__state.setValue("currentPlayer", 0)
        self.__state.setValue("gameOver", "")

        self.__playersSymbol = ["X", "O"]
        self.__state.setValue("playerNames", ["nobody", "nobody"])

        # print(f"GameState objects: {self.__state.objects}")
        # "Private" Members for internal use only
        self.__boardFull = 0

    # This is the function that is called when the server receives a message
    # from one of the clients, most likely an event/move. Note that I haven't
    # quite figured out the erlang side of things to determine which player
    # sent the message, but that is obviously coming.
    # TODO: still right?
    def gotClientMessage(self, clientPID: Pid, event: Any) -> None:
        """
        Determine if an event (tuple[int, Any]) is valid

        If valid: broadcast new state to clients
        otherwise: tell client who requested change it's not valid
        """
        (_, move) = event

        if self.__state.getValue("board")[move // 3][move % 3] not in [
            "X",
            "O",
        ]:
            self.__updateState(event)
            self.__comms.broadcastState(self.__state)
        else:
            self.__comms.sendClientMessage(
                clientPID, ("Error: Invalid Move", self.__state.serialize())
            )

    def __updateState(self, event: tuple[int, Any] | Any) -> None:
        """
        event should be of tuple[int, Any], but the superclass allows Any
        """
        (currentPlayer, move) = event

        playerSymbol = self.__playersSymbol[currentPlayer]

        self.__state.getValue("board")[move // 3][move % 3] = playerSymbol

        self.__boardFull += 1

        # Will be overwritten below if the game is won on the last turn
        if self.__boardFull == 9:
            self.__state.setValue("gameOver", "Draw")

        for row in self.__state.getValue("board"):
            if all(elem == self.__playersSymbol[currentPlayer] for elem in row):
                self.__state.setValue(
                    "gameOver", f"Player {playerSymbol} wins!"
                )

        for i in range(3):
            if all(
                row[i] == self.__playersSymbol[currentPlayer]
                for row in self.__state.getValue("board")
            ):
                self.__state.setValue(
                    "gameOver", f"Player {playerSymbol} wins!"
                )

        wincon = [0, 0]
        for i, j in [(0, 0), (1, 1), (2, 2)]:
            if self.__state.getValue("board")[i][j] == playerSymbol:
                wincon[0] += 1
            if self.__state.getValue("board")[i][3 - j - 1] == playerSymbol:
                wincon[1] += 1

        if 3 in wincon:
            self.__state.setValue("gameOver", f"Player {playerSymbol} wins!")

        wincon = [0, 0]
        for i, j in [(0, 2), (1, 1), (2, 0)]:
            if self.__state.getValue("board")[i][j] == playerSymbol:
                wincon[0] += 1
            if self.__state.getValue("board")[i][3 - j - 1] == playerSymbol:
                wincon[1] += 1

        if 3 in wincon:
            self.__state.setValue("gameOver", f"Player {playerSymbol} wins!")

        self.__state.setValue("currentPlayer", (currentPlayer + 1) % 2)

    def addClient(self, clientPid: Pid, username: str) -> None:
        for i in range(2):
            if self.__state.getValue("playerNames")[i] == "nobody":
                self.__numConnected += 1
                self.__state.getValue("playerNames")[i] = username
                self.__comms.confirmJoin(
                    clientPid, username, (i, self.__state.serialize())
                )
                self.__comms.broadcastState(self.__state)
                break
        else:
            self.__comms.sendClientMessage(
                clientPid, ("Error: Too Many Players", self.__state.serialize())
            )

    def removeClient(self, clientPID: Pid, username: str) -> None:
        # print(f"removing client {clientPID} ({username}); "
        #   f"numConnected = {self.__numConnected}")
        # Stop when room is empty
        self.__numConnected -= 1
        if self.__numConnected == 0:
            self.__comms.shutdown()
