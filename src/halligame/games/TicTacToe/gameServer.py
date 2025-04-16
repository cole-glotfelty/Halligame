# GameServer.py

# Game logic and turn validation for Tic Tic Toe
# Written by Cole Glotfelty and Will Cordray <2025-03-29>
# Last edited by: Cole Glotfelty 2025-03-29

from halligame.utils.ServerComms import ServerCommunicate
from halligame.utils.gameState import GameState
from halligame.utils.gameServerTemplate import ServerSuper
from typing import Any

class Server(ServerSuper):
    def __init__(self, comms: ServerCommunicate) -> None:
        self.__comms = comms

        self.__usersConnected = 0

        # "Public/Client Facing Members - For GameClient.py"
        self.__state : GameState = GameState()
        self.__state.setValue("board", [[" " for x in range(3)] for y in range(3)])
        self.__state.setValue("currentPlayer", 0)
        self.__state.setValue("gameOver", "")

        self.__playersSymbol = ["X", "O"]

        # print(f"GameState objects: {self.__state.objects}")
        # "Private" Members for internal use only
        self.__boardFull = 0

    # This is the function that is called when the server receives a message 
    # from one of the clients, most likely an event/move. Note that I haven't 
    # quite figured out the erlang side of things to determine which player 
    # sent the message, but that is obviously coming.
    def gotClientMessage(self, clientPID, event: tuple[int, Any]) -> None:
        """
        Determine if an event is valid

        If valid: broadcast new state to clients
        otherwise: tell client who requested change it's not valid
        """
        (_, move) = event

        if self.__state.getValue("board")[move // 3][move % 3] not in ["X", "O"]:
            self.__updateState(event)
            self.__comms.broadcastState(self.__state)
        else:
            self.__comms.sendClientMessage(clientPID, ("Error: Invalid Move", self.__state.serialize()))

    def __updateState(self, event: tuple[int, Any]) -> None:
        (currentPlayer, move) = event

        playerSymbol = self.__playersSymbol[currentPlayer]

        self.__state.getValue("board")[move // 3][move % 3] = playerSymbol

        self.__boardFull += 1

        # Will be overwritten below if the game is won on the last turn
        if self.__boardFull == 9:
            self.__state.setValue("gameOver", "Draw")

        for row in self.__state.getValue("board"):
            if all(elem == self.__playersSymbol[currentPlayer] for elem in row):
                self.__state.setValue("gameOver", f"Player {playerSymbol} wins!")

        for i in range(3):
            if all(row[i] == self.__playersSymbol[currentPlayer] for row in self.__state.getValue("board")):
                self.__state.setValue("gameOver", f"Player {playerSymbol} wins!")

        wincon = [0, 0]
        for (i, j) in [(0, 0), (1, 1), (2, 2)]:
                if (self.__state.getValue("board")[i][j] == playerSymbol):
                    wincon[0] += 1
                if (self.__state.getValue("board")[i][3 - j - 1] == playerSymbol):
                    wincon[1] += 1

        if 3 in wincon:
            self.__state.setValue("gameOver", f"Player {playerSymbol} wins!")

        wincon = [0, 0]
        for (i, j) in [(0, 2), (1, 1), (2, 0)]:
                if (self.__state.getValue("board")[i][j] == playerSymbol):
                    wincon[0] += 1
                if (self.__state.getValue("board")[i][3 - j - 1] == playerSymbol):
                    wincon[1] += 1

        if 3 in wincon:
            self.__state.setValue("gameOver", f"Player {playerSymbol} wins!")
        
        self.__state.setValue("currentPlayer", (currentPlayer + 1) % 2)


    def addClient(self, clientPid):
        self.__usersConnected += 1
        if (self.__usersConnected > 2):
            self.__comms.sendClientMessage(clientPid, ("Error: Too Many Players", self.__state.serialize()))
        else:
            playerId = 0 if self.__usersConnected == 1 else 1
            self.__comms.confirmJoin(clientPid, (playerId, self.__state.serialize()))
