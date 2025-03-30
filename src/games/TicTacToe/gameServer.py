# GameServer.py

# Game logic and turn validation for Tic Tic Toe
# Written by Cole Glotfelty and Will Cordray <2025-03-29>
# Last edited by: Cole Glotfelty 2025-03-29

from ...utils.gameState import GameState
from typing import Any

class Server:
    # comms is the function to call when you want to send a message to the server
    def __init__(self, comms: callable) -> None:
        self.__comms = comms

        self.__usersConnected = 0

        # "Public/Client Facing Members - For GameClient.py"
        self.__state = GameState()
        self.__state.objects["board"] = [[3 * y + x + 1 for x in range(3)] for y in range(3)]
        self.__state.objects["currentPlayer"] = 0
        self.__state.objects["gameOver"] = ""

        # "Private" Members for internal use only
        self.__boardFull = 0

    def play(self) -> None:
        """
        Starts the game server's game loop
        """
        while self.__state.objects["gameOver"] == "":
            # TODO: Draw screen here or tell client to draw
            # self.drawScreen()
            event = (
                self.__state.objects["currentPlayer"],
                # TODO: we need a way to get the move from the player, that goes here
                # self.__players[self.__currentPlayer].takeTurn(),
            )
            if self.eventIsValid(event):
                self.eventUpdate(event)

            self.__currentPlayer = (self.__currentPlayer + 1) % 2

    # This is the function that is called when the server receives a message 
    # from one of the clients, most likely an event/move. Note that I haven't 
    # quite figured out the erlang side of things to determine which player 
    # sent the message, but that is obviously coming.
    def eventIsValid(self, event: tuple[int, Any], clientPID) -> None:
        """
        Determine if an event is valid

        If valid: broadcast new state to clients
        otherwise: tell client who requested change it's not valid
        """
        (_, move) = event

        if self.board[move // 3][move % 3] not in ["X", "O"]:
            self.__updateState(event)
            self.__comms(("broadcastState", self.__state.serialize()))
        else:
            self.__comms(("reply", (clientPID, "Error: Invalid Move")))

    def __updateState(self, event: tuple[int, Any]) -> None:
        (currentPlayer, move) = event

        playerSymbol = "X" if currentPlayer else "O"

        self.board[move // 3][move % 3] = playerSymbol

        self.__boardFull += 1

        # Will be overwritten below if the game is won on the last turn
        if self.__boardFull == 9:
            self.__state.objects["gameOver"] = "Draw"

        for row in self.board:
            if all(elem == self.__playersSymbol[currentPlayer] for elem in row):
                self.__state.objects["gameOver"] = f"Player {currentPlayer} wins!"

        for i in range(3):
            if all(row[i] == self.__playersSymbol[currentPlayer] for row in self.board):
                self.__state.objects["gameOver"] = f"Player {currentPlayer} wins!"


        wincon = [0, 0]
        for i in range(3):
            for j in range(3):
                if (self.board[i][j] == playerSymbol):
                    wincon[0] += 1
                if (self.board[i][3 - j - 1] == playerSymbol):
                    wincon[1] += 1

        if 3 in wincon:
            self.__state.objects["gameOver"] = f"Player {currentPlayer} wins!"


    def addUser(self, clientPID):
        print(type(clientPID))
        self.__usersConnected += 1
        if (self.__usersConnect >= 2):
            self.__comms(("reply", (clientPID, "Error: Too Many Players")))

    def removeUser(self, clientPID):
        pass


    def otherMessageType(self, clientPID, msg):
        pass
