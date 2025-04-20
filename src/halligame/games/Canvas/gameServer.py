# GameServer.py

import threading

from halligame.utils.gameServerTemplate import ServerSuper
from halligame.utils.gameState import GameState
from halligame.utils.ServerComms import ServerCommunicate


class Server(ServerSuper):
    def __init__(self, comms: ServerCommunicate) -> None:
        self.__comms = comms

        self.__stateLock = threading.Lock()

        self.__boardHeight = 30
        self.__boardWidth = 40

        self.__state: GameState = GameState()

        board = [
            ["white" for i in range(self.__boardWidth)]
            for i in range(self.__boardHeight)
        ]
        self.__state.setValue("board", board)

    def gotClientMessage(self, ClientPid, message):
        (row, col, color) = message

        with self.__stateLock:
            self.__state.getValue("board")[row][col] = color

            self.__comms.broadcastMessage(("state_diff", message))

    def addClient(self, clientPid, username):
        with self.__stateLock:
            self.__comms.confirmJoin(clientPid, self.__state.serialize())
