"""Our Canvas server.

Created:     Will Cordray,    2025-04-15
Last edited: Michael Daniels, 2025-04-22
"""

from threading import Lock
from typing import Any

from term import Pid

from halligame.utils.gameServerTemplate import ServerSuper
from halligame.utils.gameState import GameState
from halligame.utils.ServerComms import ServerCommunicate


class Server(ServerSuper):
    """Represents our game's server."""

    def __init__(self, comms: ServerCommunicate) -> None:
        """Initialize this instance."""
        #: Our ServerCommunicate instance.
        self.__comms: ServerCommunicate = comms
        #: A set of usernames who are currently connected.
        self.__players: set[str] = set()
        #: Protects self.__state.
        self.__stateLock: Lock = Lock()
        #: The game state
        self.__state: GameState = GameState()
        #: The height of the canvas, in pixels.
        self.__boardHeight: int = 30
        #: The width of the canvas, in pixels.
        self.__boardWidth: int = 40

        board = [
            ["white" for _ in range(self.__boardWidth)]
            for _ in range(self.__boardHeight)
        ]
        self.__state.setValue("board", board)

    def gotClientMessage(self, clientPid: Pid, message: Any) -> None:
        """Process messages from clients.

        The message should be of the form (row, col, color), where row and col
        are ints and color is a string containing one of the 8 primary screen 
        colors
        """
        (row, col, color) = message

        with self.__stateLock:
            self.__state.getValue("board")[row][col] = color

            self.__comms.broadcastMessage(("state_diff", message))

    def addClient(self, clientPid: Pid, username: str) -> None:
        """Add a new client to this game."""
        with self.__stateLock:
            self.__comms.confirmJoin(
                clientPid, username, self.__state.serialize()
            )
