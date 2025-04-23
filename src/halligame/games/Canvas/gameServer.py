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
        self.__comms: ServerCommunicate = comms
        """Our ServerCommunicate instance."""
        self.__players: set[str] = set()
        """A set of usernames who are currently connected."""
        self.__stateLock: Lock = Lock()
        """Protects self.__state."""
        self.__state: GameState = GameState()
        self.__boardHeight: int = 30
        self.__boardWidth: int = 40

        board = [
            ["white" for _ in range(self.__boardWidth)]
            for _ in range(self.__boardHeight)
        ]
        self.__state.setValue("board", board)

    def gotClientMessage(self, clientPid: Pid, message: Any) -> None:
        """Process messages from clients.

        The message should be of the form (row, col, color), where row and col
        are ints and color is a...
        TODO: complete the above sentence :)
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

            self.__players.add(username)
            self.__comms.broadcastMessage(("players", list(self.__players)))

    def removeClient(self, clientPID: Pid, username: str) -> None:
        """Remove a client from this game."""
        self.__players.discard(username)

        self.__comms.broadcastMessage(("players", list(self.__players)))
