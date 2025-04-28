"""Example Game Server.

A barebones game server explaining what needs to be implemented and what's
available for use when creating halligame games. This also includes some
typical patterns.

Created:     Cole Glotfelty,  2025-04-14
Last edited: Michael Daniels, 2025-04-28
"""

from typing import Any

from term import Pid

from halligame.utils.gameServerTemplate import ServerSuper
from halligame.utils.gameState import GameState
from halligame.utils.ServerComms import ServerCommunicate


class Server(ServerSuper):
    """Represents the game's server."""

    def __init__(self, comms: ServerCommunicate) -> None:
        """Constructor for the Server (you should initalize stuff here).

        One thing to note: `comms` needs to be assigned like so:

            self.__comms = comms

        We also have assorted utilities in halligame.utils. Here we're using
        the GameState class which also must be used for storing the state of
        your game. (We do this for serialization and server communication)
        """
        self.__comms = comms
        self.__usersConnected = 0
        self.__state = GameState()
        pass

    def gotClientMessage(self, clientPID: Pid, message: Any) -> None:
        """This is where your game logic should go.

        You'll receive a message from the client and should validate it here.
        (You'll probably want to use a case statement and pattern match the
        events).
        """
        valid = False  # Set by your logic
        if valid:
            self.__comms.broadcastState(self.__state)
        else:
            self.__comms.sendClientMessage(
                clientPID, ("error", "Error: Invalid Move")
            )
        pass

    def addClient(self, clientPID: Pid, username: str) -> None:
        """Callback function for when a client joins the game.

        This should also call `confirmJoin` to send a message to the client
        about a new player joining.
        """
        self.__usersConnected += 1
        msg = "You've joined!"
        self.__comms.confirmJoin(clientPID, msg)
        pass

    def removeClient(self, clientPID: Pid, username: str) -> None:
        """Callback function for when a client leaves the game."""
        pass
