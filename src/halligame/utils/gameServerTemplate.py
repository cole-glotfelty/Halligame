"""A Game Server superclass, to be subclassed by each game.

Written by Cole Glotfelty <2025-04-14>
Last edited by Michael Daniels, 2025-04-28
"""

from typing import Any

from term import Pid

from halligame.utils.ServerComms import ServerCommunicate


class ServerSuper:
    """Subclasses represent the game's server."""

    def __init__(self, comms: ServerCommunicate) -> None:
        """Constructor for the Server (you should initalize stuff here).

        One thing to note: `comms` needs to be assigned like so:

            self.__comms = comms

        We also have assorted utilities in halligame.utils. Here we're using
        the GameState class which also must be used for storing the state of
        your game. (We do this for serialization and server communication)
        """
        self.__comms = comms

    def gotClientMessage(self, clientPID: Pid, message: Any) -> None:
        """This is where your game logic should go.

        You'll receive a message from the client and should validate it here.
        (You'll probably want to use a case statement and pattern match the
        events).
        """
        pass

    def addClient(self, clientPID: Pid, username: str) -> None:
        """Callback function for when a client joins the game.

        This should also call `confirmJoin` to send a message to the client
        about a new player joining.
        """
        self.__comms.confirmJoin(clientPID, username, "")
        pass

    def removeClient(self, clientPID: Pid, username: str) -> None:
        """Callback function for when a client leaves the game."""
        pass
