"""A Game Client superclass, to be subclassed by each game.

Written by Cole Glotfelty <2025-04-14>
Last edited by Michael Daniels, 2025-04-28
"""

from typing import Any


class ClientSuper:
    """Subclasses represent the game's client."""

    def __init__(self, comms) -> None:  # noqa: ANN001
        """Constructor for the Client (you should initalize stuff here).

        One thing to note: `comms` needs to be assigned like so:

            self.__comms = comms

        We also have assorted utilities in halligame.utils. Here we're using
        the GameState class which also must be used for storing the state of
        your game. (We do this for serialization and server communication)

        Args:
            comms: halligame.utils.ClientCommunicate
                   (It can't be type-annotated due to circular imports.)
        """
        pass

    def updateState(self, state: bytes) -> None:
        """Callback function triggered by `broadcastState`.

        This is where you should modify the TUI screen for the user.
        The state should then be copied to the local state from the message
        sent by the server.
        """
        pass

    def gotServerMessage(self, msg: Any) -> None:
        """Callback function for when a message is received from the server.

        One could deconstruct the msg like below and then discriminate based on
        the status or message text.
        """
        pass

    def joinConfirmed(self, msg: Any) -> None:
        """Callback function for when the player joins the server.

        This message can be set in GameServer::addClient().
        """
        pass
