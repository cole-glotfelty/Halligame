# Example Game Server
# Written by Cole Glotfelty <2025-04-14>

# A barebones game client explaining what needs to be implemented and what's
# available for use when creating halligame games. This also includes some
# typical patterns
from typing import Any

from term import Pid

from halligame.utils.ServerComms import ServerCommunicate


class ServerSuper:
    def __init__(self, comms: ServerCommunicate) -> None:
        self.__comms = comms

    def gotClientMessage(
        self, clientPID: Pid, username: str, message: Any
    ) -> None:
        pass

    def addClient(self, clientPID: Pid, username: str) -> None:
        self.__comms.confirmJoin(clientPID, "")
        pass

    def removeClient(self, clientPID: Pid, username: str) -> None:
        pass
