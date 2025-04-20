# Example Game Server 
# Written by Cole Glotfelty <2025-04-14>

# A barebones game client explaining what needs to be implemented and what's
# available for use when creating halligame games. This also includes some 
# typical patterns 

from halligame.utils.ServerComms import ServerCommunicate


class ServerSuper:
    def __init__(self, comms: ServerCommunicate) -> None:
        self.__comms = comms

    def gotClientMessage(self, event, clientPID) -> None:
        pass

    def addClient(self, clientPID, username) -> None:
        self.__comms.confirmJoin(clientPID, "")
        pass

    def removeClient(self, clientPID, username) -> None:
        pass
