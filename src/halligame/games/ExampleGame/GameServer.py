# Example Game Server 
# Written by Cole Glotfelty <2025-04-14>

# A barebones game client explaining what needs to be implemented and what's
# available for use when creating halligame games. This also includes some 
# typical patterns 

from halligame.utils.gameServerTemplate import ServerSuper
from halligame.utils.ServerComms import ServerCommunicate
from halligame.utils.gameState import GameState

class Server(ServerSuper):
    def __init__(self, comms: ServerCommunicate) -> None:
        """
        Constructor for the Server (you should initalize stuff here)
        
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

    def gotClientMessage(self, event, clientPID) -> None:
        """
        The is where your game logic should go. You'll receive an events from 
        the client and should validate them here. (You'll probably want to use
        a case statement and pattern match the events).
        
        Arguments:
            event - an event to process if it's valid it should broadcast it's 
            new state to all players, otherwise it should tell the client that
            it's not valid and the state hasn't been updated.

            clientPID - this is the PID of the client that's responsible for the
            event.
        """
        if valid:
            self.__comms.broadcastState(self.__state)
        else:
            self.__comms.sendClientMessage(clientPID, ("error", "Error: Invalid Move"))
        pass

    def addClient(self, clientPID) -> None:
        """
        Callback function for when a client joins the game. This should also
        call `confirmJoin` to send a message to the client about a new player
        joining.

        Arguemnts;
            clientPID - PID of the client that joined the game
        """
        self.__usersConnected += 1
        msg = "You've joined!"
        self.__comms.confirmJoin(clientPID, msg)
        pass

    def removeClient(self, clientPID) -> None:
        """
        Callback function for when a client leaves the game. 

        Arguemnts;
            clientPID - PID of the client that left the game
        """
        pass
