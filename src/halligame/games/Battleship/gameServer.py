# Example Game Server 
# Written by Cole Glotfelty <2025-04-14>

# A barebones game client explaining what needs to be implemented and what's
# available for use when creating halligame games. This also includes some 
# typical patterns 

from halligame.utils.gameServerTemplate import ServerSuper
from halligame.utils.ServerComms import ServerCommunicate
from halligame.utils.gameState import GameState

from threading import Lock

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
        self.__playerShips = [[], []] # index playerID for the ships
        self.__userLock = Lock()
        self.__usersConnected = 0
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
        match event:
            case ('gridMove', playerID, move):
                self.__comms.broadcastMessage(event)

    def addClient(self, clientPID, username) -> None:
        """
        Callback function for when a client joins the game. This should also
        call `confirmJoin` to send a message to the client about a new player
        joining.

        Arguemnts;
            clientPID - PID of the client that joined the game

        Verify user count (using a lock to enforce that players must wait).
        """
        with self.__userLock:
            self.__usersConnected += 1
            if self.__usersConnected > 2:
                self.__comms.sendClientMessage(clientPID, ("error", "Error: Room Full"))
            playerId = 0 if self.__usersConnected == 1 else 1
            self.__comms.confirmJoin(clientPID, username, playerId) 

    def removeClient(self, clientPID, username) -> None:
        """
        Callback function for when a client leaves the game. 

        Arguemnts;
            clientPID - PID of the client that left the game
        """
        pass
