# Example Game Client
# Written by Cole Glotfelty <2025-04-14>

# A barebones game client explaining what needs to be implemented and what's
# available for use when creating halligame games. This also includes some 
# typical patterns 

from halligame.utils.gameClientTemplate import ClientSuper
from halligame.utils.gameState import GameState
from typing import Any

class Client(ClientSuper):
    def __init__(self, comms: callable) -> None:
        """
        Constructor for the Client (you should initalize stuff here)
        
        One thing to note: `comms` needs to be assigned like so:

            self.__comms = comms

        We also have assorted utilities in halligame.utils. Here we're using
        the GameState class which also must be used for storing the state of
        your game. (We do this for serialization and server communication)
        """
        self.__comms = comms
        self.__state = GameState()
        pass

    def updateState(self, state: bytes) -> None:
        """
        Callback function triggered by `broadcastState`. This is where you 
        should modify the TUI screen for the user. The state should then be 
        copied to the local state from the message sent by the server.
        """
        self.__state.deserialize(state)
        pass

    def gotServerMessage(self, msg) -> None:
        """
        Callback function for when a message is received from the server. One
        could deconstruct the msg like below and then discriminate based on 
        the status or message text.
        """
        (status, message) = msg
        if status == "error":
            print(message)
        pass

    def joinConfirmed(self, msg) -> None:
        """
        Callback function for when the player joins the server. This message
        can be set in GameServer::addClient().
        """
        pass
