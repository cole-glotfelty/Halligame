# Example Game Client / Game Client Super Class
# Written by Cole Glotfelty <2025-04-14>

# A barebones game client explaining what needs to be implemented and what's
# available for use when creating halligame games. This also includes some 
# typical patterns 

from halligame.utils.gameState import GameState
from typing import Any

class ClientSuper:
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
        should modify the TUI screen for the user. The state is is then copied
        from the message sent by the server and 
        """
        self.__state.deserialize(state)
        pass

    def gotServerMessage(self, msg) -> None:
        (status, message) = msg
        if status == "error":
            print(message)
        pass

    def confirmedJoin(self, msg) -> None:
        pass
