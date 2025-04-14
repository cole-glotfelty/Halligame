# Example Game Client
# Written by Cole Glotfelty <2025-04-14>

# A barebones game client explaining what needs to be implemented and what's
# available for use when creating halligame games

from halligame.utils.gameState import GameState

class Client:
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

    def play(self) -> None:
        """
        """
        pass

    def updateState(self, state: bytes) -> None:
        """
        """
        pass

    def gotMessage(self, msg) -> None:
        pass

    def confirmedJoin(self, msg) -> None:
        pass

    def otherMessage(self, msg) -> None:
        pass

    def userInput(self, input) -> None:
        pass