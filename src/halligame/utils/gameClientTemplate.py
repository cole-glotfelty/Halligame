# Example Game Client / Game Client Super Class
# Written by Cole Glotfelty <2025-04-14>

# A barebones game client explaining what needs to be implemented and what's
# available for use when creating halligame games. This also includes some 
# typical patterns 

from halligame.utils.gameState import GameState
from typing import Any

class ClientSuper:
    def __init__(self) -> None:
        pass

    def updateState(self, state: bytes) -> None:
        pass

    def gotServerMessage(self, msg) -> None:
        pass

    def confirmedJoin(self, msg) -> None:
        pass
