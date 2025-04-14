# gameState.py

# GameState Class for serializing and desearlizing game state for server 
# communication
# Written by Will Cordray, Cole Glotfelty, Michael Daniels <2025-03-29>
# Last modified by: Cole Glotfelty <2025-04-14>

# Changelog:
# Cole Glotfelty <2025-04-14> - 

import threading
import pickle

class GameState():
    """
    A Thread-Safe implementation of a dictionary for storing/managing game state
    """
    def __init__(self, args: dict = {}):
        self.__lock = threading.Lock()
        self.objects = args

    def serialize(self):
        """
        Serlialize the state dictionary into binary using pickle.
        """
        with self.__lock:
            return pickle.dumps(self.objects)

    def deserialize(self, state: bytes):
        """
        Deserlialize the state dictionary from binary using pickle.
        state: pickled binary
        """
        with self.__lock:
            self.objects = pickle.loads(state)
