# gameState.py

# GameState Class for serializing and desearlizing game state for server
# communication
# Written by Will Cordray, Cole Glotfelty, Michael Daniels <2025-03-29>
# Last modified by: Cole Glotfelty <2025-04-14>

# Changelog:
# Cole Glotfelty <2025-04-14> - Added Documentation
# Michael Daniels <2025-04-03> - Switched to pickle for serialization

import pickle
import threading


class GameState:
    """
    A Thread-Safe implementation of a dictionary for storing/managing game state
    """

    def __init__(self, args: dict = None):
        self.__lock = threading.Lock()
        if args is None:
            self.__objects = {}
        else:
            self.__objects = args

    def serialize(self):
        """
        Serlialize the state dictionary into binary using pickle.
        """
        with self.__lock:
            return pickle.dumps(self.__objects)

    def deserialize(self, state: bytes):
        """
        Deserlialize the state dictionary from binary using pickle.
        state: pickled binary
        """
        with self.__lock:
            self.__objects = pickle.loads(state)

    def getValue(self, Key):
        """
        Getter for Value
        """
        with self.__lock:
            return self.__objects[Key]

    def setValue(self, Key, Value):
        """
        Setter for Value
        """
        with self.__lock:
            self.__objects[Key] = Value
