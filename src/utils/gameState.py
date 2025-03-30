# GameState Class for serializing and desearlizing game state for server 
# communication
# Written by Will Cordray, Cole Glotfelty, Michael Daniels <2025-03-29>

import threading

class GameState():
    def __init__(self, args = {}):
        self.__lock = threading.Lock()
        self.objects = args

    def serialize(self):
        with self.__lock:
            list(self.objects.items())

    def deserialize(self, state):
        with self.__lock:
            self.objects = dict(state)
