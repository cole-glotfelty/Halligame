import importlib # allows us to import a module based on the name
import os

import threading

from erpy import stdio_port_connection
from term import Atom


class ServerCommunicate():
    def __init__(self, gameName):
        self.__serverGameInstance = None # TODO: figure out the include problem in clientCommunicate first


    def monitorServerMessages(self):
        for msg in self.inbox:
            if msg == Atom("close"):
                break
            
            self.__serverGameInstance.eventIsValid(msg) # send them the new state

    def sendMessage(self, Msg):
        self.port.send(Msg)

if __name__ == '__main__':
    c = ServerCommunicate("TicTacToe")
