import importlib # allows us to import a module based on the name
import os

import threading

from erpy import stdio_port_connection
from term import Atom
 
 # NOTE: Threads are spawned as Daemons, so they might be killed at any time

class ServerCommunicate():
    def __init__(self, gameName):
        self.inbox, self.port = stdio_port_connection()

        self.__serverGameInstance = None # TODO: figure out the include problem in clientCommunicate first

        self.__monitorInboxThread = threading.Thread(target = self.monitorServerMessages,
                                         args = [])
        self.__monitorInboxThread.daemon = True

    def monitorServerMessages(self):
        for msg in self.inbox:
            if msg == Atom("close"):
                break
            
            self.__serverGameInstance.eventIsValid(msg) # send them the new state

    def sendMessage(self, Msg):
        self.port.send(Msg)
    
    def startInboxMonitor(self):
        self.monitorThread.start()

if __name__ == '__main__':
    c = ServerCommunicate("TicTacToe")

    c.startInboxMonitor()
