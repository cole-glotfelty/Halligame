# import importlib # allows us to import a module based on the name
# import os # TODO: rm?

import threading

from erpy import stdio_port_connection
from term import Atom
from halligame.games import *
 
 # NOTE: Threads are spawned as Daemons, so they might be killed at any time

class ServerCommunicate():
    def __init__(self, gameName):
        self.inbox, self.port = stdio_port_connection()

        # TODO: make this dependent on game provided
        self.__serverGameInstance = TicTacToe.Server(self.sendMessage)

        self.__monitorInboxThread = threading.Thread(target = self.monitorServerMessages,
                                         args = [])
        self.__monitorInboxThread.daemon = True

    def monitorServerMessages(self):
        for msg in self.inbox:
            if msg == Atom("close"):
                break
            
            if (msg[0] == "new_client"):
                self.__serverGameInstance.addUser(msg[1]) # send them the user
            elif (msg[0] == "remove_client"):
                self.__serverGameInstance.removeUser(msg[1])
            elif (msg[0] == "event"):
                self.__serverGameInstance.eventIsValid(msg[1][0], msg[1][1])
            elif (msg[0] == "other"):
                self.__serverGameInstance.otherMessageType(msg[1][0], msg[1][1])

    def play(self):
        self.__serverGameInstance.play()

    def sendMessage(self, Msg):
        self.port.send(Msg)
    
    def startInboxMonitor(self):
        self.__monitorInboxThread.start()
    
    def joinInboxMonitor(self):
        self.__monitorInboxThread.join()

    def shutDownServer(self):
        self.sendMessage(("terminate", "normal"))

        self.__monitorInboxThread.join()

if __name__ == '__main__':
    c = ServerCommunicate("TicTacToe")

    c.startInboxMonitor()

    c.joinInboxMonitor()

    c.shutDownServer()
