import importlib # allows us to import a module based on the name
import os

import threading

from erpy import stdio_port_connection
from term import Atom


class ClientCommunicate():
    # TODO: there are some serious shenanigans of imports going on here and I hate it
    def __init__(self, gameName):
        self.inbox, self.port = stdio_port_connection()

        os.chdir("../games") # TODO: this will change
        try:
            self.__module = importlib.import_module('game')
            print("Success!")
        except ModuleNotFoundError as e:
            print("Module not found")
            print(e)
        except Exception as e:
            print("Unknown module import error")
        os.chdir("../utils") # TODO: this will change

        # TODO: this may not work
        initFunc = getattr(self.__module, gameName) # assumes that the module contains a class of the same name (e.g. class TicTacToe)
        self.__serverGameInstance = initFunc(self.sendMessage) # set up comms function

        self.__monitorInboxThread = threading.Thread(target = self.monitorServerMessages,
                                                 args = [])
        self.__monitorInboxThread.daemon = True


    def monitorServerMessages(self):
        for msg in self.inbox:
            if msg == Atom("close"):
                break
            
            self.__gameInstance.updateState(msg) # send them the new state

    def sendMessage(self, Msg):
        self.port.send(Msg)
    
    def startInboxMonitor(self):
        self.__monitorInboxThread.start()


if __name__ == '__main__':
    c = ClientCommunicate("TicTacToe")

    c.startInboxMonitor()
