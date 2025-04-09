import importlib # allows us to import a module based on the name
import os
import sys

import threading

from pyrlang import Node
from pyrlang.process import Process
from pyrlang.gen.server import GenServerInterface
from term import Atom
from halligame.games import *
from random import randint

class ClientCommunicate(Process):
    # TODO: there are some serious shenanigans of imports going on here and I hate it
    def __init__(self, gameName):
        # self.inbox, self.port = stdio_port_connection()
        super().__init__()
        node.register_name(self, f'pyClient')

        # self.__serverGameInstance = TicTacToe.Client(self.sendMessage, self.port)
        # "TicTacToe"
        #    - Import the tictactoe module
        #    - Call the init function of that tictactoe module
        gameModule = importlib.import_module("halligame.games." + gameName)
        self.__serverGameInstance = gameModule.Client(self.sendMessage)
        self.__commGenServer = GenServerInterface(self,
                                                  (Atom(commServerName),
                                                   Atom("communicationServer")))
        # os.chdir("../games") # TODO: this will change
        # try:
        #     self.__module = importlib.import_module('game')
        #     print("Success!")
        # except ModuleNotFoundError as e:
        #     print("Module not found")
        #     print(e)
        # except Exception as e:
        #     print("Unknown module import error")
        # os.chdir("../utils") # TODO: this will change
        
        self.__commGenServer.cast_nowait((Atom("add_client"), self.pid_))

        # # TODO: this may not work
        # initFunc = getattr(self.__module, gameName) # assumes that the module contains a class of the same name (e.g. class TicTacToe)
        # self.__serverGameInstance = initFunc(self.sendMessage) # set up comms function

    def handle_one_inbox_message(self, msg):
        # print(f"erpyClientComm got message {msg}")
        if msg == Atom("close"):
            exit(0)

        try:
            if (msg[0] == "state"):
                self.__serverGameInstance.updateState(msg[1])
            elif (msg[0] == "reply"):
                self.__serverGameInstance.gotReply(msg[1])
            elif (msg[0] == "confirmed_join"):
                self.__serverGameInstance.setPlayerId(msg[1])
            else:
                raise UserWarning("Unknown message")
        except:
            print(f"Could not process message {msg}")

    def sendMessage(self, msg):
        if msg == "close":
            # self.exit()
            node.destroy()
            # exit(0)
        else:
            node.send_nowait(sender = self.pid_,
                            receiver = (Atom(commServerName),
                                        Atom("communicationServer")),
                            message = (self.pid_, (Atom("data"), (Atom("event"), msg))))

# This is an entry point!
# Arguments on command line:
# Argument 1: player ID (0 or 1)
# Argument 2: name of communicationServer node ("name@host"), from the
# erpyServerCommunicate you launched seperately
if __name__ == '__main__':
    name = f'{randint(0, 999999) :06d}@{os.environ["HOST"]}'
    print(name)
    commServerName = sys.argv[1]
    print(commServerName)
    node = Node(node_name = name, cookie = "COOKIE")
    clientComms = ClientCommunicate("TicTacToe")
    node.run()
