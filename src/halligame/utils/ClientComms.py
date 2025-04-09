# ClientComms.py

# Communication Module for clients to communicate with the game/communication
# server.
# Writen by: Will Cordray & Michael Daniels
# Last Edited by: Cole Glotfelty <2025-04-09>

# Changelog:
# Cole Glotfelty <2025-04-09> - Added documentation to functions

import importlib # allows us to import a module based on the name
import os
import sys

from pyrlang import Node
from pyrlang.process import Process
from pyrlang.gen.server import GenServerInterface
from term import Atom
from halligame.games import *
from random import randint

class ClientCommunicate(Process):
    # TODO: there are some serious shenanigans of imports going on here and I hate it
    def __init__(self, gameName):
        super().__init__()
        node.register_name(self, f'pyClient')

        # "TicTacToe"
        #    - Import the tictactoe module
        #    - Call the init function of that tictactoe module
        gameModule = importlib.import_module("halligame.games." + gameName)
        self.__serverGameInstance = gameModule.Client(self.sendMessage)
        self.__commGenServer = GenServerInterface(self,
                                                  (Atom(commServerName),
                                                   Atom("communicationServer")))
        
        self.__commGenServer.cast_nowait((Atom("add_client"), self.pid_))

    def handle_one_inbox_message(self, msg: tuple):
        """
        Await messages/check inbox of erlang/pyrlang node and call callback
        function when it receives a known message, other wise, informs the user
        there has been some sort of error and we've received an unknown message

        msg : message received
        """
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
        """
        Given a message (msg) send it to the server

        msg : message to send
        """
        if msg == "close":
            node.destroy()
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
