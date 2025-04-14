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
import asyncio

from pyrlang import Node
from pyrlang.process import Process
from pyrlang.gen.server import GenServerInterface
from term import Atom
from halligame.games import *
from random import randint
from time import sleep

from halligame.utils.screen import Screen

class ClientCommunicate(Process):
    # TODO: there are some serious shenanigans of imports going on here and I hate it
    def __init__(self, gameName, commServerName):
        super().__init__()
        node.register_name(self, f'pyClient')

        self.__commServerName = commServerName

        # "TicTacToe"
        #    - Import the tictactoe module
        #    - Call the init function of that tictactoe module
        gameModule = importlib.import_module("halligame.games." + gameName)
        self.__clientGameInstance = gameModule.Client(self)

        self.__commGenServer = GenServerInterface(self,
                                                  (Atom(commServerName),
                                                   Atom("communicationServer")))
        
        self.__commGenServer.cast_nowait((Atom("new_client"), self.pid_))

    def handle_one_inbox_message(self, msg: tuple):
        """
        Await messages/check inbox of erlang/pyrlang node and call callback
        function when it receives a known message, other wise, informs the user
        there has been some sort of error and we've received an unknown message

        msg : message received
        """
        print(f"DEBUG: ClientComms got message {msg}")
        if msg == Atom("close"):
            exit(0)

        if (msg[0] == "state"):
            self.__clientGameInstance.updateState(msg[1])
        elif (msg[0] == "reply"):
            self.__clientGameInstance.gotServerMessage(msg[1])
        elif (msg[0] == "confirmed_join"):
            print(f"Calling Confirmed Join")
            self.__clientGameInstance.confirmedJoin(msg[1])
        else:
            raise ValueError("ClientComms Received an invalid message")

    def sendMessage(self, msg):
        """
        Given a message (msg) send it to the server

        msg : message to send
        """
        node.send_nowait(sender = self.pid_,
                        receiver = (Atom(self.__commServerName),
                                    Atom("communicationServer")),
                        message = (self.pid_, (Atom("data"), (Atom("event"), msg))))

    def shutdown(self):
        print("in shutdown")
        asyncio.run(self.__commGenServer.cast((Atom("remove_client"), self.pid_)))
        print("sent cast")
        sleep(0.2)
        print("destroying node")
        node.destroy()
        print("node destroyed, exiting")
        sys.exit(0)

def start(commServerName, gameName):
    global name, node
    name = f'{randint(0, 999999) :06d}@{os.environ["HOST"]}'
    print("ClientComms NodeName: ", name)
    node = Node(node_name = name, cookie = "Sh4rKM3ld0n")
    clientComms = ClientCommunicate(gameName, commServerName)
    node.run()

# This is an entry point!
# Arguments on command line:
# Argument 1: name of communicationServer node ("name@host"), from the
# erpyServerCommunicate you launched seperately
if __name__ == '__main__':
    commServerName = sys.argv[1]
    start(commServerName, "TicTacToe")
