import importlib # allows us to import a module based on the name
import os

import threading

from pyrlang import Node
from pyrlang.process import Process
from term import Atom
from halligame.games import *

class ClientCommunicate(Process):
    # TODO: there are some serious shenanigans of imports going on here and I hate it
    def __init__(self, gameName):
        # self.inbox, self.port = stdio_port_connection()
        super().__init__()
        self.get_node().register_name(self, f'pyClient')

        # self.__serverGameInstance = TicTacToe.Client(self.sendMessage, self.port)
        # "TicTacToe"
        #    - Import the tictactoe module
        #    - Call the init function of that tictactoe module
        gameModule = importlib.import_module("halligame.games." + gameName)
        self.__serverGameInstance = gameModule.Client(self.sendMessage , 1) # TODO: fix, hard-coded player id to 1

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
        
        # Horrible hack: pretty sure $gen_cast (and this message format)
        # is an undocumentented OTP implementation detail. But it works!
        self.sendMessage((Atom('$gen_cast'), (Atom("add_client"), self.pid_)))
        # # TODO: this may not work
        # initFunc = getattr(self.__module, gameName) # assumes that the module contains a class of the same name (e.g. class TicTacToe)
        # self.__serverGameInstance = initFunc(self.sendMessage) # set up comms function

    def handle_one_inbox_message(self, msg):
        print(f"erpyClientComm got message {msg}")
        if msg == Atom("close"):
            exit(0)

        if (msg[0] == "state"):
            self.__serverGameInstance.updateState(msg[1])
        elif (msg[0] == "reply"):
            self.__serverGameInstance.gotReply(msg[1])

    def sendMessage(self, Msg):
        self.get_node().send_nowait(sender = self.pid_,
                                    # TODO: hard-coded
                         receiver = (Atom("mdanie09_TicTacToe_communicationserver@vm-hw02"),
                                     Atom("communicationServer")),
                         message = Msg)

if __name__ == '__main__':
    name = f'{os.environ["USER"]}-TicTacToe-gameclient@{os.environ["HOST"]}'
    n = Node(node_name = name, cookie = "COOKIE")
    c = ClientCommunicate("TicTacToe")
    n.run()

