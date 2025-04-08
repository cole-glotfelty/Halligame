# import importlib # allows us to import a module based on the name

import subprocess
from time import sleep
import argparse
import sys
import importlib

from pyrlang import Node
from pyrlang.process import Process
from pyrlang.gen.server import GenServerInterface

from term import Atom
from halligame.games import *
from halligame.utils.gameState import GameState
import os
from random import randint
 
class ServerCommunicate(Process):
    def __init__(self, gameName, NodeName):
        super().__init__()
        self.__commserver_name = f"{randint(0, 999999):06d}"
        self.__full_commserver_name = self.__commserver_name + "@" +  os.environ["HOST"]
        print(f"Communication server node: {self.__full_commserver_name}")
        subprocess.Popen(['bash', '-xc',
                        #   'ls'
                          f'erl -noinput -sname {self.__commserver_name} -setcookie COOKIE -eval "communicationServer:start_link([\'TicTacToe\', \'{NodeName}\'])"'
                          ],
                          cwd = '../communicationServer')
        node.register_name(self, Atom("pyServer"))

        self.__commGenServer = GenServerInterface(self,
                                                  (self.__full_commserver_name,
                                                   Atom("communicationServer")))

        sleep(1)
        self.__commGenServer.cast_nowait((Atom("replace_server"), self.pid_))

        sleep(0.5)
        gameModule = importlib.import_module("halligame.games." + gameName)
        self.__serverGameInstance = gameModule.Server(self)

    def handle_one_inbox_message(self, msg):
        print(f"DEBUG: erpyServerComm got message {msg}")
        if msg == "close":
            self.exit()
            exit(0)
            
        if (msg[0] == "new_client"):
            self.__serverGameInstance.addUser(msg[1]) # send them the user
        elif (msg[0] == "remove_client"):
            self.__serverGameInstance.removeUser(msg[1])
        elif (msg[0] == "event"):
            self.__serverGameInstance.eventIsValid(msg[1][1], msg[1][0])
        elif (msg[0] == "other"):
            self.__serverGameInstance.otherMessageType(msg[1][0], msg[1][1])

    def play(self):
        self.__serverGameInstance.play()

    def sendMessage(self, Msg):
        node.send_nowait(
                            sender = self.pid_,
                            receiver = (self.__full_commserver_name,
                                        Atom("communicationServer")),
                            message = Msg)

    # State should have type halligame.utils.GameState
    def sendState(self, State : GameState):
        print(f"Sending serialized state {State.serialize()}")
        self.sendMessage((self.pid_,
                          (Atom("data"),
                           (Atom("broadcastState"), State.serialize()))))
    def shutDownServer(self):
        self.sendMessage(("terminate", "normal"))
    
    def sendClientMessage(self, pid, msg):
        node.send_nowait(
                            sender = self.pid_,
                            receiver = pid,
                            message = msg)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-g", "--game", type=str, default="")
    parser.add_argument("-n", "--node_name", type=str, default="")
    args = parser.parse_args()

    if (args.game == ""):
        print("ERROR: No Game Supplied to ServerCommunicate", file=sys.stderr)
    elif (args.node_name == ""):
        print("ERROR: No Node Name Supplied", file=sys.stderr)
    else:
        node = Node(node_name = args.node_name, cookie = "COOKIE")
        serverComms = ServerCommunicate(args.game, args.node_name)
        node.run()

        serverComms.shutDownServer()
