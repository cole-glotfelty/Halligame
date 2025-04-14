# ServerComms.py

# Communication from the game server to the communication server/client
# Written by: Will Cordray & Michael Daniels


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
    def __init__(self, gameName: str, NodeName: str):
        super().__init__()
        self.__commserver_name = f"{randint(0, 999999):06d}"
        self.__full_commserver_name = self.__commserver_name + "@" +  os.environ["HOST"]
        print(f"Communication server node: {self.__full_commserver_name}")
        subprocess.Popen(['bash', '-xc',
                        #   'ls'
                          f'erl -noinput -sname {self.__commserver_name} -setcookie Sh4rKM3ld0n -eval "communicationServer:start_link([\'TicTacToe\', \'{NodeName}\'])"'
                          ],
                          cwd = '../communicationServer')
        node.register_name(self, Atom("pyServer"))

        self.__commGenServer = GenServerInterface(self,
                                                  (self.__full_commserver_name,
                                                   Atom("communicationServer")))

        sleep(0.5)
        self.__commGenServer.cast_nowait((Atom("replace_server"), self.pid_))

        sleep(0.5)
        gameModule = importlib.import_module("halligame.games." + gameName)
        self.__serverGameInstance = gameModule.Server(self)

    def handle_one_inbox_message(self, msg):
        print(f"DEBUG: ServerComms got message {msg}")
        if msg == "close":
            self.exit()
            exit(0)
            
        if (msg[0] == "new_client"):
            self.__serverGameInstance.addClient(msg[1]) # send them the client
        elif (msg[0] == "remove_client"):
            self.__serverGameInstance.removeClient(msg[1])
        elif (msg[0] == "event"):
            # msg[1][1] = clientPid
            # msg[1][0] = Message
            self.__serverGameInstance.eventIsValid(msg[1][1], msg[1][0])
        elif (msg[0] == "other"):
            self.__serverGameInstance.otherMessageType(msg[1][0], msg[1][1])
        else:
            raise ValueError("Unknown Message ID in ServerComms: " + str(msg))

    def play(self):
        self.__serverGameInstance.play()

    # backend for sending a message
    def __backendSendMessage(self, Msg):
        node.send_nowait(sender = self.pid_,
                         receiver = (self.__full_commserver_name,
                                     Atom("communicationServer")),
                         message = Msg)

    # front end wrapper for sending a message with correct formatting
    def __sendMessage(self, Msg):
        print(f"DEBUG: Sending Message from ServerComms to commServer:", Msg)
        
        # convert the command to an atom for the server to process
        if (Msg[0] == "broadcastState" or Msg[0] == "reply" 
            or Msg[0] == "terminate"):
            Msg = (Atom(Msg[0]), *Msg[1:])
            print(Msg)

        self.__backendSendMessage((self.pid_, 
                            (Atom("data"), 
                             Msg)))

    # State should have type halligame.utils.GameState
    def sendState(self, State : GameState):
        print(f"Sending serialized state {State}")
        self.__sendMessage((Atom("broadcastState"), State.serialize()))

    def confirmJoin(self, ClientPid, Message):
        self.__sendMessage((Atom("confirmed_join"), (ClientPid, Message)))

    def shutdown(self):
        self.__backendSendMessage(("terminate", "normal"))
        node.destroy()

    def sendClientMessage(self, ClientPid, Message):
        self.__sendMessage(("reply", (ClientPid, Message)))
        # node.send_nowait(sender = self.pid_,
        #                  receiver = ClientPid,
        #                  message = Message)

def start(game : str, node_name : str):
    global node
    node = Node(node_name, cookie = "Sh4rKM3ld0n")
    serverComms = ServerCommunicate(game, node_name)
    node.run()

    serverComms.play()


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
        start(args.game, args.node_name)
