# ServerComms.py

# Communication from the game server to the communication server/client
# Written by: Will Cordray & Michael Daniels


# import importlib # allows us to import a module based on the name

import importlib
import subprocess

from pyrlang import Node
from pyrlang.gen.server import GenServerInterface
from pyrlang.process import Process
from term import Atom

from halligame.games import *  # noqa: F403
from halligame.utils.gameState import GameState


class ServerCommunicate(Process):
    def __init__(self, gameName: str, nodeName: str):
        super().__init__()
        node.register_name(self, Atom("pyServer"))
        self.__gameName = gameName
        self.__nodeName = nodeName
        self.__serverBroker = None
        self.__connectedClients = set()
        self.__thisUser = (
            subprocess.run(["whoami"], capture_output=True)
            .stdout.decode()
            .strip()
        )

        gameModule = importlib.import_module("halligame.games." + gameName)
        self.__serverGameInstance = gameModule.Server(self)

        # Pyrlang has node name and registered name backwards >:(
        self.__sendMessage(
            (Atom("serverbroker@vm-projectweb3"), Atom("serverbroker")),
            (Atom("getBrokerPid"), self.pid_),
        )

    def handle_one_inbox_message(self, msg):
        print(f"DEBUG: ServerComms got message {msg}\n")
        if msg == "close":
            self.shutdown()
            exit(0)
        elif msg[0] == Atom("brokerPid"):
            self.__serverBroker = GenServerInterface(self, msg[1])
            self.__serverBroker.cast_nowait(
                (
                    Atom("register_gameserver"),
                    self.__gameName,
                    self.__nodeName,
                    self.pid_,
                )
            )
        elif msg[0] == "new_client":
            clientPid = msg[1]
            username = msg[2]
            self.__serverGameInstance.addClient(clientPid, username)
        elif msg[0] == "remove_client":
            ClientPid = msg[1]
            username = msg[2]
            
            self.__sendMessage(ClientPid, ("quit_confirm", self.pid_))

            self.__connectedClients.remove(ClientPid)
            self.__serverGameInstance.removeClient(ClientPid, username)
        elif msg[0] == "message":
            clientPid = msg[1][0]
            message = msg[1][1]
            self.__serverGameInstance.gotClientMessage(clientPid, message)
        else:
            raise ValueError(
                "ServerComms Received an unknown message: " + str(msg)
            )

    # wrapper for sending a message with correct formatting
    def __sendMessage(self, dest, msg):
        # print(f"DEBUG: Sending Message from ServerComms to {dest}: {msg}")
        node.send_nowait(sender=self.pid_, receiver=dest, message=msg)

    # State should have type halligame.utils.GameState
    def broadcastState(self, State: GameState):
        for ClientPid in self.__connectedClients:
            self.__sendMessage(ClientPid, ("state", State.serialize()))

    def broadcastMessage(self, Message):
        for ClientPid in self.__connectedClients:
            self.sendClientMessage(ClientPid, Message)

    def confirmJoin(self, ClientPid, Message):
        self.__connectedClients.add(ClientPid)
        self.__sendMessage(ClientPid, ("confirmed_join", Message))

    def sendClientMessage(self, ClientPid, Message):
        self.__sendMessage(ClientPid, ("message", Message))

    def shutdown(self):
        self.__serverBroker.cast_nowait(
            (Atom("unregister_gameserver"), self.pid_)
        )
        node.destroy()
    
    def sendMsgViaServerBroker(self, fromName, toUser, message):
        # Note: fromName should be a username (all lowercase), or
        # an arbitrary string containing at least one capital letter.
        self.__serverBroker.cast_nowait((Atom("message_user"), fromName,
                                         toUser, message))


def start(game: str, node_name: str):
    global node
    node = Node(node_name, cookie="Sh4rKM3ld0n")
    ServerCommunicate(game, node_name)
    node.run()
