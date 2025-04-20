# ClientComms.py

# Communication Module for clients to communicate with the game/communication
# server.
# Writen by: Will Cordray & Michael Daniels
# Last Edited by: Cole Glotfelty <2025-04-09>

# Changelog:
# Cole Glotfelty <2025-04-09> - Added documentation to functions

import importlib  # allows us to import a module based on the name
import socket
import sys
import threading
from random import randint
from typing import Any

from pyrlang import Node
from pyrlang.process import Process
from term import Atom, Pid

from halligame.games import *  # noqa: F403
from halligame.utils.common import whoami

node: Node  # Placeholder for mypy
name: str  # Placeholder for mypy


class ClientCommunicate(Process):
    def __init__(self, gameName: str, serverPid: Pid) -> None:
        super().__init__()
        node.register_name(self, "pyClient")

        gameModule = importlib.import_module("halligame.games." + gameName)
        self.__clientGameInstance = gameModule.Client(self)

        self.__serverPid = serverPid
        self.__delayQuitUntilConfirmation = threading.Semaphore(0)
        self.__thisUser = whoami()
        self.__backendSendMessage(("new_client", self.pid_, self.__thisUser))

    def handle_one_inbox_message(self, msg: tuple) -> None:
        """
        Await messages/check inbox of erlang/pyrlang node and call callback
        function when it receives a known message, other wise, informs the user
        there has been some sort of error and we've received an unknown message

        msg : message received
        """
        if msg == "close":
            exit(0)

        messageContents = msg[1]

        if msg[0] == "state":
            self.__clientGameInstance.updateState(messageContents)
        elif msg[0] == "message":
            self.__clientGameInstance.gotServerMessage(messageContents)
        elif msg[0] == "confirmed_join":
            self.__clientGameInstance.joinConfirmed(messageContents)
        elif msg[0] == "quit_confirm":
            # can continue quit process
            self.__delayQuitUntilConfirmation.release()
        else:
            raise ValueError(
                "ClientComms Received an unknown message" + str(msg)
            )

    def sendMessage(self, msg: Any) -> None:
        """
        Given a message (msg) send it to the server

        msg : message to send
        """

        self.__backendSendMessage(("message", (self.pid_, msg)))

    def __backendSendMessage(self, msg: Any) -> None:
        node.send_nowait(
            sender=self.pid_,
            receiver=(Atom(self.__serverPid), Atom("pyServer")),
            message=msg,
        )

    def shutdown(self) -> None:
        self.__backendSendMessage(("remove_client", self.pid_, self.__thisUser))

        # the following will block until receiving a confirmation message from
        # the server
        self.__delayQuitUntilConfirmation.acquire()

        node.destroy()
        sys.exit(0)


def start(commServerName: str, gameName: str) -> None:
    global name, node
    name = f"{randint(0, 999999):06d}@{socket.gethostname()}"
    print("DEBUG: ClientComms NodeName: ", name)
    node = Node(node_name=name, cookie="Sh4rKM3ld0n")
    ClientCommunicate(gameName, commServerName)
    node.run()
