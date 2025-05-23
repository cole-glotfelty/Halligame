"""Communication Module for clients to communicate with the game server.

Authors: Will Cordray & Michael Daniels
Last Edited: Michael Daniels 2025-04-21
"""

# Changelog:
# Cole Glotfelty <2025-04-09> - Added documentation to functions

import importlib  # allows us to import a module based on the name
import socket
import sys
from random import randint
from threading import Semaphore
from typing import Any

from pyrlang import Node
from pyrlang.process import Process
from term import Atom, Pid

from halligame.games import *  # noqa: F403
from halligame.utils.misc import whoami

node: Node  # Placeholder for mypy
name: str  # Placeholder for mypy


class ClientCommunicate(Process):
    """Used by one game client to communicate with its server."""

    def __init__(self, gameName: str, serverPid: Pid) -> None:
        """Set up this instance."""
        super().__init__()
        node.register_name(self, "pyClient")

        gameModule = importlib.import_module("halligame.games." + gameName)
        #: The game client itself.
        self.__clientGameInstance = gameModule.Client(self)
        #: The pid of this game's server.
        self.__serverPid: Pid = serverPid
        #: Used to make sure the server gets our quit message.
        self.__delayQuitUntilConfirmation: Semaphore = Semaphore(0)
        #: The current user's username.
        self.__thisUser: str = whoami()

        self.__backendSendMessage(("new_client", self.pid_, self.__thisUser))

    def handle_one_inbox_message(self, msg: tuple) -> None:
        """Handle incoming messages."""
        match msg:
            case "close":
                exit(0)
            case "state", newState:
                self.__clientGameInstance.updateState(newState)
            case "message", messageContents:
                self.__clientGameInstance.gotServerMessage(messageContents)
            case "confirmed_join", messageContents:
                self.__clientGameInstance.joinConfirmed(messageContents)
            case "quit_confirm", _fromPid:
                # can continue quit process
                self.__delayQuitUntilConfirmation.release()
            case _:
                raise ValueError(
                    "ClientComms Received an unknown message" + str(msg)
                )

    def sendMessage(self, msg: Any) -> None:
        """Send a message to the game server."""
        self.__backendSendMessage((Atom("message"), (self.pid_, msg)))

    def __backendSendMessage(self, msg: Any) -> None:
        """Send arbitrary messages to the game server."""
        node.send_nowait(
            sender=self.pid_, receiver=self.__serverPid, message=msg
        )

    def shutdown(self) -> None:
        """Shut down this Pyrlang node.

        Waits for confirmation from the game server, to avoid the condition
        where telling the server we quit doesn't go through.
        """
        self.__backendSendMessage(("remove_client", self.pid_, self.__thisUser))

        self.__delayQuitUntilConfirmation.acquire()

        node.destroy()
        sys.exit(0)


def start(serverNodeName: str, gameName: str, serverPid: Pid) -> None:
    """Start the client instance and its Pyrlang node."""
    global name, node
    name = f"{randint(0, 999999):06d}@{socket.gethostname()}"
    print("DEBUG: ClientComms NodeName: ", name)
    node = Node(node_name=name, cookie="Sh4rKM3ld0n")
    ClientCommunicate(gameName, serverPid)
    node.run()
