"""Communication from the game server to the client.

Written by: Will Cordray & Michael Daniels
"""

import asyncio
import importlib
from typing import Any

from psutil import pid_exists
from pyrlang import Node
from pyrlang.gen.server import GenServerInterface
from pyrlang.process import Process
from term import Atom, Pid

from halligame.games import *  # noqa: F403
from halligame.utils.gameState import GameState

node: Node  # Placeholder for mypy

WAIT_TIME_SEC = 30
"""How long we should wait between checks that the parent hasn't died."""


class ServerCommunicate(Process):
    """Used by one game server to communicate with its clients & the broker."""

    def __init__(self, gameName: str, nodeName: str, shellPid: int) -> None:
        """Set up this instance."""
        super().__init__()
        node.register_name(self, Atom("pyServer"))
        gameModule = importlib.import_module("halligame.games." + gameName)
        #: The name of the game to join.
        self.__gameName: str = gameName
        #: The name of this node.
        self.__nodeName: str = nodeName
        #: Used to communicate with the server broker.
        self.__serverBroker: GenServerInterface
        #: __connectedClients contains tuples (clientPid, username)
        self.__connectedClients: set[tuple[Pid, str]] = set()
        #: The linux PID of the parent shell.
        self.__shellPid: int = shellPid
        #: The game server itself.
        self.__serverGameInstance = gameModule.Server(self)

        # Pyrlang has node name and registered name backwards >:(
        self.__sendMessage(
            (Atom("serverbroker@vm-projectweb3"), Atom("serverbroker")),
            (Atom("getBrokerPid"), self.pid_),
        )

        event_loop = asyncio.get_event_loop()
        event_loop.call_soon(self.__checkOSProcessAlive)

    def handle_one_inbox_message(self, msg: Any) -> None:
        """Handle incoming messages."""
        match msg:
            case "close":
                self.shutdown()
                exit(0)
            case "brokerPid", brokerPid:
                self.__serverBroker = GenServerInterface(self, brokerPid)
                self.__serverBroker.cast_nowait(
                    (
                        Atom("register_gameserver"),
                        self.__gameName,
                        self.__nodeName,
                        self.pid_,
                    )
                )
            case "new_client", clientPid, username:
                self.__serverGameInstance.addClient(clientPid, username)
                node.monitor_process(self.pid_, clientPid)
            case "remove_client", clientPid, username:
                self.__sendMessage(clientPid, ("quit_confirm", self.pid_))
                self.__connectedClients.discard((clientPid, username))
                self.__serverGameInstance.removeClient(clientPid, username)
            case "message", (clientPid, message):
                self.__serverGameInstance.gotClientMessage(clientPid, message)
            case "DOWN", _ref, "process", fromPid, _reason:
                for clientPid, username in self.__connectedClients:
                    if clientPid == fromPid:
                        self.__connectedClients.discard((clientPid, username))
                        self.__serverGameInstance.removeClient(
                            clientPid, username
                        )
                        break
            case _:
                raise ValueError(
                    "ServerComms Received an unknown message: " + str(msg)
                )

    def __sendMessage(
        self, dest: Atom | tuple[Atom, Atom] | Pid, msg: Any
    ) -> None:
        """A wrapper for sending a message with correct formatting.

        dest is either:

        * an Atom(local registered name),
        * a tuple (Atom(node name), Atom(registered name))
            (note that this is backwards from Erlang!), or
        * a Pid.
        """
        node.send_nowait(sender=self.pid_, receiver=dest, message=msg)

    # State should have type halligame.utils.GameState
    def broadcastState(self, state: GameState) -> None:
        """Send the game state to every client."""
        for clientPid, _username in self.__connectedClients:
            self.__sendMessage(clientPid, ("state", state.serialize()))

    def broadcastMessage(self, msg: Any) -> None:
        """Send a message (Atom("message", msg)) to every connected client."""
        for clientPid, _username in self.__connectedClients:
            self.sendClientMessage(clientPid, msg)

    def confirmJoin(self, clientPid: Pid, username: str, msg: Any) -> None:
        """Tell a client that it joined successfully, then tell the broker."""
        self.__connectedClients.add((clientPid, username))
        self.__sendMessage(clientPid, ("confirmed_join", msg))
        self.__serverBroker.cast_nowait(
            (Atom("joined_gameserver"), username, clientPid, self.pid_)
        )

    def sendClientMessage(self, clientPid: Pid, msg: Any) -> None:
        """Send a client a message."""
        self.__sendMessage(clientPid, ("message", msg))

    def shutdown(self) -> None:
        """Shut down this Pyrlang node. Informs clients and the serverbroker."""
        self.broadcastMessage("close")

        self.__serverBroker.cast_nowait(
            (Atom("unregister_gameserver"), self.pid_)
        )

        event_loop = asyncio.get_event_loop()
        event_loop.call_later(1, exit)

    def sendMsgViaServerBroker(
        self, fromName: str, toUser: str, message: str
    ) -> None:
        """Sends a message to a user via the server broker.

        fromName should be a username (all lowercase), or
        an arbitrary string containing at least one capital letter.
        """
        self.__serverBroker.cast_nowait(
            (Atom("message_user"), fromName, toUser, message)
        )

    def __checkOSProcessAlive(self) -> None:
        """A watchdog.

        Checks whether the OS process whose ID is stored in self.__shellPid
        is alive. If not, shutdown. If so, check again in WAIT_TIME_SEC seconds.
        """
        if not pid_exists(self.__shellPid):
            self.shutdown()
            exit(0)
        event_loop = asyncio.get_event_loop()
        event_loop.call_later(WAIT_TIME_SEC, self.__checkOSProcessAlive)


def start(game: str, node_name: str, shellPid: int) -> None:
    """Start the game server and its Pyrlang node.

    Args:
        game:      the name of the game to start
        node_name: the name of the Pyrlang node to start.
        shellPid:  the Linux PID of the launching shell.
                We willquit when the shell dies.
    """
    global node
    node = Node(node_name, cookie="Sh4rKM3ld0n")
    ServerCommunicate(game, node_name, shellPid)
    node.run()
