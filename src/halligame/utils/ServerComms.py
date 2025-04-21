# ServerComms.py

# Communication from the game server to the communication server/client
# Written by: Will Cordray & Michael Daniels

import asyncio
import importlib
from typing import Any, TypeAlias

from psutil import pid_exists
from pyrlang import Node
from pyrlang.gen.server import GenServerInterface
from pyrlang.process import Process
from term import Atom, Pid

from halligame.games import *  # noqa: F403
from halligame.utils.gameState import GameState

node: Node  # Placeholder for mypy
DestType: TypeAlias = Atom | tuple[Atom, Atom] | Pid

WAIT_TIME_SEC = 30


class ServerCommunicate(Process):
    def __init__(self, gameName: str, nodeName: str, shellPid: int) -> None:
        super().__init__()
        node.register_name(self, Atom("pyServer"))
        self.__gameName = gameName
        self.__nodeName = nodeName
        self.__serverBroker: GenServerInterface
        #: __connectedClients is the tuple (clientPid, username)
        self.__connectedClients: set[Pid, str] = set()
        self.__shellPid = shellPid

        gameModule = importlib.import_module("halligame.games." + gameName)
        self.__serverGameInstance = gameModule.Server(self)

        # Pyrlang has node name and registered name backwards >:(
        self.__sendMessage(
            (Atom("serverbroker@vm-projectweb3"), Atom("serverbroker")),
            (Atom("getBrokerPid"), self.pid_),
        )

        event_loop = asyncio.get_event_loop()
        event_loop.call_soon(self.__checkOSProcessAlive)

    def handle_one_inbox_message(self, msg: Any) -> None:
        # print(f"DEBUG: ServerComms got message {msg}\n")
        match msg:
            case "close":
                self.shutdown()
                exit(0)
            case Atom("brokerPid"), brokerPid:
                self.__serverBroker = GenServerInterface(self, brokerPid)
                self.__serverBroker.cast_nowait(
                    (
                        Atom("register_gameserver"),
                        self.__gameName,
                        self.__nodeName,
                        self.pid_,
                    )
                )
            case Atom("new_client"), clientPid, username:
                self.__serverGameInstance.addClient(clientPid, username)
                node.monitor_process(self.pid_, clientPid)
            case Atom("remove_client"), clientPid, username:
                self.__sendMessage(clientPid, ("quit_confirm", self.pid_))
                self.__connectedClients.discard((clientPid, username))
                self.__serverGameInstance.removeClient(clientPid, username)
            case Atom("message"), (clientPid, message):
                self.__serverGameInstance.gotClientMessage(clientPid, message)
            case Atom("DOWN"), _ref, Atom("process"), fromPid, _reason:
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

    def __sendMessage(self, dest: DestType, msg: Any) -> None:
        """wrapper for sending a message with correct formatting"""
        # print(f"DEBUG: Sending Message from ServerComms to {dest}: {msg}")
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
        """
        Confirm to a client that their join was successful.
        Tells the ServerBroker this.
        """
        self.__connectedClients.add((clientPid, username))
        self.__sendMessage(clientPid, ("confirmed_join", msg))
        self.__serverBroker.cast_nowait(
            (Atom("joined_gameserver"), username, clientPid, self.pid_)
        )

    def sendClientMessage(self, clientPid: Pid, msg: Any) -> None:
        """Send a client a message."""
        self.__sendMessage(clientPid, ("message", msg))

    def shutdown(self) -> None:
        """Shut down this Pyrlang node.
        Informs clients and the serverbroker."""
        self.broadcastMessage("close")

        self.__serverBroker.cast_nowait(
            (Atom("unregister_gameserver"), self.pid_)
        )

        event_loop = asyncio.get_event_loop()
        event_loop.call_later(1, exit)

    def sendMsgViaServerBroker(
        self, fromName: str, toUser: str, message: str
    ) -> None:
        """
        Sends a message to a user via the server broker.

        fromName should be a username (all lowercase), or
        an arbitrary string containing at least one capital letter.
        """
        self.__serverBroker.cast_nowait(
            (Atom("message_user"), fromName, toUser, message)
        )

    def __checkOSProcessAlive(self) -> None:
        """
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

    Parameters:
    game:      the name of the game to start
    node_name: the name of the Pyrlang node to start.
    shellPid:  the Linux PID of the launching shell.
               We willquit when the shell dies.
    """
    global node
    node = Node(node_name, cookie="Sh4rKM3ld0n")
    ServerCommunicate(game, node_name, shellPid)
    node.run()
