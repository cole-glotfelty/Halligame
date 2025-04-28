"""Send and recieve messages from the server broker in the background.

This supports our "write", "invite", and "online" functionality.

Created by:  Michael Daniels, 2025-04-19
"""

import asyncio
import os
import socket
import subprocess
from argparse import ArgumentParser
from random import randint
from typing import Any

from psutil import pid_exists
from pyrlang import Node
from pyrlang.gen.server import GenServerInterface
from pyrlang.process import Process
from term import Atom, Pid

from halligame.utils.misc import ensure_epmd, whoami

#: How long we should wait between checks that the parent hasn't died.
WAIT_TIME_SEC = 30


class UserBackground(Process):
    """Represents a background process."""

    def __init__(self, shellPid: str, username: str) -> None:
        """Set up this background process."""
        super().__init__()
        node.register_name(self, Atom("backgroundProc"))

        #: A string containing the linux PID of the parent shell.
        self.__shellPid: Pid = shellPid
        #: The current user's username.
        self.__username: str = username
        #: Used to communicate with the server broker.
        self.__serverBroker: GenServerInterface

        # Pyrlang has node name and registered name backwards
        self.__sendMessage(
            (Atom("serverbroker@vm-projectweb3"), Atom("serverbroker")),
            (Atom("getBrokerPid"), self.pid_),
        )

        event_loop = asyncio.get_event_loop()
        event_loop.call_soon(self.__checkOSProcessAlive)

    def handle_one_inbox_message(self, msg: Any) -> None:
        """Handle incoming messages."""
        match msg:
            case "brokerPid", brokerPid:
                self.__serverBroker = GenServerInterface(self, brokerPid)
                toSend = (
                    Atom("add_user"),
                    self.__username,
                    self.__shellPid,
                    self.pid_,
                )
                # print(f"Sending {toSend}")
                self.__serverBroker.cast_nowait(toSend)
            case "message", fromUser, content:
                # Tuple is {'message', FromName, Message}
                subprocess.run(
                    ["echo", f"Got message from {fromUser}: {content}"]
                )
                subprocess.run(
                    ["echo", f'Want to reply? Run "hg write {fromUser}"!']
                )
                subprocess.run(["echo", "(Press enter now.)"])
            case "invite", gameName, inviterName, joinCommand:
                # Tuple is {'invite', GameName, InviterName, JoinCommand}
                # gameName = gameName
                # inviterName = inviterName
                # joinCommand = joinCommand.decode()
                subprocess.run(
                    [
                        "echo",
                        f"You were invited to a game of {gameName} "
                        f"by {inviterName}!",
                    ]
                )
                subprocess.run(["echo", f"Run {joinCommand} to play!"])
                subprocess.run(["echo", "(Press enter now.)"])
            case _:
                subprocess.run(["echo", f"Unrecognized message {msg}"])

    def __checkOSProcessAlive(self) -> None:
        """A watchdog.

        Checks whether the OS process whose ID is stored in self.__shellPid
        is alive. If not, shutdown. If so, check again in WAIT_TIME_SEC seconds.
        """
        if not pid_exists(int(self.__shellPid)):
            self.shutdown()
            os._exit(0)
        event_loop = asyncio.get_event_loop()
        event_loop.call_later(WAIT_TIME_SEC, self.__checkOSProcessAlive)

    #
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

    def shutdown(self) -> None:
        """Shut down this Pyrlang node."""
        node.destroy()


def start(shellPid: str) -> None:
    """Start our Pyrlang node."""
    ensure_epmd()

    UserBackground(shellPid, whoami())
    node.run()


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("ParentShellPid")
    args = parser.parse_args()

    hostname = socket.gethostname()
    node = Node(f"{randint(0, 999999):06d}@{hostname}", cookie="Sh4rKM3ld0n")

    try:
        start(args.ParentShellPid)
    except KeyboardInterrupt:
        os._exit(0)
