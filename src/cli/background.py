"""
background.py allows for erlang messages to be sent and recieved from the
server broker, in the background.
Michael Daniels, 2025-04-19
"""

import asyncio
import io
import os
import socket
from argparse import ArgumentParser
from random import randint
from typing import Any, TypeAlias

from psutil import pid_exists
from pyrlang import Node
from pyrlang.gen.server import GenServerInterface
from pyrlang.process import Process
from term import Atom, Pid

from halligame.utils.common import ensure_epmd, whoami

WAIT_TIME_SEC = 30

DestType: TypeAlias = Atom | tuple[Atom, Atom] | Pid


class UserBackground(Process):
    def __init__(self, shellPid: str, username: str, ttyName: str) -> None:
        super().__init__()
        node.register_name(self, Atom("backgroundProc"))
        self.__shellPid = shellPid
        self.__serverBroker = None
        self.__username = username
        self.__ttyName = ttyName

        # print("DEBUG: Sending getBroker message")

        # Pyrlang has node name and registered name backwards
        self.__sendMessage(
            (Atom("serverbroker@vm-projectweb3"), Atom("serverbroker")),
            (Atom("getBrokerPid"), self.pid_),
        )
        self.__serverBroker = GenServerInterface(self, None)

        # print("DEBUG: Sent getBroker message")

        event_loop = asyncio.get_event_loop()
        event_loop.call_soon(self.__checkOSProcessAlive)

    def handle_one_inbox_message(self, msg: Any) -> None:
        # print(f"DEBUG: Got message {msg}")
        if msg[0] == Atom("brokerPid"):
            # print(f"DEBUG: pid is {msg[1]}")
            self.__serverBroker = GenServerInterface(self, msg[1])
            toSend = (
                Atom("add_user"),
                self.__username,
                self.__shellPid,
                self.pid_,
            )
            # print(f"Sending {toSend}")
            self.__serverBroker.cast_nowait(toSend)
        elif msg[0] == Atom("message"):
            # Tuple is {'message', FromName, Message}
            with openTty(self.__ttyName) as f:
                print(
                    f"Got message from {msg[1]}: {msg[2]}", file=f, flush=True
                )
                print(
                    f'Want to reply? Run "hg write {msg[1]}"!',
                    file=f,
                    flush=True,
                )
                print("(Press enter now.)", file=f, flush=True)
        elif msg[0] == Atom("invite"):
            # Tuple is {'invite', GameName, InviterName, JoinCommand}
            with openTty(self.__ttyName) as f:
                print(
                    f"You were invited to a game of {msg[1]} by {msg[2]}!",
                    file=f,
                    flush=True,
                )
                print(f"Run {msg[3]} to play!", file=f, flush=True)

    def __checkOSProcessAlive(self) -> None:
        """
        Checks whether the OS process whose ID is stored in self.__shellPid
        is alive. If not, shutdown. If so, check again in WAIT_TIME_SEC seconds.
        """
        if not pid_exists(int(self.__shellPid)):
            self.shutdown()
            exit(0)
        event_loop = asyncio.get_event_loop()
        event_loop.call_later(WAIT_TIME_SEC, self.__checkOSProcessAlive)

    #
    def __sendMessage(self, dest: DestType, msg: Any) -> None:
        """
        wrapper for sending a message with correct formatting
        dest is either:
            * an Atom(local registered name),
            * a tuple (Atom(node name), Atom(registered name))
              (note that this backwards from Erlang), or
            * a Pid.
        """
        # print(f"DEBUG: Sending Message from Background to {dest}: {msg}")
        node.send_nowait(sender=self.pid_, receiver=dest, message=msg)

    def shutdown(self) -> None:
        node.destroy()


def openTty(tty: str):  # noqa: ANN201
    """
    Open the TTY whose path is given for writing. The caller must close it.
    """
    return io.TextIOWrapper(
        io.FileIO(os.open(tty, os.O_NOCTTY | os.O_RDWR), "w")
    )


def start(shellPid: str, tty: str) -> None:
    ensure_epmd()

    UserBackground(shellPid, whoami(), tty)
    node.run()


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("ParentShellPid")
    parser.add_argument("tty")
    args = parser.parse_args()

    hostname = socket.gethostname()
    node = Node(f"{randint(0, 999999):06d}@{hostname}", cookie="Sh4rKM3ld0n")

    try:
        start(args.ParentShellPid, args.tty)
    except KeyboardInterrupt:
        exit(0)
