# background.py
# 

from pyrlang import Node
from pyrlang.process import Process
from pyrlang.gen.server import GenServerInterface
from term import Atom
from random import randint
from argparse import ArgumentParser
import subprocess
import asyncio
import psutil
import socket
import os, io

WAIT_TIME_SEC = 30

class UserBackground(Process):
    def __init__(self, shellPid : str, username : str, ttyName : str):
        super().__init__()
        node.register_name(self, Atom("backgroundProc"))
        self.__shellPid = shellPid
        self.__serverBroker = None
        self.__username = username
        self.__ttyName = ttyName

        # print("DEBUG: Sending getBroker message")
    
        # Pyrlang has node name and registered name backwards
        self.__sendMessage((Atom("serverbroker@vm-projectweb3"), 
                            Atom("serverbroker")),
                           (Atom("getBrokerPid"), self.pid_))
        self.__serverBroker = None
        
        # print("DEBUG: Sent getBroker message")

        event_loop = asyncio.get_event_loop()
        event_loop.call_soon(self.checkOSProcessAlive)
        
    def handle_one_inbox_message(self, msg):
        # print(f"DEBUG: Got message {msg}")
        if msg[0] == Atom("brokerPid"):
            # print(f"DEBUG: pid is {msg[1]}")
            self.__serverBroker = GenServerInterface(self, msg[1])
            toSend = (Atom("add_user"), self.__username,
                      self.__shellPid, self.pid_)
            # print(f"Sending {toSend}")
            self.__serverBroker.cast_nowait(toSend)
        elif msg[0] == Atom("message"):
            with openTty(self.__ttyName) as f:
                print(f"Got message {msg}", file = f, flush = True)
                print("Reply? Press enter when done.", file = f, flush = True)
                f.readline()

    def checkOSProcessAlive(self):
        if not psutil.pid_exists(int(self.__shellPid)):
            self.shutdown()
            exit(0)
        event_loop = asyncio.get_event_loop()
        event_loop.call_later(WAIT_TIME_SEC, self.checkOSProcessAlive)

    # wrapper for sending a message with correct formatting
    def __sendMessage(self, dest, msg):
        # print(f"DEBUG: Sending Message from Background to {dest}: {msg}")
        node.send_nowait(sender = self.pid_,
                         receiver = dest,
                         message = msg)
    
    def shutdown(self):
        node.destroy()

def openTty(tty):
    return io.TextIOWrapper(io.FileIO(os.open(tty, os.O_NOCTTY | os.O_RDWR),
                                      "r+"))

def start(shellPid : str, tty : str):
    ensure_epmd()
    global node
    hostname = socket.gethostname()
    node = Node(f"{randint(0, 999999):06d}@{hostname}", cookie = "Sh4rKM3ld0n")
    username = subprocess.run(["whoami"], capture_output=True).stdout
    UserBackground(shellPid, username, tty)
    node.run()

# TOOD: put in a utils thing, copied from cli.py
def ensure_epmd():
    epmd_running = False
    for proc in psutil.process_iter(['pid', 'name']):
        if proc.info['name'] == 'epmd':
            epmd_running = True
            break
    if not epmd_running:
        subprocess.Popen(['epmd', '-daemon'])

if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument('ParentShellPid')
    parser.add_argument('tty')
    args = parser.parse_args()
    
    try:
        start(args.ParentShellPid, args.tty)
    except KeyboardInterrupt:
        exit(0)

    