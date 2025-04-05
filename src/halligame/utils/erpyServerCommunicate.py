# import importlib # allows us to import a module based on the name
# import os # TODO: rm?

import subprocess
from time import sleep

from pyrlang import Node
from pyrlang.process import Process
from term import Atom, Reference
from halligame.games import *
import os
 
 # NOTE: Threads are spawned as Daemons, so they might be killed at any time

class ServerCommunicate(Process):
    def __init__(self, gameName):
        super().__init__()
        self.__commserver_name = f'{os.environ["USER"]}_{gameName}_communicationserver'
        self.__full_commserver_name = self.__commserver_name + "@" +  os.environ["HOST"]
        subprocess.Popen(['bash', '-xc',
                        #   'ls'
                          f'erl -noinput -sname {self.__commserver_name} -setcookie COOKIE -run communicationServer start_link TicTacToe > commserver_output'
                          ],
                          cwd = '../../communicationServer')
        self.get_node().register_name(self, Atom("pyServer"))

        sleep(2) # TODO: needed?

        self.sendMessage((Atom('$gen_cast'), (Atom("replace_server"), self.pid_)))
        # TODO: make this dependent on game provided
        self.__serverGameInstance = TicTacToe.Server(self.sendMessage)

    def handle_one_inbox_message(self, msg):
        print(f"erpyServerComm got message {msg}")
        if msg == Atom("close"):
            self.exit()
            exit(0)
            
        if (msg[0] == "new_client"):
            self.__serverGameInstance.addUser(msg[1]) # send them the user
        elif (msg[0] == "remove_client"):
            self.__serverGameInstance.removeUser(msg[1])
        elif (msg[0] == "event"):
            self.__serverGameInstance.eventIsValid(msg[1][0], msg[1][1])
        elif (msg[0] == "other"):
            self.__serverGameInstance.otherMessageType(msg[1][0], msg[1][1])

    def play(self):
        self.__serverGameInstance.play()

    def sendMessage(self, Msg):
        self.get_node().send_nowait(
                            sender = self.pid_,
                            receiver = (self.__full_commserver_name,
                                        Atom("communicationServer")),
                            message = Msg)
        # self.port.send(Msg)

    def shutDownServer(self):
        self.sendMessage(("terminate", "normal"))

if __name__ == '__main__':
    name = f'{os.environ["USER"]}-TicTacToe-gameserver@{os.environ["HOST"]}'
    n = Node(node_name = name, cookie = "COOKIE")
    # n = Node(node_name = f'{os.environ["USER"]}-TicTacToe-gameserver', cookie = "COOKIE")
    c = ServerCommunicate("TicTacToe")
    print("name: " + name)
    # print("HOST: " + os.environ["HOST"])
    n.run()

    c.shutDownServer()
