# draw to the screen

import curses
import time
import threading


class Screen():
    # gotInputFunc = the function to call when receiving input
    def __init__(self, gotInputFunc, fps=30.0):
        self.__gotInput = gotInputFunc
        self.__timeBetweenFrames = 1.0 / float(fps)

        self.initCurses()

        self.__stdscr.clear()

        self.__lock = threading.Lock() # prevent drawing of screen while writing
        self.__done = False

        # starting monitoring for user input
        self.__monitorInputThread = threading.Thread(target = self.monitorInput,
                                                   args = [])
        self.__monitorInputThread.daemon = True # kill thread when main done
        self.__monitorInputThread.start()


    def initCurses(self):
        self.__stdscr = curses.initscr() # turn the terminal into a curses window
        curses.noecho() # prevent user input from appearing on screen
        curses.cbreak() # get key input before user types [enter]

        self.__stdscr.nodelay(True) # 
        self.__stdscr.keypad(True) # enable support for special keys like up-arrow.

    # does the opposite of initCurses
    def __del__(self):
        curses.nocbreak()
        self.__stdscr.keypad(False)
        curses.echo()

        curses.endwin()

    # Note that .getch() also refreshes the screen
    def monitorInput(self):
        while True:
            with self.__lock:
                c = self.__stdscr.getch()
                if (c != curses.ERR): # there is user input
                    self.__gotInput(c)

            time.sleep(self.__timeBetweenFrames) # only check once per frame

    # CLIENT FUNCTIONS:
    def height(self):
        return curses.LINES
    
    def width(self):
        return curses.COLS

    # clears the screen (removes all text)
    def clearScreen(self):
        with self.__lock:
            self.__stdscr.clear()
    
    # prints a string to the screen, starting at (row, col)
    def write(self, row, col, stringToPrint):
        with self.__lock:
            self.__stdscr.addstr(row, col, stringToPrint)
