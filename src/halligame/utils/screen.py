# draw to the screen

import curses
import time
import threading

# TODO: resizing a window from smaller to bigger means that .print() continues 
# to print in the middle of the newly enlarged screen (so long as there is no 
# endline to reset it), which seems weird
    # FIX: try checking for terminal resizes with getch and 
    # if (c == curses.KEY_RESIZE): curses.update_lines_cols()

# TODO: when printing without an endline, when the text reaches the end of 
# the bottom line it starts over at the front of that line and write over 
# what it had previously written, instead of the desired behavior of starting 
# a newline
    # FIX: compute ahead of time if printing the string will overreach the end 
    # of the line, and then split the printedd string across those multiple 
    # lines manually

class Screen():
    # gotInputFunc = the function to call when receiving input
    def __init__(self, gotInputFunc):
        self.__gotInput = gotInputFunc

        self.__lock = threading.Lock()

        self.initCurses()

        self.__stdscr.clear()

        # starting monitoring for user input
        self.__monitorInputThread = threading.Thread(target = self.monitorInput,
                                                   args = [])
        self.__monitorInputThread.daemon = True # kill thread when main done
        self.__monitorInputThread.start()

        self.__stdscr.move(self.__height() - 1, 0) # set starting printing loc

        # give screen time to set up (instantaneous printing causes weird bugs)
        # idk why
        time.sleep(0.1) 

    def initCurses(self):
        self.__stdscr = curses.initscr() # turn the terminal into a curses window
        curses.noecho() # prevent user input from appearing on screen
        curses.cbreak() # get key input before user types [enter]

        curses.curs_set(0) # make the cursor invisible

        self.__stdscr.keypad(True) # enable support for special keys like up-arrow.


    # does the opposite of initCurses
    # DON'T FORGET TO CALL!!!!!
    def shutdown(self):
        curses.nocbreak()
        self.__stdscr.keypad(False)
        curses.echo()

        curses.curs_set(2) # make the cursor visible again

        curses.endwin()

    # Note that .getch() also refreshes the screen
    def monitorInput(self):
        window = curses.newwin(self.__height(), self.__width())
        # self.__stdscr.touchwin() # send the main window to the front
        while True:
            c = window.getch()

            if (c != curses.ERR): # there is user input
                self.__gotInput(c)

            time.sleep(0.01) # slow down calls a little bit

    def __height(self):
        rows, cols = self.__stdscr.getmaxyx()
        return rows

    def __width(self):
        rows, cols = self.__stdscr.getmaxyx()
        return cols

    # CLIENT FUNCTIONS:
    def height(self):
        with self.__lock:
            return self.__height()

    def width(self):
        with self.__lock:
            return self.__width()

    # clears the screen (removes all text)
    def clearScreen(self):
        with self.__lock:
            self.__stdscr.clear()
            self.__stdscr.move(self.__height() - 1, 0) # move cursor to bottom left

    # prints a string to the screen, starting at (row, col)
    def write(self, row, col, toPrint):
        with self.__lock:
            (prevRow, prevCol) = self.__stdscr.getyx()
            try:
                self.__stdscr.addstr(row, col, str(toPrint))
            except curses.error:
                pass # ignore out of bounds characters # TODO
            finally:
                self.__stdscr.move(prevRow, prevCol) # reset cursor position

    def print(self, toPrint, end="\n"):
        with self.__lock:
            if (type(toPrint) == int):
                printing = chr(toPrint)
            else:
                printing = str(toPrint)
            
            printing += end

            # need to print lines one at a time because of bug in ncurses:
                # "A bug in ncurses, the backend for this Python module, 
                # can cause SegFaults when resizing windows. This is 
                # fixed in ncurses-6.1-20190511. If you are stuck with an 
                # earlier ncurses, you can avoid triggering this if you do 
                # not call addstr() with a str that has embedded newlines. 
                # Instead, call addstr() separately for each line."
            lines = printing.split('\n')
            for i in range(len(lines) - 1):
                self.__print(lines[i], newline=True)
            self.__print(lines[-1], newline=False)

    # private helper
    def __print(self, printing, newline):
        try:
            self.__stdscr.addstr(printing)

            if (newline):
                # move up one line, and reset cursor to bottom left
                self.__stdscr.move(0, 0) # so deleteln deletes the top line
                self.__stdscr.deleteln()
                self.__stdscr.move(self.__height() - 1, 0)
        except curses.error:
            # reset cursor position to correct loc
            self.__stdscr.move(self.__height() - 1, 0)

    # refreshes the window (sends updates to screen)
    def refresh(self):
        with self.__lock:
            if (self.__stdscr.is_wintouched()): # only refresh if touched
                self.__stdscr.refresh()
