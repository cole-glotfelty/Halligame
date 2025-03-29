# draw to the screen

import curses
import time
import threading

# INTERFACE:
#   __init__():               initializes the class
#   shutdown():               closes the curses window and restores the normal 
#                             settings of the terminal. Don't forget to call!
#   height():                 Gives the height of the terminal window
#   width():                  gives the width of the terminal window
#   clearScreen():            Removes everything from the screen
#   write(row, col, toPrint): Adds the contents of toPrint starting at 
#                             (row, col) on the screen. toPrint must be 
#                             convertible to string.
#   print(toPrint, end="\n"): Prints the contents of toPrint as if it were a 
#                             normal terminal. end is appended to the end of 
#                             toPrint before printing. toPrint must be 
#                             convertible to string.
#   refresh():                Refreshes the screen, making all pending changes 
#                             visible to the user (changes like write, print,  
#                             and clearScreen are not visible until refreshing)


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

# TODO: update write so that it has more of the features and protections of 
#       print (e.g. internal newlines)

# TODO: Add color support

class Screen():
    # gotInputFunc = the function to call when receiving input
    def __init__(self, gotInputFunc):
        self.__gotInput = gotInputFunc

        self.__lock = threading.Lock()

        self.__initCurses()

        self.__stdscr.clear()

        # starting monitoring for user input
        self.__monitorInputThread = threading.Thread(target = self.__monitorInput,
                                                   args = [])
        self.__monitorInputThread.daemon = True # kill thread when main done
        self.__monitorInputThread.start()

        self.__stdscr.move(self.__height() - 1, 0) # set starting printing loc

        # give screen time to set up (instantaneous printing causes weird bugs)
        # idk why
        time.sleep(0.1) 

    def __initCurses(self):
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
    def __monitorInput(self):
        window = curses.newwin(self.__height(), self.__width())
        window.keypad(True) # enable support for special keys like up-arrow.
        while True:
            c = window.getch() # this blocks until input

            if (type(c) == int): # if normal character, convert to char
                c = chr(c)
            self.__gotInput(c)
    
    def height(self):
        with self.__lock:
            return self.__height()

    def width(self):
        with self.__lock:
            return self.__width()

    def __height(self):
        rows, cols = self.__stdscr.getmaxyx()
        return rows

    def __width(self):
        rows, cols = self.__stdscr.getmaxyx()
        return cols

    # clears the screen (removes all text)
    def clearScreen(self):
        with self.__lock:
            self.__stdscr.clear()
            self.__stdscr.move(self.__height() - 1, 0) # move cursor to bottom left

    # prints a string to the screen, starting at (row, col)
    def write(self, row, col, toPrint):
        with self.__lock:
            (prevRow, prevCol) = self.__stdscr.getyx() # save cursor position

            printing = str(toPrint)

            lines = printing.split('\n')
            for i in range(len(lines) - 1):
                self.__write(row + i, col, lines[i])
            self.__write(row + len(lines) - 1, col, lines[-1])

            self.__stdscr.move(prevRow, prevCol) # reset cursor position

    def __write(self, row, col, printing):
        try:
            self.__stdscr.addstr(row, col, printing)
        except curses.error:
            pass # ignore out of bounds characters # TODO

    def print(self, toPrint, end="\n"):
        with self.__lock:
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
