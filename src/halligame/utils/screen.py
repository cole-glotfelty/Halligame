# screen.py

# draw to the screen using ncurses
# Written by: Will Cordray
# TODO: Will pls do todos/add docstrings to functions :)

import curses
import time
import threading
import subprocess
import sys

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

# TODO: when printing without an endline, when the text reaches the end of 
# the bottom line it starts over at the front of that line and write over 
# what it had previously written, instead of the desired behavior of starting 
# a newline
    # FIX: compute ahead of time if printing the string will overreach the end 
    # of the line, and then split the printedd string across those multiple 
    # lines manually

class Screen():
    # gotInputFunc = the function to call when receiving input
    def __init__(self, gotInputFunc: callable, gotMouseClickFunc: callable):
        self.__gotInput = gotInputFunc
        self.__gotMouse = gotMouseClickFunc

        self.__lock = threading.Lock()

        self.__initCurses()
        self.__colorSupport = (curses.COLORS >= 8)
        self.__extendedColorSupport = (curses.COLORS >= 128)

        self.__stdscr.clear()

        # starting monitoring for user input
        self.__monitorInputThread = threading.Thread(target = self.__monitorInput,
                                                     args = [])
        self.__monitorInputThread.daemon = True # kill thread when main done
        self.__monitorInputThread.start()

        try:
            self.__stdscr.move(self.__terminalHeight(), 0) # set starting printing loc
        except: # terminal window is being weird
            pass

        self.__colors = {"black": curses.COLOR_BLACK,
                         "blue": curses.COLOR_BLUE,
                         "cyan": curses.COLOR_CYAN,
                         "green": curses.COLOR_GREEN,
                         "magenta": curses.COLOR_MAGENTA,
                         "red": curses.COLOR_RED,
                         "white": curses.COLOR_WHITE,
                         "yellow": curses.COLOR_YELLOW}
        self.__colorPairs = {}
        self.__nextColorID = 10
        self.__nextColorPairID = 10

        self.__clickableRegions = []

        # give screen time to set up (instantaneous printing causes weird bugs)
        # idk why
        time.sleep(0.1)

    def __initCurses(self) -> None:
        self.__stdscr = curses.initscr() # turn the terminal into a curses window
        curses.start_color() # enable color support
        curses.use_default_colors() # keep the curr colors of the terminal
        curses.mousemask(curses.ALL_MOUSE_EVENTS) # enable mouse support
        curses.noecho() # prevent user input from appearing on screen
        curses.cbreak() # get key input before user types [enter]

        curses.curs_set(0) # make the cursor invisible

        self.__stdscr.keypad(True) # enable support for special keys like up-arrow.


    # does the opposite of initCurses
    # DON'T FORGET TO CALL!!!!!
    def shutdown(self) -> None:
        curses.nocbreak()
        self.__stdscr.keypad(False)
        curses.echo()

        curses.curs_set(2) # make the cursor visible again

        curses.endwin()

        if self.__colorSupport:
            # Needed to reset colors to the old state.
            subprocess.run(["reset"], stdout = sys.stdout)

    # Note that .getch() also refreshes the screen
    def __monitorInput(self) -> None:
        window = curses.newwin(self.terminalHeight(), self.terminalWidth())
        window.keypad(True) # enable support for special keys like up-arrow.
        while True:
            c = window.getch() # this blocks until input

            if (c == curses.KEY_MOUSE):
                (_, mcol, mrow, _, bstate) = curses.getmouse()

                regionId = self.__findRegion(mrow, mcol)
                mouseEventType = self.__getMouseEventType(bstate)
                self.__gotMouse(mrow, mcol, regionId, mouseEventType)
            else:
                if (type(c) == int): # if int, try to convert to char
                    try:
                        c = chr(c)
                    except ValueError: # other code, so ignore
                        pass
                self.__gotInput(c)

    def __findRegion(self, mrow, mcol):
        with self.__lock:
            # reverse the list to check more recently defined regions first
            for (row, col, height, width, id) in reversed(self.__clickableRegions):
                if (mrow >= row and mrow < row + height and 
                    mcol >= col and mcol < col + width):
                   return id
        return None

    def __getMouseEventType(self, bstate) -> str:
            # if left click
            if (bstate & curses.BUTTON1_PRESSED != 0 or
                bstate & curses.BUTTON1_CLICKED != 0):
                return "left_click"
            elif (bstate & curses.BUTTON2_PRESSED != 0 or
                  bstate & curses.BUTTON2_CLICKED != 0):
                return "middle_click"
            elif (bstate & curses.BUTTON3_PRESSED != 0 or
                  bstate & curses.BUTTON3_CLICKED != 0):
                return "right_click"
            else:
                return "other"

    def terminalHeight(self) -> int:
        with self.__lock:
            return self.__terminalHeight()

    def terminalWidth(self) -> int:
        with self.__lock:
            return self.__terminalWidth()

    def __terminalHeight(self):
        rows, cols = self.__stdscr.getmaxyx()
        return rows

    def __terminalWidth(self):
        rows, cols = self.__stdscr.getmaxyx()
        return cols

    # clears the screen (removes all text)
    def clearScreen(self) -> None:
        with self.__lock:
            self.__stdscr.clear()
            self.__stdscr.move(self.__terminalHeight() - 1, 0) # move cursor to bottom left

    # prints a string to the screen, starting at (row, col)
    def write(self, row: int, col: int, toPrint, colorPairId=None) -> None:
        with self.__lock:
            if not self.__colorSupport or colorPairId not in self.__colorPairs:
                colorPairId = None
            
            (prevRow, prevCol) = self.__stdscr.getyx() # save cursor position

            printing = str(toPrint)

            lines = printing.split('\n')
            for i in range(len(lines) - 1):
                self.__write(row + i, col, lines[i], colorPairId)
            self.__write(row + len(lines) - 1, col, lines[-1], colorPairId)

            self.__stdscr.move(prevRow, prevCol) # reset cursor position

    def __write(self, row: int, col: int, printing: str, colorPairId=None) -> None:
        try:
            if (colorPairId == None):
                self.__stdscr.addstr(row, col, printing)
            else:
                colorPairCode = self.__colorPairs[colorPairId]
                self.__stdscr.addstr(row, col, printing, curses.color_pair(colorPairCode))
        except curses.error:
            pass # ignore out of bounds characters # TODO

    def print(self, toPrint, end="\n") -> None:
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
    def __print(self, printing: str, newline: bool) -> None:
        try:
            self.__stdscr.addstr(printing)

            if (newline):
                # move up one line, and reset cursor to bottom left
                self.__stdscr.move(0, 0) # so deleteln deletes the top line
                self.__stdscr.deleteln()
                self.__stdscr.move(self.__terminalHeight() - 1, 0)
        except curses.error:
            # reset cursor position to correct loc
            self.__stdscr.move(self.__terminalHeight() - 1, 0)

    # refreshes the window (sends updates to screen)
    def refresh(self) -> None:
        with self.__lock:
            if (self.__stdscr.is_wintouched()): # only refresh if touched
                self.__stdscr.refresh()

    # rgb between 0 and 1000
    def addColor(self, r: int, g: int, b: int, colorId) -> None:
        if not self.__extendedColorSupport:
            return
        with self.__lock:
            self.__colors[colorId] = self.__nextColorID
            r = self.__scaleColor(r)
            g = self.__scaleColor(g)
            b = self.__scaleColor(b)
            
            curses.init_color(self.__nextColorID, r, g, b)

            self.__nextColorID += 1

    # scales a color from between 0 and 256 to between 0 and 1000
    def __scaleColor(self, color):
        return min(max(int(color * (1000.0 / 256.0)), 0), 1000)

    def addColorPair(self, foreground, background, pairId) -> None:
        if not self.__extendedColorSupport:
            return
        with self.__lock:
            self.__colorPairs[pairId] = self.__nextColorPairID

            fgColorCode = self.__colors[foreground]
            bgColorCode = self.__colors[background]

            curses.init_pair(self.__nextColorPairID, fgColorCode, bgColorCode)

            self.__nextColorPairID += 1

    def setStyle(self, colorPairId) -> None:
        with self.__lock:
            if not self.__colorSupport or colorPairId not in self.__colorPairs:
                return
            colorPairCode = self.__colorPairs[colorPairId]

            self.__stdscr.bkgd(' ', curses.color_pair(colorPairCode))

    def addClickableRegion(self, row: int, col: int, 
                           height: int, width: int, id) -> None:
        with self.__lock:
            self.__clickableRegions.append((row, col, height, width, id))

    def clearClickableRegions(self):
        with self.__lock:
            self.__clickableRegions = []

    def getCenteredRow(self, toPrint):
        with self.__lock:
            return (self.__terminalHeight() // 2) - (len(str(toPrint).split("\n")) // 2)

    def getCenteredCol(self, toPrint):
        with self.__lock:
            toPrintSplit = str(toPrint).split("\n")

            if (len(toPrintSplit) > 0):
                return (self.__terminalWidth() // 2) - (len(toPrintSplit[0]) // 2)
            else:
                return (self.__terminalWidth() // 2)
