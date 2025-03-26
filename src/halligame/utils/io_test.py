from screen import Screen
import threading
import time

def main():
    screenInstance = Screen(lambda x:x)

    time.sleep(1)
    screenInstance.write(1, 1, "hello")

    time.sleep(0.5)
    screenInstance.write(10, 10, str(screenInstance.width()))

    time.sleep(0.5)

main()
