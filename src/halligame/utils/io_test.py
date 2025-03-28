from screen import Screen
import threading
import time

def main():
    screenInstance = Screen(lambda input: gotInput(screenInstance, input))

    time.sleep(1)
    screenInstance.print("hello")
    screenInstance.refresh()

    time.sleep(1)
    screenInstance.refresh()
    screenInstance.print(screenInstance.width())
    screenInstance.refresh()

    screenInstance.print("hellothere\nhi", end="")
    screenInstance.print("!#4123i4u12y34y12y35987yjfabjasbdif uhabwiouerh siuhv abviauskhdnfxasukdh broiewufhb\nasidv hasjkvdh askjd hiqouwebfh iwenudhvlkj sandklbv qwei usdouvhaisuhefjkc bvasjdh vbaiuwefbn jkbasjdfhlahkjsdfhakjsdhfjklashdfkjashdfkljashdfkjashdfkjlahsdfkljhasdfkljhasdkjfhaskljdfhiuqwehriuh iunsvdi hasdivbasjdvhabsdv bhi")
    screenInstance.refresh()
    time.sleep(1)

    screenInstance.write(40, 100, "I'm here!\nAnd now here!")
    screenInstance.refresh()

    
    # while (True):
    #     screenInstance.write(40, 100, "I'm here!")
    #     screenInstance.print(screenInstance.height())
    #     # time.sleep(0.1)
    #     screenInstance.print(screenInstance.width())
    #     screenInstance.refresh()
    #     time.sleep(0.1)

    time.sleep(100)

    screenInstance.shutdown()

def gotInput(screenInstance, input):
    screenInstance.print(input, end="")
    screenInstance.refresh()

main()
