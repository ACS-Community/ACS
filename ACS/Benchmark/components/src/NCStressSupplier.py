#!/usr/bin/env python

import getopt
import sys
from perftestImpl.NotificationServiceStressSupplier import debugprint
from perftestImpl.NotificationServiceStressSupplier import NCStressSupplier


def printUsageAndExit():
    print ""
    print "Usage: " + sys.argv[0] + " [OPTIONS] sizeOfMessage numMessages delayBetweenMessages"
    print ""
    print "   OPTIONS:"
    print ""
    print "   -d | --debug : display extra debug information to stdout as the script is running"
    print "   -h | --help : display this usage statement"
    print ""
    print "   and:"
    print ""
    print "   sizeOfMessage is the number of bytes in each event/message that will be sent"
    print "   numMessages is the total number of messages/events to be sent"
    print "   delayBetweenMessages is the time in milliseconds to pause between messages/events"
    print ""
    sys.exit(2)

def main():
    shortflags = "dh"
    longflags = ['debug','help']
    try:
        opts, args = getopt.getopt(sys.argv[1:], shortflags, longflags)
    except getopt.GetoptError:
        printUsageAndExit()

    debug = False
    for o, a in opts:
        if o in ("-d", "--debug"):
            debug = True
        if o in ("-h", "--help"):
            printUsageAndExit()

    if(len(args) < 3):
        printUsageAndExit()
    sizeOfMessage = int(args[0])
    debugprint("size of message: " + str(sizeOfMessage), debug)
    numMessages = int(args[1])
    debugprint("num messages: " + str(numMessages), debug)
    delayBetweenMessages = int(args[2])
    debugprint("delay between messages: " + str(delayBetweenMessages), debug)

    # instantiate a NCStressSupplier object
    myTestSupplier = NCStressSupplier(debug, sizeOfMessage, numMessages, delayBetweenMessages)

    # send the messages, per the command line arguments provided
    myTestSupplier.sendMessages()

if __name__ == "__main__":
    main()
