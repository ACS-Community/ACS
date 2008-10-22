#!/usr/bin/env python

import getopt
import sys
import os
import popen2
from time import sleep
from perftestImpl.NotificationServiceStressConsumer import NCStressConsumer
from perftestImpl.NotificationServiceStressConsumer import debugprint

def printUsageAndExit():
    print ""
    print "Usage: " + sys.argv[0] + " [OPTIONS] timeToWaitForMessages numToExpect waitBetweenLogs"
    print ""
    print "   OPTIONS:"
    print ""
    print "   -d | --debug : display extra debug information to stdout as the script is running"
    print "   -h | --help : display this usage statement"
    print "   -o | --out <file>: the name of the output file in which to write the timing results; default is stdout"
    print ""
    print "   and:"
    print ""
    print "   timeToWaitForMessages is the amount of time (in seconds) to wait for all the messages to arrive"
    print "   numToExpect is the number of messages we expect (or hope) will arrive"
    print "   waitBetweenLogs is the time in seconds to wait between receiving one and the next log"
    print ""
    sys.exit(2)

def main():
    shortflags = "dho:m:"
    longflags = ['debug', 'help', 'output=', 'manager=']
    try:
        opts, args = getopt.getopt(sys.argv[1:], shortflags, longflags)
    except getopt.GetoptError, ex:
        print "ERROR parsing command-line arguments"
        print ex
        printUsageAndExit()

    debug = False
    outputFileName = None
    managerRef = None

    for o, a in opts:
        if o in ("-d", "--debug"):
            debug = True
        if o in ("-o", "--output"):
            outputFileName = a
        if o in ("-m", "--manager"):
            managerRef = a
        if o in ("-h", "--help"):
            printUsageAndExit()

    if(len(args) < 2):
        printUsageAndExit()

    if(managerRef != None):
        debugprint("exporting Manager Reference: " + managerRef, debug)
        os.putenv("MANAGER_REFERENCE", managerRef)

    timeToWait = int(args[0])
    debugprint("time to wait for messages: " + str(timeToWait), debug)
    numToExpect = int(args[1])
    debugprint("expected number of messages: " + str(numToExpect), debug)
    if(len(args) > 2):
        waitBetweenLogs = int(args[2])
    else:
        waitBetweenLogs = 0
    debugprint("time to wait between each log: " + str(waitBetweenLogs), debug)

    consumer = NCStressConsumer(numToExpect, outputFileName,waitBetweenLogs)
    sleep(timeToWait)

    consumer.disconnect()

if __name__ == "__main__":
    main()
