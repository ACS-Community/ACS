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
    print "Usage: " + sys.argv[0] + " [OPTIONS] timeToWaitForMessages"
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

    if(len(args) < 1):
        printUsageAndExit()

    if(managerRef != None):
        debugprint("exporting Manager Reference: " + managerRef, debug)
        os.putenv("MANAGER_REFERENCE", managerRef)

    timeToWait = int(args[0])
    debugprint("time to wait for messages: " + str(timeToWait), debug)

    consumer = NCStressConsumer(outputFileName)
    sleep(timeToWait)

    consumer.disconnect()

if __name__ == "__main__":
    main()
