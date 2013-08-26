#!/usr/bin/env python

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
from time import sleep
import getopt
import sys
import os
import popen2
import traceback
import acstime
from Acspy.Util.ACSCorba import getManagerCorbaloc

# this is a modified version of that found in acs time utilities; the one there
# has a bug... remove this when / if we fix the bug. The bug is that it gives
# precision of whole numbers due to not converting to float.
def duration2py(duration):
        '''
        Convert an ACS duration to a Python duration.

        Parameters: duration is in units of 100 nanoseconds

        Return: duration converted to seconds

        Raises: Nothing
        '''
        if isinstance(duration, long):
            duration = acstime.Duration(duration)

        sec = float(duration.value) / float(10000000L)
        return sec

# function to determine whether all of the messages we are expecting have been received
def verifyMessagesReceived(fileNames, numToExpect, debug):
    allReceived = []
    for i in range(0, len(fileNames)):
        allReceived.append(False)

    for fileName in fileNames:
        file = open(fileName, "r")
        lines = file.readlines()
        if(lines.count("push_structured_event called!\n") == numToExpect):
            allReceived[fileNames.index(fileName)] = True
        file.close()

    retVal = True
    for i in allReceived:
       if(i == False):
           retVal = False

    return retVal
    
# function to print the usage statement (and exit)
def printUsageAndExit():
    print ""
    print "Usage: " + sys.argv[0] + " [OPTIONS] numMsgs sizeOfMsg numLocalConsumers numRemoteConsumers delayBetweenMsgs"
    print ""
    print "   OPTIONS:"
    print ""
    print "   -d | --debug : display extra debug information to stdout as the script is running"
    print "   -h | --help : display this usage statement"
    print "   -r | --remoteHost <hostname>: the name of the host on which to run remote consumers"
    print ""
    print "   and:"
    print ""
    print "   numMsgs is the number of messages to send using the notification service"
    print "   sizeOfMsg is the size (in bytes) of each message which will be sent on the notification service"
    print "   numLocalConsumers is the number of consumers to run on the local host"
    print "   numRemoteConsumers is the number of consumers to run on the remote host"
    print "   delayBetweenMsgs is the time (in milliseconds) to pause between sending each msg"
    print ""
    sys.exit(2)

# function to print something out if we're in debug mode
def debugprint(stringToPrint, debugMode):
   if(debugMode == True):
      print "DEBUG: " + stringToPrint

# function to prepare memory output files for plotting by gnuplot
def prepMemOutputFiles(numLocalConsumers, numRemoteConsumers, debug):
    sedCmd = "cat ./tmp/notifyServiceMemory-local.orig.log | sed -e /^[^0-9]/d | sed -e /^$/d > ./tmp/notifyServiceMemory-local.log"
    os.system(sedCmd)

    count = 0
    for i in range(0, numLocalConsumers):
        os.system("cat ./tmp/consumerMemory" + str(i) + "-local.orig.log | sed -e /^[^0-9]/d | sed -e /^$/d  > ./tmp/consumerMemory" \
            + str(i) + "-local.log")
        count += 1

    count = 0
    for i in range(0, numRemoteConsumers):
        os.system("cat ./tmp/consumerMemory" + str(i) + "-remote.orig.log | sed -e /^[^0-9]/d | sed -e /^$/d > ./tmp/consumerMemory" \
            + str(i) + "-remote.log")
        count += 1

# function to prepare the sar output files for plotting by gnuplot
def prepSarOutputFiles(numLocalConsumers, numRemoteConsumers, debug):
    sedCmd = "cat ./tmp/notifyServiceCpu-local.orig.log \
         | sed -e /Linux/d | sed -e /PID/d | sed -e /^[^0-9]/d | sed -e /^$/d > ./tmp/notifyServiceCpu-local.log"
    os.system(sedCmd)

    count = 0
    for i in range(0, numLocalConsumers):
        os.system("cat ./tmp/consumerCpu" + str(i) + "-local.orig.log | sed -e /Linux/d | sed -e /PID/d  \
            | sed -e /^[^0-9]/d | sed -e /^$/d > ./tmp/consumerCpu" + str(i) + "-local.log")
        count += 1

    count = 0
    for i in range(0, numRemoteConsumers):
        os.system("cat ./tmp/consumerCpu" + str(i) + "-remote.orig.log | sed -e /Linux/d | sed -e /PID/d  \
           | sed -e /^[^0-9]/d | sed -e /^$/d > ./tmp/consumerCpu" + str(i) + "-remote.log")
        count += 1

# function to find the proper column in sar output for cpu usage (%user)
def findCpuColumnInSarOutput(filename):
   grepCmd = 'grep "\%user" ' + filename
   resultsPopen = popen2.Popen3(grepCmd)
   results = resultsPopen.fromchild.readlines()
   if((results == None) or (len(results) == 0)):
      print "FATAL ERROR: sar output for file: " + filename + " is not in the proper format"
      sys.exit(2)
   resultVals = results[0].split()
   retVal = -1
   for i in range(0, len(resultVals)):
      if(resultVals[i] == "%user"):
          retVal = i+1
          break
   return retVal

# function to prep output files for plotting by gnuplot
def prepOutputFiles(numConsumers, label, debug):
    for i in range(0, numConsumers):
        filename = './tmp/consumer' + str(i) + '-' + label + '.out'
        debugprint("prepping output file: " + filename, debug)
        sedCmd = 'cat ' + filename + ' | sed -e /addSubscription/d | sed -e /initCORBA/d | sed -e /^[^0-9\-]/d | sed -e /^$/d' + ' > ' + filename + '.tmp'
        debugprint("with sed command: " + sedCmd, debug)
        os.system("cp " + filename + " " + filename + ".orig")
        os.system(sedCmd)
        os.system('mv ' + filename + '.tmp ' + filename)

# function to calculate the total time (in seconds) of receiving all events
def calculateTotalTimes(numConsumers, label, debug):
    totalTimes = []
    if(numConsumers <= 0):
        return None

    for i in range(0, numConsumers):
        # read in the data from the file, as a list of strings
        fileName = './tmp/consumer' + str(i) + '-' + label + '.out'
        inputFile = open(fileName, "r")
        strdata = inputFile.readlines()

        firstReceptionTime = 0L
        lastReceptionTime = 0L

        # find the first occurence of "reception time:" in the list - this is the last one received
        for line in strdata:
            if line.startswith("reception time:"):
               lastReceptionTime = long(line.split(":")[1])

        # find the last occurence of "reception time:" in the list - this is the first one received
        strdata.reverse()
        for line in strdata:
            if line.startswith("reception time:"):
               firstReceptionTime = long(line.split(":")[1])

        debugprint("first reception time for consumer " + label + str(i) + " is:  " + str(firstReceptionTime) + \
                   " and last reception: " + str(lastReceptionTime), debug)

        # calculate the difference in the times in seconds
        totalTimes.append(float(duration2py(long(lastReceptionTime - firstReceptionTime))))
    
    return totalTimes


# function to calculate the reception time summary information (e.g. averages, max, min, etc.)
def calculateReceptionTimeSummary(numConsumers, label, debug):
    if(numConsumers <= 0):
        return None

    summaryData = []
    for i in range(0, numConsumers):
        # read in the data from the file, as a list of strings
        fileName = './tmp/consumer' + str(i) + '-' + label + '.out'
        inputFile = open(fileName, "r")
        strdata = inputFile.readlines()

        # convert string data to float
        floatdata = []
        for j in range(0, len(strdata)):
               floatdata.append(float(strdata[j]))

        # calculate the sum of all the data points
        sum = 0.0
        for datapoint in floatdata:
            sum += datapoint

        # calculate the average 
        if(len(floatdata) == 0):
           print "FATAL ERROR processing data: data file has no data points!"
           sys.exit(2)

        average = float(sum) / float(len(floatdata))
        debugprint("Average for: " + fileName + " is: " + str(average) + " and max is: " + str(max(floatdata)) \
            + " and min is: " + str(min(floatdata)) + " and total summed is: " + str(len(floatdata)), debug)
        summaryData.append([average, max(floatdata), min(floatdata), i, len(floatdata)])

    return summaryData

# function to plot the sar (cpu usage) information
def plotSarResults(numLocalConsumers, numRemoteConsumers, debug):
    plotConsumerCpuResults(numLocalConsumers, "local", debug)
    plotConsumerCpuResults(numRemoteConsumers, "remote", debug)

    gnuplotCommandFile = open("./tmp/gnuplot-notifyServiceCpu.txt", "w")
    print >> gnuplotCommandFile, 'set terminal png'
    print >> gnuplotCommandFile, 'set out "../doc/notificationServiceCpu.png"'
    print >> gnuplotCommandFile, 'set title "Cpu performance for the notification service"'
    print >> gnuplotCommandFile, 'set timefmt "%H:%M:%S"'
    print >> gnuplotCommandFile, 'set xdata time'
    print >> gnuplotCommandFile, 'set format x "%M:%S"'
    print >> gnuplotCommandFile, 'set xlabel "Time (seconds)"'
    print >> gnuplotCommandFile, 'set ylabel "Cpu usage (percent)"'
    cpuColumn = findCpuColumnInSarOutput("./tmp/notifyServiceCpu-local.orig.log")
    print >> gnuplotCommandFile, 'plot "./tmp/notifyServiceCpu-local.log" using 1:' + str(cpuColumn) + ' title "%user: notification service" with lines'
    gnuplotCommandFile.close()
    os.system("gnuplot ./tmp/gnuplot-notifyServiceCpu.txt")

# function to plot the memory results
def plotMemoryResults(numLocalConsumers, numRemoteConsumers, debug):
    plotConsumerMemoryResults(numLocalConsumers, "local", debug)
    plotConsumerMemoryResults(numRemoteConsumers, "remote", debug)

    gnuplotCommandFile = open("./tmp/gnuplot-notifyServiceMemory.txt", "w")
    print >> gnuplotCommandFile, 'set terminal png'
    print >> gnuplotCommandFile, 'set out "../doc/notificationServiceMemory.png"'
    print >> gnuplotCommandFile, 'set title "Memory usage of the notification service"'
    print >> gnuplotCommandFile, 'set xlabel "Time (sampled every 1 second)"'
    print >> gnuplotCommandFile, 'set timefmt "%s"'
    print >> gnuplotCommandFile, 'set xdata time'
    print >> gnuplotCommandFile, 'set ylabel "memory allocated (kb)"'
    print >> gnuplotCommandFile, 'plot "./tmp/notifyServiceMemory-local.log" using 1:2 title "Total memory usage" with lines, ' \
             + '"./tmp/notifyServiceMemory-local.log" using 1:3 title "Resident memory size" with lines'
    gnuplotCommandFile.close()
    os.system("gnuplot ./tmp/gnuplot-notifyServiceMemory.txt")

# function to plot the memory usage for consumer(s)
def plotConsumerMemoryResults(numConsumers, label, debug):
    if(numConsumers <= 0 ):
        return

    gnuplotCommandFile = open("./tmp/gnuplotConsumerMemory-" + label + ".txt", "w")
    print >> gnuplotCommandFile, 'set terminal png'
    print >> gnuplotCommandFile, 'set out "../doc/consumerMemory-' + label + '.png"'
    print >> gnuplotCommandFile, 'set title "Memory usage for ' + label + ' consumer(s)"'
    print >> gnuplotCommandFile, 'set xlabel "Time (sampled every 1 second)"'
    print >> gnuplotCommandFile, 'set timefmt "%s"'
    print >> gnuplotCommandFile, 'set xdata time'
    print >> gnuplotCommandFile, 'set ylabel "memory allocated (kb)"'

    if(numConsumers == 1):
        plotString = 'plot "./tmp/consumerMemory0-' + label + '.log" using 1:2 title "Total mem consumer: 0 ' \
            + '" with lines, "./tmp/consumerMemory0-' + label + '.log" using 1:3 title "Resident mem size consumer: 0" with lines'
        print >> gnuplotCommandFile, plotString
    else:
        for i in range(0, numConsumers):
            if(i == 0):
                plotString = 'plot "./tmp/consumerMemory0-' + label \
                    + '.log" using 1:2 title "Total mem consumer: 0" with lines, "./tmp/consumerMemory0-' + label \
                    + '.log" using 1:3 title "Resident mem size consumer: 0"'
            else:
                plotString = '"./tmp/consumerMemory' + str(i) + '-' + label + '.log" using 1:2 title "Total mem consumer: ' + str(i) \
                    + ' ' + label + '" with lines, "./tmp/consumerMemory' + str(i) + '-' + label \
                    + '.log" using 1:3 title "Resident mem size consumer: ' + str(i) + ' ' + label + '"'
            if(i < numConsumers - 1):
                print >> gnuplotCommandFile, plotString + " with lines, \\"
            else: 
                print >> gnuplotCommandFile, plotString + " with lines"

    gnuplotCommandFile.close()
    os.system("gnuplot ./tmp/gnuplotConsumerMemory-" + label + ".txt")

# function to plot the cpu statistics of consumer(s)
def plotConsumerCpuResults(numConsumers, label, debug):
    if(numConsumers <= 0 ):
        return

    gnuplotCommandFile = open("./tmp/gnuplotConsumerCpu-" + label + ".txt", "w")
    print >> gnuplotCommandFile, 'set terminal png'
    print >> gnuplotCommandFile, 'set out "../doc/consumerCpu-' + label + '.png"'
    print >> gnuplotCommandFile, 'set title "Cpu performance for ' + label + ' consumer(s)"'
    print >> gnuplotCommandFile, 'set xlabel "Time (sampled every 1 second)"'
    print >> gnuplotCommandFile, 'set timefmt "%H:%M:%S"'
    print >> gnuplotCommandFile, 'set xdata time'
    print >> gnuplotCommandFile, 'set format x "%M:%S"'
    print >> gnuplotCommandFile, 'set ylabel "Cpu usage (percent)"'

    cpuColumn = findCpuColumnInSarOutput("./tmp/consumerCpu0-" + label + ".orig.log")
    if(numConsumers == 1):
        plotString = 'plot "./tmp/consumerCpu0-' + label + '.log" using 1:' + str(cpuColumn) + ' title "%user ' + label + '" with lines'
        print >> gnuplotCommandFile, plotString
    else:
        for i in range(0, numConsumers):
            if(i == 0):
                plotString = 'plot "./tmp/consumerCpu' + str(i) + '-' + label + '.log" using 1:' + str(cpuColumn) + ' title "%user ' \
                   + str(i) + ' ' + label + '"'
            else:
                plotString = '"./tmp/consumerCpu' + str(i) + '-' + label + '.log" using 1:' + str(cpuColumn) + ' title "%user ' \
                   + str(i) + ' ' + label + '"'
            if(i < numConsumers - 1):
                print >> gnuplotCommandFile, plotString + " with lines, \\"
            else: 
                print >> gnuplotCommandFile, plotString + " with lines"

    gnuplotCommandFile.close()
    os.system("gnuplot ./tmp/gnuplotConsumerCpu-" + label + ".txt")

# function to plot the event reception times
def plotConsumerReceptionResults(numConsumers, label, numMsgs, outputDir):
    if(numConsumers <= 0):
        return

    gnuplotCommandFile = open("./tmp/gnuplot-" + label + ".txt", "w")
    print >> gnuplotCommandFile, 'set terminal png'
    print >> gnuplotCommandFile, 'set out "' + outputDir + '/consumerReception-' + label + '.png"'
    print >> gnuplotCommandFile, 'set title "Arrival time for events on ACS notification channel"'
    print >> gnuplotCommandFile, 'set xlabel "Event No."'
    print >> gnuplotCommandFile, 'set xrange [0:' + str(numMsgs) + ']'
    print >> gnuplotCommandFile, 'set ylabel "Time elapsed between send and reception (seconds)"'
    if(numConsumers == 1):
        plotString = 'plot ' + '"./tmp/consumer0-' + label + '.out" title "Consumer ' + label + '"'
        print >> gnuplotCommandFile, plotString
    else:
        for i in range(0, numConsumers):
            if(i == 0):
                plotString = 'plot ' + '"./tmp/consumer' + str(i) + '-' + label + '.out" title "Consumer ' + str(i) + ' ' + label + '"'
            else:
                plotString = '"./tmp/consumer' + str(i) + '-' + label + '.out" title "Consumer ' + str(i) + ' ' + label + '"'
            if(i < numConsumers - 1):
                print >> gnuplotCommandFile, plotString,
                print >> gnuplotCommandFile, ', ',
            else: 
                print >> gnuplotCommandFile, plotString

    gnuplotCommandFile.close()
    os.system("gnuplot ./tmp/gnuplot-" + label + ".txt")

# function to generate the final report as HTML
def generateHTML(numLocalConsumers, numRemoteConsumers, numMsgs, sizeOfMsg, delayBetweenMsgs, localSummaryData, \
                 remoteSummaryData, totalTimesLocal, totalTimesRemote, outputDir):
    if(numLocalConsumers <= 0 and numRemoteConsumers <=0):
        return

    htmlFile = open(outputDir + "/notificationServiceReport.html", "w")
    print >> htmlFile, '<html><head><title>ACS Notification Service Benchmarks</title></head><body>'
    print >> htmlFile, '<H1>Test Summary:</H1>'
    print >> htmlFile, 'For this test, we sent: ' + str(numMsgs) + ' messages of size: ' + str(sizeOfMsg) + \
             ' bytes, with a delay of: ' + str(delayBetweenMsgs) + ' milliseconds between each message sent. <br>'
    hostNamePopen = popen2.Popen3("hostname -f")
    hostStdout = hostNamePopen.fromchild.readlines()
    host = "unknown"
    if(len(hostStdout) >= 1):
       host = hostStdout[0].strip()
    print >> htmlFile, 'The acs services were run on the host: ' + host + ' <br>' 
    print >> htmlFile, 'There were: ' + str(numLocalConsumers) + ' local consumers and: ' + str(numRemoteConsumers) + ' remote consumers.'

    print >> htmlFile, '<H2>Reception times: </H2>'
    count = 0
    if(localSummaryData != None):
        for avg, max, min, consumerNumber, totalReceived  in localSummaryData:
            print >> htmlFile, 'Local consumer ' + str(consumerNumber) + ': avg: ' + str(avg) + '; maximum: ' + str(max) + '; minimum: ' \
            + str(min) + ' (seconds) <br>'
            if(totalTimesLocal[count] != 0 and totalTimesRemote[count] != None):
                print >> htmlFile, 'Received: ' + str(totalReceived) + ' events in: ' + str(totalTimesLocal[count]) + \
                         ' seconds for a rate of: ' + str(float(totalReceived/totalTimesLocal[count])) + ' events/second<br>'
            else:
                print >> htmlFile, 'Unable to calculate events/second (rate) because of test failure.<br>'
            count = count + 1
            if(totalReceived != numMsgs):
                print >> htmlFile, '<font color=RED>ERROR</font> local consumer ' + str(consumerNumber) + ' expected: ' + str(numMsgs) \
                    + ' events, but only received: ' + str(totalReceived) + ' events<br><br>'
            else:
                print >> htmlFile, '<font color=GREEN>SUCCESS</font> local consumer ' + str(consumerNumber) + ' expected: ' \
                    + str(numMsgs) + ' events, and received: ' + str(totalReceived) + ' events<br><br>'

    count = 0
    if(remoteSummaryData != None):
        for avg, max, min, consumerNumber, totalReceived in remoteSummaryData:
            print >> htmlFile, 'Remote consumer ' + str(consumerNumber) + ': avg: ' + str(avg) + '; maximum: ' + str(max) + '; minimum: '\
            + str(min) + ' (seconds) <br>'
            if(totalTimesRemote[count] != 0 and totalTimesRemote[count] != None ):
                print >> htmlFile, 'Received: ' + str(totalReceived) + ' events in: ' + str(totalTimesRemote[count]) + \
                         ' seconds for a rate of: ' + str(float(totalReceived/totalTimesRemote[count])) + ' events/second<br>'
            else:
                print >> htmlFile, 'Unable to calculate events/second (rate) because of test failure.<br>'
            count = count + 1
            if(totalReceived != numMsgs):
                print >> htmlFile, '<font color=RED>ERROR</font> remote consumer ' + str(consumerNumber) + ' expected: ' + str(numMsgs) \
                    + ' events, but only received: ' + str(totalReceived) + ' events <br><br>'
            else:
                print >> htmlFile, '<font color=GREEN>SUCCESS</font> remote consumer ' + str(consumerNumber) + ' expected: ' \
                    + str(numMsgs) + ' events, and received: ' + str(totalReceived) + ' events<br><br>'

    print >> htmlFile, '<H2>CPU load of CORBA notification service process: </H2>'
    print >> htmlFile, '<img src="notificationServiceCpu.png"/>'
    print >> htmlFile, '<H2>Memory usage of CORBA notification service process: </H2>'
    print >> htmlFile, '<img src="notificationServiceMemory.png"/>'
    if(numLocalConsumers >= 1):
        print >> htmlFile, '<H2>Reception times for local consumer(s): </H2>'
        print >> htmlFile, '<img src="consumerReception-local.png"/>'
        print >> htmlFile, '<H2>CPU Performance of local consumer(s): </H2>'
        print >> htmlFile, '<img src="consumerCpu-local.png"/>'
        print >> htmlFile, '<H2>Memory usage of local consumer(s): </H2>'
        print >> htmlFile, '<img src="consumerMemory-local.png"/>'
    if(numRemoteConsumers >= 1):
        print >> htmlFile, '<H2>Reception times for remote consumer(s): </H2>'
        print >> htmlFile, '<img src="consumerReception-remote.png"/>'
        print >> htmlFile, '<H2>CPU Performance of remote consumer(s): </H2>'
        print >> htmlFile, '<img src="consumerCpu-remote.png"/>'
        print >> htmlFile, '<H2>Memory usage of remote consumer(s): </H2>'
        print >> htmlFile, '<img src="consumerMemory-remote.png"/>'
    print >> htmlFile, '</body></html>'

# main method - TODO: refactor to make it smaller and more concise, splitting into separate functions for 
#               various large chunks of functionality
def main():
    shortflags = "dhr:"
    longflags = ['debug', 'help', 'remoteHost=']
    try:
        opts, args = getopt.getopt(sys.argv[1:], shortflags, longflags)
    except getopt.GetoptError, ex:
        print "ERROR parsing command-line arguments"
        print ex
        printUsageAndExit()

    debug = False
    remoteHost = None
    for o, a in opts:
        if o in ("-d", "--debug"):
            debug = True
        if o in ("-r", "--remoteHost"):
            remoteHost = a
        if o in ("-h", "--help"):
            printUsageAndExit()

    if(len(args) < 5):
        printUsageAndExit()

    numMsgs = int(args[0])
    debugprint("Will send: " + str(numMsgs) + " messages", debug)
    sizeOfMsg = int(args[1])
    debugprint("Msg size will be: " + str(sizeOfMsg) + " bytes", debug)
    numLocalConsumers = int(args[2])
    debugprint("Will use: " + str(numLocalConsumers) + " local consumers", debug)
    numRemoteConsumers = int(args[3])
    debugprint("Will use: " + str(numRemoteConsumers) + " remote consumers", debug)
    delayBetweenMsgs = int(args[4])
    debugprint("Will pause: " + str(delayBetweenMsgs) + " milliseconds between sending each msg ", debug)

    if((remoteHost == None and numRemoteConsumers > 0) or (remoteHost != None and numRemoteConsumers == 0)):
        print "" 
        print "ERROR: specifying remote host properties."
        print ""
        printUsageAndExit()

    # make the temporary directory for intermediate files and logs
    os.system("rm -rf ./tmp; mkdir ./tmp")

    # determine if ACS is running
    debugprint("Checking to see if ACS is running...", debug)
    acsStatusProcess = popen2.Popen4("acsStatus | grep ERROR")
    acsStatusOutput = acsStatusProcess.fromchild.readlines()
    if(acsStatusOutput != None and len(acsStatusOutput) >= 1):
        # acs is not running; exit
        print "ERROR: ACS is not running; start ACS and then run this script."
        sys.exit(2)

    # these imports must come after we check for ACS for some strange reason; else all fails due to
    # notification service client doing some strange things
    from perftestImpl.NotificationServiceStressConsumer import NCStressConsumer
    from perftestImpl.NotificationServiceStressSupplier import NCStressSupplier

    # calculate time to wait based on a heuristic: 100 seconds plus the number of messages 
    # times (the quantity of) the delay plus 5 ms - this formula is fairly arbitrary but seems to be
    # (more than) enough.
    if(sizeOfMsg > 100000):
        largeMsgFactor = sizeOfMsg / 100000
    else:
        largeMsgFactor = 0
    debugprint("Calculating the time to wait for messages...", debug)
    timeToWait = 120 + numMsgs * (delayBetweenMsgs + largeMsgFactor)
    debugprint("Max 'wait' time for receiving all msgs will be: " + str(timeToWait) + " seconds", debug)

    managerCorbaLoc = getManagerCorbaloc()
    userName = os.environ["USER"]

    # start monitoring the notification service and consumers, using sar
    debugprint("starting local monitoring for notify service ", debug)
    notifyGrepCmd = "ps -ef | grep Notify_Service | grep -v Logging | grep -v Archive | grep -v logging | grep -v grep | awk '{ print $2 }'"
    notifyGrepProcess = popen2.Popen3(notifyGrepCmd)
    notifyPids = notifyGrepProcess.fromchild.readline().strip()
    debugprint("pid for notification service is: " + str(notifyPids), debug)
    notifySarCmd = "exec sar -x " + notifyPids + " 1 9999 >& ./tmp/notifyServiceCpu-local.orig.log"
    debugprint("sar command is: " + notifySarCmd, debug)
    notifySarProcess = popen2.Popen3(notifySarCmd)
    notifyMonitorProcess = popen2.Popen3("exec monitorProcess " + notifyPids + " 1 ./tmp/notifyServiceMemory-local.orig.log")

    # start the local consumers
    localProcesses = []
    ncChannelCreated = False
    for i in range(0, numLocalConsumers):
       debugprint("starting local consumer " + str(i), debug)
       cmdString = "NCStressConsumer -d " + str(timeToWait) + " " + str(numMsgs) + " >& ./tmp/consumer" + str(i) + "-local.out"
       debugprint("using local command " + cmdString, debug)
       localProcesses.append(popen2.Popen4(cmdString))
       # special case to stop race condition during initial creation of NC channel - it seems the first consumer creates 
       # the channel and if the others start too fast, they get 'stuck'; must wait for the channel to be created before 
       # starting subsequent consumers. This race condition only occurs if ACS has just been started; subsequent runs are ok.
       if(i == 0):
           sleep(5)
           ncChannelCreated = True

    # start the remote consumers
    remoteProcesses = []
    for i in range(0, numRemoteConsumers):
       debugprint("starting remote consumer " + str(i), debug)
       sshCmdString = 'ssh ' + userName + '@' + remoteHost + ' "NCStressConsumer -d -m ' + managerCorbaLoc + ' ' \
           + str(timeToWait) + " " + str(numMsgs) + '"' + ' >& ./tmp/consumer' + str(i) + '-remote.out'
       debugprint("using command: " + sshCmdString, debug)
       remoteProcesses.append(popen2.Popen3(sshCmdString))
       # special case to stop race condition during initial creation of NC channel - it seems the first consumer creates 
       # the channel and if the others start too fast, they get 'stuck'; must wait for the channel to be created before 
       # starting subsequent consumers. This race condition only occurs if ACS has just been started; subsequent runs are ok.
       if(i == 0 and ncChannelCreated == False):
           sleep(5)

    # pause to allow all consumers to get fully up and running 
    debugprint("sleeping a bit to let all consumers start...", debug)
    for i in range (1, (numLocalConsumers + numRemoteConsumers)):
        debugprint(".", debug)
        sleep(3)

    # monitor the local consumers
    localConsumerMonitors = []
    localConsumerMemoryMonitors = []
    if(numLocalConsumers != None):
        grepCmd = "ps -ef | grep NCStressConsumer | grep -v ssh | grep -v .out | grep -v grep | awk '{ print $2 }'"
        debugprint("grepping for local consumer pid with command:  " + grepCmd, debug)
        localGrepProcess = popen2.Popen3(grepCmd)
        pids = localGrepProcess.fromchild.readlines()
        if(pids != None):
            debugprint("found local consumer pid ", debug)
            debugprint("local pids: " + str(pids), debug)

        count = 0
        for pid in pids:
            localConsumerSarCmd = 'exec sar -x ' + pid.strip() + ' 1 9999 >& ./tmp/consumerCpu' + str(count) + '-local.orig.log'
            debugprint("monitoring local consumer: " + str(count) + " w/ cmd: " + localConsumerSarCmd, debug)
            localConsumerSarProcess = popen2.Popen3(localConsumerSarCmd)
            localConsumerMonitors.append(localConsumerSarProcess)
            localConsumerMemoryMonitorProcess = popen2.Popen3("exec monitorProcess " + pid.strip() + " 1 ./tmp/consumerMemory" \
                + str(count) + "-local.orig.log")
            localConsumerMemoryMonitors.append(localConsumerMemoryMonitorProcess)
            count += 1

    # monitor the remote consumers
    remoteConsumerMonitors = []
    remoteConsumerMemoryMonitors = []
    if(numRemoteConsumers != None):
        sshCmd = 'ssh ' + userName + '@' + remoteHost \
            + ' "ps -ef | grep NCStressConsumer | grep -v grep" 2>/dev/null | awk \'{ print $2 }\''
        debugprint("grepping for remote consumer pid with command: " + sshCmd, debug)
        remoteSshProcess = popen2.Popen3(sshCmd)
        pids = remoteSshProcess.fromchild.readlines()
        if(pids != None):
            debugprint("found remote consumer pid ", debug)
            debugprint("remote pids:  " + str(pids), debug)

        count = 0
        for pid in pids:
            debugprint("monitoring remote consumer: " + str(count) + " with pid: " + pid.strip(), debug)
            remoteConsumerSshCmd = 'ssh ' + userName + '@' + remoteHost + ' sar -x ' + pid.strip() + ' 1 9999 >& ./tmp/consumerCpu'\
               + str(count) + '-remote.orig.log'
            remoteConsumerSarProcess = popen2.Popen3(remoteConsumerSshCmd)
            remoteConsumerMonitors.append(remoteConsumerSarProcess)
            remoteConsumerSshCmd = 'ssh ' + userName + '@' + remoteHost + ' remoteMonitorProcess ' + pid.strip() + ' 1 >& ./tmp/consumerMemory'\
               + str(count) + '-remote.orig.log'
            remoteConsumerMemoryProcess = popen2.Popen3(remoteConsumerSshCmd)
            remoteConsumerMemoryMonitors.append(remoteConsumerMemoryProcess)
            count += 1

    # send messages
    myTestSupplier = NCStressSupplier(debug, sizeOfMsg, numMsgs, delayBetweenMsgs)
    debugprint("sending messages", debug)
    myTestSupplier.sendMessages()

    # wait for local things to complete 
    fileStrings = []
    for i in range(0, numLocalConsumers):
        debugprint("waiting for local consumer" + str(i) + " to complete", debug)
        fileStrings.append("./tmp/consumer" + str(i) + "-local.out")

    count = 0
    allReceived = False
    while(count < timeToWait and allReceived is False):
        allReceived = verifyMessagesReceived(fileStrings, numMsgs, debug)
        sleep(2.0)
        count += 2

    # kill local monitoring of the consumers, while not killing the monitoring of things like notify service (yet)
    debugprint("killing local monitoring processes", debug)
    for localConMon in localConsumerMonitors:
        debugprint("killing local sar process: " + str(localConMon.pid), debug)
        killCmd = "kill -9 " + str(localConMon.pid)
        popen2.Popen4(killCmd)
    for localConMemMon in localConsumerMemoryMonitors:
        debugprint("killing local memory monitor process: " + str(localConMemMon.pid), debug)
        killCmd = "kill -9 " + str(localConMemMon.pid)
        popen2.Popen4(killCmd)

    # wait for remote things to complete 
    fileStrings = []
    for i in range(0, numRemoteConsumers):
        debugprint("waiting for remote consumer " + str(i) + " to complete", debug)
        fileStrings.append("./tmp/consumer" + str(i) + "-remote.out")
 
    count = 0
    allReceived = False
    while(count < timeToWait and allReceived is False):
        allReceived = verifyMessagesReceived(fileStrings, numMsgs, debug)
        sleep(2.0)
        count += 2

    # kill remote monitor processes
    debugprint("killing remote sar processes", debug) 
    popen2.Popen4('ssh ' + userName + '@' + remoteHost + ' killall sar >& /dev/null')
    popen2.Popen4('ssh ' + userName + '@' + remoteHost + ' killall remoteMonitorProcess >& /dev/null')
    popen2.Popen4('ssh ' + userName + '@' + remoteHost + ' killall python >& /dev/null')

    # kill monitoring of everything else on local machine (e.g. notify service, etc.)
    debugprint("killing all other local monitors", debug)
    killCmd = "kill -9 " + str(notifySarProcess.pid)
    popen2.Popen4(killCmd)
    killCmd = "kill -9 " + str(notifyMonitorProcess.pid)
    popen2.Popen4(killCmd)
     
    # calculate total time to receive all msgs
    totalTimesLocal = calculateTotalTimes(numLocalConsumers, "local", debug)
    totalTimesRemote = calculateTotalTimes(numRemoteConsumers, "remote", debug)

    # prep output files
    prepOutputFiles(numLocalConsumers, "local", debug)
    prepOutputFiles(numRemoteConsumers, "remote", debug)
    prepSarOutputFiles(numLocalConsumers, numRemoteConsumers, debug)
    prepMemOutputFiles(numLocalConsumers, numRemoteConsumers, debug)

    # generate plots
    debugprint("plotting results", debug)
    plotConsumerReceptionResults(numLocalConsumers, "local", numMsgs, "../doc")
    plotConsumerReceptionResults(numRemoteConsumers, "remote", numMsgs, "../doc")
    plotSarResults(numLocalConsumers, numRemoteConsumers, debug)
    plotMemoryResults(numLocalConsumers, numRemoteConsumers, debug)

    # calculate summary data
    localSummaryData = calculateReceptionTimeSummary(numLocalConsumers, "local", debug)
    remoteSummaryData = calculateReceptionTimeSummary(numRemoteConsumers, "remote", debug)

    if(localSummaryData != None):
        for avg, max, min, consumerNumber, totalMsgsReceived  in localSummaryData:
            debugprint("Local consumer " + str(consumerNumber) + " avg: " + str(avg) + "; maximum: " + str(max) + "; minimum: " + str(min), debug)
            if(totalMsgsReceived != numMsgs):
                print "ERROR: local consumer " + str(consumerNumber) + " expected: " + str(numMsgs) + " but only received: " \
                + str(totalMsgsReceived)

    if(remoteSummaryData != None):
        for avg, max, min, consumerNumber, totalMsgsReceived in remoteSummaryData:
            debugprint("Remote consumer " + str(consumerNumber) + " avg: " + str(avg) + "; maximum: " + str(max) + "; minimum: " + str(min), debug)
            if(totalMsgsReceived != numMsgs):
                print "ERROR: remote consumer " + str(consumerNumber) + " expected: " + str(numMsgs) + " but only received: " \
                + str(totalMsgsReceived)

    # generate the final report as HTML 
    generateHTML(numLocalConsumers, numRemoteConsumers, numMsgs, sizeOfMsg, \
                 delayBetweenMsgs, localSummaryData, remoteSummaryData, totalTimesLocal, \
                 totalTimesRemote, "../doc")

if __name__ == "__main__":
    main()
