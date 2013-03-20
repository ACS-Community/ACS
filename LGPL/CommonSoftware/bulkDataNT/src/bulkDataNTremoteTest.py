#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2010 
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id: bulkDataNTremoteTest.py,v 1.18 2013/03/03 19:14:49 gchiozzi Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
#


from subprocess import Popen, PIPE
import os
import getopt, sys
import time
import re

class RemoteProcData:
    def __init__(self, host, cmd, proc, outFile):
        self.host    = host
        self.command = cmd
        self.outFile = outFile
        self.process = proc
        self.out     = None
        
###########################################################################
#
# Main class executing the test:
# - gets configuration parameters in constructor
# - starts/stops senders
# - starts/stops receivers
# - calculates statistics and makes plots
#
# Assumptions:
# - All hosts have a common, shared, home directory
# - Output files will be written in:
#   filePath/self.filePrefix+GenSender.$HOST+flowString
#   ATTENTION: filePath is by default the home and it must be an
#              absolut path IDENTICALLY shared by all machines involved
# - ssh is used for remote login.
#   Must be configured not to require a password.
###########################################################################

class bulkDataNTtestSuite:
    
    def __init__(self, startTime, receivers, senders, size=640000, loops=10, sourceFile='.bash_profile', acsdataEnv=None,
                 throttle=0, qos_lib="BulkDataQoSLibrary", filePrefix='', filePath=None, multicast=False):
        self.startTime       = startTime
        self.senders         = senders
        self.numOfSenders    = len(senders)
        self.receivers       = receivers
        self.numOfReceivers  = len(receivers)
        self.dataSizeInBytes = size
        self.loops           = loops
        self.sendersData     = {}
        self.receiversData   = {}
        self.sourceFile      = sourceFile
        self.acsdataEnv      = acsdataEnv
        self.throttle        = throttle
        self.qos_lib         = qos_lib
        self.filePrefix      = filePrefix
        self.filePath        = filePath
        self.multicast       = multicast

        #
        ### GCH ### This is the base, default unicast port
        #           I will see later, but the strategy now is to increase
        #           is automatically is there are more receivers on the same host
        #
        self.baseUnicastPort = 4800        
        
        self.preCmd = "source " + self.sourceFile + "; export ENABLE_BULKDATA_NT=true; export BULKDATA_NT_DEBUG="
        self.preCmd+=str(bd_nt_debug)+ "; export ACS_LOG_STDOUT=2; "

        if self.acsdataEnv is not None:
            self.preCmd+="export ACSDATA="+self.acsdataEnv+"; "

        if self.filePath is None:
            self.filePath = os.getenv("HOME")+"/"

        if verbose == False:
            self.postCmd=" | grep -v lost > "
        else:
            self.postCmd=" | grep -v lost | tee "

        #
        # Prepares sendersData structures with output file names
        #
        flow = 0
        for h in self.senders:
            # Builds the flows names
            flowString = format(flow, "02d")
            outFile    = self.filePath+self.filePrefix+"GenSender."+h+"-"+flowString

            sndCmd = ("ssh " + os.environ['USER']+ "@" + h + 
                       " '" + self.preCmd + " bulkDataNTGenSender -l " + 
                       str(self.loops)+" -b "+str(self.dataSizeInBytes)+" -s TS -f "+flowString+
                       " --qos_lib="+self.qos_lib+
                       " -o "+str(self.throttle)+
                       self.postCmd+outFile+"'")
            self.sendersData[h+'-'+flowString]= RemoteProcData(h, sndCmd, None, outFile)
            flow+=1

        #
        # Prepares receiversData structures with output file names
        # Let's add a sequential number (even thought it is not the flow as above)
        # to make sure we can run more on the same host without collisions
        #
        # Assume there is one flow per each host entry
        noOfFlows = self.numOfSenders

        flow = 0
        comma=""
        flowString=""
        
        print 'Prepare list of flows for receivers with ' + str(noOfFlows) + ' flows'
        for fn in range(noOfFlows):
            flowString+=comma
            flowString+=format(flow, "02d")
            comma=","
            flow+=1
                  
        seqNum = 0
        previousReceiver = ""
        for h in sorted(self.receivers):
            print "Now: " + h + " Before: " + previousReceiver
            if self.multicast == True:
                xxxcastString=""
            else:
               if h != previousReceiver:
                   port = self.baseUnicastPort 
               else:
                   port = port+1
               xxxcastString="-u%d" % (port)
               previousReceiver = h
               
            # Builds the flows names
            seqString = format(seqNum, "02d")
            outFile    = self.filePath+self.filePrefix+"GenReceiver."+h+"-"+seqString

            rcvCmd = ("ssh " + os.environ['USER']+ "@" + h + 
                       " '" + self.preCmd + " bulkDataNTGenReceiver -n -s TS -f "+flowString+
                       " "+xxxcastString+
                       " --qos_lib="+self.qos_lib+
                       self.postCmd+outFile+"'")
            self.receiversData[h+'-'+seqString]= RemoteProcData(h, rcvCmd, None, outFile)
            seqNum+=1

    ######################################
    #   Starts the senders
    #   Send data
    #   Wait for their completion
    ######################################
    def runSenders(self):
        flow = 0
        i=0
        print 'Going to start remote senders on hosts: ' + str(self.senders)
        
        for h in sorted(self.sendersData):
            print '----> Starting host: '+self.sendersData[h].host
            print '#',i,' Executing command: '+ self.sendersData[h].command
            self.sendersData[h].process = Popen(self.sendersData[h].command,
                                                stdin=PIPE, stdout=PIPE, stderr=PIPE, shell=True)
            if self.sendersData[h].process is None:
                print 'Problem to execute: ' + self.sendersData[h].command
                
        print 'Going to send data'
        time.sleep(40)                
        for h in sorted(self.sendersData):
            print '----> Triggering sending data for host: '+h
            sp = self.sendersData[h].process           
            sp.poll()
            # Once the Sender is started, sends twice \n:
            # - first to send the data
            # - then to exit after having sent the data
            if sp.returncode is None:
                sp.stdin.write('\n\n')
            else:
                print 'Command for host: '+h+' : '+self.sendersData[h].command+' exited with an error: ', sp.returncode, sp.stderr.read()
                
        for h in sorted(self.sendersData):
            print '----> Waiting termination for host: '+h
            sp = self.sendersData[h].process                
            sp.wait()
            print '----> Waiting termination for host: '+h+' TERMINATED'
            self.sendersData[h].output=o=sp.stdout.read().strip()
            if sp.returncode is 0:
                print "Command for: "+h+" ", sp.returncode, " : "+o
            else:
                print 'Command for host: '+h+' : '+self.sendersData[h].command+' exited with an error: ', sp.returncode, sp.stderr.read()
        print '----> All senders terminated. Waiting a few seconds for synchronization'
        time.sleep(10)                
                 
    #######################################################
    #   Starts the receivers
    #
    #   Gets the number of flows and builds their names
    #   May be we could pass the list of names directly?
    ######################################################
    def startReceivers(self):

        flow = 0
        i=0
        print 'Going to start remote receivers on hosts: ' + str(self.receivers)
        
        for h in sorted(self.receiversData):
            print '----> Starting host: '+self.receiversData[h].host
            print '#',i,' Executing command: '+ self.receiversData[h].command
            self.receiversData[h].process = Popen(self.receiversData[h].command,
                                                stdin=PIPE, stdout=PIPE, stderr=PIPE, shell=True)
            if self.receiversData[h].process is None:
                print 'Problem to execute: ' + self.receiversData[h].command
                

        time.sleep(10)  # Wait for the receivers to be fully initialized.
        print 'Going to wait for data'

    ######################################
    #   Stops the receivers
    ######################################
    def stopReceivers(self):


        for h in sorted(self.receiversData):
            print '----> Asking receiver to exit for host: '+h
            sp = self.receiversData[h].process           
            sp.poll()
            # Sends \n to request exit
            if sp.returncode is None:
                sp.stdin.write('\n') # ENTER
            else:
                print 'Command for host: '+h+' : '+self.receiversData[h].command+' exited with an error: ', sp.returncode, sp.stderr.read()
                
        for h in sorted(self.receiversData):
            print '----> Waiting termination for host: '+h
            sp = self.receiversData[h].process                
            sp.wait()
            print '----> Waiting termination for host: '+h+' TERMINATED'
            self.receiversData[h].output=o=sp.stdout.read().strip()
            if sp.returncode is 0:
                print "Command for: "+h+" ", sp.returncode, " : "+o
            else:
                print 'Command for host: '+h+' : '+self.receiversData[h].command+' exited with an error: ', sp.returncode, sp.stderr.read()
        print '----> All receivers terminated. Waiting a few seconds for synchronization'
        time.sleep(10)                

    ######################################
    #   Process testy results
    ######################################
    def processTestResults(self):

        plotFileName   = self.filePath+self.filePrefix+'Result'+'.plot'
        plotFile       = open(plotFileName,'w')
        plotFile.write('set title \"Throughput over time\"\n')
        plotFile.write('set xlabel \"time [sec.msec]\"\n')
        plotFile.write('set ylabel \"Data Rate [MBytes/sec]\"\n')
        plotFile.write('plot ')

        histoFileName  = self.filePath+self.filePrefix+'ResultH'+'.plot'
        histoFile      = open(histoFileName,'w')
        histoFile.write('set title \"Throughput distribution\"\n')
        histoFile.write('set datafile separator \",\"\n')
        histoFile.write('bin(x, s) = s*int(x/s)\n')
        histoFile.write('bw = 1.0\n')
        histoFile.write('set boxwidth bw*0.6\n')
        histoFile.write('set style fill solid 0.4\n')
        histoFile.write('set mxtics 5\n')
        histoFile.write('set yrange [0:*]\n')
        histoFile.write('set xlabel \"Data Rate [MBytes/sec]\"\n')
        histoFile.write('set ylabel \"Frequency\"\n')

        ####################
        # Builds the gnuplot copmmand line to plot the distribution of the reached data rates
        # echo "plot "alma78-00.TP" using (bin($3,bw)+bw/2):(1.0) smooth frequency
        #      ti 'freq' w boxes, "alma78-01.TP" ....  >> $HF
        ###
        histoFile.write('plot ')

        
        comma = ""
        for h in sorted(self.sendersData):
            print    "---->   Processing            : "+self.sendersData[h].outFile
            inputFile = open(self.sendersData[h].outFile)
            dataFileName = self.sendersData[h].outFile+'.TP'
            dataFile  = open(dataFileName,'w')

            plotFile.write (comma + '\"' + dataFileName +
                            '\" using 2:3')
            histoFile.write(comma + '\"' + dataFileName +
                            '\" using (bin($3,bw)+bw/2):(1.0) smooth frequency w boxes ti ' +
                            '\"' + dataFileName + ' dist\"')
            #
            # Reset counters to calculate effective data transfer rate
            #
            timeInitial = 0
            timeFinal   = 0
            bytesTotal  = 0
            comma = ","
            for line in inputFile.readlines():
                
                #
                # Saves the first timestamp when starting sending data.
                #
                match = re.search('Going to send parameter:',line)
                if match is not None:
                    elements = line.split(" ")
                    hours       = int(elements[0][11:13])
                    minutes     = int(elements[0][14:16])
                    seconds     = float(elements[0][17:24])
                    timeInitial = hours*3600+minutes*60+seconds
                    
                #
                # Sums up the bytes in each iteration.
                #
                match = re.search('DataWriter protocol status',line)
                if match is not None:
                    elements   = line.replace("(", "#").replace(")", "#").split("#")
                    bytesTotal = int(elements[1])
                    elements = line.split(" ")
                    hours       = int(elements[0][11:13])
                    minutes     = int(elements[0][14:16])
                    seconds     = float(elements[0][17:24])
                    timeFinal   = hours*3600+minutes*60+seconds
                    effectiveRate = bytesTotal/1000000/(timeFinal-timeInitial) # is a MB 1*10 6 or 1024*1024 bytes?
                    print "----> Effective data rate   : %f MBytes/sec" % effectiveRate

                    
                #
                # Extracts statistics and writes them on STDOUT
                #
                match = re.search('Average transfer rate for all',line)
                if match is not None:
                    elements = line.split()
                    print "----> Average transfer rate : "+elements[11]+" "+elements[12]
                match = re.search('protocol',line)
                if match is not None:
                    elements = line.split("[")
                    print "----> Protocol status       : "+elements[2].replace("\n", "").replace("]", "")
                match = re.search('cache Status:',line)
                if match is not None:
                    elements = line.split(":",3)
                    print "----> Cache status          :"+elements[3].replace("\n", "")
                #
                # Extracts transfer rates for each sample
                # and writes them in the output file, to be used for plotting
                #
                match = re.search('Transfer rate',line)
                if match is not None:
                    elements = line.split(" ")
                    hours   = int(elements[0][11:13])
                    minutes = int(elements[0][14:16])
                    seconds = float(elements[0][17:24])
                    time    = hours*3600+minutes*60+seconds
                    outLine = '%s, %.3f, %s\n' % (elements[0][11:24], time, elements[9])
                    dataFile.write(outLine)

                    
            inputFile.close()
            dataFile.close()

        plotFile.write('\n')
        plotFile.write("set terminal png\n")
        plotFile.write("set output \""+self.filePath+self.filePrefix+"Result.png\"\n")
        plotFile.write("replot\n")
        plotFile.close()

        histoFile.write('\n')
        histoFile.write("set terminal png\n")
        histoFile.write("set output \""+self.filePath+self.filePrefix+"ResultH.png\"\n")
        histoFile.write("replot\n")
        histoFile.close()

        # GCH 20130223
        # We have to use different command line options for
        # different versions of pgplot.
        # For the time being I have seen in ALMA version 4.0 and 4.4
        #
        cmd = Popen("gnuplot --version", shell=True, stdout=PIPE)
        output = cmd.stdout.read()
        elements = output.split()
        if elements[1] == "4.0":
           persistOpt="-persist"
        else:
           persistOpt="-p"
           
        os.system("gnuplot " + persistOpt + " " + plotFileName)
        os.system("gnuplot " + persistOpt + " " + histoFileName)

####################################################
#
# Main routine:
# - parsing of command line parameters
# - help
# - startup
####################################################

if __name__ == '__main__':
    b           = 640000
    l           = 50
    senders     = None
    receivers   = None
    acsdata     = None
    sf          = ".bash_profile"
    verbose     = False
    multicast   = False
    processRes  = False
    throttle    = 0
    qos_lib     = "BulkDataQoSLibrary"
    bd_nt_debug = 1
    
    opts, args = getopt.getopt(sys.argv[1:], "hs:r:b:l:mv", ["help", "senders=", "receivers=", "source=", "acsdata=", "verbose", "process", "throttle=", "qos_lib=", "bulkdata_nt_debug="])
    for o,a in opts:
        if o in ("-h", "--help"):
            print sys.argv[0]+' - Command line options'
            print '  -s senderHost1[,senderHost2,....]  or  --senders=senderHost1[,senderHost2,....]'
            print '    List of hosts where senders have to be (remotely) executed.'
            print '    Each of the senders will get assigned an increasing flow number.'
            print '  -r receiverHost1[,receiverHost2,....]  or  --receivers=receiverHost1[,receiverHost2,....]'
            print '  -m                              : Uses multicast instead of the default unicast'
            print '  -v / --verbose                  : Verbose mode: output from the spawned processes is collected and sent to stdout'
            print '  [-b data in bytes]              : Size of each transmitted test data package in bytes. Default: ',b
            print '  [-l number of loops/iterations] : Number of iterations. Default: ',l
            print '  [--source=file to be sourced]   : File to be sourced at ssh login. Default: ',sf
            print '  [--acsdata=path to ACSDATA for configuration files]'
            print '  [--process]                     : Process results to extract statistics and plots'
            print '  [--throttle=MBsec]              : Throttling in MBytes/sec. Default: 0.0 (no throttling)'
            print '  [--qos_lib=qosLibName]          : Name for the QoS library to use.'
            print '  [--bulkdata_nt_debug=level]     : BDNT debug level (BULKDATA_NT_DEBUG). 0 - no debug'
            print '                                    Default:     BulkDataQoSLibrary'
            print '                                    Alternative: TCPBulkDataQoSLibrary'
            print '  -h or --help                    : This help'
            sys.exit()
        elif o in ("-s", "--senders"):
            senders=a.split(",")
        elif o=="-b":
            b=a
        elif o=="-l":
            l=a
        elif o=="-m":
            multicast=True
        elif o in ("-r", "--receivers"):
            receivers=a.split(",")
        elif o=="--source":
            sf=a
        elif o=="--acsdata":
            acsdata=a
        elif o=="--bulkdata_nt_debug":
           bd_nt_debug=a
        elif o=="--process":
            processRes=True
        elif o=="--throttle":
            throttle=a
        elif o=="--qos_lib":
            qos_lib=a
        elif o in ("-v", "--verbose"):
            verbose=True
           
    if senders is None:
        print 'No sender has been given (use option -s/--senders=)!'
        sys.exit()

    if receivers is None:
        print 'No receiver has been given (use option -r/--receivers=)!'
        sys.exit()

    #
    # Prints first of all the complete given command line
    # Then the starting time.
    # The starting time will be also saved to be appended to all file names.
    #
    startTime = time.strftime("%Y%m%d:%H:%M:%S")
    print '---------------------------------------------------------------------------'
    print "----> Command line: " + " ".join(sys.argv[:])
    print "----> Start time  : " + startTime

    #
    # This prefix will be appended to all file names.
    # Now it is a file name + the start time
    # ToDo: Add a command line option to override the default
    #
    filePrefix = "bdTest-"+startTime+"-"

    #
    #
    #
    filePath = os.getenv("HOME")+"/"
    #
    # Instantiate the object that actually runs the tests, by passing the
    # command line parameters reeived from the command line.
    #
    testSuite = bulkDataNTtestSuite(startTime=startTime, filePrefix=filePrefix,
                                    receivers=receivers,
                                    senders=senders, size=b, loops=l,
                                    sourceFile=sf, acsdataEnv=acsdata,
                                    throttle=throttle, qos_lib=qos_lib, multicast=multicast)

    #
    # Starts the receiver, collecting from all senders
    #
    if receivers is not None:
        print '----> Starting receivers'
        testSuite.startReceivers() 
    print '---------------------------------------------------------------------------'

    #
    # Starts one or more senders
    # and return when all data has been send and the senders have exited.
    #
    print '----> Starting Senders'
    testSuite.runSenders()

    #
    # Stops the receiver
    #
    if receivers is not None:
        print '---------------------------------------------------------------------------'
        print '----> Stopping receivers'
        testSuite.stopReceivers()

    #
    # Prints the time all processes have exited
    #
    print '---------------------------------------------------------------------------'
    print "----> Finish time : " + time.strftime("%Y%m%d:%H:%M:%S")

    #
    # Process test results
    #
    if processRes is True:
        print '----> Process test results'
        testSuite.processTestResults()

    #
    # The end, in a readable way
    #
    print '\n\n   ___oOo___   \n\n'
    
# ___oOo___
