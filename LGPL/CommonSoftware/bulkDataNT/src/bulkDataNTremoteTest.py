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
# "@(#) $Id: bulkDataNTremoteTest.py,v 1.13 2013/02/24 22:26:04 gchiozzi Exp $"
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
#   ~/bulkDataNTGenSender.$HOST+flowString
# - ssh is used for remote login.
#   Must be configured not to require a password.
###########################################################################

class bulkDataNTtestSuite:
    
    def __init__(self, hosts, size=640000, loops=10, sourceFile='.bash_profile', acsdataEnv=None,
                 throttle=0, qos_lib="BulkDataQoSLibrary"):
        self.hosts = hosts
        self.numOfHosts = len(hosts)
        self.dataSizeInBytes=size
        self.loops=loops
        self.sendersData = {}
        self.receiverData = None
        self.sourceFile=sourceFile
        self.acsdataEnv = acsdataEnv
        self.throttle = throttle
        self.qos_lib = qos_lib
        self.preCmd="source " + self.sourceFile + "; export ENABLE_BULKDATA_NT=true; export BULKDATA_NT_DEBUG=1; export ACS_LOG_STDOUT=2; "

        if self.acsdataEnv is not None:
            self.preCmd+="export ACSDATA="+self.acsdataEnv+"; "

        if verbose == False:
            self.postCmd=" | grep -v lost > "
        else:
            self.postCmd=" | grep -v lost | tee "

        #
        # Prepares sendersData structures with output file names
        #
        flow = 0
        for h in self.hosts:
            # Builds the flows names
            flowString = format(flow, "02d")
            outFile    = "bulkDataNTGenSender."+h+"-"+flowString
#            sndCmd = ("ssh " + os.environ['USER']+ "@" + h + 
#                       " '" + self.preCmd + " bulkDataNTGenSender -l " + 
#                       str(self.loops)+" -b "+str(self.dataSizeInBytes)+" -s TS -f "+flowString+
#                       " --qos_lib="+self.qos_lib+
#                       " -o "+str(self.throttle)+
#                       self.postCmd+outFile+"'")
            sndCmd = ("ssh " + os.environ['USER']+ "@" + h + 
                       " '" + self.preCmd + " bulkDataNTGenSender -l " + 
                       str(self.loops)+" -b "+str(self.dataSizeInBytes)+" -s TS -f "+flowString+
                        self.postCmd+outFile+"'")
            self.sendersData[h+'-'+flowString]= RemoteProcData(h, sndCmd, None, outFile)
            flow+=1

    ######################################
    #   Starts the senders
    #   Send data
    #   Wait for their completion
    ######################################
    def runSenders(self):
        flow = 0
        i=0
        print 'Going to start remote senders on hosts: ' + str(self.hosts)
        
        for h in sorted(self.sendersData):
            print '----> Starting host: '+self.sendersData[h].host
            print '#',i,' Executing command: '+ self.sendersData[h].command
            self.sendersData[h].process = Popen(self.sendersData[h].command,
                                                stdin=PIPE, stdout=PIPE, stderr=PIPE, shell=True)
            if self.sendersData[h].process is None:
                print 'Problem to execute: ' + self.sendersData[h].command
                
        print 'Going to send data'
        time.sleep(10)                
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
    #   Starts the receiver
    #
    #   Gets the number of flows and builds their names
    #   May be we could pass the list of names directly?
    ######################################################
    def startReceiver(self, rcvHost, multicast):
        # Assume there is one flow per each host entry
        noOfFlows = self.numOfHosts

        flow = 0
        comma=""
        flowString=""
        
        print 'Going to start remote receiver on host: ' + rcvHost + ' with ' + str(noOfFlows) + ' flows'
        for fn in range(noOfFlows):
            flowString+=comma
            flowString+=format(flow, "02d")
            comma=","
            flow+=1
                  
        #
        # GCH: Why is there a -u option here?
        #        rcvCmd = ("ssh " + os.environ['USER']+ "@" + rcvHost + 
        #                  " '" + self.preCmd + " bulkDataNTGenReceiver -n -u -s TS -f "+flowString+
        #                  self.postCmd+"bulkDataNTGenReceiver.$HOST'")
        #
        if multicast == True:
            xxxcastString=""
        else:
            xxxcastString="-u"
        outFile = "bulkDataNTGenReceiver."+rcvHost
        rcvCmd = ("ssh " + os.environ['USER']+ "@" + rcvHost + 
                  " '" + self.preCmd + " bulkDataNTGenReceiver -n -s TS -f "+flowString+" "+xxxcastString+
                  self.postCmd+outFile+"'")
        print ' Executing command: '+ rcvCmd
        process = Popen(rcvCmd, stdin=PIPE, stdout=PIPE, stderr=PIPE, shell=True)
        if process is not None:
            self.receiverData=RemoteProcData(rcvHost, rcvCmd, process, outFile)
        else:
            print 'Problem to execute: ' + rcvCmd
            return
        time.sleep(2)  # Wait for the receiver to be fully initialized.
        print 'Going to wait for data'

    ######################################
    #   Stops the receiver
    ######################################
    def stopReceiver(self):        
        rp = self.receiverData.process           
        rp.poll()
        if rp.returncode is None:
            rp.stdin.write('\n') # ENTER
        else:
            print 'command  : '+self.receiverData.command+' exited with an error: ', rp.returncode, rp.stderr.read()
                
        rp = self.receiverData.process                
        print '----> Waiting termination for receiver'
        rp.wait()
        print '----> Waiting termination for receiver - DONE'
        self.receiverData.output=o=rp.stdout.read().strip()
        if rp.returncode is 0:
            print "Receiver Command ", rp.returncode, " : "+o
        else:
            print 'Command : '+self.receiverData.command+' exited with an error: ', rp.returncode, rp.stderr.read()

    ######################################
    #   Stops the receiver
    ######################################
    def processTestResults(self):

        plotFileName   = 'bulkDataNTremoteTest-time.plot'
        plotFile       = open(plotFileName,'w')
        plotFile.write('set title \"Throughput over time\"\n')
        plotFile.write('set xlabel \"time [sec.msec]\"\n')
        plotFile.write('set ylabel \"Data Rate [MBytes/sec]\"\n')
        plotFile.write('plot ')

        histoFileName  = 'bulkDataNTremoteTest-histo.plot'
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
            inputFile = open(os.path.expanduser("~")+"/"+self.sendersData[h].outFile)
            dataFileName = self.sendersData[h].outFile+'.TP'
            dataFile  = open(dataFileName,'w')

            plotFile.write (comma + '\"' + dataFileName +
                            '\" using 2:3')
            histoFile.write(comma + '\"' + dataFileName +
                            '\" using (bin($3,bw)+bw/2):(1.0) smooth frequency w boxes ti ' +
                            '\"' + dataFileName + ' dist\"')
            comma = ","
            for line in inputFile.readlines():
                #
                # Extracts statistics and writes them on STDOUT
                #
                match = re.search('Average transfer rate for all',line)
                if match is not None:
                    elements = line.split()
                    print "\tAverage transfer rate : "+elements[11]+" "+elements[12]
                match = re.search('protocol',line)
                if match is not None:
                    elements = line.split("[")
                    print "\tProtocol status       : "+elements[2].replace("\n", "").replace("]", "")
                match = re.search('cache Status:',line)
                if match is not None:
                    elements = line.split(":",3)
                    print "\tCache status          :"+elements[3].replace("\n", "")
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
        plotFile.write("set output \"result.png\"\n")
        plotFile.write("replot\n")
        plotFile.close()

        histoFile.write('\n')
        histoFile.write("set terminal png\n")
        histoFile.write("set output \"resultH.png\"\n")
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
    senderHosts = None
    receiverHost= None
    acsdata     = None
    sf          = ".bash_profile"
    verbose     = False
    multicast   = False
    processRes  = False
    throttle    = 0
    qos_lib     = "BulkDataQoSLibrary"
    
    opts, args = getopt.getopt(sys.argv[1:], "hs:r:b:l:mv", ["help", "senders=", "receivers=", "source=", "acsdata=", "verbose", "process"])
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
            print '                                    Default:     BulkDataQoSLibrary'
            print '                                    Alternative: TCPBulkDataQoSLibrary'
            print '  -h or --help                    : This help'
            sys.exit()
        elif o in ("-s", "--senders"):
            senderHosts=a.split(",")
        elif o=="-b":
            b=a
        elif o=="-l":
            l=a
        elif o=="-m":
            multicast=True
        elif o in ("-r", "--receivers"):
            receiverHost=a
            print 'option -r/--receivers= only one receiver supported in this version'
        elif o=="--source":
            sf=a
            print 'source file: '+sf
        elif o=="--acsdata":
            acsdata=a
            print 'ACSDATA: '+ acsdata
        elif o=="--process":
            processRes=True
        elif o=="--throttle":
            throttle=a
            print 'throttle: '+throttle
        elif o=="--qos_lib":
            qos_lib=a
            print 'qos_lib: '+qos_lib
        elif o in ("-v", "--verbose"):
            verbose=True
           
    if senderHosts is None:
        print 'No sender has been given (use option -s/--senders=)!'
        sys.exit()

    #
    # Instantiate the object that actually runs the tests, by passing the
    # command line parameters reeived from the command line.
    #
    testSuit = bulkDataNTtestSuite(hosts=senderHosts, size=b, loops=l, sourceFile=sf, throttle=throttle, qos_lib=qos_lib)

    #
    # Starts the receiver, collecting from all senders
    #
    if receiverHost is not None:
        print '=================> Starting receiver'
        testSuit.startReceiver(receiverHost, multicast) 
    print '---------------------------------------------------------------------------'

    #
    # Starts one or more senders
    # and return when all data has been send and the senders have exited.
    #
    print '=================> Starting Senders'
    testSuit.runSenders()

    #
    # Stops the receiver
    #
    if receiverHost is not None:
        print '---------------------------------------------------------------------------'
        print '=================> Stopping receiver'
        testSuit.stopReceiver()

    #
    # Process test results
    #
    if processRes is True:
        print '=================> Process test results'
        testSuit.processTestResults()

    
# ___oOo___
