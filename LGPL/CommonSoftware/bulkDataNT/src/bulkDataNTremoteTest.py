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
# "@(#) $Id: bulkDataNTremoteTest.py,v 1.9 2013/02/13 16:14:07 gchiozzi Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
#


from subprocess import Popen, PIPE
import os
import getopt, sys
import time

class RemoteProcData:
    def __init__(self, host, cmd, proc):
        self.command =cmd
        self.process = proc
        self.host = host
        self.out=None
        
####################################################
#
# Main class executing the test:
# - gets configuration parameters in constructor
# - starts/stops senders
# - starts/stops receivers
#
####################################################

class bulkDataNTtestSuite:
    
    def __init__(self, hosts, size=640000, loops=10, sourceFile='.bash_profile', acsdataEnv=None):
        self.hosts = hosts
        self.numOfHosts = len(hosts)
        self.dataSizeInBytes=size
        self.loops=loops
        self.sendersData = {}
        self.receiverData = None
        self.sourceFile=sourceFile
        self.acsdataEnv = acsdataEnv
        self.preCmd="source " + self.sourceFile + "; export ENABLE_BULKDATA_NT=true; export BULKDATA_NT_DEBUG=1; export ACS_LOG_STDOUT=2; "
        if self.acsdataEnv is not None:
            self.preCmd+="export ACSDATA="+self.acsdataEnv+"; "

        if verbose == False:
            self.postCmd=" | grep -v lost > "
        else:
            self.postCmd=" | grep -v lost | tee "
    
    ######################################
    #   Starts the senders
    #   Send data
    #   Wait for their completion
    ######################################
    def startSenders(self):
        flow = 0
        i=0
        print 'Going to start remote senders on hosts: ' + str(self.hosts)
        
        for h in self.hosts:
            # Builds the flows names
            flowString = format(flow, "02d")      
            print '----> Starting host: '+h+' with flow '+flowString
            sndCmd = ("ssh " + os.environ['USER']+ "@" + h + 
                       " '" + self.preCmd + " bulkDataNTGenSender -l " + 
                       str(self.loops)+" -b "+str(self.dataSizeInBytes)+" -s TS -f "+flowString+
                       self.postCmd+"bulkDataNTGenSender.$HOST-"+flowString+"'")
            print '#',i,' Executing command: '+ sndCmd
            process = Popen(sndCmd, stdin=PIPE, stdout=PIPE, stderr=PIPE, shell=True)
            if process is not None:
                self.sendersData[h+'-'+flowString]= RemoteProcData(h, sndCmd, process)
                flow+=1
            else:
                print 'Problem to execute: ' + sndCmd
            i+=1
                
        print 'Going to send data'
        time.sleep(3)                
        for h in self.sendersData:
            print '----> Triggering sending data for host: '+h
            sp = self.sendersData[h].process           
            sp.poll()
            if sp.returncode is None:
                sp.stdin.write('\n\n')
            else:
                print 'Command for host: '+h+' : '+self.sendersData[h].command+' exited with an error: ', sp.returncode, sp.stderr.read()
                
        for h in self.sendersData:
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
        time.sleep(3)                
                 
    #######################################################
    #   Starts the receiver
    #
    #   Gets the number of flows and builds their names
    #   May be we could pass the list of names directly?
    ######################################################
    def startReceiver(self, rcvHost, noOfFlows, multicast):
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
            
        rcvCmd = ("ssh " + os.environ['USER']+ "@" + rcvHost + 
                  " '" + self.preCmd + " bulkDataNTGenReceiver -n -s TS -f "+flowString+" "+xxxcastString+
                  self.postCmd+"bulkDataNTGenReceiver.$HOST'")
        print ' Executing command: '+ rcvCmd
        process = Popen(rcvCmd, stdin=PIPE, stdout=PIPE, stderr=PIPE, shell=True)
        if process is not None:
            self.receiverData=RemoteProcData(rcvHost, rcvCmd, process)
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
    
    opts, args = getopt.getopt(sys.argv[1:], "hs:r:b:l:mv", ["help", "senders=", "receivers=", "source=", "acsdata=", "verbose"])
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
            print 'option -r/--receivers= not supported yet'
        elif o=="--source":
            sf=a
            print 'source file: '+sf
        elif o=="--acsdata":
            acsdata=a
            print 'ACSDATA: '+ acsdata
        elif o in ("-v", "--verbose"):
            verbose=True
            print 'source file: '+sf
            
    if senderHosts is None:
        print 'No sender has been given (use option -s/--senders=)!'
        sys.exit()

    #
    # Instantiate the object that actually runs the tests, by passing the
    # command line parameters reeived from the command line.
#            print p.pid, p.returncode, p.poll()
    #
    testSuit = bulkDataNTtestSuite(hosts=senderHosts, size=b, loops=l, sourceFile=sf)

    #
    # Starts the receiver, collecting from all senders
    #
    if receiverHost is not None:
        print '=================> Starting receiver'
        testSuit.startReceiver(receiverHost, len(senderHosts), multicast) 
    print '---------------------------------------------------------------------------'

    #
    # Starts one or more senders
    # and return when all data has been send and the senders have exited.
    #
    print '=================> Starting Senders'
    testSuit.startSenders()
    #
    # Finally, stops the receiver
    #
    if receiverHost is not None:
        print '---------------------------------------------------------------------------'
        print '=================> Stopping receiver'
        testSuit.stopReceiver()

# ___oOo___
