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
# "@(#) $Id: bulkDataNTremoteTest.py,v 1.6 2013/01/14 07:58:01 bjeram Exp $"
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
        

class bulkDataNTtestSuite:
    
    def __init__(self, hosts, size=640000, loops=10, sourceFile='.bash_profile'):
        self.hosts = hosts
        self.numOfHosts = len(hosts)
        self.dataSizeInBytes=size
        self.loops=loops
        self.sendersData = {}
        self.receiverData = None
        self.sourceFile=sourceFile
        self.preCmd="source " + self.sourceFile + "; export ENABLE_BULKDATA_NT=true; export BULKDATA_NT_DEBUG=1; export ACS_LOG_STDOUT=2; export XACSDATA=$PWD/workspaceEclipse/bulkDataNT;"
        self.postCmd=" | grep -v lost | tee "
    
    def startSenders(self):
        flow = 0
        i=0
        print 'Going to start remote senders on hosts: ' + str(self.hosts)
        
        for h in self.hosts:
            flowString = format(flow, "02d")      
            sndCmd = ("ssh " + os.environ['USER']+ "@" + h + 
                       " '" + self.preCmd + " bulkDataNTGenSender -l " + 
                       str(self.loops)+" -b "+str(self.dataSizeInBytes)+" -s TS -f "+flowString+
                       self.postCmd+"bulkDataNTGenSender.$HOST'")
            print '#',i,' Executing command: '+ sndCmd
            process = Popen(sndCmd, stdin=PIPE, stdout=PIPE, stderr=PIPE, shell=True)
#            print p.pid, p.returncode, p.poll()
            if process is not None:
                self.sendersData[h]= RemoteProcData(h, sndCmd, process)
                flow+=1
            else:
                print 'Problem to execute: ' + sndCmd
            i+=1
                
        print 'Going to send data'
        time.sleep(3)                
        for h in self.sendersData:
            sp = self.sendersData[h].process           
            sp.poll()
            if sp.returncode is None:
                sp.stdin.write('\n\n')
            else:
                print 'command for host: '+h+' : '+self.sendersData[h].command+' exited with an error: ', sp.returncode, sp.stderr.read()
                
        for h in self.sendersData:
            sp = self.sendersData[h].process                
            sp.wait()
            self.sendersData[h].output=o=sp.stdout.read().strip()
            if sp.returncode is 0:
                print "Command for: "+h+" ", sp.returncode, " : "+o
            else:
                print 'command for host: '+h+' : '+self.sendersData[h].command+' exited with an error: ', sp.returncode, sp.stderr.read()
                 
    def startReceiver(self, rcvHost, noOfFlows):
        flow = 0
        comma=""
        flowString=""
        print 'Going to start remote receiver on host: ' + rcvHost + ' with ' + str(noOfFlows) + ' flows'
        for fn in range(noOfFlows):
            flowString+=comma
            flowString+=format(flow, "02d")
            comma=","
            flow+=1
                  
        rcvCmd = ("ssh " + os.environ['USER']+ "@" + rcvHost + 
                  " '" + self.preCmd + " bulkDataNTGenReceiver -n -u -s TS -f "+flowString+
                  self.postCmd+"bulkDataNTGenReceiver.$HOST'")
        print ' Executing command: '+ rcvCmd
        process = Popen(rcvCmd, stdin=PIPE, stdout=PIPE, stderr=PIPE, shell=True)
#         print p.pid, p.returncode, p.poll()
        if process is not None:
            self.receiverData=RemoteProcData(rcvHost, rcvCmd,process)
        else:
            print 'Problem to execute: ' + rcvCmd
            return
        print 'Going to wait for data'
         
    def stopReceiver(self):        
        rp = self.receiverData.process           
        rp.poll()
        if rp.returncode is None:
            rp.stdin.write('\n') # ENTER
        else:
            print 'command  : '+self.receiverData.command+' exited with an error: ', rp.returncode, rp.stderr.read()
                
        rp = self.receiverData.process                
        rp.wait()
        self.receiverData.output=o=rp.stdout.read().strip()
        if rp.returncode is 0:
            print "Receiver Command ", rp.returncode, " : "+o
        else:
            print 'Command : '+self.receiverData.command+' exited with an error: ', rp.returncode, rp.stderr.read()
            
if __name__ == '__main__':
    b=640000
    l=50
    senderHosts=None
    receiverHost=None
    sf=".bash_profile"
    opts, args = getopt.getopt(sys.argv[1:], "hs:r:b:l:", ["help", "senders=", "receivers=", "source="])
    for o,a in opts:
        if o in ("-h", "--help"):
            print sys.argv[0]+' -s senderHost1[,senderHost2,....] -h [-b data in bytes] [-l number of loops/iterations] [--source=file to be sourced]'
            sys.exit()
        elif o in ("-s", "--senders"):
            senderHosts=a.split(",")
        elif o=="-b":
            b=a
        elif o=="-l":
            l=a
        elif o in ("-r", "--receivers"):
            receiverHost=a
            print 'option -r/--receivers= not supported yet'
        elif o=="--source":
            sf=a
            print 'source file: '+sf
            
    if senderHosts is None:
        print 'No sender has been given (use option -s/--senders=)!'
        sys.exit()
                                                    
    testSuit = bulkDataNTtestSuite(hosts=senderHosts, size=b, loops=l, sourceFile=sf)
    if receiverHost is not None:
        testSuit.startReceiver(receiverHost, len(senderHosts)) 
    print '---------------------------------------------------------------------------'
    
    testSuit.startSenders()
    
    if receiverHost is not None:
        print '---------------------------------------------------------------------------'
        testSuit.stopReceiver()

# ___oOo___
