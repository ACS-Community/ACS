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
# "@(#) $Id: bulkDataNTremoteTest.py,v 1.2 2012/10/23 15:22:51 bjeram Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
#


from subprocess import Popen, PIPE
import os
import getopt, sys
import time

class bulkDataNTtestSuite:
    
    def __init__(self, hosts, size=640000, loops=10):
        self.hosts = hosts
        self.numOfHosts = len(hosts)
        self.dataSizeInBytes=size
        self.loops=loops
    
    def startSenders(self):
        flow = 0
        output = []
        process = []
        cmd = []
        cmdMap={}
        i=0
        print 'Going to start remote senders on the following hosts: ' + str(self.hosts)
        for h in self.hosts:
            flowString = format(flow, "02d")      
            command = ("ssh " + os.environ['USER']+ "@" + h + 
                       " 'source .bash_profile; export BULKDATA_NT_DEBUG=1; export ACS_LOG_STDOUT=2; bulkDataNTGenSender -l "+str(self.loops)+" -b "
                       +str(self.dataSizeInBytes)+" -s TS -f "+flowString+
                       " | grep -v lost | tee bulkDataNTGenSender.$HOST'")
            print "#",i,' : '+ command
            p = Popen(command, stdin=PIPE, stdout=PIPE, stderr=PIPE, shell=True)
#            print p.pid, p.returncode, p.poll()
            if p is not None:
                process.append(p)
                cmd.append(command)
                cmdMap[h]=command
                flow+=1
                i+=1
            else:
                print 'Problem to execute: ' + command
                
        print 'Going to send data'
        time.sleep(3)                
        for i in range(1, self.numOfHosts):
            p = process[i-1]
            p.poll()
            if p.returncode is None:
                p.stdin.write('\n\n')
            else:
                print "#",i, 'command: '+cmd[i-1]+' exit with an error: ', p.returncode, p.stderr.read()
                
        for i in range(1, self.numOfHosts):
            p = process[i-1]
            p.wait()
            output.append(p.stdout.read().strip())
            if p.returncode is 0:
                print "#",i," : ", p.returncode, " : "+output[i-1]
            else:
                print "#",i, 'command: '+cmd[i-1]+' exit with an error: ', p.returncode, p.stderr.read() 
            
if __name__ == '__main__':
    b=640000
    l=50
    opts, args = getopt.getopt(sys.argv[1:], "hs:r:b:l:", ["help", "senders=", "receivers="])
    for o,a in opts:
        if o in ("-h", "--help"):
            print sys.argv[0]+' -s senderHost1[,senderHost2,....] -h [-b data in bytes] [-l number of loops/iterations]'
        elif o=="-s":
            h=a.split(",")
        elif o=="-b":
            b=a
        elif o=="-l":
            l=a
                                                
    testSuit = bulkDataNTtestSuite(hosts=h, size=b, loops=l)            
    testSuit.startSenders()

# ___oOo___
