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
# "@(#) $Id: bulkDataNTtest.py,v 1.1 2012/10/23 09:44:16 bjeram Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
#


from subprocess import Popen, PIPE
import os
import sys
import time

class bulkDataNTtestSuite(object):
    
    
    def startSenders(self):
        flow = 0
        numOfHosts = len(sys.argv)
        output = []
        process = []
        cmd = []
        print 'aa:' + str(sys.argv)
        for i in range(1, numOfHosts):
            flowString = format(flow, "02d")      
            command = ("ssh " + os.environ['USER']+ "@" + sys.argv[i] + 
                       " 'source .bash_profile; export BULKDATA_NT_DEBUG=1; export ACS_LOG_STDOUT=2; bulkDataNTGenSender -l 50 -b 640000 -s TS -f "+
                       flowString+
                       " | grep -v lost | tee bulkDataNTGenSender.$HOST'")
            print "#",i,' : '+ command
            p = Popen(command, stdin=PIPE, stdout=PIPE, stderr=PIPE, shell=True)
#            print p.pid, p.returncode, p.poll()
            if p is not None:
                process.append(p)
                cmd.append(command)
                flow+=1
            else:
                print 'Problem to execute: ' + command
                
        print 'Going to start sending data'
        time.sleep(3)                
        for i in range(1, numOfHosts):
            p = process[i-1]
            p.poll()
            if p.returncode is None:
                p.stdin.write('\n\n')
            else:
                print "#",i, 'command: '+cmd[i-1]+' exit with an error: ', p.returncode, p.stderr.read()
                
        for i in range(1, numOfHosts):
            p = process[i-1]
            p.wait()
            output.append(p.stdout.read().strip())
            if p.returncode is 0:
                print "#",i," : ", p.returncode, " : "+output[i-1]
            else:
                print "#",i, 'command: '+cmd[i-1]+' exit with an error: ', p.returncode, p.stderr.read() 
            
if __name__ == '__main__':
    testSuit = bulkDataNTtestSuite()            
    testSuit.startSenders()

# ___oOo___
