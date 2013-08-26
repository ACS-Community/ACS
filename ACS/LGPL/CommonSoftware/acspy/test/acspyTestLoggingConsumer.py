#!/usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2002 
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration)
# and Cosylab 2002, All rights reserved
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307  USA
#
# @(#) $Id: acspyTestLoggingConsumer.py,v 1.2 2008/11/06 16:13:02 agrimstrup Exp $
###############################################################################
'''
Tests logging consumer

The tests waits until a number of logs (passed in the command line) has been
received.
Note that these logs are produced by already running processes like the containers 
so it can take and arbitrary time to complete...
'''
from Acspy.Nc.LoggingConsumer import LoggingConsumer
from Acspy.Common.Log         import getLogger
import Acspy.Clients
from sys  import argv
from time import sleep
###############################################################################
count = 0
magicNumber = int(argv[1])

def myHandler(xml):
    '''
    '''
    global count 
    if count < magicNumber:
#        print xml
        count = count + 1
    return

#create the consumer
myConsumer = LoggingConsumer(myHandler)
myConsumer.consumerReady()

# A SimpleClient is instantiated to produce logs and
# limit the execution time and the dependency from non identified 
# external processes that are supposed to produce logs
logger = getLogger("acspyTestLoggingConsumerLogger")
for i in range(magicNumber+1):
    logger.logInfo('Log #'+str(i))

while (count!=magicNumber):
    sleep(1)

#shutdown everything cleanly
myConsumer.disconnect()
print "%d of %d messages received" % (count, magicNumber)

