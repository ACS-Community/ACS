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
# @(#) $Id: acspyTestArchiveConsumerPS.py,v 1.1 2006/11/24 07:55:57 cparedes Exp $
###############################################################################
'''
Tests archive consumer
'''
from Acspy.Nc.ArchiveConsumer import ArchiveConsumer
from Acspy.Clients.SimpleClient import PySimpleClient
from sys  import argv
from time import sleep
###############################################################################
count = 0
magicNumber = int(argv[2])

def myHandler(ts, device, parameter, value):
    '''
    '''
    global count 
    if count < magicNumber:
        count = count + 1
    return

#create the consumer
myConsumer = ArchiveConsumer(myHandler)
myConsumer.consumerReady()
#activate the component which will publish the events automatically
joe = PySimpleClient()
ps = joe.getComponent("TEST_PS_1")
#give the consumer a chance to receive the events
sleep(int(argv[1]))

#shutdown everything cleanly
myConsumer.disconnect()
joe.releaseComponent("TEST_PS_1")
joe.disconnect()

if count==magicNumber:
    print "Test passed!"
else:
    print "Test failed:", count
