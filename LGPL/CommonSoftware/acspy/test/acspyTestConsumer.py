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
# @(#) $Id: acspyTestConsumer.py,v 1.3 2006/03/08 18:53:20 dfugate Exp $
###############################################################################
'''
Tests the Python Consumer/Supplier.
'''
###############################################################################
from Acspy.Nc.Consumer import Consumer
from time import sleep
from sys  import argv
import acsnc
#-----------------------------------------------------------------------------
count = 0
magicNumber = int(argv[3])
g = None
#-----------------------------------------------------------------------------
def dataHandler(someParam):
    '''
    '''
    global count
    global g
    
    if count < magicNumber:
        if count==(magicNumber-1):
            g.removeSubscription(acsnc.EventDescription)
        count = count + 1
        sleep(1.5)
        
    return
#-----------------------------------------------------------------------------
g = Consumer(str(argv[1]))
g.addSubscription(acsnc.EventDescription, dataHandler)
g.consumerReady()

sleep(int(argv[2]))

if count==magicNumber:
    print "Test passed!"
else:
    print "Test failed!"
    
g.disconnect()
#-------------------------------------------------------------------------------
