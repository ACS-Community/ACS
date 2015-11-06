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
# @(#) $Id: acspyTestSupplierReconn.py,v 1.1 2005/06/15 20:16:48 dfugate Exp $
###############################################################################
'''
Tests the Python Supplier.
'''
###############################################################################
from Acspy.Nc.Supplier import Supplier
from sys import argv
from time import sleep
import acsnc

num_events_dropped = 0

class MyEventCallback:
    def eventDropped(self,event): 
        global num_events_dropped
        num_events_dropped += 1

    def eventSent(self,event):
        pass
        #print "Event sent!!!!"


#create supplier
g = Supplier(str(argv[1]))
#create data to send
h = acsnc.EventDescription("no name",
                           17L,
                           17L)

#send variable number of events
n_errors = 0
n_changes = 0
publishing = True
print "Sending %d events" % (int(argv[2]))
for i in range(int(argv[2])):
    try:
        #print "-------> Publishing event %d" % (i)
        g.publishEvent(h,event_callback=MyEventCallback())
        if not publishing:
            publishing = True
            n_changes += 1
    except:
        n_errors += 1
        if publishing:
            publishing = False
            n_changes += 1
    sleep(1)

print "%d exceptions caught" % (n_errors)
print "%d events dropped" % (num_events_dropped)
print "%d changes found while publishing events" % (n_changes)

#disconnect
g.disconnect()
