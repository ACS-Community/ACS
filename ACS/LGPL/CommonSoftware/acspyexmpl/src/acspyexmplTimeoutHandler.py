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
# @(#) $Id: acspyexmplTimeoutHandler.py,v 1.7 2005/05/06 22:43:44 dfugate Exp $
#------------------------------------------------------------------------------
'''
DESCRIPTION
Timeout Handler provides the complete implementation of the
<a href="../../idl/html/interfaceacstime_1_1TimeoutHandler.html">TimeoutHandler</a>
IDL interface. It then uses PySimpleClient to find the Default Clock and Timer
components to schedule a single timeout event to occur in three seconds. It should
be noted this example does not take advantage of the DurationHelper or EpochHelper
classes which are also available.

WHAT CAN I GAIN FROM THIS EXAMPLE?
- PySimpleClient usage.
- Accessing (remote) components.
- Manipulating BACI properties
- Implementation of the <a href="../../idl/html/interfaceacstime_1_1TimeoutHandler.html">TimeoutHandler</a> IDL interface
- Obtaining the current time from the <a href="../../idl/html/interfaceacstime_1_1Clock.html">Clock</a> component
- Scheduling timeout events to occur using the <a href="../../idl/html/interfaceacstime_1_1Timer.html">Timer</a> component

LINKS
- <a href="../../idl/html/interfaceacstime_1_1TimeoutHandler.html">TimeoutHandler IDL Documentation</a>
- <a href="../../idl/html/interfaceacstime_1_1Clock.html">Clock IDL Documentation</a>
- <a href="../../idl/html/interfaceacstime_1_1Timer.html">Timer IDL Documentation</a>
'''

# Import the PySimpleClient class
from Acspy.Clients.SimpleClient import PySimpleClient

# To give the timeouts a chance to occur
from time import sleep

from Acspy.Common.TimeHelper import getTimeStamp

# CORBA stubs
import acstime, acstime__POA

#---------------------------------------------------
class TimeoutHandlerImpl(acstime__POA.TimeoutHandler):
    '''
    Implementation of the TimeoutHandler IDL interface
    '''
    def handleTimeout(self, e):
        '''
        This method is overriden to do something useful
        once a timeout occurs
        '''
        print "The current time is: ", e.value
#---------------------------------------------------

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()

# Get the current time from the standard CLOCK1 Clock device and add 3 seconds to it
# This is when the timeout will occur.
start = long(getTimeStamp().value) + long(30000000)

# Get the standard Timer component which will
# schedule the timeout
timer = simpleClient.getDefaultComponent("IDL:alma/acstime/Timer:1.0")

# Create timeout handler and schedule its timeout
myHandler = TimeoutHandlerImpl()
myHandler2 = TimeoutHandlerImpl()

# A Duration of 0 implies the timeout will only occur once
id1 = timer.schedule(simpleClient.activateOffShoot(myHandler),
                     acstime.Epoch(start),
                     acstime.Duration(long(20000000)))

# A Duration of 0 implies the timeout will only occur once
id2 = timer.schedule(simpleClient.activateOffShoot(myHandler2),
                     acstime.Epoch(start),
                     acstime.Duration(long(0)))

# Give it ten seconds to run
sleep(10)

timer.cancel(long(id1))

# Give it ten more seconds to run
sleep(10)

# Release it
simpleClient.disconnect()
print "The end __oOo__"











