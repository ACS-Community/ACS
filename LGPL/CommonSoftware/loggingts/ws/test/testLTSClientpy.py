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
#------------------------------------------------------------------------------

#from loggingts.ACSLogTypeExample import simpleLog,complexLog
from ACSLogTypeExampleLTS  import simpleLog,complexLog

# Import the acspy.PySimpleClient class
from Acspy.Clients.SimpleClient import PySimpleClient

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()


simpleClient.getLogger().logInfo("Starting test client.")

simpleLog().log()

simpleLog("Array01","Antenna01").log()

a=complexLog()
a.setsomeDoubleMember(3.14159)
a.setsomeStringMember("test string")
a.setsomeLongMember(42)
a.setsomeBooleanMember(True)
a.log()

b=complexLog()
b.setArray("Array01")
b.setAntenna("Antenna01")
b.setsomeDoubleMember(3.14159)
b.setsomeStringMember("test string")
b.setsomeLongMember(42)
b.setsomeBooleanMember(True)
b.log()

simpleClient.disconnect()
simpleClient.getLogger().logInfo("Exiting test client.")
