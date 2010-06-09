#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2008 
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
# "@(#) $Id: CERNAlarmTestSender.py,v 1.2 2010/06/09 02:42:45 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-10-22  created
#

import sys
import Acsalarmpy
import Acsalarmpy.FaultState as FaultState
import Acsalarmpy.Timestamp as Timestamp

if len(sys.argv) < 2:
    print "\n\nUsage: \n\nTestAcsAlarmSending <NUM_ALARMS_TO_SEND>\n\nwhere NUM_ALARMS_TO_SEND is how many alarms you wish to send.\n\n"
else:
    numAlarmsToSend = int(sys.argv[1])

    # Test data for our fault
    family = 'Mount'
    member = 'ALARM_SOURCE_MOUNT'
    code = 1

    print "Testing long-hand style of sending alarms"

    Acsalarmpy.AlarmSystemInterfaceFactory.init()
    
    alarmSource = Acsalarmpy.AlarmSystemInterfaceFactory.createSource("ALARM_SYSTEM_SOURCES")

    # Create a test fault
    fltstate = Acsalarmpy.AlarmSystemInterfaceFactory.createFaultState(family,member, code)
    fltstate.descriptor = FaultState.ACTIVE_STRING
    fltstate.userTimestamp = Timestamp.Timestamp()
    fltstate.userProperties[FaultState.ASI_PREFIX_PROPERTY_STRING] = "prefix"
    fltstate.userProperties[FaultState.ASI_SUFFIX_PROPERTY_STRING] = "suffix"
    fltstate.userProperties["TEST_PROPERTY"] = "TEST_VALUE"

    # The heart of the test
    for i in range(1,numAlarmsToSend+1):
        alarmSource.push(fltstate)

    Acsalarmpy.AlarmSystemInterfaceFactory.done()
    sys.stdout.flush()
    

#
# ___oOo___
