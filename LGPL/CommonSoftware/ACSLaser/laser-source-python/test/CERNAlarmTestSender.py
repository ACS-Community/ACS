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
# "@(#) $Id: CERNAlarmTestSender.py,v 1.4 2013/02/19 15:22:28 acaproni Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-10-22  created
#

import sys
from time import sleep
from string import find
import Acsalarmpy
import Acsalarmpy.FaultState as FaultState
import Acsalarmpy.Timestamp as Timestamp
import ACSJMSMessageEntity_idl
from Acspy.Nc.Consumer import Consumer
from acsnc import ALARMSYSTEM_DOMAIN_NAME

msgCount = 0

def alarmDataHandler(some_param):
    global msgCount
    pos = some_param.text.find("<fault-state family=\"Mount\" member=\"ALARM_SOURCE_MOUNT\" code=") 
    if pos !=-1:
        triplet= some_param.text[pos:]
        pos = triplet.find(">")
        if pos!=-1:
            triplet = triplet[:pos+1]
        print "Triplet received",triplet.strip()
        sys.stdout.flush()
        msgCount += 1
    return

if len(sys.argv) < 2:
    print "\n\nUsage: \n\nTestAcsAlarmSending <NUM_ALARMS_TO_SEND>\n\nwhere NUM_ALARMS_TO_SEND is how many alarms you wish to send.\n\n"
else:
    numAlarmsToSend = int(sys.argv[1])
    
    c = Consumer("CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES",None,ALARMSYSTEM_DOMAIN_NAME)
    c.addSubscription(ACSJMSMessageEntity_idl._0_com.cosylab.acs.jms.ACSJMSMessageEntity, alarmDataHandler)
    c.consumerReady()

    # Test data for our fault
    family = 'Mount'
    member = 'ALARM_SOURCE_MOUNT'

    print "Testing long-hand style of sending alarms"

    Acsalarmpy.AlarmSystemInterfaceFactory.init()
    
    alarmSource = Acsalarmpy.AlarmSystemInterfaceFactory.createSource("ALARM_SYSTEM_SOURCES")

    # The heart of the test
    for code in range(1,numAlarmsToSend+1):
        # Create a test fault
        fltstate = Acsalarmpy.AlarmSystemInterfaceFactory.createFaultState(family,member, code)
        fltstate.descriptor = FaultState.ACTIVE_STRING
        fltstate.userTimestamp = Timestamp.Timestamp()
        fltstate.userProperties[FaultState.ASI_PREFIX_PROPERTY_STRING] = "prefix"
        fltstate.userProperties[FaultState.ASI_SUFFIX_PROPERTY_STRING] = "suffix"
        fltstate.userProperties["TEST_PROPERTY"] = "TEST_VALUE"
        alarmSource.push(fltstate)
    print numAlarmsToSend,"alarms sent"
    
    
    numAlarmsToReceive=numAlarmsToSend
    timeout=120 
    now=0
    while msgCount < numAlarmsToReceive or now>timeout:
        sleep(1)
        now= now +1

    if msgCount==numAlarmsToReceive:
        print "Consumer received %d messages\n" % msgCount
    else:
        print "ERROR: consumer received",msgCount,"messages instead of",numAlarmsToReceive
        
    Acsalarmpy.AlarmSystemInterfaceFactory.done()
    sys.stdout.flush()
    c.disconnect()

#
# ___oOo___
