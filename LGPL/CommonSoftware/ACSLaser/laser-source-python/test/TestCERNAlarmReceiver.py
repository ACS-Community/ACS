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
# "@(#) $Id: TestAcsAlarmSending.py,v 1.1 2008/10/09 19:13:20 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-10-22  created
#

import sys
from time import sleep
import ACSJMSMessageEntity_idl
from Acspy.Nc.Consumer import Consumer

msgCount = 0

def alarmDataHandler(some_param):
    global msgCount
#    print msgCount, some_param.text
    msgCount += 1
    return


if len(sys.argv) < 2:
    print "\n\nUsage: \n\n" << "TestAcsAlarmSending <NUM_ALARMS_TO_SEND>\n\n" << "where NUM_ALARMS_TO_SEND is how many alarms you wish to send.\n\n"
else:
    numAlarmsToReceive = int(sys.argv[1])

c = Consumer("CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES")
c.addSubscription(ACSJMSMessageEntity_idl._0_com.cosylab.acs.jms.ACSJMSMessageEntity, alarmDataHandler)
c.consumerReady()
while msgCount < numAlarmsToReceive:
    sleep(1)

print "Consumer received %d messages\n" % msgCount
c.disconnect()
