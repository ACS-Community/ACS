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

from Acspy.Clients.SimpleClient import PySimpleClient
from sys                        import argv
from sys                        import exit
from TMCDB                      import MonitorCollector
import MonitorErrImpl
import MonitorErr

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()

mc = simpleClient.getComponent(argv[1])

print "Test EH(registerNonCollocatedMonitoredDevice): RegisteringDeviceProblem"
try:
    mc.registerNonCollocatedMonitoredDevice('FAKE_DEVICE', '98765')
except MonitorErr.RegisteringDeviceProblemEx, _ex:
    ex = MonitorErrImpl.RegisteringDeviceProblemExImpl(exception=_ex)
    ex.Print();
    
    
mc.registerNonCollocatedMonitoredDevice('TEST_NONCOLLOCATED_PS_1', '98765')    
mc.startMonitoring('TEST_NONCOLLOCATED_PS_1')    
    
print "Test EH(registerNonCollocatedMonitoredDevice): DeviceAlreadyRegistered"
try:
    mc.registerNonCollocatedMonitoredDevice('TEST_NONCOLLOCATED_PS_1', '98765')
except MonitorErr.RegisteringDeviceProblemEx, _ex:
    ex = MonitorErrImpl.RegisteringDeviceProblemExImpl(exception=_ex)
    ex.Print();   

mc.deregisterMonitoredDevice('TEST_NONCOLLOCATED_PS_1')

#cleanly disconnect
simpleClient.releaseComponent(argv[1])
simpleClient.disconnect()


