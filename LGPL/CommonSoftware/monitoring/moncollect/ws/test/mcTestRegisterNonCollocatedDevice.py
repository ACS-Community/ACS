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

# Test Case 1: Register non collocated device non registrable
#              Test EH(registerNonCollocatedMonitoredDevice): RegisteringDeviceProblem"
print "Test Case 1: Register non collocated device non registrable"
print "--------------------------------------------"
try:
    mc.registerNonCollocatedMonitoredDevice('FAKE_DEVICE', '98765')
    print " TEST FAIL: FAKE_DEVICE registered"
except MonitorErr.RegisteringDeviceProblemEx, _ex:
    ex = MonitorErrImpl.RegisteringDeviceProblemExImpl(exception=_ex)
    ex.Print();
    print " TEST SUCCESS: FAKE_DEVICE not registered"

# Test Case 2: Register non collocated device with success
#              Test registerNonCollocatedMonitoredDevice: OK
print "Test Case 2: Register non collocated device with success"
print "--------------------------------------------"
try:
    mc.registerNonCollocatedMonitoredDevice('TEST_NONCOLLOCATED_PS_1', '98765')
    print " TEST SUCCESS: TEST_NONCOLLOCATED_PS_1 Monitored Device registered"
except MonitorErr.RegisteringDeviceProblemEx, _ex:
    ex = MonitorErrImpl.RegisteringDeviceProblemExImpl(exception=_ex)
    ex.Print();
    print " TEST FAIL: TEST_NONCOLLOCATED_PS_1 not registered"

# Test Case 3: Register non collocated device already registered
#              Test EH(registerNonCollocatedMonitoredDevice): DeviceAlreadyRegistered
print "Test Case 3: Register non collocated device already registered"
print "--------------------------------------------"
try:
    mc.registerNonCollocatedMonitoredDevice('TEST_NONCOLLOCATED_PS_1', '98765')
    print " TEST FAIL: TEST_NONCOLLOCATED_PS_1 registered"
except MonitorErr.RegisteringDeviceProblemEx, _ex:
    ex = MonitorErrImpl.RegisteringDeviceProblemExImpl(exception=_ex)
    ex.Print();
    print " TEST SUCCESS: TEST_NONCOLLOCATED_PS_1 not registered"

# Test Case 4: Unregister non collocated device non registered
#              Test EH(deregisterMonitoredDevice): DeviceNotRegisteredEx
print "Test Case 4: Unregister device non registered"
print "--------------------------------------------"
try:
    mc.deregisterMonitoredDevice('FAKE_DEVICE')
    print " TEST FAIL: FAKE_DEVICE unregistered"
except MonitorErr.DeviceNotRegisteredEx, _ex:
    ex = MonitorErrImpl.DeviceNotRegisteredExImpl(exception=_ex)
    ex.Print();
    print " TEST SUCCESS: FAKE_DEVICE not unregistered"

# Test Case 5: Unregister non collocated device with success
#              Test deregisterMonitoredDevice: OK
print "Test Case 5: Unregister device with success"
print "--------------------------------------------"
try:
    mc.deregisterMonitoredDevice('TEST_NONCOLLOCATED_PS_1')
    print " TEST SUCCESS: TEST_NONCOLLOCATED_PS_1 unregistered"
except MonitorErr.DeviceNotRegisteredEx, _ex:
    ex = MonitorErrImpl.DeviceNotRegisteredExImpl(exception=_ex)
    ex.Print();
    print " TEST FAIL: TEST_NONCOLLOCATED_PS_1 not unregistered"

# Test Case 6: Unregister non collocated device already unregistered
#              Test EH(deregisterMonitoredDevice): DeviceNotRegisteredEx
print "Test Case 6: Unregister device already unregistered"
print "--------------------------------------------"
try:
    mc.deregisterMonitoredDevice('TEST_NONCOLLOCATED_PS_1')
    print " TEST FAIL: TEST_NONCOLLOCATED_PS_1 unregistered"
except MonitorErr.DeviceNotRegisteredEx, _ex:
    ex = MonitorErrImpl.DeviceNotRegisteredExImpl(exception=_ex)
    ex.Print();
    print " TEST SUCCESS: TEST_NONCOLLOCATED_PS_1 not unregistered"

#cleanly disconnect
simpleClient.releaseComponent(argv[1])
simpleClient.disconnect()


