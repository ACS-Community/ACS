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
from TMCDB                      import propertySerailNumber
from omniORB                    import any
import MonitorErrImpl
import MonitorErr
import time

# definition of in-test exceptions
class noDataException (Exception) : pass
class notNulDataException (Exception) : pass

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()

mc = simpleClient.getComponent(argv[1])

# Test preparation
try:
    tc =   simpleClient.getComponent('MC_TEST_COMPONENT')
    psns =[propertySerailNumber('doubleSeqProp', ['12124']),propertySerailNumber('doubleProp', ['3432535'])]    
    mc.registerMonitoredDeviceWithMultipleSerial('MC_TEST_COMPONENT', psns)
    print "Test preparation with SUCCESS"
except MonitorErr.RegisteringDeviceProblemEx, _ex:
    ex = MonitorErrImpl.RegisteringDeviceProblemExImpl(exception=_ex)
    ex.Print();
    print "Test preparation FAIL"
    
# Test Case 1: Start component monitoring
try:
    # Reset any values
    tc.reset();
    # Verify that no monitor values exist
    data = mc.getMonitorData()
    # Calculate if data is present
    for d in data:
        for blob in d.monitorBlobs:
            alreadyStoredData=0
            for blobData in any.from_any(blob.blobDataSeq):
                alreadyStoredData+=1
    if alreadyStoredData != 0:
        print "TEST FAIL: Data present before start monitor"
        raise notNulDataException
    # Start monitor and wait some time to generate data
    mc.startMonitoring('MC_TEST_COMPONENT')
    time.sleep(10)
    # Verify if monitor data have been generated
    data = mc.getMonitorData()
    # Calculate if data is present
    for d in data:
        for blob in d.monitorBlobs:
            alreadyStoredData=0
            for blobData in any.from_any(blob.blobDataSeq):
                alreadyStoredData+=1
    if alreadyStoredData == 0:
        print "TEST FAIL: No monitor data is found (first attempt)"
        raise noDataException
    
except noDataException:
    pass
except notNulDataException:
    pass
except MonitorErr.RegisteringDeviceProblemEx, _ex:
    ex = MonitorErrImpl.RegisteringDeviceProblemExImpl(exception=_ex)
    ex.Print();

# Print out recovered data
print "RESULTS FROM TEST CASE1: Start component monitoring", len(data);
print "Number of Devices:", len(data);
for d in data:
    print d.componentName, d.deviceSerialNumber 
    for blob in d.monitorBlobs:
        print "\t", blob.propertyName, blob.propertySerialNumber
        i=0
        for blobData in any.from_any(blob.blobDataSeq):
            if i<20:
                print "\t\t", blobData
                i+=1

# Test Case 2: Data retrieval does not stop component monitoring
try:
    # Verify that retrieving data has not stopped the monitor
    time.sleep(10)
    data = mc.getMonitorData()
    # Calculate if data is present
    for d in data:
        for blob in d.monitorBlobs:
            alreadyStoredData=0
            for blobData in any.from_any(blob.blobDataSeq):
                alreadyStoredData+=1
    if alreadyStoredData == 0:
        print "TEST FAIL: No monitor data is found (second attempt)"
        raise noDataException
    
except noDataException:
    pass
except MonitorErr.RegisteringDeviceProblemEx, _ex:
    ex = MonitorErrImpl.RegisteringDeviceProblemExImpl(exception=_ex)
    ex.Print();

# Print out recovered data
print "RESULTS FROM TEST CASE2: Data retrieval does not stop component monitoring", len(data);
print "Number of Devices:", len(data);
for d in data:
    print d.componentName, d.deviceSerialNumber
    for blob in d.monitorBlobs:
        print "\t", blob.propertyName, blob.propertySerialNumber
        i=0
        for blobData in any.from_any(blob.blobDataSeq):
            if i<20:
                print "\t\t", blobData
                i+=1

# Test Case 3: Stop component monitoring
try:
    # Stop monitoring
    mc.stopMonitoring('MC_TEST_COMPONENT')
    # get rid of old data retrieved
    data = mc.getMonitorData()
    
    # Wait some time to later calculate if stop worked
    time.sleep(2)
    data = mc.getMonitorData()
    # Calculate if data is present
    for d in data:
        for blob in d.monitorBlobs:
            alreadyStoredData=0
            for blobData in any.from_any(blob.blobDataSeq):
                alreadyStoredData+=1
    if alreadyStoredData != 0:
        print "TEST FAIL: Monitor data is found"
        raise notNulDataException
    
except notNulDataException:
    pass
except MonitorErr.RegisteringDeviceProblemEx, _ex:
    ex = MonitorErrImpl.RegisteringDeviceProblemExImpl(exception=_ex)
    ex.Print();

# Print out recovered data
print "RESULTS FROM TEST CASE2: Stop component monitoring", len(data);
print "Number of Devices:", len(data);
for d in data:
    print d.componentName, d.deviceSerialNumber
    for blob in d.monitorBlobs:
        print "\t", blob.propertyName, blob.propertySerialNumber
        i=0
        for blobData in any.from_any(blob.blobDataSeq):
            if i<20:
                print "\t\t", blobData
                i+=1

mc.deregisterMonitoredDevice('MC_TEST_COMPONENT')

#cleanly disconnect
simpleClient.releaseComponent(argv[1])
simpleClient.disconnect()


