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
from sys                        import stdout
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
cname = 'MC_TEST_COMPONENT4'
try:
    tc =   simpleClient.getComponent(cname)
    psns =[propertySerailNumber('doubleSeqProp', ['12124']),
           propertySerailNumber('doubleProp', ['3432535'])
           ]
    # doubleSeqProp component archival delta percent defined to 10.0 (MC_TEST_COMPONENT4.xml)
    # doubleProp component archival delta defined percent to 10.0 (MC_TEST_COMPONENT4.xml)
    # longProp component archival delta defined percent to 20.0 (MC_TEST_COMPONENT4.xml)
    # longSeqProp component archival delta defined percent to 20.0 (MC_TEST_COMPONENT4.xml)
    # patternProp component archival delta defined percent to 7.0 (MC_TEST_COMPONENT4.xml)
    mc.registerMonitoredDeviceWithMultipleSerial(cname, psns)
    print "Test preparation with SUCCESS"
except MonitorErr.RegisteringDeviceProblemEx, _ex:
    ex = MonitorErrImpl.RegisteringDeviceProblemExImpl(exception=_ex)
    ex.Print();
    print "Test preparation FAIL"
    
# Test Case 1: Verify static configuration of archive delta percent parameter
try:
    # Reset any values
    tc.reset();
    # Verify that no monitor values exist
    data = mc.getMonitorData()
    # Calculate if data is present
    alreadyStoredData=0
    for d in data:
        for blob in d.monitorBlobs:
            for blobData in any.from_any(blob.blobDataSeq):
                alreadyStoredData+=1
    if alreadyStoredData != 0:
        print "TEST FAIL: Data present before start monitor"
        raise notNulDataException
    # Start monitor and wait some time to generate data
    mc.startMonitoring(cname)
    time.sleep(10)
    # Stop monitoring
    mc.stopMonitoring(cname)
    # Verify if monitor data has been generated
    data = mc.getMonitorData()
    # Calculate if data is present
    alreadyStoredData=0
    for d in data:
        for blob in d.monitorBlobs:
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
print "RESULTS FROM TEST CASE1: Verify static configuration of archive delta percent parameter"
# Remind of manual verifications
print "Perform manual verification of delta archival percent for properties according to MC_TEST_COMPONENT4.xml:"
print " - doubleSeqProp: 10.0"
print " - doubleProp:    10.0"
print " - longProp:      20.0"
print " - longSeqProp:   20.0"
print " - patternProp:   7.0"

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

mc.deregisterMonitoredDevice(cname)

#cleanly disconnect
simpleClient.releaseComponent(argv[1])
simpleClient.disconnect()
stdout.flush()
