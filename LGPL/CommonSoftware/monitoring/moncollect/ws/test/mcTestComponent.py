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

def get_id_comp_attr(blob):
    return str((blob.propertyName, blob.propertySerialNumber))

MIN_BLOBS=9
MAX_BLOBS=20
# Check the following:
#  - Arrays have arr_length values
#  - All array values are equal
#  - Blob data values are consecutive
#  - There are at least MIN_BLOBS blobs
#  - There are at most MAX_BLOBS blobs
#  - When first_value is not None, the first blob must have this value
def check_data(blob, arr_length, first_value):
    global MIN_BLOBS, MAX_BLOBS
    print "\t", blob.propertyName, blob.propertySerialNumber

    last_values = []

    values = []
    for blobData in any.from_any(blob.blobDataSeq):
        if type(blobData['value']) == list:
            if len(blobData['value']) != arr_length:
                print "Error! Expected array of %d values but was of size %d" % (arr_length, len(blobData['value']))
            if len(blobData['value']) > 0:
                value = blobData['value'][0]
                all_equal = True
                for val in blobData['value']:
                    if value != val:
                        all_equal = False
                if not all_equal:
                    print "\t\tError! Not all values are equal: ", blobData['value']
            else:
                value = None
        else:
            value = blobData['value']
        values.append(value)

    if len(values) > 0:
        consecutive = True
        n = values[0]
        if first_value != None:
            if first_value != n:
                print "\t\tError! We expected that the first blob had a value of: ", first_value, " but was ", n
            else:
                print "\t\tGreat! First blob has the expected value: ", first_value
        for v in values:
            if n != v:
                print "\t\t", n, " != ", v
                consecutive = False
            n += 1
        if not consecutive:
            print "\t\tError! Values are not consecutive: ", values
        else:
            print "\t\tGreat! All values are consecutive"
        if len(values) >= MIN_BLOBS:
            print "\t\tGreat! Collected at least %d values: %d" % (MIN_BLOBS,len(values))
        else:
            print "\t\tError! Collected less than %d values: %d" % (MIN_BLOBS,len(values))

        if len(values) <= MAX_BLOBS:
            print "\t\tGreat! Collected at most %d values: %d" % (MAX_BLOBS,len(values))
        else:
            print "\t\tError! Collected more than %d values: %d" % (MAX_BLOBS,len(values))
        
        return values[-1]
    else:
        print "\t\tNo values"
        return None

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
    alreadyStoredData=0
    for d in data:
        for blob in d.monitorBlobs:
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
last_values = {}
print "RESULTS FROM TEST CASE1: Start component monitoring"
print "Number of Devices:", len(data);
for d in data:
    print d.componentName, d.deviceSerialNumber
    for blob in d.monitorBlobs:
        id_comp_attr = get_id_comp_attr(blob)
        last_value = check_data(blob, 25, None)
        last_values[id_comp_attr] = last_value
        """
        print "\t", blob.propertyName, blob.propertySerialNumber
        i=0
        for blobData in any.from_any(blob.blobDataSeq):
            if i<20:
                print "\t\t", blobData
                i+=1
        """

# Test Case 2: Data retrieval does not stop component monitoring
try:
    # Verify that retrieving data has not stopped the monitor
    time.sleep(10)
    data = mc.getMonitorData()
    # Calculate if data is present
    alreadyStoredData=0
    for d in data:
        for blob in d.monitorBlobs:
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
print "RESULTS FROM TEST CASE2: Data retrieval does not stop component monitoring"
print "Number of Devices:", len(data);
for d in data:
    print d.componentName, d.deviceSerialNumber
    for blob in d.monitorBlobs:
        id_comp_attr = get_id_comp_attr(blob) 
        last_value = check_data(blob, 25, last_values[id_comp_attr]+1)
        last_values[id_comp_attr] = last_value
        """
        print "\t", blob.propertyName, blob.propertySerialNumber
        i=0
        for blobData in any.from_any(blob.blobDataSeq):
            if i<20:
                print "\t\t", blobData
                i+=1
        """

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
    alreadyStoredData=0
    for d in data:
        for blob in d.monitorBlobs:
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
print "RESULTS FROM TEST CASE2: Stop component monitoring"
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


