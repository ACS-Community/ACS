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

# Check the following:
#  - Arrays have arr_length values
#  - All array values are equal
#  - Blob data values are consecutive
#  - There are at least min_blobs blobs
#  - There are at most max_blobs blobs
#  - When val0_gt is not None, the first blob must have a value greater than this value
#  - When val0_lt is not None, the first blob must have a value lower than this value
#  - When val0_eq is not None, the first blob must have a value equal to it
def check_data(blob, arr_length, val0_gt, val0_lt, val0_eq, min_blobs, max_blobs):
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
        if val0_gt != None:
            if val0_gt >= n:
                print "\t\tError! We expected first blob value was greater than: ", val0_gt, " but was ", n
            else:
                print "\t\tGreat! First blob is greater than: ", val0_gt
        if val0_lt != None:
            if val0_lt <= n:
                print "\t\tError! We expected first blob value was lower than: ", val0_lt, " but was ", n
            else:
                print "\t\tGreat! First blob is lower than: ", val0_lt
        if val0_eq != None:
            if val0_eq != n:
                print "\t\tError! We expected first blob value was: ", val0_eq, " but was ", n 
            else:
                print "\t\tGreat! First blob is: ", val0_eq
        for v in values:
            if n != v:
                print "\t\t", n, " != ", v
                consecutive = False
            n += 1
        if not consecutive:
            print "\t\tError! Values are not consecutive: ", values
        else:
            print "\t\tGreat! All values are consecutive"
        if len(values) >= min_blobs:
            print "\t\tGreat! Collected at least %d values: %d" % (min_blobs,len(values))
        else:
            print "\t\tError! Collected less than %d values: %d" % (min_blobs,len(values))

        if len(values) <= max_blobs:
            print "\t\tGreat! Collected at most %d values: %d" % (max_blobs,len(values))
        else:
            print "\t\tError! Collected more than %d values: %d" % (max_blobs,len(values))
        
        return values[-1]
    else:
        print "\t\tNo values"
        return None




# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()

mc = simpleClient.getComponent(argv[1])

# First test: Nominal test. Some data buffered and recovered
cname = 'MC_TEST_COMPONENT'
try:
    tc = simpleClient.getComponent(cname)
    psns =[propertySerailNumber('doubleSeqProp', ['12124']),propertySerailNumber('doubleProp', ['3432535'])]    
    mc.registerMonitoredDeviceWithMultipleSerial(cname, psns)
    tc.reset();
    mc.startMonitoring(cname)    
    time.sleep(2)
    mc.stopMonitoring(cname)
    
except MonitorErr.RegisteringDeviceProblemEx, _ex:
    ex = MonitorErrImpl.RegisteringDeviceProblemExImpl(exception=_ex)
    ex.Print(); 

time.sleep(1)
data = mc.getMonitorData()

# First log entry: Print results of the first part of the test
last_values = {}
print "Nominal test. Number of Devices:", len(data);
for d in data:
    print d.componentName, d.deviceSerialNumber 
    for blob in d.monitorBlobs:
        id_comp_attr = get_id_comp_attr(blob)
        last_values[id_comp_attr] = check_data(blob, 25, None, None, None, 2, 2)
        """
        id_comp = get_id_comp_attr(blob)

        print "\t", blob.propertyName, blob.propertySerialNumber
        for blobData in any.from_any(blob.blobDataSeq):
            print "\t\t", blobData
        """

# Second test: Cyclic buffering activation. Reseting MC_TEST_COMPONENT at beginning, verification of data loss
try:
    tc.reset();
    mc.startMonitoring(cname)
    time.sleep(220)
    mc.stopMonitoring(cname)
    
except MonitorErr.RegisteringDeviceProblemEx, _ex:
    ex = MonitorErrImpl.RegisteringDeviceProblemExImpl(exception=_ex)
    ex.Print(); 

time.sleep(1)
data = mc.getMonitorData()
    
# Second log entry: Print results of the second part of the test
print "Cyclic buffer test. Number of Devices:", len(data);
for d in data:
    print d.componentName, d.deviceSerialNumber
    for blob in d.monitorBlobs:
        id_comp_attr = get_id_comp_attr(blob)
        lv = last_values[id_comp_attr]
        last_values[id_comp_attr] = check_data(blob, 25, lv + 15, None, None, 200, 200)
        """
        print "\t", blob.propertyName, blob.propertySerialNumber
        for blobData in any.from_any(blob.blobDataSeq):
            print "\t\t", blobData
        """
    
# Third test: Verification of cyclic buffer restarted after acquisition (no MC_TEST_COMPONENT reset)
try:
    mc.startMonitoring(cname)
    time.sleep(10)
    mc.stopMonitoring(cname)
    
except MonitorErr.RegisteringDeviceProblemEx, _ex:
    ex = MonitorErrImpl.RegisteringDeviceProblemExImpl(exception=_ex)
    ex.Print();
    
data = mc.getMonitorData()
    
# Third log entry: Print results of the third part of the test
print "Cyclic buffer reset test. Number of Devices:", len(data);
for d in data:
    print d.componentName, d.deviceSerialNumber 
    for blob in d.monitorBlobs:
        id_comp_attr = get_id_comp_attr(blob)
        lv = last_values[id_comp_attr]
        last_values[id_comp_attr] = check_data(blob, 25, lv, lv + 3, None, 8, 13)
        """
        print "\t", blob.propertyName, blob.propertySerialNumber
        i=0
        for blobData in any.from_any(blob.blobDataSeq):
            if i<5:
                print "\t\t", blobData
                i+=1
        """
mc.deregisterMonitoredDevice(cname)

#cleanly disconnect
simpleClient.releaseComponent(argv[1])
simpleClient.disconnect()


