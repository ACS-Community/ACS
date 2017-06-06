#!/usr/bin/env python
from Acspy.Clients.SimpleClient import PySimpleClient
from Acspy.Common.Err import ACSError
from Acspy.Common import TimeHelper
from Acspy.Nc.Supplier import Supplier
from time import sleep, time
import bulkdata

c = PySimpleClient()
supplier = Supplier(bulkdata.CHANNELNAME_ERR_PROP)
#print bulkdata.BAD_SENDER
#print bulkdata.BAD_RECEIVER
#print bulkdata.OK
print "I will send events ..."
flow = "00"
for x in range(0,10000):
    timestamp = TimeHelper.TimeUtil().epoch2py(TimeHelper.getTimeStamp())
    h = bulkdata.errorStatusBlock(flow, bulkdata.BAD_RECEIVER, timestamp)
#    if x % 2 == 0:
#        h.status = bulkdata.OK
    supplier.publishEvent(h)
    sleep(0.1)
    print (x,h)

supplier.disconnect()
