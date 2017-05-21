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
# @(#) $Id: acspyTestSupplierReconn.py,v 1.1 2005/06/15 20:16:48 dfugate Exp $
###############################################################################
'''
Tests the Python Supplier.
'''
###############################################################################
# argv1: channel name
# argv2: number of events to publish
# argv3: autoreconnect
# argv4: notify service restarted or stopped
from Acspy.Nc.Supplier import Supplier
from Acspy.Nc.Consumer import Consumer
from sys import argv
from time import sleep
import acsnc
import datetime
import time
import os
import sys
from subprocess import call

num_suppliers = 1 
num_consumers = 5
channel_name = str(argv[1])
num_events = int(argv[2])
autoreconnect = True if str(argv[3]) == "AUTORECONNECT" else False
ns_restarted = True if str(argv[4]) == "NS_RESTARTED" else False

idx_events = [0] * num_suppliers
num_events_dropped = [0] * num_suppliers
num_exceptions_thrown = [0] * num_suppliers
num_events_sent = [0] * num_suppliers
events_status = [True] * num_suppliers
transitions = [[]] * num_suppliers

#------------------------------------------------------------------------------
class MyEventCallback:
    def __init__(self,idx):
        self.idx = idx

    def eventDropped(self,event): 
        global idx_events, num_events_dropped, events_status, transitions
        idx_events[self.idx] += 1
        num_events_dropped[self.idx] += 1
        if events_status[self.idx]:
            events_status[self.idx] = False
            transitions[self.idx].append(idx_events[self.idx])

    def eventSent(self,event):
        global idx_events, num_events_sent, events_status, transitions
        idx_events[self.idx] += 1
        num_events_sent[self.idx] += 1
        if not events_status[self.idx]:
            events_status[self.idx] = True
            transitions[self.idx].append(idx_events[self.idx])

    def exceptionThrown(self,event):
        global idx_events, num_exceptions_thrown, events_status, transitions
        idx_events[self.idx] += 1
        num_exceptions_thrown[self.idx] += 1
        if events_status[self.idx]:
            events_status[self.idx] = False
            transitions[self.idx].append(idx_events[self.idx])
       

#------------------------------------------------------------------------------
def publish_all_data(suppliers,num_events,data):
    #send variable number of events
    num_suppliers = len(suppliers)

    # Create callback objects
    cb_objs = []
    for idx_supplier in range(len(suppliers)):
        cb_objs.append(MyEventCallback(idx_supplier))

    curr_t = datetime.datetime.now()
    print curr_t," ===  Sending %d events" % (num_events)
    for i in range(num_events):
        for idx_supplier in range(len(suppliers)):
            try:
                #print "-------> Publishing event %d" % (i)
                suppliers[idx_supplier].publishEvent(data,event_callback=cb_objs[idx_supplier])
            except:
                cb_objs[idx_supplier].exceptionThrown(data)

        sleep(1)
        curr_t = datetime.datetime.now()
        print curr_t," ===  Published events at iteration %d" % (i)

    for i in range(len(suppliers)):
        print datetime.datetime.now()," ===  %d: %d exceptions caught" % (i,num_exceptions_thrown[i])
        print datetime.datetime.now()," ===  %d: %d events dropped" % (i,num_events_dropped[i])
        print datetime.datetime.now()," ===  %d: %d events sent" % (i,num_events_sent[i])
        print datetime.datetime.now()," ===  %d: Transitions: %s" % (i,str(transitions[i]))
        
      
#------------------------------------------------------------------------------
# Method to print a message
def lm(msg):
    t = str(datetime.datetime.fromtimestamp(time.time()))
    print "%s %s"%(t.replace(" ","T"), msg)


n_events_received = 0
#------------------------------------------------------------------------------
class TestConsumer (Consumer):
    def processEvent (self,type_name=None,event_name=None,corba_any=None,se=None):
        global n_events_received
        n_events_received += 1

save_stdout = sys.stdout
def no_stdout():
    global save_stdout
    save_stdout = sys.stdout
    f = open(os.devnull, 'w')
    sys.stdout = f

def restore_stdout():
    global save_stdout
    sys.stdout = save_stdout

#------------------------------------------------------------------------------
def send_events(supplier, event, n_events):
    n_events_sent = 0
    for i in range(n_events):
        try:
            supplier.publishEvent(event)
            n_events_sent += 1
        except:
            lm("Error! Event %d coudln't be sent"%(i))
    lm("==========================  Successfully sent %d events"%(n_events_sent)) 

#------------------------------------------------------------------------------
def assert_n_events_received(n_events_expected, n_events_received):
    if n_events_expected != n_events_received:
        lm("==========================  Error! Expected %d events but was %d"%(n_events_expected, n_events_received)) 
    else:
        lm("==========================  Great! Received %d events"%(n_events_received)) 


#------------------------------------------------------------------------------
# Create consumers
lm("==========================  Creating %d consumers"%(num_consumers)) 
consumers = []
for i in range(num_consumers):
    c = TestConsumer(channel_name)
    c.set_autoreconnect(True)
    c.addSubscription(acsnc.EventDescription)
    c.consumerReady()
    consumers.append(c)

#-----------------------------------------------------------------------------
# Wait 10 seconds to ensure the Notify Service is restarted
lm("==========================  Restart the Notify Service and wait some time to allow consumers reconnecting again")
call(["acspyExecNotifyService.sh","NotifyEventChannelFactory","RESTART"])
sleep(15)
lm("==========================  At this point the Notify Service should have been restarted and all consumers reconnected")

#------------------------------------------------------------------------------
# Create a supplier and send events
n_events = 20
lm("==========================  Create a supplier and send %d events"%(n_events))
s = Supplier(channel_name)
s.set_autoreconnect(True)
h = acsnc.EventDescription("no name", 17L, 17L)
send_events(s, h, n_events)

#------------------------------------------------------------------------------
# Wait and check the number of events received
lm("==========================  Events sent. Wait 5 seconds")
sleep(5)
assert_n_events_received((n_events * num_consumers), n_events_received)

#------------------------------------------------------------------------------
# Suspend consumers
lm("==========================  Suspend consumers")
for c in consumers: c.suspend()

#-----------------------------------------------------------------------------
# Restart the Notify Service
lm("==========================  Restat the Notify Service and wait 15 sec while consumers are suspended")
call(["acspyExecNotifyService.sh","NotifyEventChannelFactory","RESTART"])
sleep(15)
lm("==========================  At this point the Notify Service should have been restarted and no consumers reconnected")

#------------------------------------------------------------------------------
# Resume the consumers
lm("==========================  Resume consumers. All consumers should reconnect after 10 sec")
for c in consumers: c.resume()
sleep(10)

#------------------------------------------------------------------------------
lm("==========================  At this point all consumers should be reconnected")

#------------------------------------------------------------------------------
# Send events
n_events_received = 0
n_events = 20
lm("==========================  Send %d events"%(n_events))
send_events(s, h, n_events)

#------------------------------------------------------------------------------
# Wait and check the number of events received
lm("==========================  Events sent. Wait 5 seconds")
sleep(5)
assert_n_events_received((n_events * num_consumers), n_events_received)

#------------------------------------------------------------------------------
# Disable auto reconnection of consumers
lm("==========================  Disabling auto reconnection of consumers")
for c in consumers: c.set_autoreconnect(False)
lm("==========================  Disabled auto reconnection of consumers")

#------------------------------------------------------------------------------
# Restarting the Notify Service and send some events
n_events_received = 0
n_events = 20
lm("==========================  Restarting the Notify Service and send %d events"%(n_events))
call(["acspyExecNotifyService.sh","NotifyEventChannelFactory","RESTART"])
sleep(15)
send_events(s, h, n_events)
sleep(5)
assert_n_events_received(0, n_events_received)

#------------------------------------------------------------------------------
# Enable auto reconnection of consumers
lm("==========================  Enabling auto reconnection of consumers and wait to allow them to reconnect")
for c in consumers: c.set_autoreconnect(True)
sleep(10)
lm("==========================  Enabled auto reconnection of consumers and all of them should be connected again")

#------------------------------------------------------------------------------
# Resume and suspend some times the consumers
lm("==========================  Suspend and resume some times the consumers")
for c in consumers: c.suspend()
for c in consumers: c.resume()
for c in consumers: c.suspend()
for c in consumers: c.resume()
for c in consumers: c.suspend()

#------------------------------------------------------------------------------
# Restart the Notify Service while consumers are suspended
lm("==========================  Restarting the Notify Service while consumers are suspended")
call(["acspyExecNotifyService.sh","NotifyEventChannelFactory","RESTART"])
sleep(15)
lm("==========================  At this point the Notify Service should have been restarted")

#------------------------------------------------------------------------------
# Send some events
n_events_received = 0
n_events = 20
lm("==========================  Send %d events after the Notify Service has been restarted and the consumers are suspended"%(n_events))
send_events(s, h, n_events)
sleep(5)
assert_n_events_received(0, n_events_received)

#------------------------------------------------------------------------------
# Resume the consumers and expect to reconnect to the channel
lm("==========================  Resuming the consumers and expect to reconnect to the channel")
for c in consumers: c.resume()
sleep(10)
lm("==========================  At this point all consumers should have been reconnected to the channel")

#------------------------------------------------------------------------------
# Send some events
n_events_received = 0
n_events = 20
lm("==========================  Send %d events. All consumers are already resumed"%(n_events))
send_events(s, h, n_events)
sleep(5)
assert_n_events_received((n_events * num_consumers), n_events_received)


#------------------------------------------------------------------------------
# Stop the Notify Service
lm("==========================  Stopping the Notify Service and wait 15 sec")
call(["acspyExecNotifyService.sh","NotifyEventChannelFactory","STOP"])
sleep(15)
lm("==========================  Waited 15 sec with the Notify Service stopped")

#------------------------------------------------------------------------------
lm("==========================  Starting the Notify Service and wait 15 sec")
call(["acspyExecNotifyService.sh","NotifyEventChannelFactory","START"])
sleep(15)
lm("==========================  At this point the consumers should be connected again")

#------------------------------------------------------------------------------
# Send some events
n_events_received = 0
n_events = 20
lm("==========================  Send %d events"%(n_events))
send_events(s, h, n_events)
sleep(5)
assert_n_events_received((n_events * num_consumers), n_events_received)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Disconnect the consumers except the first one
lm("==========================  Disconnect consumers except the first one")
for c in consumers[1:]:
    c.disconnect()


#------------------------------------------------------------------------------
#
lm("==========================  Resume and suspend many times the first consumer")
consumers[0].resume()
consumers[0].resume()
consumers[0].suspend()
consumers[0].suspend()
consumers[0].resume()
consumers[0].resume()
consumers[0].suspend()
consumers[0].suspend()
consumers[0].resume()
consumers[0].resume()
lm("==========================  Finished Resuming and suspending the first consumer")

#------------------------------------------------------------------------------
# Disconnect the consumers except the first one
lm("==========================  Disconnect the first consumer")
consumers[0].disconnect()


