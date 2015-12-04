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
from sys import argv
from time import sleep
import acsnc
import datetime

num_suppliers = 1 
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

#create suppliers
g = Supplier(channel_name)
g.set_autoreconnect(autoreconnect)
#g_autorec = Supplier(channel_name)
#g_autorec.set_autoreconnect(True)

#create data to send
h = acsnc.EventDescription("no name",
                           17L,
                           17L)

publish_all_data([g], num_events, h)

for idx in range(len(transitions)):
    n_transitions = len(transitions[idx])
    if autoreconnect:
        if ns_restarted:
            if n_transitions != 0 and n_transitions != 2:
                print datetime.datetime.now(),"===  %d: Wrong number of transitions. We expected 0 or 2 but was %d" % (idx,n_transitions)
        else:
            if n_transitions != 1:
                print datetime.datetime.now(),"===  %d: Wrong number of transitions. We expected 1 but was %d" % (idx,n_transitions)

    else:
        if ns_restarted:
            if n_transitions != 1:
                print datetime.datetime.now(),"===  %d: Wrong number of transitions. We expected 1 but was %d" % (idx,n_transitions)
        else:
            if n_transitions != 1:
                print datetime.datetime.now(),"===  %d: Wrong number of transitions. We expected 1 but was %d" % (idx,n_transitions)


#disconnect
g.disconnect()
#g_autorec.disconnect()
