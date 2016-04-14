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
# @(#) $Id: acsncTestConSup.py,v 1.6 2006/03/08 17:50:44 dfugate Exp $

"""
"""

from Acspy.Clients.SimpleClient import PySimpleClient
from sys                        import argv
from time                       import sleep
import time
import datetime
import threading

def lm(msg):
    t = str(datetime.datetime.fromtimestamp(time.time()))
    print "%s %s"%(t.replace(" ","T"),msg)

class ExecConsumerThread(threading.Thread):
    def __init__(self, threadID, client, ch_name, autoreconnect, sleep_time, counter_lower_than, counter_greater_than):
        threading.Thread.__init__(self)
        self.threadID = threadID
        self.comp_name = "CON_COMP_" + str(threadID)
        self.client = client
        self.ch_name = ch_name
        self.autoreconnect = autoreconnect
        self.sleep_time = sleep_time
        self.counter_lower_than = counter_lower_than
        self.counter_greater_than = counter_greater_than

    def run(self):
        consumer = self.client.getComponent(self.comp_name)
        consumer.execTest(self.ch_name, self.autoreconnect)
        sleep(self.sleep_time)
        self.lm("Checking the counter of consumer " + self.comp_name)
        if self.counter_lower_than > 0:
            consumer.checkCounterLowerThan(self.counter_lower_than)
        if self.counter_greater_than > 0:
            consumer.checkCounterGreaterThan(self.counter_greater_than)
        sleep(4+self.threadID)
        self.lm("Releasing component " + self.comp_name)
        self.client.releaseComponent(self.comp_name)
        self.lm("Released component " + self.comp_name)

    def lm(self,msg):
        t = str(datetime.datetime.fromtimestamp(time.time()))
        print "%s %s"%(t.replace(" ","T"),msg)

class ExecSupplierThread(threading.Thread):
    def __init__(self, threadID, client, ch_name, autoreconnect, n_sec, n_events, sleep_val):
        threading.Thread.__init__(self)
        self.threadID = threadID
        self.comp_name = "SUP_COMP_" + str(threadID)
        self.client = client
        self.ch_name = ch_name
        self.autoreconnect = autoreconnect
        self.n_sec = n_sec
        self.n_events = n_events
        self.sleep_val = sleep_val

    def run(self):
        supplier = self.client.getComponent(self.comp_name)
        supplier.execTest(self.ch_name, self.autoreconnect, self.n_sec, self.n_events, self.sleep_val)
        sleep(5)
        self.lm("Releasing component " + self.comp_name)
        self.client.releaseComponent(self.comp_name)
        self.lm("Released component " + self.comp_name)

    def lm(self,msg):
        t = str(datetime.datetime.fromtimestamp(time.time()))
        print "%s %s"%(t.replace(" ","T"),msg)



# Get input parameters
ch_name = str(argv[1])
autoconnect = str(argv[2]) == 'autoreconnect'
sleep_time = int(argv[3])
counter_lower_than = int(argv[4])
counter_greater_than = int(argv[5])
n_consumers = int(argv[6])
n_suppliers = 1
n_sup_sec = sleep_time - 15
n_events_per_sec = 100

simpleClient = PySimpleClient()
threads = []
sup_threads = []
lm("Creating consumer threads")
for i in range(n_consumers):
    th = ExecConsumerThread(i+1,simpleClient,ch_name,autoconnect,sleep_time,counter_lower_than,counter_greater_than)
    threads.append(th)

lm("Creating supplier threads")
for i in range(n_suppliers):
    th = ExecSupplierThread(i+1,simpleClient,ch_name,autoconnect,n_sup_sec,n_events_per_sec,1)
    sup_threads.append(th)

lm("Starting consumer threads")
for th in threads:
    th.start()

sleep(1)
lm("Starting supplier threads")
for th in sup_threads:
    th.start()

lm("Waiting supplier threads")
for th in sup_threads:
    th.join()

lm("Waiting consumer threads")
for th in threads:
    th.join()

sleep(20)
lm("End multiple consumers")
#simpleClient.disconnect()

"""
comp_names = []

# Make an instance of the PySimpleClient
consumers = []
simpleClient = PySimpleClient()
for i in range(n_consumers):
    comp_name = "CON_COMP_" + str(i+1)
    comp_names.append(comp_name)
    consumer = simpleClient.getComponent(comp_name)
    consumers.append(consumer)

for consumer in consumers:
    thread.start_new_thread(exec_test, (consumer, ch_name, autoreconnect))
    #consumer.execTest(ch_name, autoconnect)

sleep(sleep_time)

for consumer in consumers:
    if counter_lower_than > 0:
        consumer.checkCounterLowerThan(counter_lower_than)
    if counter_greater_than > 0:
        consumer.checkCounterGreaterThan(counter_greater_than)

sleep(5)
for comp_name in comp_names:
    lm("Releasing consumer component %s"%(comp_name))
    simpleClient.releaseComponent(comp_name)
sleep(30)
lm("End consumers")
"""
