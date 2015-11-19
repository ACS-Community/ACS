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
import threading

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()

#consumer = simpleClient.getComponent("NamedCh_CON1")

n_events = 20
sleep_sec = 1 
n_supp = 5 

def worker(i, num_events, sleep_value, autoreconnect):
	name = "NamedCh_SUP" + str(i)
	supplier = simpleClient.getComponent(name)
	supplier.sendEvents2(num_events, sleep_value, autoreconnect)
	sleep(15)
	simpleClient.releaseComponent(name)
	sleep(10)

# Create one thread for each component
threads = []
autoreconnect = True
for i in range(1, n_supp+1):
	threads.append(threading.Thread(target=worker, args=(i, n_events, sleep_sec, autoreconnect)))
	autoreconnect = not autoreconnect

# Start all threads
for th in threads:
	th.start()

# Wait until all threads have terminated
for th in threads:
	th.join()

simpleClient.disconnect()
