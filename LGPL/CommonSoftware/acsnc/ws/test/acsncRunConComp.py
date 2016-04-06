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

# Get input parameters
test_num = int(argv[1])
comp_name = str(argv[2])
ch_name = str(argv[3])

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()
consumer = simpleClient.getComponent(comp_name)

if test_num == 1:
    autoconnect = str(argv[4]) == 'autoreconnect'
    sleep_time = int(argv[5])
    counter_lower_than = int(argv[6])
    counter_greater_than = int(argv[7])
    consumer.execTest(ch_name, autoconnect)
    sleep(sleep_time)
    if counter_lower_than > 0:
        consumer.checkCounterLowerThan(counter_lower_than)
    if counter_greater_than > 0:
        consumer.checkCounterGreaterThan(counter_greater_than)

elif test_num == 2:
    consumer.execTestResumeSuspend(ch_name)

sleep(5)
simpleClient.releaseComponent(comp_name)
