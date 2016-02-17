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

autoconnect = str(argv[3]) == 'autoreconnect'
sleep_time = int(argv[4])
counter_lower_than = int(argv[5])
counter_greater_than = int(argv[6])

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()

consumer = simpleClient.getComponent(argv[1])
consumer.execTest(argv[2], autoconnect)
sleep(sleep_time)
if counter_lower_than > 0:
    consumer.checkCounterLowerThan(counter_lower_than)
if counter_greater_than > 0:
    consumer.checkCounterGreaterThan(counter_greater_than)
sleep(5)
simpleClient.releaseComponent(argv[1])
sleep(2)
simpleClient.disconnect()
