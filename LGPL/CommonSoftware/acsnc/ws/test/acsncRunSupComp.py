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

channel_name = str(argv[2])
autoconnect = str(argv[3]) == 'autoreconnect'
num_sec = int(argv[4])
num_events = int(argv[5])
sleep_val = int(argv[6])

# in long numSec,in long numEvents,in long sleepVal

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()

supplier = simpleClient.getComponent(argv[1])
supplier.execTest(channel_name, autoconnect, num_sec, num_events, sleep_val)
sleep(2)
simpleClient.releaseComponent(argv[1])
