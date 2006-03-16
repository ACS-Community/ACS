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
# @(#) $Id: acssimInParamsTest.py,v 1.2 2006/03/16 00:03:02 dfugate Exp $
#------------------------------------------------------------------------------

'''
Tests to ensure parameters are available.
'''
from sys import argv
from Acspy.Clients.SimpleClient import PySimpleClient
import acstime
from time import sleep

compName = argv[1]

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()
comp = simpleClient.getComponent(compName)
my_epoch = acstime.Epoch(34L)

#should see my_epoch printed to standard out from the container-side
comp.getTimeInterval(my_epoch)

#should see TSArray and my_epoch printed to standard out from the container-side
comp.toISO8601(acstime.TSArray, my_epoch)
    
simpleClient.releaseComponent(compName)
simpleClient.disconnect()

sleep(10)
