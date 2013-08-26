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
# @(#) $Id: acspyTestClientMinimum.py,v 1.16 2007/09/21 20:00:37 agrimstrup Exp $

"""
Demonstration of how to use the acspyPySimpleClient class
to access an ACS DO from a Python program
"""

from Acspy.Clients.SimpleClient import PySimpleClient

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()

# Get the standard MOUNT1 Mount device
mount = simpleClient.getComponent("MOUNT1")

# Get the actAz property and print it out
print "MOUNT1 actual azimuth: ", mount._get_actAz().get_sync()[0]

# See what's available
components = [ c.name for c in simpleClient.availableComponents() ]
components.sort()
print "Available components: ", components

from ACS__POA import OffShoot

class MyOffShoot(OffShoot):
    pass

simpleClient.activateOffShoot(MyOffShoot())

#cleanly disconnect
simpleClient.releaseComponent("MOUNT1")
simpleClient.disconnect()

print "The end __oOo__"
