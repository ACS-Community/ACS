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
# @(#) $Id: acspyexmplMountClient.py,v 1.11 2006/03/30 16:58:36 dfugate Exp $
#------------------------------------------------------------------------------
'''
DESCRIPTION
Mount Client is a simple example of a PySimpleClient which inquires manager
about what components are currently available and then plays with 
one of a <a href="../../idl/html/interfaceMOUNT_ACS_1_1Mount.html">Mount</a> components
BACI properties.

WHAT CAN I GAIN FROM THIS EXAMPLE?
- PySimpleClient usage.
- Using manager to dynamically determine what components are available.
- Accessing (remote) components.
- Manipulating BACI properties

LINKS
- <li><a href="../../idl/html/interfaceMOUNT_ACS_1_1Mount.html">Mount IDL Documentation</a>
'''

# Import the acspy.PySimpleClient class
from Acspy.Clients.SimpleClient import PySimpleClient


# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()

# Print information about the available COBs
components = simpleClient.availableComponents()

simpleClient.getLogger().logInfo("COBs available are: ")
for cob in components:
    simpleClient.getLogger().logInfo(cob.name + " of type " + cob.type)

# Do something on a device.
simpleClient.getLogger().logInfo("We can directly manipulate a device once we get it, which is easy.")
try:
    # Get the standard MOUNT1 Mount device
    mount = simpleClient.getComponent("MOUNT1")

    # Get the actAz property
    actAzProperty = mount._get_actAz()

    # Ask the current value of the property
    (azm, compl) = actAzProperty.get_sync()
    simpleClient.getLogger().logInfo("MOUNT1 actual azimuth: " + str(azm))

    # Release it
    simpleClient.releaseComponent("MOUNT1")
    
except Exception, e:
    simpleClient.getLogger().logCritical("Sorry, I expected there to be a Mount in the system and there isn't.")
    simpleClient.getLogger().logDebug("The exception was:" + str(e))

simpleClient.disconnect()
print "The end __oOo__"
