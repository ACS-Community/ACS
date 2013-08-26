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
# @(#) $Id: acspyexmplMountCallback.py,v 1.10 2006/03/30 16:58:36 dfugate Exp $
#------------------------------------------------------------------------------
'''
DESCRIPTION
Mount Callback is a complex example of a PySimpleClient which provides the
complete implementation of a BACI callback class and then retrieves a reference
to one of a <a href="../../idl/html/interfaceMOUNT_ACS_1_1Mount.html">Mount</a> components
BACI properties. Once it is has the BACI property, a monitor is created which
is destroyed after ten seconds.

WHAT CAN I GAIN FROM THIS EXAMPLE?
- PySimpleClient usage.
- Accessing (remote) components.
- Manipulating BACI properties.
- Providing your own implementation of callback classes instead of using the Acspy.Common.Callbacks package.
- Creating monitors for BACI properties.

LINKS
- <a href="classacspyexmplMountCallback_1_1MyMonitor.html">MyMonitor Class Documentation</a>
- <a href="../../idl/html/interfaceMOUNT_ACS_1_1Mount.html">Mount IDL Documentation</a>
'''
#------------------------------------------------------------------------------
from Acspy.Clients.SimpleClient import PySimpleClient # Import the acspy.PySimpleClient class
import ACS, ACS__POA                                  # Import the Python CORBA stubs for BACI
from   omniORB.CORBA import TRUE, FALSE
#------------------------------------------------------------------------------
#First a callback implementation class must be defined
class MyMonitor(ACS__POA.CBdouble):
    #------------------------------------------------------------------------------
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    mount device for any monitors we may create.
    '''
    #------------------------------------------------------------------------------
    def __init__ (self, propName = None): 
        '''
        If the propertys name is specified, it is printed to STDOUT later on.
        '''
        self.count = 0
        if propName != None:
            self.propName = propName
        else:
            self.propName = "NoName"
    #------------------------------------------------------------------------------
    def __del__(self):
        '''
        Do nothing
        '''
    #------------------------------------------------------------------------------
    def working (self, value, completion, desc):
        '''
        Really this is the method that does all the work and the developer should
        be concerned with.
        Parameters: value = the double we are interested in
                    completion = completion structure
                    desc = callback struct description
        '''
        self.count = self.count + 1
        if self.count <= 5:
            print "Working: ", str(self.propName), " is ", str(value)
    #------------------------------------------------------------------------------
    def done (self, value, completion, desc):
        '''
        Invoked asynchronously when the DO has finished.  Normally this is invoked
        just before a monitor is destroyed.
        Parameters: value = the final value of the double we are interested in
                    completion = completion structure
                    desc = callback struct description
        '''
        print "Done: ", str(self.propName), " is ", str(value)
    #------------------------------------------------------------------------------
    def negotiate (self, time_to_transmit, desc):
        '''
        For simplicitys sake, we always return true.  If you want more detailed,
        information on this method, please see the BACI specs.
        Parameters: See the BACI specs.
        '''
        return TRUE
#------------------------------------------------------------------------------


# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()

# Do something on a device.
simpleClient.getLogger().logInfo("We can directly manipulate a device once we get it, which is easy!!")

# Get the standard MOUNT1 Mount device
mount = simpleClient.getComponent("MOUNT1")
# Get the actAz property
actAzProperty = mount._get_actAz()

# Create a callback monitor for the actAz Property
cbMon = MyMonitor("actAz")
# Activate the callback monitor
cbMonServant = simpleClient.activateOffShoot(cbMon)
# Create the real monitor registered with MOUNT1
desc = ACS.CBDescIn(0L, 0L, 0L)
actMon = actAzProperty.create_monitor(cbMonServant, desc)

# Tell MOUNT1 that the monitor's working method should be invoked 1 once second
actMon.set_timer_trigger(10000000)
# destroy the monitor after ten seconds
from time import sleep
sleep(10)
actMon.destroy()
sleep(5)

# Release it
simpleClient.releaseComponent("MOUNT1")

simpleClient.disconnect()
print "The end __oOo__"











