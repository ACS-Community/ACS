#!/usr/bin/env python
# @(#) $Id: acspyexmplFridgeNCSupplier.py,v 1.11 2006/03/30 16:58:36 dfugate Exp $
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
#------------------------------------------------------------------------------
'''
DESCRIPTION
This trivial example shows how to publish notification channel events in
Python using the standard FRIDGE::temperatureDataBlockEvent example. In particular,
50 events are sent, pausing for one second between each event.

WHAT CAN I GAIN FROM THIS EXAMPLE?
- Supplier class usage.
- Publishing events.

LINKS
- 
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from time import sleep
#--CORBA STUBS-----------------------------------------------------------------
import FRIDGE
#--ACS Imports-----------------------------------------------------------------
from Acspy.Nc.Supplier      import Supplier
from Acspy.Common.Log       import getLogger
#--GLOBALS---------------------------------------------------------------------
LOGGER = getLogger("FridgeNCSupplier")
#------------------------------------------------------------------------------
if __name__ == "__main__":

    #Create a supplier
    LOGGER.logInfo('Creating an instance of Supplier')
    g = Supplier(FRIDGE.CHANNELNAME_FRIDGE)

    #Create an instance of our user-defined IDL structure
    h = FRIDGE.temperatureDataBlockEvent(3.7, FRIDGE.ATREF)

    #Send 50 events
    LOGGER.logInfo("Ready to send NC events...")
    for i in range(50):
        g.publishEvent(simple_data=h)
        #Really just used for testing purposes
        sleep(1)
    
    LOGGER.logInfo("Events all done . . . exiting")
    #cleanly disconnect from the channel
    g.disconnect()
#------------------------------------------------------------------------------











