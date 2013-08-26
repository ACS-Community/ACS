#!/usr/bin/env python
# @(#) $Id: acscourseMountSupplier.py,v 1.4 2005/07/04 16:51:58 dfugate Exp $
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
This Python script is designed to provide the developer with a sample implementation
of an event Supplier.
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from time import sleep
#--CORBA STUBS-----------------------------------------------------------------
import ACSCOURSE_MOUNT
#--ACS Imports-----------------------------------------------------------------
from Acspy.Nc.Supplier      import Supplier
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
if __name__ == "__main__":
    #Create a supplier
    g = Supplier(ACSCOURSE_MOUNT.MOUNT_CHANNEL)

    #Create an instance of our user-defined IDL structure
    h = ACSCOURSE_MOUNT.MountEventData(1.1, 2.2)

    #Send 50 distinct events
    for i in range(50):
        g.publishEvent(h)
        sleep(1)
    
    #Cleanly kill the supplier
    g.disconnect()
#------------------------------------------------------------------------------
