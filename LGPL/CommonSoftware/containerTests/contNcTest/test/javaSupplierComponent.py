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
# @(#) $Id: javaSupplierComponent.py,v 1.1 2008/05/23 12:53:45 eallaert Exp $
"""
"""
from Acspy.Clients.SimpleClient import PySimpleClient
from time import sleep
import sys

simpleClient = PySimpleClient()
sleep(5)

print "Getting the Supplier"
sys.stdout.flush()
mySupplierComp = simpleClient.getComponent("SUPPLIERCOMP1")


print "Sending events"
sys.stdout.flush()
for i in range(50):
    mySupplierComp.sendEvents(1)
    sleep(1)

simpleClient.releaseComponent("SUPPLIERCOMP1")

print "Finished sending events"
sys.stdout.flush()
