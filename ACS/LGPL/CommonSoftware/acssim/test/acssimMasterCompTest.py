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
# @(#) $Id: acssimMasterCompTest.py,v 1.6 2006/10/13 07:34:21 gchiozzi Exp $
#------------------------------------------------------------------------------

'''
Master component test.
'''
from sys import argv
from time import sleep
#import ACS
from Acspy.Clients.SimpleClient import PySimpleClient

compName = "MS1"

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()

try:
    comp = simpleClient.getComponent(compName)
    joe = comp._get_currentStateHierarchy().get_sync()[0]
    joe = joe[len(joe)-1]
    print "Value is:", joe
except Exception, e:
    # We simply print a message and let the exception 
    print "An exception occurred!\n"
    print e

sleep(5)
simpleClient.releaseComponent(compName)
simpleClient.disconnect()
