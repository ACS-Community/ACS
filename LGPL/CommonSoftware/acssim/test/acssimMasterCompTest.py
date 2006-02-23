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
# @(#) $Id: acssimMasterCompTest.py,v 1.3 2005/11/23 18:40:03 dfugate Exp $
#------------------------------------------------------------------------------

'''
Master component test.
'''
from sys import argv
import ACS
from Acspy.Clients.SimpleClient import PySimpleClient

compName = "MS1"
compMethod = "_get_currentStateHierarchy().get_sync()[0]"


# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()
comp = simpleClient.getComponent(compName)

joe = eval("comp." + compMethod)
try:
    joe = joe[len(joe)-1]
except:
    pass

print "Value is:", joe

    
simpleClient.releaseComponent(compName)
simpleClient.disconnect()
