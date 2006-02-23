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
# @(#) $Id: componentsGenericTestPy.py,v 1.2 2004/09/29 22:53:47 dfugate Exp $
#------------------------------------------------------------------------------

'''
Used to test the performance of various ACS components in a generic manner.
The format of usage is:
  componentGenericTest componentName methodA(Param1,Param2) methodB() ... 

and sample usage could be:
  componentGenericTest setup(1000L,5L) method()
  
'''
from sys import argv
from Acspy.Clients.SimpleClient import PySimpleClient

compName = argv[1]

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()
comp = simpleClient.getComponent(compName)

print

# Run the test
for methodInvocation in argv[2:]:
  exec "comp." + str(methodInvocation)

simpleClient.releaseComponent(compName)
simpleClient.disconnect()
