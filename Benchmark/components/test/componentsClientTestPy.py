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
# @(#) $Id: componentsClientTestPy.py,v 1.2 2004/09/29 22:53:47 dfugate Exp $
#------------------------------------------------------------------------------

'''
Used to test the performance of various ACS components in a generic manner.
The format of usage is:
  componentGenericTest componentName methodInvocations compMethod

and sample usage could be:
  componentGenericTest FRIDGE1 7 dummieMethod()
  
'''
from sys import argv
from Acspy.Clients.SimpleClient import PySimpleClient
from Acspy.Util.Profiler import Profiler

compName = argv[1]
invocations = int(argv[2])
compMethod = argv[3]

msg = argv[4]

# Make an instance of the PySimpleClient
profiler = Profiler()
simpleClient = PySimpleClient()
comp = simpleClient.getComponent(compName)


for i in range(0, invocations):
  try:
    profiler.start()
    exec "comp." + compMethod
    profiler.stop()
  except Exception, e:
    profiler.stop()
    print "An exception occured:", e

profiler.fullDescription(msg)

simpleClient.releaseComponent(compName)
simpleClient.disconnect()
