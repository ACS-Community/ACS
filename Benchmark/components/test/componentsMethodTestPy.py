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
# @(#) $Id: componentsMethodTestPy.py,v 1.4 2004/11/02 00:42:52 dfugate Exp $
#------------------------------------------------------------------------------

'''
Sample usage:
   componentsMethodTestPy FRIDGE1 1000 100
'''
from sys import argv
from Acspy.Clients.SimpleClient import PySimpleClient
from Acspy.Util.Profiler import Profiler

compName = argv[1]
count = long(argv[2])
size  = long(argv[3])
waitTime = long(argv[4])
msg = argv[5]

# Make an instance of the PySimpleClient
profiler = Profiler()
simpleClient = PySimpleClient()
comp = simpleClient.getComponent(compName)
comp.setup(count, size, waitTime)

for i in range(0, count):
    profiler.start()
    comp.testReturnSize()
    profiler.stop()


profiler.fullDescription(msg)

simpleClient.releaseComponent(compName)
simpleClient.disconnect()
