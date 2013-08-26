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
# @(#) $Id: acspyTestContainer.py,v 1.10 2005/05/26 17:23:26 dfugate Exp $

"""
Test component services via a component. In short, this provides complete
testing of the container.
"""

from Acspy.Clients.SimpleClient import PySimpleClient
import acspytest
from time import sleep

simpleClient = PySimpleClient()

#test an immortal component first
print "Testing TESTCOMPONENT (i.e., immortal components)"
test = simpleClient.getComponent("TESTCOMPONENT")
test.sayHello()
val = test.testServices()
if not val:
    print "TESTCOMPONENT.testServices() FAILED!!!"

#test a mortal component next
print "Testing TESTCOMPONENT1 (i.e., mortal components)"
test1 = simpleClient.getComponent("TESTCOMPONENT1")
test1.sayHello()
val = test1.testServices()
if not val:
    print "TESTCOMPONENT1.testServices() FAILED!!!"

#should have been activated and deactivated twice already
print "Testing TESTCOMPONENT2 (i.e., getComponent() and component activation/deactivation"
test2 = simpleClient.getComponent("TESTCOMPONENT2")
test2.invokeSayHello("TESTCOMPONENT")
test2.invokeSayHello("TESTCOMPONENT1")
#DWF-removed because it causes problems for ACS 4.1.x manager involving cyclic dependencies
#test2.invokeSayHello("TESTCOMPONENT2")
test2.invokeSayHello("TESTCOMPONENT3")
test2.invokeSayHello("TESTCOMPONENT4")
test2.invokeSayHello("TESTCOMPONENT5")

#release everything
sleep(5)
print "Deactivating components"
simpleClient.releaseComponent("TESTCOMPONENT")
simpleClient.releaseComponent("TESTCOMPONENT1")

print "The end __oOo__"

