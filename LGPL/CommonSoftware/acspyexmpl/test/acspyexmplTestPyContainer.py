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
# @(#) $Id: acspyexmplTestPyContainer.py,v 1.8 2006/10/25 14:10:28 bjeram Exp $

"""
Test very simple components.
"""

from Acspy.Clients.SimpleClient import PySimpleClient

simpleClient = PySimpleClient()

try:
    #test an immortal cob first
    print "Testing HELLODEMO1 (i.e., immortal components)"
    test1 = simpleClient.getComponent("HELLODEMO1")
    print test1.sayHello()
    print test1.sayHelloWithParameters("I'm an 'inString'", 3.14)

    print
    print "Now test HELLODEMO2 (i.e., non-immortal components)"
    test2 = simpleClient.getComponent("HELLODEMO2")
    print test2.sayHello()
    print test2.sayHelloWithParameters("I'm an 'inString'", 3.14)

    print
    print "Now test getting HELLODEMO2 non sticky"
    test3 = simpleClient.getComponentNonSticky("HELLODEMO2")
    print test3.sayHello()
    print test3.sayHelloWithParameters("I'm an 'inString'", 3.14)
    
except Exception, e:
    print "Test FAILED!!!"
    print "The exception was:", e

from time import sleep
sleep(5)
print "The end __oOo__"
