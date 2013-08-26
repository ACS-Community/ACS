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
###############################################################################
"""
Tests the Python Quality of Service (QoS) system's timeout capabilities.
NOTE: this test case is modeled loosely after the test case for the acsQoS C++ 
      implementation located in ACS/LGPL/CommonSoftware/acsQoS/ws/test.
"""

from Acspy.Clients.SimpleClient import PySimpleClient
import Acspy.Common.QoS as QofS
import sys

def exitGracefully(client):
   client.releaseComponent("TEST_QOS_COMPONENT")
   client.disconnect() 
   sys.exit()

# instantiate the simple client
mySimpleClient = PySimpleClient()

# get the test component used for the method invocations that we invoke to test timeouts
testQoSComponent = mySimpleClient.getComponent("TEST_QOS_COMPONENT")

# first, call method on the servant with no QoS timeout set
try:
	testQoSComponent.echo(0, 1000)
	print "Method w/o timeout executed successfully."

except Exception, e:
	print "ERROR 0: timeout exception caught on method invocation when not expected."
	exitGracefully(mySimpleClient)

# test setting/resetting of a (local) timeout and also test invocation of a method 
# (with a timeout set) that doesn't exceed the time out
try:
   timeoutOne = QofS.Timeout(400)

   # this method should result in an exception because 1000 exceeds timeout of 400
   testQoSComponent.echo(0, 1000)

   # if we got here, the exception was not thrown; that is an error.
   print "ERROR 1: exception was expected but not thrown."
   exitGracefully(mySimpleClient)

except Exception, e: 
   del timeoutOne
   print "exception caught, this is expected."
   print "The exception was:", e

# check if the timeout ended when the Timeout object went out of scope
try:
	testQoSComponent.echo(0, 1000)
	print "Method w/o timeout executed successfully."

except Exception, e:
   print "ERROR 2: exception caught on method invocation when not expected."
   print "The exception was:", e
   exitGracefully(mySimpleClient)

# call methods on the servant with a timeout on the ORB
try:
   QofS.setORBTimeout(300)
   testQoSComponent.echo(0, 400)

except Exception, e:
   print "exception caught, this is expected."
   print "The exception was:", e

try:
   testQoSComponent.echo(0, 250)
   QofS.resetORBTimeout()

except Exception, e:
   print "ERROR 3: exception caught when not expected."
   print "The exception was:", e
   exitGracefully(mySimpleClient)

# check if the timeout on the ORB was properly reset
try:
	testQoSComponent.echo(0, 1000)
	print "Method w/o timeout executed successfully."

except Exception, e:
   print "ERROR 4: exception caught on method invocation when not expected."
   print "The exception was:", e
   exitGracefully(mySimpleClient)

# call methods on the servant with a timeout on the CORBA object
try:
   QofS.setObjectTimeout(testQoSComponent, 400)
   testQoSComponent.echo(0, 1000)
   print "ERROR 5:  exception was expected but not thrown."
   exitGracefully(mySimpleClient)

except Exception, e:
   print "exception caught, this is expected."
   print "The exception was:", e

# check if the timeout on the object properly resets
try:
   QofS.resetObjectTimeout(testQoSComponent)
   testQoSComponent.echo(0, 1000)
   print "Method w/o timeout executed successfully."

except Exception, e:
   print "ERROR 6: exception caught on method invocation when not expected."
   print "The exception was:", e
   exitGracefully(mySimpleClient)

exitGracefully(mySimpleClient)
