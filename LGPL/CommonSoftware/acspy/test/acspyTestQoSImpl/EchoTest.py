import acspyTestQoS__POA
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Servants.ACSComponent       import ACSComponent
from time import sleep

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
Module designed to test the Quality of Service (QoS) functionality of ACS.
'''
#------------------------------------------------------------------------------
class EchoTest(acspyTestQoS__POA.EchoTest, # CORBA/POA (generated from IDL)
             ACSComponent,        #Base IDL interface for all ACS-aware components
             ContainerServices,   #Developer niceties
             ComponentLifecycle): #HLA stuff

    def __init__(self):
        ACSComponent.__init__(self)
        ContainerServices.__init__(self)
        return
    
    '''
    Component designed to test the functionality of the Python container.
    '''
    def echo(self, paramToEcho, millisecondsToDelay):
        '''
        Python implementation of echo method from TestQoS IDL module (EchoTest IDL interface).
        '''
        sleep(millisecondsToDelay / 1000); # sleep uses seconds; divide milliseconds by 1000
        return paramToEcho
 
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Creating an object"
    g = PyTest()
    g.sayHello()
    print "Done..."

    print "1:", g
    print "1:", dir(g)
    print methodName
    
    methodName = otherComp.__getattr__(methodName)
    print "2:", methodName
    params = eval(params)
    print "4:", params
    if params == None:
        apply(methodName)
    else:
        apply(methodName, params, {})

