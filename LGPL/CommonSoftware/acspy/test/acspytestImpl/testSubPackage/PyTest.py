import acspytest__POA
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Servants.ACSComponent       import ACSComponent
from CORBA import TRUE, FALSE
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
Module designed to test the full functionality of the Python Container.  Since
Python is not a compiled language, its vital that everything be tested.
'''
#------------------------------------------------------------------------------
print "#*# Inside acspytest.testSubPackage.PyTest #*#"


class PyTest(acspytest__POA.PyTest,
             ACSComponent,  #Base IDL interface
             ContainerServices,  #Developer niceties
             ComponentLifecycle):

    def __init__(self):
        ACSComponent.__init__(self)
        ContainerServices.__init__(self)
        return

    def sayHello(self):
        return
    #------------------------------------------------------------------------------
    def invokeSayHello(self, compName):
        return
    #------------------------------------------------------------------------------
    def testMount(self, compName):
        return
    #------------------------------------------------------------------------------
    def testAnything(self, compName, methodName, params):
        return
    #------------------------------------------------------------------------------
    def testServices(self):
        return TRUE
    #------------------------------------------------------------------------------ 
#------------------------------------------------------------------------------
