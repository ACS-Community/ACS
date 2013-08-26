# @(#) $Id: HelloDemo.py,v 1.8 2005/04/22 00:15:56 dfugate Exp $
#
# Copyright (C) 2001
# Associated Universities, Inc. Washington DC, USA.
#
# Produced for the ALMA project
#
# This library is free software; you can redistribute it and/or modify it under
# the terms of the GNU Library General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your option) any
# later version.
#
# This library is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
# details.
#
# You should have received a copy of the GNU Library General Public License
# along with this library; if not, write to the Free Software Foundation, Inc.,
# 675 Massachusetts Ave, Cambridge, MA 02139, USA.  Correspondence concerning
# ALMA should be addressed as follows:
#
# Internet email: alma-sw-admin@nrao.edu
# "@(#) $Id: HelloDemo.py,v 1.8 2005/04/22 00:15:56 dfugate Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/08/20  Created.
#------------------------------------------------------------------------------
'''
DESCRIPTION
HelloDemo is a trivial Python component implementation for the trivial IDL
interface <a href="../../idl/html/interfacedemo_1_1HelloDemo.html">HelloDemo</a>. This is
about as basic as it gets and developers new to Python components should
begin with this example.

WHAT CAN I GAIN FROM THIS EXAMPLE?
- an example derived from the <a href="../../idl/html/interfaceACS_1_1ACSComponent.html">ACS::ACSComponent</a> IDL interface.
- an introduction to the ACS API.
- using ContainerServices to obtain references to other components.
- limited ACS logging macros.

LINKS
- <a href="classHelloDemo_1_1HelloDemo.html">Hello Demo Class Reference</a>
- <a href="../../idl/html/interfacedemo_1_1HelloDemo.html">Hello Demo IDL Documentation</a>
'''
#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------
import demo__POA
import demo
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Servants.ACSComponent       import ACSComponent
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class HelloDemo(demo__POA.HelloDemo,  #CORBA stubs for IDL interface
                ACSComponent,  #Base IDL interface
                ContainerServices,  #Developer niceties
                ComponentLifecycle):  #HLA stuff
    '''
    Simple component implementation provided as a reference for developers.
    '''
    def __init__(self):
        '''
        Just call superclass constructors here.
        '''
        ACSComponent.__init__(self)
        ContainerServices.__init__(self)
        return
    #------------------------------------------------------------------------------
    #--Override ComponentLifecycle methods-----------------------------------------
    #------------------------------------------------------------------------------
    def initialize(self):
        '''
        Override this method inherited from ComponentLifecycle
        '''
        self.getLogger().logInfo("called...")

        try:
            import acsexmplLamp
            lamp = self.getComponent("LAMP1")
            self.brightness = lamp._get_brightness()
        except Exception, e:
            print "LAMP1 unavailable"
            print e
    #------------------------------------------------------------------------------
    def cleanUp(self):
        '''
        Override this method inherited from ComponentLifecycle
        '''
        self.getLogger().logInfo("called...") 
        self.releaseComponent("LAMP1")
    #------------------------------------------------------------------------------
    #--Implementation of IDL methods-----------------------------------------------
    #------------------------------------------------------------------------------
    def sayHello(self):
        '''
        Python implementation of IDL method.
        string sayHello();
        '''
        self.getLogger().logInfo("called...") 
        return "hello"
    #------------------------------------------------------------------------------
    def sayHelloWithParameters(self, inString, inoutDouble):
        '''
        Python implementation of IDL method.
        string sayHelloWithParameters(in string inString, inout double inoutDouble, out long outInt);
        '''
        self.getLogger().logInfo("called with arguments inString="
                                 + inString
                                 + "; inoutDouble="
                                 + str(inoutDouble)
                                 + ". Will return 'hello'...")
        return ("hello", inoutDouble, 23)

#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Creating an object"
    g = HelloDemo()
    print "Done..."










