#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# Copyright (c) European Southern Observatory, 2011 
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id: LifeCycleTest.py,v 1.2 2011/10/28 14:52:25 hsommer Exp $"
#
# who       when      what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/08/20  Created.

#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------
import lifecycleTest__POA
import lifecycleTest
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Servants.ACSComponent       import ACSComponent
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class LifeCycleTest(lifecycleTest__POA.TestLifeCycleComp,  #CORBA stubs for IDL interface
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
    #--Implementation of IDL methods-----------------------------------------------
    #------------------------------------------------------------------------------
    def dummyInterface(self):
        '''
        Python implementation of IDL method.
        '''
        self.getLogger().logInfo("dummyInterface called...") 

#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Creating an object"
    g = LifeCycleTest()
    print "Done..."










