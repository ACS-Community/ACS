# @(#) $Id: HelloWorld.py,v 1.1 2008/04/02 13:22:01 acaproni Exp $
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
# "@(#) $Id: HelloWorld.py,v 1.1 2008/04/02 13:22:01 acaproni Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# acaproni   2008/04/02  Created.
#------------------------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------
import CorbaRefTest__POA
import CorbaRefTest
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Servants.ACSComponent       import ACSComponent
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class HelloWorld(CorbaRefTest__POA.HelloWorld,  #CORBA stubs for IDL interface
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
        self.getLogger().logInfo("initialize called...")

    #------------------------------------------------------------------------------
    def cleanUp(self):
        self.getLogger().logInfo("cleanUp called...") 
    #------------------------------------------------------------------------------
    #--Implementation of IDL methods-----------------------------------------------
    #------------------------------------------------------------------------------
    def displayMessage(self):
        self.getLogger().logInfo("displayMessage called...") 
        print "hello"
 #------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Creating an object"
    g = HelloWorld()
    print "Done..."










