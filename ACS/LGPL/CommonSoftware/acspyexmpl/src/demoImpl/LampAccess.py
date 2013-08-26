# @(#) $Id: LampAccess.py,v 1.9 2005/04/22 00:15:56 dfugate Exp $
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
# "@(#) $Id: LampAccess.py,v 1.9 2005/04/22 00:15:56 dfugate Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/08/20  Created.
#------------------------------------------------------------------------------
''' 
DESCRIPTION
Another simple component example. LampAccess has added power in the fact that
it shows developers how to set the values of BACI properties asynchronously
in addition to retrieving them synchronously.

WHAT CAN I GAIN FROM THIS EXAMPLE?
- an example derived from the <a href="../../idl/html/interfaceACS_1_1ACSComponent.html">ACS::ACSComponent</a>
IDL interface.
- using ContainerServices to obtain references to other components.
- utilizing callback helper classes to invoke asynchronous methods.
- synchronously retrieving the values of BACI properties.
- limited ACS logging macros.

LINKS
- <a href="classLampAccess_1_1LampAccess.html">Lamp Access Class Reference</a>
- <a href="../../idl/html/interfacedemo_1_1LampAccess.html">Lamp Access IDL Documentation</a>
'''
#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------
import demo__POA
import demo
import ACS
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Servants.ACSComponent       import ACSComponent
from Acspy.Common.Callbacks            import CBvoid
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class LampAccess(demo__POA.LampAccess,  #CORBA stubs for IDL interface
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
            lamp = self.getComponent("LAMP1")
            self.brightness = lamp._get_brightness()
        except Exception, e:
            print "LAMP1 unavailable"
            print e
            self.brightness = None
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
    def setLampBrightness(self, brightness):
        '''
        Python implementation of IDL method.
        void setLampBrightness(in double brightness) raises (LampUnavailable);
        '''
        self.getLogger().logInfo("called...")

        #raise the CORBA exception if the lamp doesn't exist
        if self.brightness == None:
            raise demo.LampUnavailable()

        #set the brightness
        #first create a CBvoid class to be used with the set asynchronous command
        joe = CBvoid()
        self.brightness.set_async(float(brightness),  #double value for the property
                                  self.activateOffShoot(joe),  #create the CORBA servant for callback instance
                                  ACS.CBDescIn(0L, 0L, 0L))  #just cut and paste this code=)

        #Nothing to return
        return
    #------------------------------------------------------------------------------
    def getLampBrightness(self):
        '''
        Python implementation of IDL method.
        double getLampBrightness() raises (LampUnavailable);
        '''
        self.getLogger().logInfo("called...")

        #raise the CORBA exception is the lamp doesn't exist
        if not self.brightness:
            raise demo.LampUnavailable()

        #get the brightness value along with the completion
        (realValue, completion) = self.brightness.get_sync()

        #return what the developer is interested in
        return realValue

#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Creating an object"
    g = LampAccess()
    print "Done..."










