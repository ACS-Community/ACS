# @(#) $Id: LampCallback.py,v 1.8 2005/04/22 00:15:56 dfugate Exp $
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
# "@(#) $Id: LampCallback.py,v 1.8 2005/04/22 00:15:56 dfugate Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/08/20  Created.
#------------------------------------------------------------------------------
'''
DESCRIPTION
LampCallback is fairly complicated as far as Python components are concerned.
As its name implies, it creates a callback in the form of a BACI monitor on
one of a <a href="../../idl/html/interfaceacsexmplLamp_1_1Lamp.html">Lamp</a> components
BACI properties.

WHAT CAN I GAIN FROM THIS EXAMPLE?
- an example derived from the <a href="../../idl/html/interfaceACS_1_1ACSComponent.html">ACS::ACSComponent</a>
IDL interface.
- using ContainerServices to obtain references to other components.
- utilizing callback helper classes to create BACI monitors.
- synchronously retrieving the values of BACI properties.
- an introduction to the Python ACS Error System API.
- limited ACS logging macros.
 
LINKS
- <a href="classLampCallback_1_1LampCallback.html">Lamp Callback Class Reference</a>
- <a href="../../idl/html/interfacedemo_1_1LampCallback.html">Lamp Callback IDL Documentation</a>
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

from Acspy.Common.Callbacks            import CBdouble
from Acspy.Common.Err                  import ACSError
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class LampCallback(demo__POA.LampCallback,  #CORBA stubs for IDL interface
                   ACSComponent,  #Base IDL interface
                   ContainerServices,  #Developer niceties
                   ComponentLifecycle):  #HLA stuff
    '''
    Simple component implementation provided as a reference for developers.
    '''
    def __init__(self):
        '''
        Just call superclass constructors here and declare members.
        '''
        ACSComponent.__init__(self)
        ContainerServices.__init__(self)
        
        self.brightness = None
        self.cb = None
        self.monitor = None
        return
    #------------------------------------------------------------------------------
    #--Override ComponentLifecycle methods-----------------------------------------
    #------------------------------------------------------------------------------
    def cleanUp(self):
        '''
        Override this method inherited from ComponentLifecycle
        '''
        self.getLogger().logInfo("called...")

        if self.brightness != None:
            self.releaseComponent("LAMP1")
            self.cb._release()  #remember this is a CORBA object!!!
        return
    #------------------------------------------------------------------------------
    #--Implementation of IDL methods-----------------------------------------------
    #------------------------------------------------------------------------------
    def monitorLampBrightness(self):
        '''
        Python implementation of IDL method.
        double monitorLampBrightness() raises (LampUnavailable);
        '''
        if self.brightness == None:  #method failed or hasn't been called before
            try:
                lamp = self.getComponent("LAMP1")
                self.brightness = lamp._get_brightness()
                self.cb = CBdouble(name="brightness", archive=1)
                self.monitor = None
            except Exception, e:
                raise demo.LampUnavailable()
            
        #only create a monitor if one has not been created yet.
        if self.monitor == None:
            self.monitor = self.brightness.create_monitor(self.activateOffShoot(self.cb), #callback double
                                                          ACS.CBDescIn(0L, 0L, 0L)) #just cut and past this
            #monitors once per second
            self.monitor.set_timer_trigger(10000000)
            self.getLogger().logInfo("Monitor for brightness property created!")
        else:
            self.getLogger().logInfo("Request to create an additional monitor for brightness property denied!")
            
        return self.brightness.get_sync()[0]
    #------------------------------------------------------------------------------
    def stopMonitor(self):
        '''
        Python implementation of IDL method.
        void stopMonitor() raises (LampUnavailable);
        '''
        print "brightness monitor values were:", self.cb.values

        if self.monitor != None:
            self.monitor.destroy()  #destroy the CORBA monitor
            self.monitor = None  #reset
            self.cb.values = []  #reset
        else:
            raise demo.LampUnavailable()
        
        return
    #------------------------------------------------------------------------------
    def exceptionMethod(self):
        '''
        Python implementation of IDL method.
        void exceptionMethod() raises (LampUnavailable);
        '''
        raise demo.LampUnavailable()
        return
    #------------------------------------------------------------------------------
    def acsExceptionMethodVoid(self):
        '''
        Python implementation of IDL method.
        void acsExceptionMethodVoid() raises (ACSErr::ACSException);
        '''
        try:
            raise EOFError, "Not a real EOF error"
        except EOFError:
            raise ACSError(exception = EOFError)
        return
    #------------------------------------------------------------------------------
    def acsExceptionMethodDouble(self):
        '''
        Python implementation of IDL method.
        double acsExceptionMethodDouble() raises (ACSErr::ACSException);
        '''
        self.acsExceptionMethodVoid()
        return 3.14
#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Creating an object"
    g = LampCallback()
    print "Done..."










