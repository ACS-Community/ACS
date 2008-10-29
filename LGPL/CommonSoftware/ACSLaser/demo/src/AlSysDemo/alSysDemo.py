#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2008 
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
# "@(#) $Id: alSysDemo.py,v 1.1 2008/10/29 17:46:17 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-10-29  created
#

#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------
import alarmsystemdemo__POA
import alarmsystemdemo
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Servants.ACSComponent       import ACSComponent
import Acsalarmpy
import Acsalarmpy.FaultState as FaultState
import Acsalarmpy.Timestamp as Timestamp
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class alSysDemo(alarmsystemdemo__POA.Mount,  #CORBA stubs for IDL interface
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
    def _sendAlarm(self,family,member,code,active):
        
        Acsalarmpy.AlarmSystemInterfaceFactory.init()
    
        alarmSource = Acsalarmpy.AlarmSystemInterfaceFactory.createSource("ALARM_SYSTEM_SOURCES")

        # Create a test fault
        fltstate = Acsalarmpy.AlarmSystemInterfaceFactory.createFaultState(family,member, code)
        if active:
            fltstate.descriptor = FaultState.ACTIVE_STRING
        else:
            fltstate.descriptor = FaultState.TERMINATE_STRING
        fltstate.userTimestamp = Timestamp.Timestamp()
        fltstate.userProperties[FaultState.ASI_PREFIX_PROPERTY_STRING] = "prefix"
        fltstate.userProperties[FaultState.ASI_SUFFIX_PROPERTY_STRING] = "suffix"
        fltstate.userProperties["TEST_PROPERTY"] = "TEST_VALUE"

        # The heart of the test
        alarmSource.push(fltstate)

        Acsalarmpy.AlarmSystemInterfaceFactory.done()
    #------------------------------------------------------------------------------
    #--Implementation of IDL methods-----------------------------------------------
    #------------------------------------------------------------------------------
    def faultMount(self):
        '''
        Python implementation of IDL method.
        string sayHello();
        '''
        self._sendAlarm("Mount","ALARM_SOURCE_MOUNTPY", 1, True)

    #------------------------------------------------------------------------------
    def terminate_faultMount(self):
        '''
        Python implementation of IDL method.
        string sayHelloWithParameters(in string inString, inout double inoutDouble, out long outInt);
        '''
        self._sendAlarm("Mount","ALARM_SOURCE_MOUNTPY", 1, False)

#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Creating an object"
    g = alSysDemo()
    print "Done..."


#
# ___oOo___
