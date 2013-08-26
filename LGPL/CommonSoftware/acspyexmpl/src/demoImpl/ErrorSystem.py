# @(#) $Id: ErrorSystem.py,v 1.2 2005/02/08 01:41:31 dfugate Exp $
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
# "@(#) $Id: ErrorSystem.py,v 1.2 2005/02/08 01:41:31 dfugate Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/08/20  Created.
#------------------------------------------------------------------------------
'''
DESCRIPTION
ErrorSystem is a Python component implementation for the IDL
interface <a href="../../idl/html/interfacedemo_1_1ErrorSystem.html">ErrorSystem</a>.
This example shows how to use the Python ACS Error System

WHAT CAN I GAIN FROM THIS EXAMPLE? 
- an example derived from the <a href="../../idl/html/interfaceACS_1_1ACSComponent.html">ACS::ACSComponent</a> IDL interface.
- using the ACS Error System.

LINKS
- <a href="classHelloDemo_1_1ErrorSystem.html">ErrorSystem Class Reference</a>
- <a href="../../idl/html/interfacedemo_1_1ErrorSystem.html">ErrorSystem IDL Documentation</a>
'''
#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------
import demo__POA
import demo
import ErrorSystemExampleImpl
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Servants.ACSComponent       import ACSComponent
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class ErrorSystem(demo__POA.ErrorSystem,  #CORBA stubs for IDL interface
                  ACSComponent,  #Base IDL interface
                  ContainerServices,  #Developer niceties
                  ComponentLifecycle):  #HLA stuff
    '''
    Implementation of a component relying heavily on the ACS Error System.
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
    
    #------------------------------------------------------------------------------
    #--Implementation of IDL methods-----------------------------------------------
    #------------------------------------------------------------------------------
    def tryToScheduleSomething(self):
        '''
        Python implementation of IDL method.
        void tryToScheduleSomething() 
        raises (ErrorSystemExample::NothingCanBeScheduledErrorEx);
        '''
        raise ErrorSystemExampleImpl.NothingCanBeScheduledErrorExImpl()
    #------------------------------------------------------------------------------
    def tryToProcessSomething(self):
        '''
        Python implementation of IDL method.
        void tryToProcessSomething() 
        raises (ErrorSystemExample::PipelineProcessingRequestErrorEx);
        '''
        try:
            self.tryToScheduleSomething()
        except Exception, e:
            raise ErrorSystemExampleImpl.PipelineProcessingRequestErrorExImpl(exception=e)
    #------------------------------------------------------------------------------
    def usingWrapperClasses1(self):
        '''
        Python implementation of IDL method.
        void usingWrapperClasses1()
        raises (ErrorSystemExample::NothingCanBeScheduledErrorEx);
        '''
        try:
            raise ErrorSystemExampleImpl.NothingCanBeScheduledErrorExImpl()
        except Exception, e:
            raise ErrorSystemExampleImpl.NothingCanBeScheduledErrorExImpl(exception=e, create=0)
    #------------------------------------------------------------------------------
    def usingWrapperClasses2(self):
        '''
        Python implementation of IDL method.
        void usingWrapperClasses2();
        '''
        try:
            raise ErrorSystemExampleImpl.NothingCanBeScheduledErrorExImpl()
        except Exception, e:
            e.Print()
    #------------------------------------------------------------------------------


#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Creating an object"
    g = ErrorSystem()
    print "Done..."










