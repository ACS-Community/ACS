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
#
# @(#) $Id$

'''
This example shows a client that:
    logs into manager via PySimpleClient
    activates the ErrorComponent component specified from the command-line
    calls the methods in this component to show examples of error handling
    releases the component
    logs out of manager

Error handling examples are encapsulated in the ClientErrorComponent class.
Each method in the class shows an example.

What can I gain from this example?
    SimpleClient usage.
    Dealing with errors accessing (remote) components.
'''

from sys import argv
import CORBA
from Acspy.Clients.SimpleClient import PySimpleClient
from Acspy.Common.TimeHelper    import getTimeStamp
from Acspy.Common.EpochHelper   import EpochHelper
from Acspy.Common.Err           import pyExceptionToCORBA
from Acspy.Common.Err           import addComplHelperMethods
from Acspy.Common.Log           import acsPrintExcDebug

import acsexmplErrorComponent
import ACSErrTypeCommonImpl
import ACSErrTypeCommon
import ACSErrTypeOKImpl
from Acspy.Common.Err import ACSError
import acstime

#-----------------------------------------------------------------------------
class ClientErrorComponent:
    '''
    This class demonstrates error handling when accessing a (remote)
    component.
    
    When the class is instantiated, it gets a reference to the 
    ErrorComponent counterpart throw the manager services provided
    by the SimpleClient.
    When the destructor is called, the reference to the component is released.
    This strategy ensures a clean handling of references to Components.
    
    Each of the public methods of the class demonstrates some error handling pattern.
    Each method is self-contained and it should never throw exceptions in itself,
    but for ACSErrTypeCommon::CouldntAccessComponentEx, to be used when the
    reference to the component is not properly initialised.
    Other errors are always completely handled internally.
    '''
    def __init__(self, client, error_comp):
        '''
        Constructor
        
        Parameters: client is a simple client reference
        errorComponent is the name of the ErrorComponent
        '''
        self.client = client
        self.error_comp = error_comp
        self.foo = None
        self.logger = self.client.getLogger()
        self.eh = EpochHelper()
    
        self.logger.logTrace("ClientErrorComponent")
        
        self.foo = self.client.getComponent(self.error_comp)
        
        if self.foo == None:
            raise ACSErrTypeCommonImpl.CouldntAccessComponentExImpl()
        
    def __del__(self):
        '''
        Destructor. Releases ErrorComponent
        '''
        self.logger.logTrace("ClientErrorComponent")
        self.client.releaseComponent(self.error_comp)
        
    def TestOk(self):
        '''
        Here everything should go fine.
        
        Parameters: None
        
        Returns: None
        
        Raises: ACSErrTypeCommonImpl.CouldntAccessComponentExImpl
        '''
        self.logger.logTrace("ClientErrorComponent")
        
        if self.foo == None:
            raise ACSErrTypeCommonImpl.CouldntAccessComponentExImpl()
        
        try:
            self.foo.displayMessage()
        
        except CORBA.SystemException, ex:
            # Map......
            acsPrintExcDebug()
            displayMessageEx = ACSErrTypeCommonImpl.GenericErrorExImpl()
            displayMessageEx.setErrorDesc("badMethod has thrown a CORBA exception")
            displayMessageEx.log()
            
        except:
            acsPrintExcDebug()
            displayMessageEx = ACSErrTypeCommonImpl.GenericErrorExImpl()
            displayMessageEx.setErrorDesc("badMethod has thrown an UNEXPECTED exception")
            displayMessageEx.log()
    
    def TestReceiveRemoteException(self):
        '''
        Example 1: Calls a method that throws an exception
                   with a stack trace:
            Catches the exception, 
            Adds context information
            Sends it to the logging system
        
        Parameters: None
        
        Returns: Nothing
        
        Raises: ACSErrTypeCommonImpl.CouldntAccessComponentExImpl
        '''
        self.logger.logTrace("ClientErrorComponent")

        if self.foo == None:
            raise ACSErrTypeCommonImpl.CouldntAccessComponentExImpl()
        
        self.logger.logInfo("Example 1: Calls a method that throws an exception.")
        
        try:
            self.foo.badMethod(3)
            
        except ACSErrTypeCommon.GenericErrorEx, ex:
            badMethodEx = ACSErrTypeCommonImpl.GenericErrorExImpl(exception=ex)
            badMethodEx.setErrorDesc("badMethod has thrown the expected exception")
            badMethodEx.log()
            
            self.eh.value(badMethodEx.getTimeStamp())
            tString = self.eh.toString(acstime.TSArray, "", 0, 0)
            self.logger.logDebug("Time of the exception:" + tString)
            
        except CORBA.SystemException, ex:
            # Map......
            acsPrintExcDebug()
            badMethodEx = ACSErrTypeCommonImpl.GenericErrorExImpl()
            badMethodEx.setErrorDesc("badMethod has thrown a CORBA exception")
            badMethodEx.log()

            self.eh.value(badMethodEx.getTimeStamp())
            tString = self.eh.toString(acstime.TSArray, "", 0, 0)
            self.logger.logDebug("Time of the CORBA exception:" + tString)
            
        except:
            acsPrintExcDebug()
            badMethodEx = ACSErrTypeCommonImpl.GenericErrorExImpl()
            badMethodEx.setErrorDesc("badMethod has thrown an UNEXPECTED exception")
            badMethodEx.log()

            self.eh.value(badMethodEx.getTimeStamp())
            tString = self.eh.toString(acstime.TSArray, "", 0, 0)
            self.logger.logDebug("Time of the unexpected exception:" + tString)
    
    def TestReceiveRemoteCompletion(self):
        '''
        Example 2: Calls a method that returns a completion
                   If the completion contains an error, then:
        
            Catches the exception, 
            prints it locally 
            sends it to the logging system
        
        Parameters: None
        
        Returns: Nothing
                 
        Raises: ACSErrTypeCommonImpl.CouldntAccessComponentExImpl
        '''
        self.logger.logTrace("ClientErrorComponent")
        
        if self.foo == None:
            raise ACSErrTypeCommonImpl.CouldntAccessComponentExImpl()
    
        try:
            # OK Completion
            self.logger.logInfo("Example 2a: Calls a method that returns an OK completion.")
            comp = self.foo.completionFromException(0)

            addComplHelperMethods(comp)

	    if comp.isErrorFree() == 1:
		 self.logger.logInfo("Completion Ok, without error trace")
	    else:
		 self.logger.logInfo("Completion with error trace (UNEXPECTED)")
		 comp.log()

	    # ERROR completion with an error trace inside.
            self.logger.logInfo("Example 2b: Calls a method that returns an Error completion.")
            
	    comp2 = self.foo.completionFromException(3)
            addComplHelperMethods(comp2)
            if comp2.isErrorFree() == 1:
		 self.logger.logInfo("Completion Ok, without error trace (UNEXPECTED)")
	    else:
		 self.logger.logInfo("Completion with error trace")
		 comp2.log()
        
        except CORBA.SystemException, ex:
        
            # Map......
            acsPrintExcDebug()
            displayMessageEx = ACSErrTypeCommonImpl.GenericErrorExImpl()
            displayMessageEx.setErrorDesc("completionFromException has thrown an UNEXPECTED CORBA exception")
            displayMessageEx.log()
	except:
            acsPrintExcDebug()
            badMethodEx = ACSErrTypeCommonImpl.GenericErrorExImpl()
            badMethodEx.setErrorDesc("completionFromException has thrown an UNEXPECTED exception")
            badMethodEx.log()

#-----------------------------------------------------------------------------  
if __name__=="__main__":
    
    client = PySimpleClient()

    print("starting acspyexmplClientErrorComponent")
    # Here we instantiate the object used to show examples of error handling.
    # Each method call demonstrate one aspect of error hanlding.
    # See the class documentation for details.
    try:
	print("Creating ClientErrorComponent");
        clientErrorComponent = ClientErrorComponent(client, argv[1])
        
        #Call the displayMessage() method existing in the interface for ErrorComponent
        print("Calling TestOk()");
	clientErrorComponent.TestOk()
	print("Calling TestReceiveRemoteException()");
        clientErrorComponent.TestReceiveRemoteException()
        clientErrorComponent.TestReceiveRemoteCompletion()
    
    except ACSError, ex:
        # We should never get here, because the methods in the example
        # should be all self contained and none of them should throw
        # any exception.
        badMethodEx = ACSErrTypeCommonImpl.GenericErrorExImpl(exception = ex)
        badMethodEx.setErrorDesc("Examples of error handling have thrown an ACS exception")
        badMethodEx.log()
    
    except Exception, ex:
        # We should never get here, because the methods in the example
        #  should be all self contained and none of them should throw
        # any exception.
        badMethodEx = pyExceptionToCORBA(ex)
        badMethodEx.setData("Reason", "Examples of error handling have thrown an UNEXPECTED exception")
        badMethodEx.log()
        
    else:
        del clientErrorComponent
    
    client.disconnect()
