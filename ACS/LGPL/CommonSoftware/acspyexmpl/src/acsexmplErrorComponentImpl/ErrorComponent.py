#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2005 
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
# "@(#) $Id: ErrorComponent.py,v 1.6 2007/03/27 07:08:28 nbarriga Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# dfugate  2005-11-23  created
#

#----------------------------------------------------------------------------
'''
This module implements the IDL:alma/acsexmplErrorComponent/ErrorComponent:1.0
component specification. It clearly and concisely demonstrates some of the ACS
Error System features available to Python developers.
'''
#--CORBA STUBS-----------------------------------------------------------------

#import the CORBA servant stubs for the ErrorComponent IDL interface
import acsexmplErrorComponent__POA

#import the ACS-generated implementations of IDL exceptions defined
#by ACSErrTypeCommon. The classes defined within this module add quite a bit
#of extra functionality to the pure IDL exceptions such as:
# - visual respresentations of ACSErr.ErrorTrace's common to all 
#   ACS-generated exceptions
# - logging of exceptions
# - exception member getter/setter methods
import ACSErrTypeCommonImpl

#import the ACS-generated implementations of IDL exceptions defined
#by ACSErrTypeOK.xml. See description above.
import ACSErrTypeOKImpl

#--ACS Imports-----------------------------------------------------------------

#standard imports needed to implement a component
from Acspy.Servants.ACSComponent       import ACSComponent
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle

#here we import a special method from the Log module. acsPrintExcDebug
#is a quite useful method which prints the same data as the native
#stack.print_exc function with one stipulation - the $ACS_LOG_STDOUT
#environment variable must be set at the DEBUG level or lower for
#this to occur.
from Acspy.Common.Log                  import acsPrintExcDebug

#--CORBA Imports-----------------------------------------------------------------
from CORBA import BAD_PARAM


#------------------------------------------------------------------------------
class ErrorComponent(acsexmplErrorComponent__POA.ErrorComponent,
                     ACSComponent,
                     ContainerServices,
                     ComponentLifecycle):
    '''
    Implementation of ErrorComponent IDL interface.
    '''
    #------------------------------------------------------------------------------
    def __init__(self):
        '''
        Just call superclass constructors here and get a member logger.
        '''
        ACSComponent.__init__(self)
        ContainerServices.__init__(self)
        self.logger = self.getLogger()
        
    #------------------------------------------------------------------------------
    def displayMessage(self):
        '''
        Implementation of IDL method:
            void displayMessage();
        
        For details on what this method does, please see the IDL Doxygen
        documentation.
        '''
        print "Hello World"

    #------------------------------------------------------------------------------
    def badMethod(self, depth):
        '''
        Implementation of IDL method:
            void badMethod(in short depth) raises (ACSErrTypeCommon::GenericErrorEx);
        
        For details on what this method does, please see the IDL Doxygen
        documentation.
        '''
        self.logger.logTrace("ErrorComponent.badMethod")
        if depth >= 1:
            try:
                # We decrement the depth, because we are going to add one
                # error here in any case.
                self.__errorTrace(depth - 1)

            except ACSErrTypeCommonImpl.GenericErrorExImpl, ex:
                ex2 = ACSErrTypeCommonImpl.GenericErrorExImpl(exception=ex)
                ex2.addData("ErrorDesc", "Generated multi level exception")
                raise ex2

            except:
                #never expect this code to be executed but just in case
                #there was some unknown problem generating the ACS-based
                #exception
                acsPrintExcDebug()
                ex2 = ACSErrTypeCommonImpl.GenericErrorExImpl()
                ex2.addData("ErrorDesc", "Got unexpected exception")
                raise ex2

            #We should get here only if a depth<=1 was requested.
            ex = ACSErrTypeCommonImpl.GenericErrorExImpl()
            ex.addData("ErrorDesc", "An error trace with depth lower or equal to 1 was requested.")
            raise ex
    #------------------------------------------------------------------------------
    def exceptionFromCompletion(self, depth):
        '''
        Implementation of IDL method:
            ACSErr::Completion exceptionFromCompletion(in short depth);

        For details on what this method does, please see the IDL Doxygen
        documentation.
        '''
        self.logger.logTrace("ErrorComponent.exceptionFromCompletion")
        comp=self.__returnCompletion(depth)
        #if the completion doesn't represent an error, then do nothing
        if comp.isOK() == 0:
            ex = ACSErrTypeCommonImpl.GenericErrorExImpl(exception=comp,create=0)
            ex.setErrorDesc("Exception generated by adding an error trace from a completion")
            raise ex
    #------------------------------------------------------------------------------
    def typeException(self, depth):
        '''
        Implementation of IDL method:
            ACSErr::Completion typeException(in short depth);

        For details on what this method does, please see the IDL Doxygen
        documentation.
        '''
        self.logger.logTrace("ErrorComponent.typeException")
        if depth == 1:
            ex = ACSErrTypeCommonImpl.GenericErrorExImpl()
            raise ex.getACSErrTypeCommonEx()
        try:
            self.__errorTrace(depth-1)
        except ACSErrTypeCommonImpl.GenericErrorExImpl, ex:
            ex2 = ACSErrTypeCommonImpl.GenericErrorExImpl(exception=ex)
            raise ex2.getACSErrTypeCommonEx()
    #------------------------------------------------------------------------------
    def completionFromCompletion(self, depth):
        '''
        Implementation of IDL method:
            ACSErr::Completion completionFromCompletion(in short depth);

        For details on what this method does, please see the IDL Doxygen
        documentation.
        '''
        self.logger.logTrace("ErrorComponent.completionFromCompletion")
        comp=self.__returnCompletion(depth)
        #if the completion doesn't represent an error just return it,
        #otherwise, create a new completion out of it
        if comp.isOK() == 0:
            comp =  ACSErrTypeCommonImpl.GenericErrorCompletionImpl(exception=comp,create=0)
            comp.setErrorDesc("Put an error trace in completionFromCompletion")
        return comp                              
    #------------------------------------------------------------------------------
    def completionFromException(self, depth):
        '''
        Implementation of IDL method:
            ACSErr::Completion completionFromException(in short depth);
        
        For details on what this method does, please see the IDL Doxygen
        documentation.
        '''
        #if the depth is <=0, a completion is returned signifying
        #there was no error
        self.logger.logTrace("ErrorComponent.completionFromException");
	if depth <= 0:
            er = ACSErrTypeOKImpl.ACSErrOKCompletionImpl()

        #otherwise we must generate an exception of the requested
        #depth and then convert that into an error completion
        else:
            
            try:
                self.__errorTrace(depth)

            except ACSErrTypeCommonImpl.GenericErrorExImpl, ex:
                er = ACSErrTypeCommonImpl.GenericErrorCompletionImpl(exception=ex, create=0)
                er.setErrorDesc("Generated multi level exception")

            except:
                acsPrintExcDebug()
                er = ACSErrTypeCommonImpl.GenericErrorCompletionImpl()
                er.setErrorDesc("Got unexpected exception")

        return er
    
    #------------------------------------------------------------------------------
    def corbaSystemException(self):
        '''
        Implementation of IDL method:
            void corbaSystemException()
        
        For details on what this method does, please see the IDL Doxygen
        documentation.
        '''
        self.logger.logTrace("ErrorComponent.corbaSystemException")

        #We instantiate and throw a standard CORBA System Exception
        raise BAD_PARAM()
    #------------------------------------------------------------------------------
    def completionOnStack(self, depth):
        '''
        Implementation of IDL method:
            ACSErr::Completion completionOnStack(in short depth)

        For details on what this method does, please see the IDL Doxygen
        documentation.
        '''
        #This is just a Stub. No need for it in python
        return self.completionFromException(depth);
    #------------------------------------------------------------------------------
    def outCompletion(self):
        '''
        Implementation of IDL method:
            void outCompletion(out ACSErr::Completion comp);

        For details on what this method does, please see the IDL Doxygen
        documentation.
        '''
        return ACSErrTypeOKImpl.ACSErrOKCompletionImpl() 
    #------------------------------------------------------------------------------
    def __errorTrace(self, depth):
        '''
        This method simply throws an exception containing
        an error trace with the requested depth if > 0.
        Otherwise just returns.
        
        Note that this method throws a LOCAL exception xxxExImpl 
        and not a remote exception xxx.
        
        Parameters: depth is the depth of the error trace
        
        Returns: Nothing
        
        Raises: a ACSErrTypeCommonImpl.GenericErrorExImpl/
        ACSErrTypeCommon.GenericErrorEx exceptions of the requested
        depth.
        '''
        self.logger.logTrace("ErrorComponent.errorTrace")

        # If depth is 1, we are at the bottom and 
        # just have to throw an exception.
        # Going up the recursive chain this will be
        # attached to all other exceptions
        if depth == 1:
            ex = ACSErrTypeCommonImpl.GenericErrorExImpl()
            ex.addData("ErrorDesc", "Bottom of error trace")
            raise ex

        # If depth > 1, make a recursive call.
        # We will have to get back an exception with a trace containing
        # a depth shorter by 1 element.
        elif depth > 1:

            try:
                self.__errorTrace(depth - 1)

            except ACSErrTypeCommonImpl.GenericErrorExImpl, ex:
                ex2 = ACSErrTypeCommonImpl.GenericErrorExImpl(exception=ex)
                ex2.addData("ErrorDesc", "Generated multi level exception")
                raise ex2

            except:
                acsPrintExcDebug()
                ex2 = ACSErrTypeCommonImpl.GenericErrorExImpl()
                ex2.addData("ErrorDesc", "Got unexpected exception")
                raise ex2
            
        # We should get here only if depth <= 0,
        # I.e. if there is no exception to throw.
        return
    #------------------------------------------------------------------------------
    def __returnCompletion(self, depth):
        '''
        This method returns a GenericErrorCompletion with an ErrorTrace of depth=depth if depth >0,
        otherwise it returns a ACSErrOKCompletion with no error trace.

        Parameters: depth is the depth of the error trace

        Returns: ACSCompletion

        Raises: nothing
        '''
        self.logger.logTrace("ErrorComponent.returnCompletion")
        if depth <= 0:
            return ACSErrTypeOKImpl.ACSErrOKCompletionImpl()
        elif depth == 1:
            return ACSErrTypeCommonImpl.GenericErrorCompletionImpl()
        else:
            return ACSErrTypeCommonImpl.GenericErrorCompletionImpl(exception=self.__returnCompletion(depth-1))
