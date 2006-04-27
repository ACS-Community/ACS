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
# "@(#) $Id: ErrorComponent.py,v 1.1 2006/04/27 16:00:24 dfugate Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# dfugate  2005-11-23  created
#

#----------------------------------------------------------------------------
'''
This module implements the IDL:alma/acsexmplErrorComponent/ErrorComponent:1.0
component specification.
'''
#--CORBA STUBS-----------------------------------------------------------------
import acsexmplErrorComponent__POA
import ACSErrTypeCommonImpl
import ACSErrTypeOKImpl
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.ACSComponent       import ACSComponent
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle

from Acspy.Common.Log import acsPrintExcDebug
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
        Just call superclass constructors here.
        '''
        ACSComponent.__init__(self)
        ContainerServices.__init__(self)
        self.logger = self.getLogger()
        
    #------------------------------------------------------------------------------
    def displayMessage(self):
        '''
        void displayMessage();
        '''
        print "Hello World"

    #------------------------------------------------------------------------------
    def badMethod(self, depth):
        '''
        void badMethod(in short depth) raises (ACSErrTypeCommon::GenericErrorEx);
        '''
        self.logger.logTrace("ErrorComponent.badMethod")

        try:
            # We decrement the depth, because we are going to add one
            # error here in any case.
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

        #We should get here only if a depth<=1 was requested.
        ex = ACSErrTypeCommonImpl.GenericErrorExImpl()
        ex.addData("ErrorDesc", "An error trace with depth lower or equal to 1 was requested.")
        raise ex
    #------------------------------------------------------------------------------
    def returnCompletion(self, depth):
        '''
        ACSErr::Completion returnCompletion(in short depth);
        '''
        if depth <= 0:
            er = ACSErrTypeOKImpl.ACSErrOKCompletionImpl()

        else:
            
            try:
                self.__errorTrace(depth)

            except ACSErrTypeCommonImpl.GenericErrorExImpl, ex:
                er = ACSErrTypeCommonImpl.GenericErrorCompletionImpl(exception=ex)
                er.addData("ErrorDesc", "Generated multi level exception")

            except:
                acsPrintExcDebug()
                er = ACSErrTypeCommonImpl.GenericErrorCompletionImpl()
                er.addData("ErrorDesc", "Got unexpected exception")

        return er
    
    #------------------------------------------------------------------------------
    def __errorTrace(self, depth):
        '''
        This method simply throw an exception containing
        an error trace with the requested depth, if > 0
        Otherwise just returns.
        
        Notice that this method throws a LOCAL exception xxxExImpl 
        and not a remote exception xxx
        
        Parameters: depth is the depth of the error trace
        '''
        self.logger.logTrace("ErrorComponent.errorTrace")

        # If depth is 1, we are at the bottom and 
        # we just have to throw an exception.
        # Going up the recursive chain this will be
        # atteched to all other exceptions
        if depth == 1:
            ex = ACSErrTypeCommonImpl.GenericErrorExImpl()
            ex.addData("ErrorDesc", "Bottom of error trace")
            raise ex

        # If depth > 1, make a recursive call.
        # We will have to get back an exception with a trace with
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
        # I.e. if there is not exception to throw.
        return
