# @(#) $Id: Err.py,v 1.13 2006/04/17 17:36:22 dfugate Exp $
#
#    ALMA - Atacama Large Millimiter Array
#    (c) Associated Universities, Inc. Washington DC, USA,  2001
#    (c) European Southern Observatory, 2002
#    Copyright by ESO (in the framework of the ALMA collaboration)
#    and Cosylab 2002, All rights reserved
#
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

'''
Python helper class(es) for the ACS Error System.

TODO:
- nada
'''

__revision__ = "$Id: Err.py,v 1.13 2006/04/17 17:36:22 dfugate Exp $"

#------------------------------------------------------------------------------
import ACSErr

from Acspy.Common.Log        import getLogger
from Acspy.Common.TimeHelper import TimeUtil
from Acspy.Common.ErrorTrace import ErrorTraceHelper, ErrorTrace

from traceback import print_exc
#------------------------------------------------------------------------------
class ACSError(ErrorTraceHelper):
    '''
    ACSError is nearly identical to the C++ ACSError class. Developers who want to
    know what methods are available for the various ACS Error System generated
    exception classes should pay close attention to this class.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self,
                  error_type,
                  error_code,
                  exception = None,
                  description = "None", 
                  nvSeq = None,
                  create = 1): #create means we create a new error.
        '''
        The constructor basically allows the developer to specify any number of
        parameters for an error trace, but requires none.  Python is flexible enough
        to let us do this.
        
        Parameters:
        - error_type is the error type (a long)
        - error_code is the error code (a long)
        - exception is a previous exception from the ACS Error System
        - description is a stringified description of the errror
        - nvSeq is a name-value sequence describing the error condition. Each value
        should be of the type ACSErr.NameValue
        - create with a value of 1 implies error information will be added to the stack
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        if nvSeq==None:
            nvSeq = []
        
        # Get a Logger instance
        self.logger = getLogger("Acspy.Common.Err.ACSError")
        self.timehelper = TimeUtil()
        self.logger.logTrace('Creating a new ACSError')
        
        #If create has not been changed by the developer we create a new error trace
        #appending the old one if it exists
        if create == 1:
            self.errorTrace = ErrorTrace(error_type,
                                         error_code,
                                         exception,
                                         description, 
                                         nvSeq,
                                         3)
            
        #Someone has caught a CORBA exception and is trying to convert it into
        #this helper class.
        elif exception != None:
            try:
                #If the previous exception is an ACS Error System Exception
                if isinstance(exception.errorTrace, ACSErr.ErrorTrace):
                    #We can use an error stack...
                    self.errorTrace = exception.errorTrace
            except Exception, e:
                self.logger.logWarning('Wrong usage of the ACS Error System Python Helper Class')
                print_exc()
                raise Exception("Wrong usage of the ACS Error System Python Helper Class")
    #--------------------------------------------------------------------------
    def getErrorTrace(self):
        '''
        Overridden
        '''
        return self.errorTrace
    #--------------------------------------------------------------------------
    def setErrorTrace(self, et_obj):
        '''
        Sets the member error trace

        Parameters: et_obj is an ACSErr.ErrorTrace instances

        Returns: Nothing

        Raises: Nothing
        '''
        #set the error trace
        self.errorTrace = et_obj
#--------------------------------------------------------------------------
def addComplHelperMethods(compl_obj):
    '''
    Utility function designed to add some useful helper method to pure-CORBA
    ACSErr.Completion object instances. This means that if you invoke some
    CORBA method which returns a completion (e.g., the get_sync method of BACI
    properties), you can then run this function on it which will dynamically
    attach the following methods:
    - getTimeStamp() - returns the ACS::Time when the completion was created
    - getType() - returns the completions error type
    - getCode() - returns the completions error code
    - isErrorFree() - returns true if the completion does not contain an error trace
    - log() - logs the completions error trace
    - addData(name, value) - adds arbitrary data to the error trace. Fails if
    the error trace does not exist

    Notes:
    - no check is performed to ensure compl_obj is actually derived from
    ACSErr.Completion.

    Returns: Nothing. After invoking this method, the compl_obj parameter
    has the methods mentioned above attached to it.

    Raises: Nothing.
    '''
    #------------------------------------
    def _getTimeStamp():
        '''
        Returns the completions timestamp in ACS::Time format
        '''
        return compl_obj.timeStamp
    
    compl_obj.__dict__['getTimeStamp'] = _getTimeStamp
    #------------------------------------
    def _getType():
        '''
        Returns the completions error type
        '''
        return compl_obj.type
    
    compl_obj.__dict__['getType'] = _getType
    #------------------------------------
    def _getCode():
        '''
        Returns the completions error code
        '''
        return compl_obj.code
    
    compl_obj.__dict__['getCode'] = _getCode

    #------------------------------------
    def _isErrorFree():
        '''
        Returns true if the completion does not container an error trace
        '''
        if len(compl_obj.previousError)==0:
            return 1
        else:
            return 0
        
    compl_obj.__dict__['isErrorFree'] = _isErrorFree
    #------------------------------------
    def _log():
        '''
        Logs the Completion.
        '''
        getLogger("Acspy.Common.Err.CompletionHelper").logErrorTrace(compl_obj.previousError[0])

    compl_obj.__dict__['log'] = _log
    #------------------------------------
    def _addData(name, value):
        '''
        Adds data to the completions error trace. Will fail if the completion
        is error free.
        '''
        compl_obj.previousError[0].data.append(ACSErr.NameValue(str(name), str(value)))

    compl_obj.__dict__['addData'] = _addData
    
    
