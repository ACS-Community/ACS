# @(#) $Id: Err.py,v 1.12 2006/04/12 23:22:31 dfugate Exp $
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

__revision__ = "$Id: Err.py,v 1.12 2006/04/12 23:22:31 dfugate Exp $"

#------------------------------------------------------------------------------
import ACSErr
import acstime

from Acspy.Common.Log        import getLogger
from Acspy.Common.TimeHelper import getTimeStamp, TimeUtil

from os        import linesep, getpid
from threading import currentThread
from inspect   import stack
from time      import gmtime, asctime
from socket    import gethostname
from traceback import print_exc
#------------------------------------------------------------------------------
class ACSError:
    '''
    ACSError is nearly identical to the C++ ACSError class. Developers who want to
    know what methods are available for the various ACS Error System generated
    exception classes should pay close attention to this class.

    IMPORTANT: please take a look at the documentation for addErrorTraceHelperMethods
    as well. This function attaches additional methods to instances of this class.
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
            self.errorTrace = createErrorTrace(error_type,
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

        #attach some error trace helper methods
        addErrorTraceHelperMethods(self.errorTrace, obj_for_methods=self)
    #--------------------------------------------------------------------------
    def getErrorTrace (self):
        '''
        Gets reference to errortrace structure.

        Parameters: None

        Returns: the errortrace

        Raises: Nothing
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

        #reregister member methods with the new error trace
        addErrorTraceHelperMethods(self.errorTrace, obj_for_methods=self)

#--------------------------------------------------------------------------
def addErrorTraceHelperMethods(et_obj, obj_for_methods=None):
    '''
    Utility function designed to add some useful helper method to pure-CORBA
    ACSErr.ErrorTrace object instances. This means that if you invoke some
    CORBA method which returns an errortrace, you can then run this function
    on it which will dynamically attach the following methods:
    - getNext()
    - log()
    - errorTraceToString(error_trace)
    - printET(error_trace)
    - Print()
    - isOK()
    - addData(name, value)
    - getDescription()
    - getFileName()
    - getLineNumber()
    - getRoutine()
    - getHostName()
    - getProcess()
    - getThread()
    - getTimeStamp()
    - getErrorCode()
    - getErrorType()
    - getSeverity()
    - setTimeStamp(time)
    - setFileName(file_name)
    - setLineNumber(line_number)
    - setError(error_code=None, error_type=None)
    - setSeverity(severity)

    Params:
    et_obj - an error trace object
    obj_for_methods - an optional parameter where one can attach the methods
    to arbitrary objects instead of the error trace itself

    Notes:
    - no check is performed to ensure et_obj is actually derived from
    ACSErr.ErrorTrace.

    Returns: Nothing. After invoking this method, the et_obj parameter
    has the methods mentioned above attached to it.

    Raises: Nothing.
    '''
    
    #determine what object will have the methods attached to it
    if obj_for_methods==None:
        obj_for_methods=et_obj
        
    #------------------------------------
    def _getNext():
        '''
        Moves to the next errortrace element and returns a reference to it.

        Parameters: None

        Returns: the next errortrace element or None if it does not exist

        Raises: Nothing
        '''
        if len(et_obj.previousError) != 0:
            return et_obj.previousError[0]
        else:
            return None

    obj_for_methods.__dict__['getNext'] = _getNext
    #--------------------------------------------------------------------------
    def _log():
        '''
        Logs errortrace information into the ACS logging system.

        Parameters: None

        Returns: Nothing

        Raises: Nothing
        '''
        getLogger("Acspy.Common.Err.ErrorTraceHelper").logErrorTrace(et_obj)

    obj_for_methods.__dict__['log'] = _log

    #--------------------------------------------------------------------------
    def _errorTraceToString(error_trace, ws):
        '''
        Converts an error trace to a human-readable string.
        
        Parameters: error_trace is an errortrace
        ws is whitespace

        Returns: Nothing

        Raises: Nothing
        '''
        #figure out a nice format for time first
        epoch = acstime.Duration(error_trace.timeStamp)  #convert to an ACS epoch
        timehelper = TimeUtil()
        epoch = timehelper.epoch2py(epoch)  #convert to Python time
        epoch = gmtime(epoch)  #convert to gm time
        epoch = asctime(epoch)  #convert to nice string format
        
        nice_space = "            "
        for i in range(0, len(ws)/4):
            nice_space = nice_space + "    "

        message = "ErrorTrace ("
        message = message + "TimeStamp=" + epoch + "," + linesep
        message = message + nice_space    + "File="      + str(error_trace.file)      + "," + linesep
        message = message + nice_space    + "Line="      + str(error_trace.lineNum)   + "," + linesep
        message = message + nice_space    + "Routine="   + str(error_trace.routine)   + "," + linesep
        message = message + nice_space    + "Host="      + str(error_trace.host)      + "," + linesep
        message = message + nice_space    + "Process="   + str(error_trace.process)   + "," + linesep
        message = message + nice_space    + "Thread="    + str(error_trace.thread)    + "," + linesep
        message = message + nice_space    + "Type="      + str(error_trace.errorType) + "," + linesep
        message = message + nice_space    + "Code="      + str(error_trace.errorCode) + "," + linesep
        message = message + nice_space    + "ShortDescrip="      + str(error_trace.shortDescription) + "," + linesep
        message = message + nice_space    + "Data: "
        for i in error_trace.data:
            message = message + "Name=" + str(i.name) + ", Value=" + str(i.value) + "; "
        message = message + ")" + linesep

        return message

    obj_for_methods.__dict__['errorTraceToString'] = _errorTraceToString
    
    #--------------------------------------------------------------------------
    def _printET(error_trace, ws):
        '''
        Prints one error trace to standard out.
        
        Parameters: et is an errortrace
        ws is whitespace
        
        Returns: Nothing

        Raises: Nothing
        '''
        print ws + obj_for_methods.errorTraceToString(error_trace, ws)

    obj_for_methods.__dict__['printET'] = _printET
    #--------------------------------------------------------------------------
    def _Print():
        '''
        Prints errortrace information to standard out.
        
        Parameters: None

        Returns: Nothing

        Raises: Nothing
        '''
        joe = et_obj
        
        ws = ""
        
        while len(joe.previousError) != 0:
            obj_for_methods.printET(joe, ws)
            joe = joe.previousError[0]
            ws = ws + "    "
        obj_for_methods.printET(joe, ws)

    obj_for_methods.__dict__['Print'] = _Print
    #--------------------------------------------------------------------------
    def _isOK():
        '''
        Returns 1 if errortrace does not represent error otherwise 0.

        Parameters: None

        Returns: 0 or 1

        Raises: Nothing
        '''
        if et_obj.errorCode == 0 and et_obj.errorType == 0:
            return 1
        return 0

    obj_for_methods.__dict__['isOK'] = _isOK
    #--------------------------------------------------------------------------
    def _addData(name, value):
        '''
        Adds data to the current error
        
        Parameters: name and value will both be converted to strings.
        
        Returns: Nothing
        
        Raises: Nothing    
        '''
        et_obj.data.append(ACSErr.NameValue(str(name), str(value)))

    obj_for_methods.__dict__['addData'] = _addData
    #--------------------------------------------------------------------------
    def _getDescription():
        '''
        Returns copy of description of current error. 
        
        Parameters: None
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        return et_obj.shortDescription
    
    obj_for_methods.__dict__['getDescription'] = _getDescription
    #--------------------------------------------------------------------------
    def _getFileName():
        '''
        Returns file name information of the error
        
        Parameters: None
        
        Returns: filename
        
        Raises: Nothing
        '''
        return et_obj.file

    obj_for_methods.__dict__['getFileName'] = _getFileName
    #--------------------------------------------------------------------------
    def _getLineNumber():
        '''
        Returns line number information of the error
        
        Parameters: None
        
        Returns: line number
        
        Raises: Nothing
        '''
        return et_obj.lineNum

    obj_for_methods.__dict__['getLineNumber'] = _getLineNumber
    #--------------------------------------------------------------------------
    def _getRoutine():
        '''
        Returns routine information of the error

        Parameters: None
        
        Returns: routine name
        
        Raises: Nothing
        '''
        return et_obj.routine

    obj_for_methods.__dict__['getRoutine'] = _getRoutine
    #--------------------------------------------------------------------------
    def _getHostName():
        '''
        Returns host name information of the error
        
        Parameters: None
        
        Returns: hostname
        
        Raises: Nothing
        '''
        return et_obj.host

    obj_for_methods.__dict__['getHostName'] = _getHostName
    #--------------------------------------------------------------------------
    def _getProcess():
        '''
        Returns process information of the error. Its name or process ID.

        Parameters: None
        
        Returns: Process ID

        Raises: Nothing
        '''
        return et_obj.process

    obj_for_methods.__dict__['getProcess'] = _getProcess
    #--------------------------------------------------------------------------
    def _getThread():
        '''
        Returns thread information of the error. The name of thread or its ID.

        Parameters: None

        Returns: Thread ID

        Raises: Nothing
        '''
        return et_obj.thread

    obj_for_methods.__dict__['getThread'] = _getThread
    #--------------------------------------------------------------------------
    def _getTimeStamp():
        '''
        Returns time stamp of the error in 100th of nanoseconds.

        Parameters: None

        Returns: time stamp

        Raises: Nothing
        '''
        return et_obj.timeStamp

    obj_for_methods.__dict__['getTimeStamp'] = _getTimeStamp
    #--------------------------------------------------------------------------
    def _getErrorCode():
        '''
        Returns error code
        
        Parameters: None

        Returns: error code

        Raises: Nothing
        '''
        return et_obj.errorCode

    obj_for_methods.__dict__['getErrorCode'] = _getErrorCode
    #--------------------------------------------------------------------------
    def _getErrorType():
        '''
        Returns error type

        Parameters: None
        
        Returns: error type

        Raises: Nothing
        '''
        return et_obj.errorType

    obj_for_methods.__dict__['getErrorType'] = _getErrorType
    #--------------------------------------------------------------------------
    def _getSeverity():
        '''
        Returns error severity
        
        Parameters: None

        Returns: Severity

        Raises: Nothing
        '''
        return et_obj.severity

    obj_for_methods.__dict__['getSeverity'] = _getSeverity
    #--------------------------------------------------------------------------
    def _setTimeStamp(time):
        '''
        Sets time stamp of the error in 100th of nanoseconds.

        Parameters: the time stamp of the error
        
        Returns: Nothing

        Raises: Nothing
        '''
        et_obj.timeStamp = time

    obj_for_methods.__dict__['setTimeStamp'] = _setTimeStamp
    #--------------------------------------------------------------------------
    def _setFileName(file_name):
        '''
        Sets file name
        
        Parameters: name of the file
        
        Returns: Nothing

        Raises: Nothing
        '''
        et_obj.file = str(file_name)

    obj_for_methods.__dict__['setFileName'] = _setFileName
    #--------------------------------------------------------------------------
    def _setLineNumber(line_number):
        '''
        Sets line number
        
        Parameters: the line number
        
        Returns: Nothing

        Raises: Nothing
        '''
        et_obj.lineNum = long(line_number)

    obj_for_methods.__dict__['setLineNumber'] = _setLineNumber
    #--------------------------------------------------------------------------
    def _setError(error_code=None, error_type=None):
        '''
        Sets the error type/code
        
        Parameters: self-explanatory
        
        Returns: Nothing

        Raises: Nothing
        '''
        if error_code != None and error_type != None:
            et_obj.errorCode = long(error_code)
            et_obj.errorType = long(error_type)
        else:
            getlogger("Acspy.Common.Err.ErrorTraceHelper").logAlert('Bad parameters')

    obj_for_methods.__dict__['setError'] = _setError
    #--------------------------------------------------------------------------
    def _setSeverity(severity):
        '''
        Sets error severity
        
        Parameters: severity of the error
        
        Returns: Nothing

        Raises: Nothing
        '''
        et_obj.severity = severity

    obj_for_methods.__dict__['setSeverity'] = _setSeverity

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

#--------------------------------------------------------------------------
def createErrorTrace(error_type,
                     error_code,
                     exception = None,
                     description = "None", 
                     nvSeq = None,
                     level = 3):
    '''
    Utility method used to create a new error trace.
    
    Parameters:
    - error_type is the error type (a long)
    - error_code is the error code (a long)
    - exception is a previous exception from the ACS Error System
    - description is a stringified description of the errror
    - nvSeq is a name-value sequence describing the error condition. Each value
    should be of the type ACSErr.NameValue
    - create with a value of 1 implies error information will be added to the stack
    - offset from stack()
    
    Returns: A newly created error trace
        
    Raises: ???
    '''        
    call_frame = stack()[level]
    
    if nvSeq == None:
        nvSeq = []
    
    #Get the file name
    filename = str(call_frame[1])
    
    #Get the line number
    line = str(call_frame[2])
    
    #Get the routine name
    routine = str(call_frame[3])
    
    #Get the hostname
    host = gethostname()
    
    #Get the process ID
    process = str(getpid())
    
    #Try to get the thread ID
    if currentThread() != None:
        thread = str(currentThread().getName())
    else:
        thread = "Unavailable"
            
    #Get the ACS time
    time = getTimeStamp().value
            
    #Set the severity
    severity = ACSErr.Error
            
    try:
        #If the previous exception is an ACS Error System Exception
        if isinstance(exception.errorTrace, ACSErr.ErrorTrace):
            #We can use an error stack...
            errortrace = [ exception.errorTrace ]
    except Exception, e:
        errortrace = [] 
                
    #Create a temporary error trace which MIGHT be used...
    errortrace = ACSErr.ErrorTrace(str(filename),   #string file;
                                   int(line),        #long lineNum;
                                   str(routine),     #string routine;
                                   str(host),        #string host;
                                   str(process),     #string process;
                                   str(thread),      #string thread;
                                   long(time),       #unsigned long long timeStamp;
                                   long(error_type), #ACSErr::ACSErrType errorType;
                                   long(error_code), #ACSErr::ErrorCode errorCode;
                                   severity,         #ACSErr::Severity severity;
                                   description,      #string shortDescription;
                                   nvSeq,            #NameValueSeq data;
                                   errortrace)       #sequence<ErrorTrace, 1> previousError;
    
    return errortrace
#---------------------------------------------------
