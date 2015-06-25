# @(#) $Id: ErrorTrace.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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
Python helper class for ACSErr.ErrorTrace
'''

__revision__ = "$Id: ErrorTrace.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#------------------------------------------------------------------------------
import ACSErr
import acstime
import ACSLog

from Acspy.Common.Log        import stdoutOk
from Acspy.Common.TimeHelper import getTimeStamp, TimeUtil

from os        import linesep, getpid
from threading import currentThread
from inspect   import stack
from time      import gmtime, asctime
from socket    import gethostname
import logging
#--------------------------------------------------------------------------
class ErrorTraceHelper(object):
    '''
    Helper class for objects containing an instance of an ACSErr.ErrorTrace
    To be useful, the abstract getErrorTrace method must be implemented or 
    one needs to provide an ACSErr.ErrorTrace to the constructor.
    '''
    def __init__(self, error_trace=None):
        '''
        Constructor
        
        Parameters: error_trace is an instance of ACSErr.ErrorTrace. While
        this parameter is optional, if it is not provided the developer
        must override the getErrorTrace method to return an 
        ACSErr.ErrorTrace
        '''
        self.error_trace = error_trace
        
    #------------------------------------
    def getErrorTrace(self):
        '''
        Pure virtual. Method should return an instance of ACSErr.ErrorTrace
        '''
        if self.error_trace == None:
            raise Exception("Needs to be overridden")
        else:
            return self.error_trace
    #------------------------------------
    def getNext(self):
        '''
        Moves to the next errortrace element and returns a reference to it.

        Parameters: None

        Returns: the next errortrace element or None if it does not exist

        Raises: Nothing
        '''
        if self.getErrorTrace() != None and len(self.getErrorTrace().previousError) != 0:
            return self.getErrorTrace().previousError[0]
        else:
            return None
        
    #--------------------------------------------------------------------------
    def log(self, logger, priority = ACSLog.ACS_LOG_ERROR):
        '''
        Logs errortrace information into the ACS logging system.

        Parameters:
        - priorty which will be used for logging the error

        Returns: Nothing

        Raises: Nothing
        '''
        #DWF- yes I know I should take the severity from the error trace,
        #but does this honestly make sense given the fact that each error trace
        #has it's own severity defined. this is a limitation of the system
        #and unless there are complaints, we'll just use logging.ERROR for
        #now
        if stdoutOk(logging.ERROR):
            self.Print()
            
	if self.getErrorTrace() != None:
		logger.logErrorTrace(self.getErrorTrace(), priority)

        
    #--------------------------------------------------------------------------
    def errorTraceToString(self, error_trace, ws):
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
        message = message + nice_space    + "Severity="      + str(error_trace.severity) + "," + linesep
        message = message + nice_space    + "Data: "
        for i in error_trace.data:
            message = message + "Name=" + str(i.name) + ", Value=" + str(i.value) + "; "
        message = message + ")" + linesep

        return message
    #--------------------------------------------------------------------------
    def printET(self, error_trace, ws):
        '''
        Prints one error trace to standard out.
        
        Parameters: et is an errortrace
        ws is whitespace
        
        Returns: Nothing

        Raises: Nothing
        '''
        print ws + self.errorTraceToString(error_trace, ws)
        
    #--------------------------------------------------------------------------
    def Print(self):
        '''
        Prints errortrace information to standard out.
        
        Parameters: None

        Returns: Nothing

        Raises: Nothing
        '''
        joe = self.getErrorTrace()
        if joe != None:
        	ws = ""
	        while len(joe.previousError) != 0:
           		self.printET(joe, ws)
            		joe = joe.previousError[0]
            		ws = ws + "    "
        	self.printET(joe, ws)
        
        return
    #--------------------------------------------------------------------------
    def isOK(self):
        '''
        Returns 1 if errortrace does not represent error otherwise 0.

        Parameters: None

        Returns: 0 or 1

        Raises: Nothing
        '''
        if self.getErrorTrace().errorCode == 0 and self.getErrorTrace().errorType == 0:
            return 1
        return 0
    #--------------------------------------------------------------------------
    def addData(self, name, value):
        '''
        Adds data to the current error
        
        Parameters: name and value will both be converted to strings.
        
        Returns: Nothing
        
        Raises: Nothing    
        '''
        self.getErrorTrace().data.append(ACSErr.NameValue(str(name), 
                                                          str(value)))
    #--------------------------------------------------------------------------
    def getData(self, name):
        '''
        Gets previously set data from the ErrorTrace.
        
        Parameters: name of the data as defined by the addData method
        
        Returns: a list of previously set data (stringified) with the matching
        name. This list can be empty if there are no matches
        
        Raises: Nothing
        '''
        ret_val = []

        #cycle through the list of of name value pairs...
        for name_value in self.getErrorTrace().data:
            #looking for matching names
            if name_value.name == name:
                ret_val.append(name_value.value)
            
        return ret_val
    #--------------------------------------------------------------------------
    def setData(self, name, value):
        '''
        Sets data within the ErrorTrace, adding it if it has not been
        previously set.
        
        Parameters: name and value will both be converted to strings.
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        #sanity check
        name = str(name)
        value = str(value)
        
        #cycle through the list of of name value pairs...
        for name_value in self.getErrorTrace().data:
            #looking for matching names
            if name_value.name == name:
                #good found a match, now just set the value and return
                name_value.value = value
                return
            
        #if we've gotten this far, the data has not been added
        #before. fine - add it.
        self.addData(name, value)
        
    #--------------------------------------------------------------------------
    def getDescription(self):
        '''
        Returns copy of description of current error. 
        
        Parameters: None
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        return self.getErrorTrace().shortDescription
    #--------------------------------------------------------------------------
    def getFileName(self): # pragma: NO COVER
        '''
        Returns file name information of the error
        
        Parameters: None
        
        Returns: filename
        
        Raises: Nothing
        '''
        return self.getErrorTrace().file
    #--------------------------------------------------------------------------
    def getLineNumber(self): # pragma: NO COVER
        '''
        Returns line number information of the error
        
        Parameters: None
        
        Returns: line number
        
        Raises: Nothing
        '''
        return self.getErrorTrace().lineNum
    #--------------------------------------------------------------------------
    def getRoutine(self): # pragma: NO COVER
        '''
        Returns routine information of the error

        Parameters: None
        
        Returns: routine name
        
        Raises: Nothing
        '''
        return self.getErrorTrace().routine
    #--------------------------------------------------------------------------
    def getHostName(self): # pragma: NO COVER
        '''
        Returns host name information of the error
        
        Parameters: None
        
        Returns: hostname
        
        Raises: Nothing
        '''
        return self.getErrorTrace().host
    #--------------------------------------------------------------------------
    def getProcess(self): # pragma: NO COVER
        '''
        Returns process information of the error. Its name or process ID.

        Parameters: None
        
        Returns: Process ID

        Raises: Nothing
        '''
        return self.getErrorTrace().process
    #--------------------------------------------------------------------------
    def getThread(self): # pragma: NO COVER
        '''
        Returns thread information of the error. The name of thread or its ID.

        Parameters: None

        Returns: Thread ID

        Raises: Nothing
        '''
        return self.getErrorTrace().thread
    #--------------------------------------------------------------------------
    def getTimeStamp(self): # pragma: NO COVER
        '''
        Returns time stamp of the error in 100th of nanoseconds.

        Parameters: None

        Returns: time stamp

        Raises: Nothing
        '''
        return self.getErrorTrace().timeStamp
    #--------------------------------------------------------------------------
    def getErrorCode(self): # pragma: NO COVER
        '''
        Returns error code
        
        Parameters: None

        Returns: error code

        Raises: Nothing
        '''
        return self.getErrorTrace().errorCode
    #--------------------------------------------------------------------------
    def getErrorType(self): # pragma: NO COVER
        '''
        Returns error type

        Parameters: None
        
        Returns: error type

        Raises: Nothing
        '''
        return self.getErrorTrace().errorType
    #--------------------------------------------------------------------------
    def getSeverity(self): # pragma: NO COVER
        '''
        Returns error severity
        
        Parameters: None

        Returns: Severity

        Raises: Nothing
        '''
        return self.getErrorTrace().severity
    #--------------------------------------------------------------------------
    def setTimeStamp(self, time): # pragma: NO COVER
        '''
        Sets time stamp of the error in 100th of nanoseconds.

        Parameters: the time stamp of the error
        
        Returns: Nothing

        Raises: Nothing
        '''
        self.getErrorTrace().timeStamp = time
    #--------------------------------------------------------------------------
    def setFileName(self,file_name): # pragma: NO COVER
        '''
        Sets file name
        
        Parameters: name of the file
        
        Returns: Nothing

        Raises: Nothing
        '''
        self.getErrorTrace().file = str(file_name)
    #--------------------------------------------------------------------------
    def setLineNumber(self, line_number): # pragma: NO COVER
        '''
        Sets line number
        
        Parameters: the line number
        
        Returns: Nothing

        Raises: Nothing
        '''
        self.getErrorTrace().lineNum = long(line_number)
    #--------------------------------------------------------------------------
    def setError(self, error_code=None, error_type=None):
        '''
        Sets the error type/code
        
        Parameters: self-explanatory
        
        Returns: Nothing

        Raises: ValueError if invalid parameters are given
        '''
        if error_code != None and error_type != None:
            self.getErrorTrace().errorCode = long(error_code)
            self.getErrorTrace().errorType = long(error_type)
        else:
            raise ValueError('Bad parameters')
    #--------------------------------------------------------------------------
    def setSeverity(self, severity): # pragma: NO COVER
        '''
        Sets error severity
        
        Parameters: severity of the error
        
        Returns: Nothing

        Raises: Nothing
        '''
        self.getErrorTrace().severity = severity
#--------------------------------------------------------------------------
class ErrorTrace(ACSErr.ErrorTrace, ErrorTraceHelper):
    '''
    This class is an implementation of ACSErr.ErrorTrace that also provides
    the helper class methods defined in ErrorTraceHelper directly. Quite
    useful.
    
    '''
    def __init__(self,
                 error_type,
                 error_code,
                 exception = None,
                 description = "None", 
                 nvSeq = None,
                 level = 3,
                 severity = None,
                 sourceobject = ""):
        '''
        Parameters:
        - error_type is the error type (a long)
        - error_code is the error code (a long)
        - exception is a previous exception from the ACS Error System, or a Python 
	native exception, in which case, an ErrorTrace will be constructed. The traceback
	should be ok in most cases, but if you find that it isn't, a possible workaround is
	converting the python exception to an ACS exception using pyExceptionToCORBA()
	before passing it to an ACSError constructor. Remember, that if you don't use 
	pyExceptionToCORBA(), if 
	you are dealing with a native exception, you must pass create=1 to the ACSError 
	constructor.
        - description is a stringified description of the errror
        - nvSeq is a name-value sequence describing the error condition. Each value
        should be of the type ACSErr.NameValue
        - level is an offset from stack()
        - severity is the severity of the error
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
            
        # Client, components and containers have 
        frame = call_frame[0]
        if 'self' in frame.f_locals and 'name' in frame.f_locals['self'].__dict__:
            sourceObject = frame.f_locals['self'].name
        else:
            sourceObject = sourceobject
        
        #Set the severity
        if severity == None:
            severity = ACSErr.Error
            
	
	#let's get the traceback in case we are dealing
	#with a Python native exception
	from traceback import extract_tb,format_exc
	string_tb=format_exc()
	from sys import exc_info
	tuple_tb=extract_tb(exc_info()[2])
        try:
            #If the previous exception is an ACS Error System Exception
            if isinstance(exception.errorTrace, ACSErr.ErrorTrace):
                #We can use an error stack...
                errortrace = [ exception.errorTrace ]
        except Exception, e:
           if exception == None:
                errortrace = []
           else:	
		#In this case we are dealing with a native Python Exception
		#so we have to "transform" it to an ACS exception
		#(actually we are only interested in the ErrorTrace)
                from ACSErrTypePythonNativeImpl import PythonExImpl
		

                #next get the type of native_ex. this will be used as the short
                #description
                descript = exception.__doc__

                #create the new exception
                new_except = PythonExImpl()

                #redo the error trace so that the level is one lower
                new_et = ErrorTrace(new_except.getErrorType(),
                                    new_except.getErrorCode(),
                                    description = new_except.getDescription(),
                                    level = 2, sourceobject=sourceObject)

		#Now modify some fields
		new_et.file=tuple_tb[0][0]
		new_et.lineNum=tuple_tb[0][1]
		new_et.routine=tuple_tb[0][2]

                new_except.setErrorTrace(new_et)

                #time to add the real description
                new_except.addData("Real Description", descript)

    		#add the real traceback
    		new_except.addData("Traceback", string_tb)


                errortrace= [ new_except.errorTrace ]

                
        #Create error trace
        errortrace = ACSErr.ErrorTrace.__init__(self,
                                                str(filename),   #string file;
                                                int(line),        #long lineNum;
                                                str(routine),     #string routine;
                                                str(host),        #string host;
                                                str(process),     #string process;
                                                str(thread),      #string thread;
                                                long(time),       #unsigned long long timeStamp;
                                                str(sourceObject),#string sourceObject;
                                                long(error_type), #ACSErr::ACSErrType errorType;
                                                long(error_code), #ACSErr::ErrorCode errorCode;
                                                severity,         #ACSErr::Severity severity;
                                                description,      #string shortDescription;
                                                nvSeq,            #NameValueSeq data;
                                                errortrace)       #sequence<ErrorTrace, 1> previousError;
        
        ErrorTraceHelper.__init__(self)
        
