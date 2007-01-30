# @(#) $Id: Log.py,v 1.23 2007/01/30 12:06:53 nbarriga Exp $
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
An interface to logging services, including the ACS Log Server.

This module is designed to be used in all Python code, and to be
failsafe, so that users can rely on getting logging output in (nearly)
all circumstances.Design choices derived from the design goals:
1) in the absence of the ACS Log Server to provide logging to a file
similar to the standard ACS logging functionality, 2) in case of
failure to log to a file, to log to stderr. Failure to log
to stderr will cause an exception; I think this effect is desirable,
but may change if I\'m persuaded otherwise.

Logging also respects the "ACS_LOG_STDOUT" environment variable.

Last but not least, developers should use the getLogger() function instead
of creating new instances of the Logger class which can take a very long
time depending on managers load.

TODO:
- make sure all methods are tested in the modular test. Have a feeling
XML-related methods are untested at this point.
'''

__revision__ = "$Id: Log.py,v 1.23 2007/01/30 12:06:53 nbarriga Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from os        import environ
from inspect   import stack
import sys
import math
import logging
from traceback import print_exc
#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.ACSHandler import ACSHandler
from Acspy.Common.ACSHandler import ACSFormatter
#--CORBA STUBS-----------------------------------------------------------------
import ACSLog
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
logging.TRACE = logging.NOTSET + 1
logging.addLevelName(logging.TRACE, "TRACE")

logging.NOTICE = logging.INFO + 1
logging.addLevelName(logging.NOTICE, "NOTICE")

logging.ALERT = logging.CRITICAL + 1
logging.addLevelName(logging.ALERT, "ALERT")

logging.EMERGENCY = logging.ALERT + 1
logging.addLevelName(logging.EMERGENCY, "EMERGENCY")


LEVELS = { ACSLog.ACS_LOG_TRACE     : logging.TRACE,  
           ACSLog.ACS_LOG_DEBUG     : logging.DEBUG,  
           ACSLog.ACS_LOG_INFO      : logging.INFO,  
           ACSLog.ACS_LOG_NOTICE    : logging.NOTICE,  
           ACSLog.ACS_LOG_WARNING   : logging.WARNING,  
           ACSLog.ACS_LOG_ERROR     : logging.ERROR,  
           ACSLog.ACS_LOG_CRITICAL  : logging.CRITICAL,  
           ACSLog.ACS_LOG_ALERT     : logging.ALERT,  
           ACSLog.ACS_LOG_EMERGENCY : logging.EMERGENCY
           }

# List of standard log message severities in ranked order
SEVERITIES = { 002 : ACSLog.ACS_LOG_TRACE,
               004 : ACSLog.ACS_LOG_DEBUG,
               010 : ACSLog.ACS_LOG_INFO,
               020 : ACSLog.ACS_LOG_NOTICE,
               040 : ACSLog.ACS_LOG_WARNING,
               0200 : ACSLog.ACS_LOG_ERROR,
               0400 : ACSLog.ACS_LOG_CRITICAL,
               01000 : ACSLog.ACS_LOG_ALERT,
               02000 : ACSLog.ACS_LOG_EMERGENCY
               }
               
#------------------------------------------------------------------------------
def getSeverity(severity_number):
    '''
    Helper function returns the nearest severity which is greater than or equal
    to the parameter.
    '''
    #get a list of the severities
    severity_list = SEVERITIES.keys()
    #sort it in place
    severity_list.sort()

    #traverse the list...
    for num in severity_list:
        #until we find the first occurence where the input param is less than
        #or equal
        if severity_number <= num:
            return num
#------------------------------------------------------------------------------           
#create a stdout handler
STDOUTHANDLER = logging.StreamHandler(sys.stdout)

#create our own formatter
ACSFORMATTER = ACSFormatter()

#register the formatter with stdouthandler
STDOUTHANDLER.setFormatter(ACSFORMATTER)
    
#determine ACS_LOG_STDOUT
if environ.has_key('ACS_LOG_STDOUT'):
    ACS_LOG_STDOUT = pow(2, max(0, int(environ['ACS_LOG_STDOUT'])-1))
else:
    ACS_LOG_STDOUT = 010

#set the filtering level for the stdout handler
STDOUTHANDLER.setLevel(LEVELS[SEVERITIES[getSeverity(ACS_LOG_STDOUT)]])

#create an ACS log svc handler
ACSHANDLER = ACSHandler()


def stdoutOk(log_priority):
    '''
    Helper method returns true if log_priority is greater than $ACS_LOG_STDOUT.
    '''
    lvl = LEVELS[SEVERITIES[getSeverity(ACS_LOG_STDOUT)]]

    return (lvl <= log_priority)

    if lvl <= log_priority:
        return 1
    else:
        return 0
    
def acsPrintExcDebug():
    '''
    Basically identical to traceback.print_exc() only one small exception -
    exception information is only printed to stdout of the ACS logging level
    is set to DEBUG or lower.
    '''
    if stdoutOk(logging.INFO):
        print_exc()
#------------------------------------------------------------------------------
class Logger(logging.Logger):
    '''
    Logger is of primary interest to developers. It is used to send log
    messages to the ACS Logging System. Developers need not create an instance
    of this class though as the getLogger() function returns a singled logger.
    '''
    #------------------------------------------------------------------------
    def __init__(self, name):
        '''
        Create a Logger instance.

        Parameters: name of this logger

        Returns: Nothing

        Raises: Nothing
        '''
        self.error_trace_list = []
        
        #pass it on to baseclass. by default all logs are sent to the handlers
        logging.Logger.__init__(self, name, logging.NOTSET)

        #add handlers
        self.addHandler(STDOUTHANDLER)
        self.addHandler(ACSHANDLER)
    #------------------------------------------------------------------------    
    def __getCallerName(self):
        '''
        Helper function returns the name of the calling function or method.
        '''
        try:
            func_name = stack()[3][3]
        except IndexError, ex:
            func_name = "Indeterminable Name"

        func_name = func_name.replace('?', 'Main')
        
        return func_name
    #------------------------------------------------------------------------
    def __formatMessage(self, msg):
        '''
        Helper function formats the message.
        '''
        func_name = self.__getCallerName()
        msg = func_name + " - " + msg
        
        return msg
    #------------------------------------------------------------------------
    def logAlert(self, msg):
        '''
        Log an alert message.

        Parameters:
        - msg is a string to be sent to the logging system

        Returns: Nothing

        Raises: Nothing
        '''
        msg = self.__formatMessage(msg)
        self.log(LEVELS[ACSLog.ACS_LOG_ALERT], msg)
        
    #------------------------------------------------------------------------
    def logCritical(self, msg):
        '''
        Log a critical message.

        Parameters:
        - msg is a string to be sent to the logging system
        
        
        Returns: Nothing

        Raises: Nothing
        '''
        msg = self.__formatMessage(msg)
        self.log(LEVELS[ACSLog.ACS_LOG_CRITICAL], msg)
        
    #------------------------------------------------------------------------
    def logDebug(self, msg):
        '''
        Log a debug message.

        Parameters:
        - msg is a string to be sent to the logging system
        

        Returns: Nothing

        Raises: Nothing
        '''
        msg = self.__formatMessage(msg)
        self.log(LEVELS[ACSLog.ACS_LOG_DEBUG], msg)
        
    #------------------------------------------------------------------------
    def logEmergency(self, msg):
        '''
        Log an emergency message.

        Parameters:
        - msg is a string to be sent to the logging system
        
        
        Returns: Nothing

        Raises: Nothing
        '''
        msg = self.__formatMessage(msg)
        self.log(LEVELS[ACSLog.ACS_LOG_EMERGENCY], msg)
        
    #------------------------------------------------------------------------
    def logInfo(self, msg):
        '''
        Log an informational message.

        Parameters:
        - msg is a string to be sent to the logging system
        
        
        Returns: Nothing

        Raises: Nothing
        '''
        msg = self.__formatMessage(msg)
        self.log(LEVELS[ACSLog.ACS_LOG_INFO], msg)
        
    #------------------------------------------------------------------------
    def logNotice(self, msg):
        '''
        Log a notice message.

        Parameters:
        - msg is a string to be sent to the logging system
        
        
        Returns: Nothing

        Raises: Nothing
        '''
        msg = self.__formatMessage(msg)
        self.log(LEVELS[ACSLog.ACS_LOG_NOTICE], msg)
        
    #------------------------------------------------------------------------
    def logTrace(self, msg):
        '''
        Log a trace message.
        
        Parameters:
        - msg is a string to be sent to the logging system
        
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        msg = self.__formatMessage(msg)
        self.log(LEVELS[ACSLog.ACS_LOG_TRACE], msg)
        
    #------------------------------------------------------------------------
    def logWarning(self, msg):
        '''
        Log a warning message.

        Parameters:
        - msg is a string to be sent to the logging system

        Returns: Nothing

        Raises: Nothing
        '''
        msg = self.__formatMessage(msg)
        self.log(LEVELS[ACSLog.ACS_LOG_WARNING], msg)
        
    #------------------------------------------------------------------------
    def logXML(self, xml):
        '''
        Log an XML string.

        Parameter:
        xml - XML string (really just any string, but the log server
        may not like anything non-XMLish -- I have not
        tried this)
        
        Returns: Nothing

        Raises: Nothing
        '''
        self.log(LEVELS[ACSLog.ACS_LOG_DEBUG], xml)
    #------------------------------------------------------------------------
    def logErrorTrace(self, errortrace, priority = ACSLog.ACS_LOG_ERROR):
        '''
        Log an error stack trace.

        Parameter:
        - errortrace (top of error stack)
        - priorty value of logging priorty

        Returns: Nothing

        Raises: Nothing
        '''
        #ok to send it directly
        if ACSHANDLER.logSvc!=None:
            ACSHANDLER.logSvc.logErrorWithPriority(errortrace, priority)

            #could have old errors cached up
            for et in self.error_trace_list:
                ACSHANDLER.logSvc.logErrorWithPriority(et, priority)

            #zero the list
            self.error_trace_list = []
                
        else:
            #save it to be processes later
            self.error_trace_list.append(errortrace)
    #------------------------------------------------------------------------
    def logTypeSafe(self, priority, timestamp, msg, rtCont, srcInfo, data):
        '''
        Log a type safe log.

        Parameter:
        - priority value of logging priority
        - timestamp time of log creation
        - msg log definition shortDescription
        - rtCont run-time context information
        - srcInfo src information
        - data name/value pairs

        Returns: Nothing

        Raises: Nothing
        '''
        ACSHANDLER.logSvc.logWithPriority(priority, timestamp, msg, rtCont, srcInfo, data)

#----------------------------------------------------------------------------
logging.setLoggerClass(Logger)

#----------------------------------------------------------------------------
def getLogger(name=None):
    '''
    This returns the singleton instance of logger.

    Used so we do not have to keep asking the slow manager for a reference to
    the logging service.

    Parameters: name of the logger

    Return: A logger

    Raises: ???
    '''
    return Logger(str(name))
#----------------------------------------------------------------------------
