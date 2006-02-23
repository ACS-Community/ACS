# @(#) $Id: Log.py,v 1.15 2005/10/17 15:52:11 dfugate Exp $
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

__revision__ = "$Id: Log.py,v 1.15 2005/10/17 15:52:11 dfugate Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from os        import environ
from os        import path
from os        import getpid
from inspect   import stack
import sys
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
SEVERITIES = { 0x0002 : ACSLog.ACS_LOG_TRACE,
               0x0004 : ACSLog.ACS_LOG_DEBUG,
               0x0010 : ACSLog.ACS_LOG_INFO,
               0x0020 : ACSLog.ACS_LOG_NOTICE,
               0x0040 : ACSLog.ACS_LOG_WARNING,
               0x0200 : ACSLog.ACS_LOG_ERROR,
               0x0400 : ACSLog.ACS_LOG_CRITICAL,
               0x1000 : ACSLog.ACS_LOG_ALERT,
               0x2000 : ACSLog.ACS_LOG_EMERGENCY
               }
#------------------------------------------------------------------------------
def getSeverity(sevNumber):
    '''
    Helper function returns the nearest severity which is greater than or equal
    to the parameter.
    '''
    #get a list of the severities
    sevList = SEVERITIES.keys()
    #sort it in place
    sevList.sort()

    #traverse the list...
    for num in sevList:
        #until we find the first occurence where the input param is less than
        #or equal
        if sevNumber <= num:
            return num
    

#------------------------------------------------------------------------------
class Logger(logging.Logger):
    '''
    Logger is of primary interest to developers. It is used to send log
    messages to the ACS Logging System. Developers need not create an instance
    of this class though as the getLogger() function returns a singled logger.
    '''
    
    #create a stdout handler
    __STDOUTHANDLER = logging.StreamHandler(sys.stdout)

    #create our own formatter
    __ACSFORMATTER = ACSFormatter()

    #register the formatter with stdouthandler
    __STDOUTHANDLER.setFormatter(__ACSFORMATTER)
    
    #determine ACS_LOG_STDOUT
    if environ.has_key('ACS_LOG_STDOUT'):
        __acsLogStdout = max(0, int(environ['ACS_LOG_STDOUT']))
    else:
        __acsLogStdout = 0x0010

    #set the filtering level for the stdout handler
    __STDOUTHANDLER.setLevel(LEVELS[SEVERITIES[getSeverity(__acsLogStdout)]])
    
    
    
    #create a file handler
    if environ.has_key('ACS_LOG_FILE'):
        __logFileName = environ['ACS_LOG_FILE']
    else:
        if environ.has_key('ACS_TMP'):
            __logFileName = path.join(environ['ACS_TMP'], 'acs_local_log')
        else:
            __logFileName = path.join(environ['ACSDATA'], 'tmp/acs_local_log')
            
    __logFileName = __logFileName + "_" +  path.basename(sys.argv[0]) + "_" + str(getpid())
    __FILEHANDLER = logging.FileHandler(__logFileName)
    __FILEHANDLER.setLevel(logging.ERROR)
    __FILEHANDLER.setFormatter(__ACSFORMATTER)

    #create an ACS log svc handler
    __ACSHANDLER = ACSHandler(0)
    
    #------------------------------------------------------------------------
    def __init__(self, name):
        '''
        Create a Logger instance.

        Parameters: name of this logger

        Returns: Nothing

        Raises: Nothing
        '''
        self.errorTraceList = []
        
        #pass it on to baseclass. by default all logs are sent to the handlers
        logging.Logger.__init__(self, name, logging.NOTSET)

        #add handlers
        self.addHandler(self.__STDOUTHANDLER)
        self.addHandler(self.__FILEHANDLER)
        self.addHandler(self.__ACSHANDLER)
    #------------------------------------------------------------------------
    def logAlert(self, msg = '', component=None):
        '''
        Log an alert message.

        Parameters:
        - msg is a string to be sent to the logging system
        - component is a deprecated keyword param.

        Returns: Nothing

        Raises: Nothing
        '''
        try:
            funcName = stack()[1][3]
        except Exception, e:
            funcName = "Indeterminable Name"

        funcName = funcName.replace('?', 'Main')
        msg = funcName + " - " + msg
        
        self.log(LEVELS[ACSLog.ACS_LOG_ALERT], msg)
        self.__checkDeprecated(component)
    #------------------------------------------------------------------------
    def logCritical(self, msg = '', component=None):
        '''
        Log a critical message.

        Parameters:
        - msg is a string to be sent to the logging system
        - component is the component which is publishing the message
        
        Returns: Nothing

        Raises: Nothing
        '''
        try:
            funcName = stack()[1][3]
        except Exception, e:
            funcName = "Indeterminable Name"

        funcName = funcName.replace('?', 'Main')
        msg = funcName + " - " + msg
        
        self.log(LEVELS[ACSLog.ACS_LOG_CRITICAL], msg)
        self.__checkDeprecated(component)
    #------------------------------------------------------------------------
    def logDebug(self, msg = '', component=None):
        '''
        Log a debug message.

        Parameters:
        - msg is a string to be sent to the logging system
        - component is the component which is publishing the message

        Returns: Nothing

        Raises: Nothing
        '''
        try:
            funcName = stack()[1][3]
        except Exception, e:
            funcName = "Indeterminable Name"

        funcName = funcName.replace('?', 'Main')
        msg = funcName + " - " + msg
        
        self.log(LEVELS[ACSLog.ACS_LOG_DEBUG], msg)
        self.__checkDeprecated(component)
    #------------------------------------------------------------------------
    def logEmergency(self, msg = '', component=None):
        '''
        Log an emergency message.

        Parameters:
        - msg is a string to be sent to the logging system
        - component is the component which is publishing the message
        
        Returns: Nothing

        Raises: Nothing
        '''
        try:
            funcName = stack()[1][3]
        except Exception, e:
            funcName = "Indeterminable Name"

        funcName = funcName.replace('?', 'Main')
        msg = funcName + " - " + msg
        
        self.log(LEVELS[ACSLog.ACS_LOG_EMERGENCY], msg)
        self.__checkDeprecated(component)
    #------------------------------------------------------------------------
    def logInfo(self, msg = '', component=None):
        '''
        Log an informational message.

        Parameters:
        - msg is a string to be sent to the logging system
        - component is the component which is publishing the message
        
        Returns: Nothing

        Raises: Nothing
        '''
        try:
            funcName = stack()[1][3]
        except Exception, e:
            funcName = "Indeterminable Name"

        funcName = funcName.replace('?', 'Main')
        msg = funcName + " - " + msg
        
        self.log(LEVELS[ACSLog.ACS_LOG_INFO], msg)
        self.__checkDeprecated(component)
    #------------------------------------------------------------------------
    def logNotice(self, msg = '', component=None):
        '''
        Log a notice message.

        Parameters:
        - msg is a string to be sent to the logging system
        - component is the component which is publishing the message
        
        Returns: Nothing

        Raises: Nothing
        '''
        try:
            funcName = stack()[1][3]
        except Exception, e:
            funcName = "Indeterminable Name"

        funcName = funcName.replace('?', 'Main')
        msg = funcName + " - " + msg
        
        self.log(LEVELS[ACSLog.ACS_LOG_NOTICE], msg)
        self.__checkDeprecated(component)
    #------------------------------------------------------------------------
    def logTrace(self, msg = '', component=None):
        '''
        Log a trace message.
        
        Parameters:
        - msg is a string to be sent to the logging system
        - component is the component which is publishing the message
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        try:
            funcName = stack()[1][3]
        except Exception, e:
            funcName = "Indeterminable Name"

        funcName = funcName.replace('?', 'Main')
        msg = funcName + " - " + msg
        
        self.log(LEVELS[ACSLog.ACS_LOG_TRACE], msg)
        self.__checkDeprecated(component)
    #------------------------------------------------------------------------
    def logWarning(self, msg = '', component=None):
        '''
        Log a warning message.

        Parameters:
        - msg is a string to be sent to the logging system
        - component is the component which is publishing the message

        Returns: Nothing

        Raises: Nothing
        '''
        try:
            funcName = stack()[1][3]
        except Exception, e:
            funcName = "Indeterminable Name"

        funcName = funcName.replace('?', 'Main')
        msg = funcName + " - " + msg
        
        self.log(LEVELS[ACSLog.ACS_LOG_WARNING], msg)
        self.__checkDeprecated(component)
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
    def logErrorTrace(self, errortrace):
        '''
        Log an error stack trace.

        Parameter:
        - errortrace (top of error stack)

        Returns: Nothing

        Raises: Nothing
        '''
        #ok to send it directly
        if self.__ACSHANDLER.logSvc!=None:
            self.__ACSHANDLER.logSvc.logError(errortrace)

            #could have old errors cached up
            for et in self.errorTraceList:
                self.__ACSHANDLER.logSvc.logError(et)

            #zero the list
            self.errorTraceList = []
                
        else:
            #save it to be processes later
            self.errorTraceList.append(errortrace)
    #------------------------------------------------------------------------
    def __checkDeprecated(self, obj):
        '''
        '''
        if obj!=None:
            print "DEPRECATED: passing a component instance as the last parameter"
            print "            parameter of logXyz methods is no longer used."
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
def setLogger(obj):
    '''
    Deprecated.
    '''
    print "DEPRECATED: passing an object to setLogger has no effect"
    return
#----------------------------------------------------------------------------
if __name__=="__main__":
    myLogger = logging.getLogger("TESTLOGGER")
    for i in range(5):
        myLogger.warning("a warning")
