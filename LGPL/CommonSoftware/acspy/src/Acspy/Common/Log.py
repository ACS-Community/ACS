# @(#) $Id: Log.py,v 1.37 2007/12/20 22:47:59 agrimstrup Exp $
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

Logging also respects the "ACS_LOG_STDOUT" and "ACS_LOG_CENTRAL" environment
variables.

Last but not least, developers should use the getLogger() function instead
of creating new instances of the Logger class which can take a very long
time depending on managers load.

TODO:
- make sure all methods are tested in the modular test. Have a feeling
XML-related methods are untested at this point.
'''

__revision__ = "$Id: Log.py,v 1.37 2007/12/20 22:47:59 agrimstrup Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from os        import environ
from inspect   import stack
import sys
import math
import logging
from traceback import print_exc
from socket import gethostname
import time
from os import getpid
from traceback import extract_stack
#--ACS Imports-----------------------------------------------------------------
import maci
from Acspy.Common.ACSHandler import ACSHandler
from Acspy.Common.ACSHandler import ACSFormatter
from Acspy.Common.ACSHandler import ACSLogRecord
from Acspy.Common.TimeHelper import TimeUtil
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

logging.OFF = logging.EMERGENCY + 1
logging.addLevelName(logging.OFF, "OFF")

# Log Levels are received as integer in the range [0,11]
# with 1 and 7 undefined.  The current code interpolates
# 1 and 7 to the next highest level, so that behaviour
# has been incorporated in the lookup table.
NLEVELS = { 0 : logging.NOTSET,
            1 : logging.TRACE,  
            2 : logging.TRACE,  
            3 : logging.DEBUG,  
            4 : logging.INFO,  
            5 : logging.NOTICE,  
            6 : logging.WARNING,  
            7 : logging.ERROR,  
            8 : logging.ERROR,  
            9 : logging.CRITICAL,  
           10 : logging.ALERT,  
           11 : logging.EMERGENCY,
           99 : logging.OFF
           }

# Since the Python handlers only use the Python constants
# we need to reverse map them back to the integer range.
# The interpolated values, 1 and 7, are reported as their
# effective log levels. 
RLEVELS = { logging.NOTSET    : 0,
            logging.TRACE     : 2,  
            logging.DEBUG     : 3,  
            logging.INFO      : 4,  
            logging.NOTICE    : 5,  
            logging.WARNING   : 6,  
            logging.ERROR     : 8,  
            logging.CRITICAL  : 9,  
            logging.ALERT     : 10,  
            logging.EMERGENCY : 11,  
            logging.OFF       : 99
           }



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
def getLevelName(lnum):
    return logging.getLevelName(NLEVELS[lnum])
               
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
#determine ACS_LOG_STDOUT
if environ.has_key('ACS_LOG_STDOUT'):
    ACS_LOG_STDOUT = int(environ['ACS_LOG_STDOUT'])
else:
    ACS_LOG_STDOUT = 3

#determine ACS_LOG_CENTRAL
if environ.has_key('ACS_LOG_CENTRAL'):
    ACS_LOG_CENTRAL = int(environ['ACS_LOG_CENTRAL'])
else:
    ACS_LOG_CENTRAL = 3

def stdoutOk(log_priority):
    '''
    Helper method returns true if log_priority is greater than $ACS_LOG_STDOUT.
    '''
    return (ACS_LOG_STDOUT <= RLEVELS[log_priority])
    
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

        #disable the propagation of log messages to higher level loggers
        self.propagate = 0
        
        #create a stdout handler
        self.stdouthandler = logging.StreamHandler(sys.stdout)
        
        #create our own formatter
        self.acsformatter = ACSFormatter()

        #register the formatter with stdouthandler
        self.stdouthandler.setFormatter(self.acsformatter)
    
        #create an ACS log svc handler
        self.acshandler = ACSHandler()

        #flag to indicate this is the default logger for this process
        self.isdefault = False

        #add handlers
        self.addHandler(self.stdouthandler)
        self.addHandler(self.acshandler)

        #set loglevels to default
        self.setLevels(maci.LoggingConfigurable.LogLevels(False,ACS_LOG_CENTRAL, ACS_LOG_STDOUT))
    #------------------------------------------------------------------------    
    def __getCallerName(self):
        '''
        Helper function returns the name of the calling function or method.
        '''
        try:
            func_name = stack()[3][3]
        except IndexError, ex:
            func_name = "Indeterminable Name"

        ffunc_name = func_name.replace('<module>', 'Main')
        
        return ffunc_name
    #------------------------------------------------------------------------
    def __formatMessage(self, msg):
        '''
        Helper function formats the message.
        '''
        func_name = self.__getCallerName()
        msg = func_name + " - " + msg
        
        return msg
    #------------------------------------------------------------------------
    def logAtLevel(self, lvl, msg):
        '''
        Log a message at the given level.

        Parameters:
        - lvl is the log level to send the message at
        - msg is a string to be sent to the logging system

        Returns: Nothing

        Raises: Nothing
        '''
        msg = self.__formatMessage(msg)
        self.log(NLEVELS[lvl], msg)
        
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
        if self.acshandler.logSvc!=None:
            self.acshandler.logSvc.logErrorWithPriority(errortrace, priority)

            #could have old errors cached up
            for et in self.error_trace_list:
                self.acshandler.logSvc.logErrorWithPriority(et, priority)

            #zero the list
            self.error_trace_list = []
                
        else:
            #save it to be processes later
            self.error_trace_list.append(errortrace)
    #------------------------------------------------------------------------
    def logTypeSafe(self, priority, timestamp, msg, rtCont, srcInfo, data, audience=None, array=None, antenna=None):
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
        if audience == None:
                audience = ""
        if array == None:
                array = ""
        if antenna == None:
                antenna = ""
        self.acshandler.logSvc.logWithPriority(priority, timestamp, msg, rtCont, srcInfo, data, audience, array, antenna)
    #------------------------------------------------------------------------
    def logNotSoTypeSafe(self, priority, msg, audience=None, array=None, antenna=None):
        '''
        Log a message indicating audience, array and/or antenna.

        Parameter:
        - priority value of logging priority
        - msg log definition shortDescription
        - audience
        - array
        - antenna
        
        Returns: Nothing

        Raises: Nothing
        '''
        cur_stack=extract_stack()
        rtCont=ACSLog.RTContext("",str(getpid()),str(gethostname()).replace("<", "").replace(">", ""),"","")
        srcInfo=ACSLog.SourceInfo(str(cur_stack[0][0]),str(cur_stack[0][2]),long(cur_stack[0][1]))
        timestamp=TimeUtil().py2epoch(time.time()).value
        if audience == None:
                audience = ""
        if array == None:
                array = ""
        if antenna == None:
                antenna = ""
        self.acshandler.logSvc.logWithAudience(priority, timestamp, msg, rtCont, srcInfo, audience, array, antenna)
    #------------------------------------------------------------------------
    def setLevels(self, loglevel):
        '''
        Adjust the priority level filter for log messages.

        Parameter:
        - maci.LoggingConfigurable.LogLevels object containing new level information

        Returns: Nothing

        Raises: Nothing
        '''
        if loglevel.useDefault and not self.isdefault:
            self.usingDefault = True
            self.stdouthandler.setLevel(self.getEffectiveHandlerLevel('stdouthandler'))
            self.acshandler.setLevel(self.getEffectiveHandlerLevel('acshandler'))
        else:
            self.usingDefault = False
            self.stdouthandler.setLevel(NLEVELS[loglevel.minLogLevelLocal]) 
            self.acshandler.setLevel(NLEVELS[loglevel.minLogLevel])
    #------------------------------------------------------------------------
    def getLevels(self):
        '''
        Return the current priority level values for the stdout and central logs.

        Parameter: None

        Returns: maci.LoggingConfigurable.LogLevels object containing the current level settings

        Raises: Nothing
        '''
        return maci.LoggingConfigurable.LogLevels(self.usingDefault or self.isdefault, RLEVELS[self.acshandler.level],
                                                  RLEVELS[self.stdouthandler.level])
    #------------------------------------------------------------------------
    def getEffectiveHandlerLevel(self, handler):
        """
        Get the effective level for this handler.

        Loop through this logger and its parents in the logger hierarchy,
        looking for a handler with non-zero logging level. Return the first one found.
        """
        logger = self.parent
        while logger:
            if logger.__dict__[handler].level:
                return logger.__dict__[handler].level
            logger = logger.parent
        return logging.NOTSET
    #------------------------------------------------------------------------
    def updateChildren(self):
        """
        Update the log levels for the leaf nodes that are using default values.

        Loop through this logger and its parents in the logger hierarchy,
        looking for a handler with non-zero logging level. Return the first one found.
        """
        loggers = getLoggerNames(self.name)
        for lname in loggers:
            if lname != self.name:
                l = getLogger(lname)
                if l.usingDefault:
                    l.setLevels(maci.LoggingConfigurable.LogLevels(True, 0, 0))
                l.updateChildren()

    #------------------------------------------------------------------------
    def setDefault(self, val):
        self.isdefault = val
    #------------------------------------------------------------------------
    def makeRecord(self, name, level, fn, lno, msg, args, exc_info, func=None, extra=None):
        """
        Build the ACSLogRecord for this information.
        """
        rv = ACSLogRecord(name, level, fn, lno, msg, args, exc_info, func)
        if extra:
            for key in extra:
                if (key in ["message", "asctime"]) or (key in rv.__dict__):
                    raise KeyError("Attempt to overwrite %r in LogRecord" % key)
                rv.__dict__[key] = extra[key]
        return rv

    
    

#----------------------------------------------------------------------------
# The Python logging module contains code to manage a hierarchy of loggers.
# The root logger has a default level setting of WARNING and would return
# logging.Logger objects.  These defaults were changed to reflect ACS
# operations.
logging.setLoggerClass(Logger)
logging.root.setLevel(logging.NOTSET)

# Moving to a hierarchical logging structure allows us to maintain default
# level information in a common node in the tree.  This logger is named
# "DefaultPythonLogger"
defaultlogger = Logger("DefaultPythonLogger")
defaultlogger.setDefault(True)

logging.Logger.root = defaultlogger
logging.Logger.manager = logging.Manager(logging.Logger.root)

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
    return logging.getLogger(str(name))
#----------------------------------------------------------------------------
def getLoggerNames(startfilter=None):
    '''
    This returns a list of defined known loggers.

    Used to support the LoggingConfigurable method get_logger_names.

    Parameters:  a string containing the beginning of the names to be returned. 

    Returns: A list of logger name strings

    Raises:  Nothing
    '''
    logkeys = logging.Logger.manager.loggerDict.keys()
    if startfilter:
        loggers = []
        for l in logkeys:
            if l.startswith(startfilter):
                loggers.append(l)
        return loggers
    else:
        return logkeys
#----------------------------------------------------------------------------
def doesLoggerExist(key_name):
    '''
    This method determines if a logger exists for the given name.

    Parameters:  name of the logger being queried

    Returns:  True if named logger already exists.

    Raises:  Nothing
    '''
    return key_name in logging.Logger.manager.loggerDict
#----------------------------------------------------------------------------
