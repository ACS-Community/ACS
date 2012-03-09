# @(#) $Id: Log.py,v 1.2 2012/03/09 14:34:28 acaproni Exp $
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
'''

__revision__ = "$Id: Log.py,v 1.2 2012/03/09 14:34:28 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from os        import environ
from inspect   import stack
import sys
import math
import logging
from logging.handlers import MemoryHandler
from traceback import print_exc
from socket import gethostname
import time
import os
from traceback import extract_stack
from atexit import register
import sched
import threading
import abc
#--ACS Imports-----------------------------------------------------------------
import maci
from Acspy.Common.ACSHandler import ACSHandler
from Acspy.Common.ACSHandler import ACSFormatter
from Acspy.Common.ACSHandler import ACSLogRecord
from Acspy.Common.TimeHelper import TimeUtil
#--CORBA STUBS-----------------------------------------------------------------
import ACSLog
#--GLOBALS---------------------------------------------------------------------

#
# _srcfile is used when walking the stack to check when we've got the first
# caller stack frame.
#
if __file__[-4:].lower() in ['.pyc', '.pyo']:
    _srcfile = __file__[:-4] + '.py'
else:
    _srcfile = __file__
_srcfile = os.path.normcase(_srcfile)

#------------------------------------------------------------------------------
logging.TRACE = logging.NOTSET + 1
logging.addLevelName(logging.TRACE, "TRACE")

logging.DELOUSE = logging.TRACE + 1
logging.addLevelName(logging.DELOUSE, "DELOUSE")

logging.NOTICE = logging.INFO + 1
logging.addLevelName(logging.NOTICE, "NOTICE")

logging.ALERT = logging.CRITICAL + 1
logging.addLevelName(logging.ALERT, "ALERT")

logging.EMERGENCY = logging.ALERT + 1
logging.addLevelName(logging.EMERGENCY, "EMERGENCY")

logging.OFF = logging.EMERGENCY + 1
logging.addLevelName(logging.OFF, "OFF")

# Since the Python handlers only use the Python constants
# we need to reverse map them back to the integer range.
# The interpolated values, 1 and 7, are reported as their
# effective log levels. 
RLEVELS = { logging.NOTSET    : 0,
            logging.TRACE     : 1,
            logging.DELOUSE   : 2,  
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



# Log Levels are received as integer in the range [0,11]
# with 1 and 7 undefined.  The current code interpolates
# 1 and 7 to the next highest level, so that behaviour
# has been incorporated in the lookup table.
LEVELS = { 0                        : logging.NOTSET,
           ACSLog.ACS_LOG_TRACE     : logging.TRACE,
           1                        : logging.TRACE,
           ACSLog.ACS_LOG_DELOUSE   : logging.DELOUSE,
           2                        : logging.DELOUSE,  
           ACSLog.ACS_LOG_TRACE     : logging.TRACE,
           3                        : logging.DEBUG,  
           ACSLog.ACS_LOG_DEBUG     : logging.DEBUG,  
           4                        : logging.INFO,  
           ACSLog.ACS_LOG_INFO      : logging.INFO,  
           5                        : logging.NOTICE,  
           ACSLog.ACS_LOG_NOTICE    : logging.NOTICE,  
           6                        : logging.WARNING,  
           ACSLog.ACS_LOG_WARNING   : logging.WARNING,  
           7                        : logging.ERROR,  
           8                        : logging.ERROR,  
           ACSLog.ACS_LOG_ERROR     : logging.ERROR,  
           9                        : logging.CRITICAL,  
           ACSLog.ACS_LOG_CRITICAL  : logging.CRITICAL,  
           10                       : logging.ALERT,  
           ACSLog.ACS_LOG_ALERT     : logging.ALERT,  
           11                       : logging.EMERGENCY,
           ACSLog.ACS_LOG_EMERGENCY : logging.EMERGENCY,
           99                       : logging.OFF
           }

#------------------------------------------------------------------------------
def getLevelName(lnum):
    return logging.getLevelName(LEVELS[lnum])
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


#------------------------------------------------------------------------
def setCapacity(capacity):
    '''
    Set the maximum capacity for the central log queue.
    
    Parameters:
    - capacity is the new maximum number of pending records
    
    Returns: Nothing
    
    Raises: NameError if no Logger object has been previously instantiated
    '''
    if capacity > 0:
        CENTRALHANDLER.capacity = capacity
    else:
        CENTRALHANDLER.capacity = 0
#------------------------------------------------------------------------
def setBatchSize(batchsize):
    '''
    Set the batch size for the central log queue.  Batch size cannot
    exceed the capacity.
    
    Parameters:
    - batchsize is the new number of records to be sent as a group.
    
    Returns: Nothing
    
    Raises: NameError if no Logger object has been previously instantiated
    '''
    if batchsize > CENTRALHANDLER.capacity:
        CENTRALHANDLER.batchsize = CENTRALHANDLER.capacity
    elif batchsize >= 0:
        CENTRALHANDLER.batchsize = batchsize
    else:
        CENTRALHANDLER.batchsize = 0
#------------------------------------------------------------------------
def setImmediateDispatchLevel(level):
    '''
    Set the immediate dispatch threshold for the central log queue.
    
    Parameters:
    - level is the new level that triggers immediate flushing of the queue.
    
    Returns: Nothing
    
    Raises: KeyError if level is not defined
            NameError if no Logger object has been previously instantiated
    '''
    CENTRALHANDLER.dispatchlevel = LEVELS[level]
#------------------------------------------------------------------------

def setDefaultLevels(levels):
    '''
    Set the default log level filtering for this process.

    Parameters:
    - level is the LogLevels object containing the new values

    Returns:  Nothing

    Raises: NameError if no Logger object has been previously instantiated
    '''
    DEFAULTLOCALHANDLER.setLevel(LEVELS[levels.minLogLevelLocal]) 
    DEFAULTCENTRALHANDLER.setLevel(LEVELS[levels.minLogLevel])
#------------------------------------------------------------------------
def getDefaultLevels():
    '''
    Retrive the current default log levels

    Parameters:  None

    Returns: LogLevels object containing the current default log levels.

    Raises: NameError if no Logger object has been previously instantiated
    '''
    return maci.LoggingConfigurable.LogLevels(True, RLEVELS[DEFAULTCENTRALHANDLER.level],
                                              RLEVELS[DEFAULTLOCALHANDLER.level])
#------------------------------------------------------------------------

# The ACS logging system attempts to reduce the amount of network traffic
# by batching log messages.  However, batching can cause long delays in
# propagating the information, especially if the process doesn't log many
# messages or if the log level is set at a high level.
#
# To address this problem, the Python logger implements a periodic flush
# thread that may be used to clear the buffered messages.  It is optional.
#

# Flush every 10 seconds is the default operation.
DEFAULT_FLUSH_PERIOD = 10

# Initialize the singleton.
#
# FLUSHTHREAD is the thread the handles the flush processing
# SCHEDULER is the manager of the event queue
# NEXTEVENT is a tuple containing the information for the next scheduled event
# INTERVAL is the time between events
try:
    FLUSHTHREAD
except:
    FLUSHTHREAD = None
    SCHEDULER = None
    NEXTEVENT = None
    INTERVAL = None
#------------------------------------------------------------------------
def flush():
    '''
    Flush the messages from the buffer and schedule the next event.

    Returns:  Nothing

    Raises: Nothing
    '''
    global NEXTEVENT

    NEXTEVENT = SCHEDULER.enter(INTERVAL,1,flush,())
    CENTRALHANDLER.flush()
#------------------------------------------------------------------------
def delay(remaining):
    '''
    Pause before checking if the next event should be processed.

    Parameter:
    - remaining is the number of seconds to wait before the next event.
    '''
    # We can't sleep the entire interval period.  If we did, we could
    # never change the interval.  As a compromise, we check the event
    # queue every second.
    time.sleep(1) 
#------------------------------------------------------------------------
def startPeriodicFlush(interval=DEFAULT_FLUSH_PERIOD):
    '''
    Configure and start the periodic flush thread.  

    Parameter:
    - interval is the number of seconds between flushes

    Returns:  Nothing

    Raises:  Nothing
    '''
    global FLUSHTHREAD
    global SCHEDULER
    global NEXTEVENT
    global INTERVAL

    # Only one flush thread is allowed per process
    if FLUSHTHREAD is None:
        INTERVAL = interval

        # Only one event queue per process
        if SCHEDULER is None:
            SCHEDULER = sched.scheduler(time.time,delay)
        NEXTEVENT = SCHEDULER.enter(INTERVAL,1,flush,())
        FLUSHTHREAD = threading.Thread(target=SCHEDULER.run)
        FLUSHTHREAD.start()

        # To ensure a clean interpreter shutdown
        register(stopPeriodicFlush)
#------------------------------------------------------------------------
def stopPeriodicFlush():
    '''
    Stop the periodic flush thread.

    Returns:  Nothing

    Raises:  Nothing
    '''
    try:
        SCHEDULER.cancel(NEXTEVENT)
    except:
        pass
    FLUSHTHREAD.join()
#------------------------------------------------------------------------
def setFlushInterval(interval):
    '''
    Change the period between flushes.

    Parameter:
    - interval is the number of seconds between flushes

    Return:  Nothing

    Raise:  Nothing
    '''
    global NEXTEVENT
    global INTERVAL

    if interval <= 0:
        # We can't go back in time so we shutdown the thread instead.
        stopPeriodicFlush()
    else:
        # The interval change takes effect immediately so the pending
        # flush has to be rescheduled
        INTERVAL = interval
        newevent = SCHEDULER.enter(INTERVAL,1,flush,())
        try:
            SCHEDULER.cancel(NEXTEVENT)
        except:
            pass
        NEXTEVENT = newevent
#------------------------------------------------------------------------
def isFlushRunning():
    '''
    Is the flush thread running?

    Returns: the state of the flush thread or False if thread has not been
             created.

    Raises:  Nothing
    '''
    try:
        return FLUSHTHREAD.isAlive()
    except:
        return False
#------------------------------------------------------------------------------
class LogThrottleAlarmerBase:
    '''
    Abstract base class for the LogThrottle to raise/clear alarms
    '''
    __metaclass__ = abc.ABCMeta
    
    @abc.abstractmethod
    def sendThrottleAlarm(self, active):
        '''
        Send/Clear the alarm for the LogThrottle
        
        Raise the alarm if active=True and clear otherwise
        '''
        return
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
        global LOCALHANDLER, CENTRALHANDLER, DEFAULTLOCALHANDLER, DEFAULTCENTRALHANDLER
        
        #pass it on to baseclass. by default all logs are sent to the handlers
        logging.Logger.__init__(self, name, logging.NOTSET)

        # The ACS logger uses two handlers one for local messages and one for
        # central messages.  There should be only one pair of handlers per
        # process.

        # Create a singleton handler for the local messages.  These messages
        # sent to stdout stream.
        try:
            LOCALHANDLER
        except NameError:
            LOCALHANDLER = logging.StreamHandler(sys.stdout)
            LOCALHANDLER.setFormatter(ACSFormatter())


        # Create a singleton handler for messages destined for the central
        # logging service.
        try:
            CENTRALHANDLER
        except NameError:
            CENTRALHANDLER = ACSHandler()
            register(CENTRALHANDLER.flush)
                
        # The default filtering level for the local and central loggers
        # are held in separate handlers.  By moving the management of the
        # logging levels to these handlers, we can allow users to set
        # log levels lower than the default value.


        # Singleton wrapper for the local message handler
        try:
            DEFAULTLOCALHANDLER
        except NameError:
            DEFAULTLOCALHANDLER = MemoryHandler(capacity=0, target=LOCALHANDLER)
            DEFAULTLOCALHANDLER.setLevel(LEVELS[ACS_LOG_STDOUT])

        # Singleton wrapper for the central message handler
        try:
            DEFAULTCENTRALHANDLER
        except NameError:
            DEFAULTCENTRALHANDLER = MemoryHandler(capacity=0, target=CENTRALHANDLER)
            DEFAULTCENTRALHANDLER.setLevel(LEVELS[ACS_LOG_CENTRAL])

        #create a stdout handler
        self.stdouthandler = DEFAULTLOCALHANDLER
        
        #create an ACS log svc handler
        self.acshandler = DEFAULTCENTRALHANDLER

        #flag to indicate if this logger is using default values
        self.usingDefault = True

        #Nested loggers should not repeat messages
        self.propagate = 0

        #add handlers
        self.addHandler(self.stdouthandler)
        self.addHandler(self.acshandler)
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

        Raises: ValueError if lvl is NOTSET or OFF
        '''
        msg = self.__formatMessage(msg)
        if lvl == 0 or lvl == 99:
            raise ValueError("Cannot log messages at level %d" % lvl)
        self.log(LEVELS[lvl], msg)
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
    def logDelouse(self, msg):
        '''
        Log a delouse message.

        Parameters:
        - msg is a string to be sent to the logging system
        

        Returns: Nothing

        Raises: Nothing
        '''
        msg = self.__formatMessage(msg)
        self.log(LEVELS[ACSLog.ACS_LOG_DELOUSE], msg)
        
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
    def logError(self, msg):
        '''
        Log an error message.

        Parameters:
        - msg is a string to be sent to the logging system
        
        
        Returns: Nothing

        Raises: Nothing
        '''
        msg = self.__formatMessage(msg)
        self.log(LEVELS[ACSLog.ACS_LOG_ERROR], msg)
        
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
    def logDelouse(self, msg):
        '''
        Log a delouse message.
        
        Parameters:
        - msg is a string to be sent to the logging system
        
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        msg = self.__formatMessage(msg)
        self.log(LEVELS[ACSLog.ACS_LOG_DELOUSE], msg)
        
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

        Raises: KeyError if priority is not in the ACSLog.Priorities
        '''
        #ok to send it directly
        if not priority in ACSLog.Priorities._items:
            raise KeyError("Invalid Log Level")
        self.log(LEVELS[priority], 'Error Trace', extra={ 'errortrace' : errortrace, 'priority' : priority})
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

        Raises: KeyError if priority is not in the ACSLog.Priorities
        '''
        if not priority in ACSLog.Priorities._items:
            raise KeyError("Invalid Log Level")
        if audience is None:
                audience = ""
        if array is None:
                array = ""
        if antenna is None:
                antenna = ""
        self.log(LEVELS[priority], msg, extra={ 'priority' : priority, 'rtCont' : rtCont, 'srcInfo' : srcInfo,
                                                'data' : data, 'audience' : audience, 'array' : array,
                                                'antenna' : antenna})
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

        Raises: KeyError if priority is not in the ACSLog.Priorities
        '''
        if not priority in ACSLog.Priorities._items:
            raise KeyError("Invalid Log Level")
        if audience is None:
                audience = ""
        if array is None:
                array = ""
        if antenna is None:
                antenna = ""
        self.log(LEVELS[priority], msg, extra={ 'priority' : priority, 'audience' : audience,
                                                'array' : array, 'antenna' : antenna})
    #------------------------------------------------------------------------
    def setLevels(self, loglevel):
        '''
        Adjust the priority level filter for log messages.

        Parameter:
        - maci.LoggingConfigurable.LogLevels object containing new level information

        Returns: Nothing

        Raises: Nothing
        '''
        if loglevel.useDefault and not self.usingDefault:
            self.usingDefault = True
            self.removeHandler(self.stdouthandler)
            self.removeHandler(self.acshandler)
            self.addHandler(DEFAULTLOCALHANDLER)
            self.addHandler(DEFAULTCENTRALHANDLER)
            self.stdouthandler = DEFAULTLOCALHANDLER
            self.acshandler = DEFAULTCENTRALHANDLER
        elif not loglevel.useDefault:
            if self.usingDefault:
                self.usingDefault = False
                self.removeHandler(self.stdouthandler)
                self.removeHandler(self.acshandler)
                self.stdouthandler = MemoryHandler(capacity=0, target=LOCALHANDLER)
                self.acshandler = MemoryHandler(capacity=0, target=CENTRALHANDLER)
                self.addHandler(self.stdouthandler)
                self.addHandler(self.acshandler)
            self.stdouthandler.setLevel(LEVELS[loglevel.minLogLevelLocal])
            self.acshandler.setLevel(LEVELS[loglevel.minLogLevel])
    #------------------------------------------------------------------------
    def getLevels(self):
        '''
        Return the current priority level values for the stdout and central logs.

        Parameter: None

        Returns: maci.LoggingConfigurable.LogLevels object containing the current level settings

        Raises: Nothing
        '''
        return maci.LoggingConfigurable.LogLevels(self.usingDefault, RLEVELS[self.acshandler.level],
                                                  RLEVELS[self.stdouthandler.level])
    #------------------------------------------------------------------------
    def findCaller(self):
        """
        Find the stack frame of the caller so that we can note the source
        file name, line number and function name.
        """
        f = logging.currentframe().f_back
        rv = "(unknown file)", 0, "(unknown function)"
        while hasattr(f, "f_code"):
            co = f.f_code
            filename = os.path.normcase(co.co_filename)
            if filename == _srcfile:
                f = f.f_back
                continue
            rv = (filename, f.f_lineno, co.co_name)
            break
        return rv
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

    
    
    def configureLogging(self, maxLogsPerSec, alarmSender=None):
        '''
        If alarmSender is not None, it must be a subclass of LogThrottleAlarmerBase
        See also ACSHandler.configureLogging
        '''
        CENTRALHANDLER.configureLogging(maxLogsPerSec,alarmSender)
#----------------------------------------------------------------------------
# The Python logging module contains code to manage a hierarchy of loggers.
# The root logger has a default level setting of WARNING and would return
# logging.Logger objects.  These defaults were changed to reflect ACS
# operations.
logging.setLoggerClass(Logger)
logging.root.setLevel(logging.NOTSET)

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
