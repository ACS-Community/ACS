# @(#) $Id: ACSHandler.py,v 1.2 2012/03/09 14:31:49 acaproni Exp $
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
from __future__ import with_statement

'''
Contains the implementation of logging objects designed to send logs to the
(CORBA) ACS logging service.

TODO:
- Everything
'''

__revision__ = "$Id: ACSHandler.py,v 1.2 2012/03/09 14:31:49 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from socket    import gethostname
from time      import gmtime,sleep,time
import logging
import logging.handlers
from os        import getpid
from os        import path
from os        import environ
import sys
from atexit    import register
import sched
import threading
import string
import abc
#--ACS Imports-----------------------------------------------------------------
from Acspy.Util.ACSCorba     import acsLogSvc
from Acspy.Common.TimeHelper import TimeUtil
from log_audience            import NO_AUDIENCE
import Acspy.Common.Log
#------------------------------------------------------------------------------
#--CORBA STUBS-----------------------------------------------------------------
import ACSLog
from ACSLog import NVPair
import CORBA
#--GLOBALS---------------------------------------------------------------------
#default logging cache size
DEFAULT_RECORD_CAPACITY = 10
DEFAULT_MAXIMUM_QUEUE = 1000
DEFAULT_IMMEDIATE_DISPATCH = logging.CRITICAL+1

#------------------------------------------------------------------------------
LEVELS = { 'CRITICAL' : ACSLog.ACS_LOG_CRITICAL,
            'ERROR' : ACSLog.ACS_LOG_ERROR,
            'WARN' : ACSLog.ACS_LOG_WARNING,
            'WARNING' : ACSLog.ACS_LOG_WARNING,
            'INFO' : ACSLog.ACS_LOG_INFO,
            'DEBUG' : ACSLog.ACS_LOG_DEBUG,
            'NOTSET' : ACSLog.ACS_LOG_TRACE
            }

#create a file handler
if environ.has_key('ACS_LOG_FILE'):
    LOG_FILE_NAME = environ['ACS_LOG_FILE']
else:
    if environ.has_key('ACS_TMP'):
        LOG_FILE_NAME = path.join(environ['ACS_TMP'], 'acs_local_log')
    else:
        LOG_FILE_NAME = path.join(environ['ACSDATA'], 'tmp/' + gethostname() + '/acs_local_log')

LOG_FILE_NAME = LOG_FILE_NAME + "_" +  path.basename(sys.argv[0]) + "_" + str(getpid())
#------------------------------------------------------------------------------
class ACSLogRecord(logging.LogRecord):
    '''
    This class extends the regular LogRecord information to capture
    specific information for ACS.
    '''
    def __init__(self, name, level, pathname, lineno,
                 msg, args, exc_info, func=None):
        logging.LogRecord.__init__(self,name,level,pathname,lineno,msg,args,exc_info,func)
        try:
            self.source,self.name = name.split('.',1)
        except:
            self.source = self.name

def makeACSLogRecord(name, level, fn, lno, msg, args, exc_info, func=None, extra=None):
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
#------------------------------------------------------------------------------
class ACSFormatter(logging.Formatter):
    '''
    This trivial helper class is used to automate setting up the way logs are
    printed.
    '''
    #--------------------------------------------------------------------------
    def __init__(self):
        '''
        Constructor.
        '''
        #first parameter to Formatter defines the overall message layout
        formatString = "%(asctime)s.%(msecs)03d %(name)s %(message)s"
        #second parameter to Formatter defines the date format
        dateFormat = "%Y-%m-%dT%H:%M:%S"
        #call super's constructor
        logging.Formatter.__init__(self, formatString, dateFormat)
        #work with gmtime only
        self.converter = gmtime
    #--------------------------------------------------------------------------
    def format(self, record):
        """
        Overload.

        
        """
        record.message = record.getMessage()
        if string.find(self._fmt,"%(asctime)") >= 0:
            record.asctime = self.formatTime(record, self.datefmt)
        s = self._fmt % record.__dict__
        if record.exc_info:
            # Cache the traceback text to avoid converting it multiple times
            # (it's constant anyway)
            if not record.exc_text:
                record.exc_text = self.formatException(record.exc_info)
        if record.exc_text:
            if s[-1] != "\n":
                s = s + "\n"
            s = s + record.exc_text
        if 'data' in record.__dict__:
            s += " ["
            if isinstance(record.data, dict):
                try:
                    for d in record.data:
                        ds = " %s=%s" % (d, record.data[d])
                        s += ds
                except TypeError:
                    pass
            elif isinstance(record.data, list):
                for p in record.data:
                    ds = " %s=%s" % p._tuple()
                    s += ds
            s += " ]"
        return s
#------------------------------------------------------------------------------
class ACSHandler(logging.handlers.BufferingHandler):
    '''
    Class ACSHandler is a BufferingHandler subclass which sends log messages to
    the (CORBA) ACS Log Svc. If the CORBA logging service is unavailable the log
    message will be saved and the handler will try to publish the log next time
    provided the buffer has not reached full capacity. If the buffer is full and
    the logging service is still unavailable, the buffer is reset and the log
    messages are lost from this handlers perspective.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, capacity=DEFAULT_MAXIMUM_QUEUE, dispatchlevel=DEFAULT_IMMEDIATE_DISPATCH,
                 batchsize=DEFAULT_RECORD_CAPACITY):
        '''
        Constructor.

        Parameters:
            capacity - the maximum size of the log cache.
            dispatchlevel - level of message that should be immediately sent
            batchsize - the number of log records to be held before sending
            flushinterval - maximum period that a message can wait in the send queue
        '''
        #ACS CORBA Logging Service Reference
        self.logSvc = None
        
        #call super's constructor
        logging.handlers.BufferingHandler.__init__(self, capacity)
        
        #setup a file handler to handle the extremely bad case that the 
        #CORBA logging service is down
        self.file_handler = None

        #messages of equal or greater level trigger a buffer flush
        self.dispatchlevel = dispatchlevel
        
        #number of records sent in a group
        self.batchsize = batchsize

        #try to get the corba logger now
        self.getCORBALogger()
        
        #mutex for controlling access to the buffer
        self.bufferlock = threading.RLock()
        
        #log throttle, see http://jira.alma.cl/browse/COMP-4541
        self.logThrottle = LogThrottle(-1);
        
        # To notify when the logThrottle is in action so that
        # it is possible to raise/clear an alarm
        self.logThrottleAlarmSender=None
        
        # To avoid sending the logThrottle alarm if its state did not change
        self.logThrottleAlarmActive=False

        #we want to make sure the buffer is flushed
        #before exiting the Python interpreter
        register(self.flush)
        
        # When flushing logs we check if their timestamp is older then the following
        # threshold and if it is the case then we encapsulate the log in another log
        # This is needed in case the operator pauses the script (@see ICTJ:ICT-548)
        self.timestampThreshold=60 # in seconds
        
    #--------------------------------------------------------------------------
    def configureLogging(self, maxLogPerTimeInterval, alarmSender=None):
        '''
        Configure the logging
        
        maxLogPerTimeInterval: Max number of logs per second (see logThrottle)
        logThrottleAlarmSender: to raise/clear alarms from thelogThrottle
        
        if alarmSender is not None, it must be a subclass of LogThrottleAlarmerBase
        '''
        self.logThrottle.configureLogging(maxLogPerTimeInterval)
        if alarmSender!=None:
            # Is alarmSender a subclass of LogThrottleAlarmerBase?
            if not isinstance(alarmSender, Acspy.Common.Log.LogThrottleAlarmerBase):
                raise TypeError('The alarm sender is not a parent of LogThrottleAlarmerBase')
        self.logThrottleAlarmSender=alarmSender
    #--------------------------------------------------------------------------
    def initFileHandler(self):
        '''
        Helper method initializes the file handler
        '''
        self.file_handler = logging.FileHandler(LOG_FILE_NAME)
        self.file_handler.setLevel(logging.NOTSET)
        self.file_handler.setFormatter(ACSFormatter())
    #--------------------------------------------------------------------------
    def shouldFlush(self, record):
        '''
        Overridden.

        This method returns true if the number of pending records exceeds
        the batchsize or if the new record matches or exceeds the immediate
        dispatch priority.
        '''
        return len(self.buffer) >= self.batchsize or \
              record.levelno >= self.dispatchlevel
    #--------------------------------------------------------------------------
    def emit(self, record):
        """
        Overridden.

        Append the record. If shouldFlush() tells us to, call flush() to process
        the buffer.
        """
        with self.bufferlock:
            if len(self.buffer) >= self.capacity:
                self.buffer = [ n for n in self.buffer if n.levelno >= logging.INFO ]
            if len(self.buffer) < self.capacity:
                self.buffer.append(record)
                
        if self.shouldFlush(record):
            self.flush()
    #--------------------------------------------------------------------------
    def flushToFile(self, record):
        '''
        Helper method.
        Sends a single record to file.
        
        Parameters:
            - record a Logging record
            
        Returns: Nothing
        
        Raises: ???
        '''
        #sanity check
        if self.file_handler is None:
            #create the file handler on demand only
            self.initFileHandler()
            
        self.file_handler.handle(record)    
    #--------------------------------------------------------------------------
    def flush(self):
        '''
        Overridden
        '''
        with self.bufferlock:
            for record in self.buffer:
                
                    try:
                        self.sendLog(record)
                    except:
                        self.flushToFile(record)
                    
            self.buffer = []
    #--------------------------------------------------------------------------
    def replaceOldRecord(self,record):
        # This method is invoked when a record has time timestamp older then
        # self.timestampThreshold seconds.
        #
        # In that case the original log is not submitted but replaced by that
        # returned by this method
        message="Log automatically generated by a python log older then "+str(self.timestampThreshold)+" secs"
        newRecord=makeACSLogRecord( \
                                 record.name, \
                                 record.levelno, \
                                 record.pathname, \
                                 record.lineno, \
                                 message, \
                                 record.args, \
                                 None, \
                                 record.funcName, \
                                 None)
        return newRecord
    #--------------------------------------------------------------------------
    def sendLog(self, record):
        '''
        Method which sends logs to the real ACS logging service.
        '''
        if(not self.logThrottle.checkPublishLogRecord()):
            if self.logThrottleAlarmSender!=None and not self.logThrottleAlarmActive:
                self.logThrottleAlarmActive=True
                self.logThrottleAlarmSender.sendThrottleAlarm(True)
            return
        else:
            if self.logThrottleAlarmSender!=None and self.logThrottleAlarmActive:
                self.logThrottleAlarmActive=False
                self.logThrottleAlarmSender.sendThrottleAlarm(False)
        
        
        # Create an RTContext object
        rt_context = ACSLog.RTContext(str(record.threadName).replace("<", "").replace(">", ""),
                                      str(record.source).replace("<", "").replace(">", ""),
                                      str(gethostname()).replace("<", "").replace(">", ""),
                                      "",
                                      str(record.name).replace("<", "").replace(">", ""))

        src_info = ACSLog.SourceInfo(str(record.module).replace("<", "").replace(">", ""),
                                     "Unknown",
                                     long(record.lineno))
       
        # Put remaining keyword arguments into NVPairSeq
        data = []
        
        if TimeUtil().py2epoch(time()).value>TimeUtil().py2epoch(record.created+self.timestampThreshold).value:
            # Reformat the record
            originalMsg=NVPair("Original message",record.getMessage())
            data.append(originalMsg)
            originalTimestamp=NVPair("Original timestamp",record.asctime)
            data.append(originalTimestamp)
            
            record=self.replaceOldRecord(record)
            
             
        #timestamp
        acs_timestamp = TimeUtil().py2epoch(record.created).value
        
        if 'priority' in record.__dict__:
            # The more exotic log functions have priority keyword arguments
            if 'errortrace' in record.__dict__:
                # The message is an ErrorTrace.
                self.logSvc.logErrorWithPriority(record.errortrace,
                                                 record.priority)
            elif 'data' in record.__dict__:
                # The message is a type-safe log message
                self.logSvc.logWithPriority(record.priority,
                                            acs_timestamp,
                                            record.getMessage(),
                                            record.rtCont if 'rtCont' in record.__dict__ and record.rtCont is not None else rt_context,
                                            record.srcInfo if 'srcInfo' in record.__dict__ and record.srcInfo is not None else src_info,
                                            record.data,
                                            record.audience,
                                            record.array,
                                            record.antenna)
            else:
                # The message is a not-so-type-safe message
                self.logSvc.logWithAudience(record.priority,
                                            acs_timestamp,
                                            record.getMessage(),
                                            rt_context,
                                            src_info,
                                            record.audience,
                                            record.array,
                                            record.antenna)
        elif record.levelname=='TRACE':
            self.logSvc.logTrace(acs_timestamp,
                                 record.getMessage(),
                                 rt_context,
                                 src_info,
                                 data)
        elif record.levelname=='DELOUSE':
            self.logSvc.logDelouse(acs_timestamp,
                                 record.getMessage(),
                                 rt_context,
                                 src_info,
                                 data)
        elif record.levelname=='DEBUG':
            self.logSvc.logDebug(acs_timestamp,
                                 record.getMessage(),
                                 rt_context,
                                 src_info,
                                 data)
        elif record.levelname=='INFO':
            self.logSvc.logInfo(acs_timestamp,
                                record.getMessage(),
                                rt_context,
                                src_info,
                                data)
        elif record.levelname=='NOTICE':
            self.logSvc.logNotice(acs_timestamp,
                                record.getMessage(),
                                rt_context,
                                src_info,
                                data)
        elif record.levelname=='WARNING' or record.levelname=='WARN':
            self.logSvc.logWarning(acs_timestamp,
                                   record.getMessage(),
                                   rt_context,
                                   src_info,
                                   data)
        #this is a special case because logError only takes
        #in error traces
        elif record.levelname=='ERROR':
            self.logSvc.logWithAudience(ACSLog.ACS_LOG_ERROR,
                                        acs_timestamp,
                                        record.getMessage(),
                                        rt_context,
                                        src_info,
                                        NO_AUDIENCE, "", "")
        elif record.levelname=='CRITICAL':
            self.logSvc.logCritical(acs_timestamp,
                                   record.getMessage(),
                                   rt_context,
                                   src_info,
                                   data)
        elif record.levelname=='ALERT':
            self.logSvc.logAlert(acs_timestamp,
                                 record.getMessage(),
                                 rt_context,
                                 src_info,
                                 data)
        elif record.levelname=='EMERGENCY':
            self.logSvc.logEmergency(acs_timestamp,
                                     record.getMessage(),
                                     rt_context,
                                     src_info,
                                     data)
        #failsafe
        else:
            self.logSvc.logCritical(acs_timestamp,
                                    record.getMessage(),
                                    rt_context,
                                    src_info,
                                    data)
    #--------------------------------------------------------------------------
    def getCORBALogger(self):
        '''
        If the CORBA logging service is up and running, returns a reference to
        it. Otherwise, returns None.
        '''
        #quick sanity check ensures we do not make a CORBA call to manager for
        #each and every log
        if self.logSvc is not None:
            return self.logSvc
        #CORBA logging service wasn't up yet. let's see if it's available now
        else:
            try:
                # Get log service ref via acsCORBA
                obj = acsLogSvc()
                if obj is None:
                    raise Exception("Logging service unavailable")
                self.logSvc = obj
            except:
                self.logSvc = None
                
        return self.logSvc
#------------------------------------------------------------------------------

    
class LogThrottle():
    '''
    Process level throttle for logs.
    '''
    
    def __init__(self, maxLogPerInterval):
        '''
        Initializes the Throttle
        
        Parameters:
            The max number of logs/s
        '''
        self.maxLogPerTimeInterval = maxLogPerInterval
        self.timeIntervalMillis = 1000
        self.intervalTimeStartMillis = time() * 1000;
        self.logCounter = 0;
        
    def configureLogging(self, maxLogPerInterval):
        self.maxLogPerTimeInterval = maxLogPerInterval
        
    def checkPublishLogRecord(self):
        '''
        Checks whether the log throttle allows logging a record. 
        No exception or other action beyond the returned boolean.
        
        Returns: True if a record can be logged, False otherwise.
        '''
        if(self.maxLogPerTimeInterval < 0):
            return True
        ctime = time() * 1000;
        
        if (ctime > self.intervalTimeStartMillis + self.timeIntervalMillis):
            self.intervalTimeStartMillis = time() * 1000
            self.logCounter = 0
        self.logCounter += 1
        return self.maxLogPerTimeInterval > self.logCounter
        