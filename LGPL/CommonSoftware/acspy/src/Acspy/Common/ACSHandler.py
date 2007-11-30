# @(#) $Id: ACSHandler.py,v 1.8 2007/11/30 23:52:45 agrimstrup Exp $
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
Contains the implementation of logging objects designed to send logs to the
(CORBA) ACS logging service.

TODO:
- Everything
'''

__revision__ = "$Id: ACSHandler.py,v 1.8 2007/11/30 23:52:45 agrimstrup Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from socket    import gethostname
from time      import gmtime
import logging
import logging.handlers
from os        import getpid
from os        import path
from os        import environ
import sys
from atexit    import register
#--ACS Imports-----------------------------------------------------------------
from Acspy.Util.ACSCorba     import acsLogSvc
from Acspy.Common.TimeHelper import TimeUtil
#--CORBA STUBS-----------------------------------------------------------------
import ACSLog
import CORBA
#--GLOBALS---------------------------------------------------------------------
#default logging cache size
DEFAULT_RECORD_CAPACITY = 10

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
        LOG_FILE_NAME = path.join(environ['ACSDATA'], 'tmp/acs_local_log')

LOG_FILE_NAME = LOG_FILE_NAME + "_" +  path.basename(sys.argv[0]) + "_" + str(getpid())
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
        formatString = "%(asctime)s.000 %(name)s %(message)s"
        #second parameter to Formatter defines the date format
        dateFormat = "%Y-%m-%dT%H:%M:%S"
        #call super's constructor
        logging.Formatter.__init__(self, formatString, dateFormat)
        #work with gmtime only
        self.converter = gmtime
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
    '''
    Constructor.

    Parameters: capcity - the size of the log cache.
    '''
    def __init__(self, capacity=DEFAULT_RECORD_CAPACITY):
        '''
        '''
        #ACS CORBA Logging Service Reference
        self.logSvc = None
        
        #call super's constructor
        logging.handlers.BufferingHandler.__init__(self, capacity)
        
        #setup a file handler to handle the extremely bad case that the 
        #CORBA logging service is down
        self.file_handler = None
        
        #try to get the corba logger now
        self.getCORBALogger()
        
        #we want to make sure the buffer is flushed
        #before exiting the Python interpreter
        register(self.flush)
        
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

        This method returns true if the superclass method of the same name
        returns true. Additionally, this method will empty the buffer if the
        superclass method returns true and the CORBA logging service is
        unavailable.
        '''
        #check the super class to see if it's OK to flush the record
        if logging.handlers.BufferingHandler.shouldFlush(self, record):
            #and also check to ensure the real logging service is up and running
            if self.getCORBALogger() != None:
                return 1
            
            #well it is possible that this cache will use up too much resources
            #and there's a chance the CORBA logger will never be available.
            #due to this fact, the buffer is sent to a file handler instead!
            else:
                for record in self.buffer:
                    self.flushToFile(record)
                
                self.buffer = []
                #OK to return true because the buffer is now empty
                return 1
        
        #for some reason or another it's not OK to flush the buffer.
        return 0
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
        if self.file_handler == None:
            #create the file handler on demand only
            self.initFileHandler()
            
        self.file_handler.handle(record)    
    #--------------------------------------------------------------------------
    def flush(self):
        '''
        Overridden
        '''
        for record in self.buffer:
            try:
                self.sendLog(record)
            except:
                self.flushToFile(record)

        self.buffer = []
    
    #--------------------------------------------------------------------------
    def sendLog(self, record):
        '''
        Method which sends logs to the real ACS logging service.
        '''
        # Extract the component and container name from the record name
        namelist = record.name.split('.')

        # Create an RTContext object
        rt_context = ACSLog.RTContext(str(record.thread).replace("<", "").replace(">", ""),
                                      str(namelist[0]).replace("<", "").replace(">", ""),
                                      str(gethostname()).replace("<", "").replace(">", ""),
                                      "",
                                      str(namelist[-1]).replace("<", "").replace(">", ""))

        src_info = ACSLog.SourceInfo(str(record.module).replace("<", "").replace(">", ""),
                                     "Unknown",
                                     long(record.lineno))
        
        #timestamp
        acs_timestamp = TimeUtil().py2epoch(record.created).value
        
        # Put remaining keyword arguments into NVPairSeq
        data = []

        if record.levelname=='TRACE':
            self.logSvc.logTrace(acs_timestamp,
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
            self.logSvc.logCritical(acs_timestamp,
                                   record.getMessage(),
                                   rt_context,
                                   src_info,
                                   data)
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
        if self.logSvc != None:
            return self.logSvc
        #CORBA logging service wasn't up yet. let's see if it's available now
        else:
            try:
                # Get log service ref via acsCORBA
                obj = acsLogSvc()
                if obj == None:
                    raise Exception("Logging service unavailable")
                self.logSvc = obj
            except:
                self.logSvc = None
                
        return self.logSvc
#------------------------------------------------------------------------------

    
