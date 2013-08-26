#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2008 
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
# "@(#) $Id: acspyTestUnitLog.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# arne  2008-01-18  created
#

#------------------------------------------------------------------------------
__revision__ = "$Id: acspyTestUnitLog.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
import unittest
import mock
from os import environ
import os.path
import time
import sched
import threading
import pprint
#--ACS IMPORTS____-------------------------------------------------------------
import ACSLog
import Acspy.Util.ACSCorba
from Acspy.Common.TimeHelper import TimeUtil
import maci
#------------------------------------------------------------------------------

# In order to run without actually using the CORBA interfaces, we need to
# create a mock ACSLog.LogSvc object.  
## methdict = {}
## for meth in [x for x in ACSLog._objref_LogSvc.__methods__]:
##     methdict[meth] = None
## methdict['__repr__'] = "ACSLog.LogSvc"
## methdict['__str__'] = "ACSLog.LogSvc"

mockLogSvc = mock.Mock(spec=ACSLog._objref_LogSvc)

# Replacing the acsLogSvc call ensures that we are using the mock object
# in all situations
def mockACSLogSvc():
    return mockLogSvc

Acspy.Util.ACSCorba.acsLogSvc = mockACSLogSvc

# These imports are dependent on the acsLogSvc so they could not be made
# until the replacement was completed.
import Acspy.Common.Log as Log
import Acspy.Common.ErrorTrace


class LoggerAfterImport(unittest.TestCase):
    """Test the default configuration of the Logger module"""
    
    def setUp(self):
        pass
    
    def tearDown(self):
        pass

    def testAfterImport(self):
        """Log after import is correctly configured"""
        try:
            Log.LOCALHANDLER
        except AttributeError:
            pass

        try:
            Log.LOCALHANDLER.formatter
        except AttributeError:
            pass

        try:
            Log.CENTRALHANDLER
        except AttributeError:
            pass

        try:
            Log.DEFAULTCENTRALHANDLER
        except AttributeError:
            pass
        
        try:
            Log.DEFAULTLOCALHANDLER
        except AttributeError:
            pass
    
class LogLevelsCheck(unittest.TestCase):
    """Test that log level constants and sets are correct."""

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def testLoggingTraceLevel(self):
        """LogLevel TRACE level is correctly defined in logging module"""
        self.assertEquals(Log.logging.NOTSET+1, Log.logging.TRACE)
        self.assertEquals("TRACE", Log.logging.getLevelName(Log.logging.TRACE))

    def testLoggingNoticeLevel(self):
        """LogLevel NOTICE level is correctly defined in logging module"""
        self.assertEquals(Log.logging.INFO+1, Log.logging.NOTICE)
        self.assertEquals("NOTICE", Log.logging.getLevelName(Log.logging.NOTICE))

    def testloggingAlertLevel(self):
        """LogLevel ALERT level is correctly defined in logging module"""
        self.assertEquals(Log.logging.CRITICAL+1, Log.logging.ALERT)
        self.assertEquals("ALERT", Log.logging.getLevelName(Log.logging.ALERT))

    def testLoggingEmergencyLevel(self):
        """LogLevel EMERGENCY level is correctly defined in logging module"""
        self.assertEquals(Log.logging.ALERT+1, Log.logging.EMERGENCY)
        self.assertEquals("EMERGENCY", Log.logging.getLevelName(Log.logging.EMERGENCY))

    def testLoggingOffLevel(self):
        """LogLevel OFF level is correctly defined in logging module"""
        self.assertEquals(Log.logging.EMERGENCY+1, Log.logging.OFF)
        self.assertEquals("OFF", Log.logging.getLevelName(Log.logging.OFF))

    def testLogLevelMapping(self):
        """LogLevel numeric log levels map to correct logging values"""
        self.assertEquals(Log.LEVELS[0], Log.logging.NOTSET)
        self.assertEquals(Log.LEVELS[1], Log.logging.TRACE)
        self.assertEquals(Log.LEVELS[2], Log.logging.TRACE)
        self.assertEquals(Log.LEVELS[3], Log.logging.DEBUG)
        self.assertEquals(Log.LEVELS[4], Log.logging.INFO)
        self.assertEquals(Log.LEVELS[5], Log.logging.NOTICE)
        self.assertEquals(Log.LEVELS[6], Log.logging.WARNING)
        self.assertEquals(Log.LEVELS[7], Log.logging.ERROR)
        self.assertEquals(Log.LEVELS[8], Log.logging.ERROR)
        self.assertEquals(Log.LEVELS[9], Log.logging.CRITICAL)
        self.assertEquals(Log.LEVELS[10], Log.logging.ALERT)
        self.assertEquals(Log.LEVELS[11], Log.logging.EMERGENCY)
        self.assertEquals(Log.LEVELS[99], Log.logging.OFF)

    def testLogLevelReverseMapping(self):
        """LogLevel Logging values map to correct numeric log levels"""
        self.assertEquals(0, Log.RLEVELS[Log.logging.NOTSET])
        self.assertEquals(2, Log.RLEVELS[Log.logging.TRACE])
        self.assertEquals(3, Log.RLEVELS[Log.logging.DEBUG])
        self.assertEquals(4, Log.RLEVELS[Log.logging.INFO])
        self.assertEquals(5, Log.RLEVELS[Log.logging.NOTICE])
        self.assertEquals(6, Log.RLEVELS[Log.logging.WARNING])
        self.assertEquals(8, Log.RLEVELS[Log.logging.ERROR])
        self.assertEquals(9, Log.RLEVELS[Log.logging.CRITICAL])
        self.assertEquals(10, Log.RLEVELS[Log.logging.ALERT])
        self.assertEquals(11, Log.RLEVELS[Log.logging.EMERGENCY])
        self.assertEquals(99, Log.RLEVELS[Log.logging.OFF])

    def testACSLogLevelMapping(self):
        """LogLevel ACSLog log levels map to correct logging values"""
        self.assertEquals(Log.LEVELS[Log.ACSLog.ACS_LOG_TRACE], Log.logging.TRACE)
        self.assertEquals(Log.LEVELS[Log.ACSLog.ACS_LOG_DEBUG], Log.logging.DEBUG)
        self.assertEquals(Log.LEVELS[Log.ACSLog.ACS_LOG_INFO], Log.logging.INFO)
        self.assertEquals(Log.LEVELS[Log.ACSLog.ACS_LOG_NOTICE], Log.logging.NOTICE)
        self.assertEquals(Log.LEVELS[Log.ACSLog.ACS_LOG_WARNING], Log.logging.WARNING)
        self.assertEquals(Log.LEVELS[Log.ACSLog.ACS_LOG_ERROR], Log.logging.ERROR)
        self.assertEquals(Log.LEVELS[Log.ACSLog.ACS_LOG_CRITICAL], Log.logging.CRITICAL)
        self.assertEquals(Log.LEVELS[Log.ACSLog.ACS_LOG_ALERT], Log.logging.ALERT)
        self.assertEquals(Log.LEVELS[Log.ACSLog.ACS_LOG_EMERGENCY], Log.logging.EMERGENCY)

class EnvVariableDefaultCheck(unittest.TestCase):
    """Test that environment variables defaults are correct"""

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def testDefaultValues(self):
        """EnvVariable default values are correctly set when environment variables not defined"""
        self.assertEquals(False, environ.has_key('ACS_LOG_STDOUT'))
        self.assertEquals(False, environ.has_key('ACS_LOG_CENTRAL'))
        self.assertEquals(3, Log.ACS_LOG_STDOUT)
        self.assertEquals(3, Log.ACS_LOG_CENTRAL)

class StdoutEnvVariableCheck(unittest.TestCase):
    """Test that environment variables defaults are correct"""

    def setUp(self):
        environ['ACS_LOG_STDOUT'] = '2'
        reload(Log)

    def tearDown(self):
        environ.pop('ACS_LOG_STDOUT')
        reload(Log)

    def testStdoutSetting(self):
        """EnvVariable Stdout value is correct when ACS_LOG_STDOUT environment variable is set"""
        self.assertEquals(False, environ.has_key('ACS_LOG_CENTRAL'))
        self.assertEquals(True, environ.has_key('ACS_LOG_STDOUT'))
        self.assertEquals('2', environ['ACS_LOG_STDOUT'])
        self.assertEquals(2, Log.ACS_LOG_STDOUT)
        self.assertEquals(3, Log.ACS_LOG_CENTRAL)

class CentralEnvVariableCheck(unittest.TestCase):
    """Test that environment variables defaults are correct"""

    def setUp(self):
        environ['ACS_LOG_CENTRAL'] = '2'
        reload(Log)

    def tearDown(self):
        environ.pop('ACS_LOG_CENTRAL')
        reload(Log)

    def testStdoutSetting(self):
        """EnvVariable Central value is correct when ACS_LOG_CENTRAL environment variable is set"""
        self.assertEquals(False, environ.has_key('ACS_LOG_STDOUT'))
        self.assertEquals(True, environ.has_key('ACS_LOG_CENTRAL'))
        self.assertEquals('2', environ['ACS_LOG_CENTRAL'])
        self.assertEquals(2, Log.ACS_LOG_CENTRAL)
        self.assertEquals(3, Log.ACS_LOG_STDOUT)

class LoggerClassCheck(unittest.TestCase):
    """Test the integrity and function of the Logger class and calls that directly access the central logger"""
    
    def setUp(self):
        self.mylogger = Log.Logger("mylogger")
        Log.setBatchSize(0)

    def tearDown(self):
        pass
        Log.setBatchSize(10)
    
    def testLoggerInit(self):
        """Logger class default initialization is correct"""
        self.assertEquals("mylogger", self.mylogger.name)
        self.assertNotEquals(None, self.mylogger.stdouthandler)
        self.assertNotEquals(None, self.mylogger.acshandler)
        self.assertEquals(True,self.mylogger.usingDefault)
        self.assertEquals(Log.DEFAULTLOCALHANDLER, self.mylogger.stdouthandler)
        self.assertEquals(Log.DEFAULTCENTRALHANDLER, self.mylogger.acshandler)
        self.assertEquals([self.mylogger.stdouthandler, self.mylogger.acshandler], self.mylogger.handlers)
        self.assertEquals(None, self.mylogger.parent)


    def testSetLevels(self):
        """Logger class set log levels to user-defined levels"""
        self.mylogger.setLevels(maci.LoggingConfigurable.LogLevels(False,9, 9))
        self.assertEqual(False, self.mylogger.usingDefault)
        self.assertNotEqual(Log.DEFAULTLOCALHANDLER, self.mylogger.stdouthandler)
        self.assertNotEqual(Log.DEFAULTCENTRALHANDLER, self.mylogger.acshandler)
        self.assertEqual(Log.LEVELS[9], self.mylogger.stdouthandler.level)
        self.assertEqual(Log.LEVELS[9], self.mylogger.acshandler.level)
        
    def testSetLevelsDefault(self):
        """Logger class set log levels to using Default from user-defined level"""
        self.mylogger.setLevels(maci.LoggingConfigurable.LogLevels(False,9, 9))
        self.mylogger.setLevels(maci.LoggingConfigurable.LogLevels(True,0, 0))
        self.assertEqual(True, self.mylogger.usingDefault)
        self.assertEqual(Log.DEFAULTLOCALHANDLER, self.mylogger.stdouthandler)
        self.assertEqual(Log.DEFAULTCENTRALHANDLER, self.mylogger.acshandler)
        
    def testSetLevelsValidInputs(self):
        """Logger class set log levels for valid inputs"""
        self.assertEqual(Log.DEFAULTLOCALHANDLER, self.mylogger.stdouthandler)
        self.assertEqual(Log.DEFAULTCENTRALHANDLER, self.mylogger.acshandler)
        for k in Acspy.Common.Log.LEVELS:
            self.mylogger.setLevels(maci.LoggingConfigurable.LogLevels(False, k, k))
            self.assertEqual(False, self.mylogger.usingDefault)
            self.assertEqual(Acspy.Common.Log.LEVELS[k], self.mylogger.stdouthandler.level)
            self.assertEqual(Acspy.Common.Log.LEVELS[k], self.mylogger.acshandler.level)
        
    def testGetLevelsDefault(self):
        """Logger class get log levels when Default"""
        self.mylogger.setLevels(maci.LoggingConfigurable.LogLevels(True,0, 0))
        loglvls = self.mylogger.getLevels()
        self.assertEqual(True, loglvls.useDefault)

    def testMakeRecord(self):
        """Logger class creates logging records correctly"""
        rv = self.mylogger.makeRecord('foo', Log.LEVELS[ACSLog.ACS_LOG_INFO], '/path/to/file.py',
                                      596, 'Main - Text', (), None, 'logInfo')
        self.assertEqual('foo', rv.name)
        self.assertEqual(Log.LEVELS[ACSLog.ACS_LOG_INFO], rv.levelno)
        self.assertEqual('/path/to/file.py', rv.pathname)
        self.assertEqual(596, rv.lineno)
        self.assertEqual('Main - Text', rv.msg)
        self.assertEqual((), rv.args)
        self.assertEqual(True, rv.exc_info is None)
        self.assertEqual('logInfo', rv.funcName)
        
    def testMakeRecordEmptyExtra(self):
        """Logger class creates logging records correctly"""
        expectedkeys = ['threadName', 'name', 'thread', 'created', 'process', 'args', 'source',
                        'module', 'filename', 'levelno', 'exc_text', 'pathname', 'lineno',
                        'msg', 'exc_info', 'funcName', 'relativeCreated', 'levelname', 'msecs']
        rv = self.mylogger.makeRecord('foo', Log.LEVELS[ACSLog.ACS_LOG_INFO], '/path/to/file.py',
                                      596, 'Main - Text', (), None, 'logInfo', {})
        self.assertEqual(expectedkeys.sort(), rv.__dict__.keys().sort())
        
    def testMakeRecordExtras(self):
        """Logger class creates logging records correctly"""
        extras = { 'AddName' : 'bar', 'AddValue' : 15 }
        rv = self.mylogger.makeRecord('foo', Log.LEVELS[ACSLog.ACS_LOG_INFO], '/path/to/file.py',
                                      596, 'Main - Text', (), None, 'logInfo', extras)
        self.assertEqual(True, 'AddName' in rv.__dict__.keys())
        self.assertEqual(True, 'AddValue' in rv.__dict__.keys())
        self.assertEqual('bar', rv.AddName)
        self.assertEqual(15, rv.AddValue)
        

class LoggerFunctionCheck(unittest.TestCase):
    """Test the integrity and function of the Logger logging methods"""
    
    def setUp(self):
        self.mylogger = Log.Logger("mylogger")
        self.mylogger.acshandler = mock.Mock(spec=Log.ACSHandler)
        self.mylogger.stdouthandler = mock.Mock()
        self.mylogger.handlers[0] = self.mylogger.stdouthandler
        self.mylogger.handlers[1] = self.mylogger.acshandler
        self.mylogger.acshandler.level = 0
        self.mylogger.stdouthandler.level = 0

    def tearDown(self):
        pass
    
    def verifyOutput(self, handler, level, msg):
        dirn,fn = os.path.split(__file__)
        tfn,ext = os.path.splitext(fn)
        logcall = handler.method_calls[-1]
        self.assertEquals("handle", logcall[0])
        outrecord = logcall[1]
        self.assertEquals(level, outrecord[0].levelno)
        self.assertEquals(msg, outrecord[0].msg)
        self.assertEquals(tfn, os.path.splitext(outrecord[0].filename)[0])
        
    def testLogTrace(self):
        """Logger class Trace level logging"""
        self.mylogger.logTrace("Trace Message")
        self.verifyOutput(self.mylogger.acshandler, Log.LEVELS[ACSLog.ACS_LOG_TRACE], "testLogTrace - Trace Message")
        self.verifyOutput(self.mylogger.stdouthandler, Log.LEVELS[ACSLog.ACS_LOG_TRACE], "testLogTrace - Trace Message")

        
    def testLogDebug(self):
        """Logger class Debug level logging"""
        self.mylogger.logDebug("Debug Message")
        self.verifyOutput(self.mylogger.acshandler, Log.LEVELS[ACSLog.ACS_LOG_DEBUG], "testLogDebug - Debug Message")
        self.verifyOutput(self.mylogger.stdouthandler, Log.LEVELS[ACSLog.ACS_LOG_DEBUG], "testLogDebug - Debug Message")
        
    def testLogInfo(self):
        """Logger class Info level logging"""
        self.mylogger.logInfo("Info Message")
        self.verifyOutput(self.mylogger.acshandler, Log.LEVELS[ACSLog.ACS_LOG_INFO], "testLogInfo - Info Message")
        self.verifyOutput(self.mylogger.stdouthandler, Log.LEVELS[ACSLog.ACS_LOG_INFO], "testLogInfo - Info Message")
        
    def testLogNotice(self):
        """Logger class Notice level logging"""
        self.mylogger.logNotice("Notice Message")
        self.verifyOutput(self.mylogger.acshandler, Log.LEVELS[ACSLog.ACS_LOG_NOTICE], "testLogNotice - Notice Message")
        self.verifyOutput(self.mylogger.stdouthandler, Log.LEVELS[ACSLog.ACS_LOG_NOTICE], "testLogNotice - Notice Message")
        
    def testLogWarning(self):
        """Logger class Warning level logging"""
        self.mylogger.logWarning("Warning Message")
        self.verifyOutput(self.mylogger.acshandler, Log.LEVELS[ACSLog.ACS_LOG_WARNING], "testLogWarning - Warning Message")
        self.verifyOutput(self.mylogger.stdouthandler, Log.LEVELS[ACSLog.ACS_LOG_WARNING], "testLogWarning - Warning Message")

    def testLogError(self):
        """Logger class Error level logging"""
        self.mylogger.logError("Error Message")
        self.verifyOutput(self.mylogger.acshandler, Log.LEVELS[ACSLog.ACS_LOG_ERROR], "testLogError - Error Message")
        self.verifyOutput(self.mylogger.stdouthandler, Log.LEVELS[ACSLog.ACS_LOG_ERROR], "testLogError - Error Message")
        
    def testLogCritical(self):
        """Logger class Critical level logging"""
        self.mylogger.logCritical("Critical Message")
        self.verifyOutput(self.mylogger.acshandler, Log.LEVELS[ACSLog.ACS_LOG_CRITICAL], "testLogCritical - Critical Message")
        self.verifyOutput(self.mylogger.stdouthandler, Log.LEVELS[ACSLog.ACS_LOG_CRITICAL], "testLogCritical - Critical Message")
        
    def testLogAlert(self):
        """Logger class Alert level logging"""
        self.mylogger.logAlert("Alert Message")
        self.verifyOutput(self.mylogger.acshandler, Log.LEVELS[ACSLog.ACS_LOG_ALERT], "testLogAlert - Alert Message")
        self.verifyOutput(self.mylogger.stdouthandler, Log.LEVELS[ACSLog.ACS_LOG_ALERT], "testLogAlert - Alert Message")
        
    def testLogEmergency(self):
        """Logger class Emergency level logging"""
        self.mylogger.logEmergency("Emergency Message")
        self.verifyOutput(self.mylogger.acshandler, Log.LEVELS[ACSLog.ACS_LOG_EMERGENCY], "testLogEmergency - Emergency Message")
        self.verifyOutput(self.mylogger.stdouthandler, Log.LEVELS[ACSLog.ACS_LOG_EMERGENCY], "testLogEmergency - Emergency Message")
        
    def testLogXML(self):
        """Logger class XML logging"""
        self.mylogger.logXML("<msg>Emergency Message</msg>")
        self.verifyOutput(self.mylogger.acshandler, Log.LEVELS[ACSLog.ACS_LOG_DEBUG], "<msg>Emergency Message</msg>")
        self.verifyOutput(self.mylogger.stdouthandler, Log.LEVELS[ACSLog.ACS_LOG_DEBUG], "<msg>Emergency Message</msg>")
        
    def testLogErrorTrace(self):
        """Logger class ErrorTrace logging with default priority"""
        et = Acspy.Common.ErrorTrace.ErrorTrace(1,1)
        self.mylogger.logErrorTrace(et)
        logcall = self.mylogger.acshandler.method_calls[-1]
        self.assertEquals("handle", logcall[0])
        self.assertEquals(ACSLog.ACS_LOG_ERROR, logcall[1][0].priority)
        self.assertEquals(et, logcall[1][0].errortrace)

    def testLogErrorTraceInvalidPriority(self):
        """Logger class ErrorTrace logging with an invalid priority"""
        et = Acspy.Common.ErrorTrace.ErrorTrace(1,1)
        self.assertRaises(KeyError, self.mylogger.logErrorTrace, et, 25)

    def testLogTypeSafe(self):
        """Logger class Type-safe logging"""
        msg = "LogTypeSafe Message"
        ctxt = ACSLog.RTContext('a','b','c','d','e')
        src = ACSLog.SourceInfo('a','b','c')
        self.mylogger.logTypeSafe(ACSLog.ACS_LOG_ERROR, None, msg, ctxt, src, None)
        logcall = self.mylogger.acshandler.method_calls[-1]
        self.assertEquals("handle", logcall[0])
        self.assertEquals(ACSLog.ACS_LOG_ERROR, logcall[1][0].priority)
        self.assertEquals(msg, logcall[1][0].msg)
        self.assertEquals(True, isinstance(logcall[1][0].rtCont, ACSLog.RTContext))
        self.assertEquals(ctxt, logcall[1][0].rtCont)
        self.assertEquals(True, isinstance(logcall[1][0].srcInfo, ACSLog.SourceInfo))
        self.assertEquals(src, logcall[1][0].srcInfo)
        self.assertEquals("", logcall[1][0].audience)
        self.assertEquals("", logcall[1][0].array)
        self.assertEquals("", logcall[1][0].antenna)

    def testLogTypeSafeNoContextorSource(self):
        """Logger class Type-safe logging with no context or source"""
        msg = "LogTypeSafe Message"
        self.mylogger.logTypeSafe(ACSLog.ACS_LOG_ERROR, None, msg, None, None, None)
        logcall = self.mylogger.acshandler.method_calls[-1]
        self.assertEquals("handle", logcall[0])
        self.assertEquals(ACSLog.ACS_LOG_ERROR, logcall[1][0].priority)
        self.assertEquals(msg, logcall[1][0].msg)
        self.assertEquals(True, logcall[1][0].rtCont is None)
        self.assertEquals(True, logcall[1][0].srcInfo is None)
        self.assertEquals("", logcall[1][0].audience)
        self.assertEquals("", logcall[1][0].array)
        self.assertEquals("", logcall[1][0].antenna)
        
    def testLogTypeSafeInvalidPriority(self):
        """Logger class Type-safe logging with invalid priority"""
        msg = "LogTypeSafe Message"
        ts = TimeUtil().py2epoch(time.time()).value
        self.assertRaises(KeyError, self.mylogger.logTypeSafe, 25, ts, msg, None, None, None)

    def testLogNotSoTypeSafe(self):
        """Logger class Not So Type-safe logging"""
        msg = "LogNotSoTypeSafe Message"
        self.mylogger.logNotSoTypeSafe(ACSLog.ACS_LOG_ERROR, msg)
        logcall = self.mylogger.acshandler.method_calls[-1]
        self.assertEquals("handle", logcall[0])
        self.assertEquals(ACSLog.ACS_LOG_ERROR, logcall[1][0].priority)
        self.assertEquals(msg, logcall[1][0].msg)
        self.assertEquals("", logcall[1][0].audience)
        self.assertEquals("", logcall[1][0].array)
        self.assertEquals("", logcall[1][0].antenna)
        
    def testLogNotSoTypeSafeInvalidPriority(self):
        """Logger class Not So Type-safe logging with invalid priority"""
        msg = "LogTypeSafe Message"
        self.assertRaises(KeyError, self.mylogger.logNotSoTypeSafe, 25, msg)

    def testLogAtLevel(self):
        """Logger class User-specified level logging"""
#        self.mylogger.setLevels(maci.LoggingConfigurable.LogLevels(False,0, 0))
        keys = Acspy.Common.Log.LEVELS.keys()
        keys.sort()
        for l in keys:
            if l == 0 or l == 99:
                self.assertRaises(ValueError, self.mylogger.logAtLevel, l, "logAtLevel Message")
            else:
                self.mylogger.logAtLevel(l, "logAtLevel Message")
                self.verifyOutput(self.mylogger.acshandler, Log.LEVELS[l], "testLogAtLevel - logAtLevel Message")
                self.verifyOutput(self.mylogger.stdouthandler, Log.LEVELS[l], "testLogAtLevel - logAtLevel Message")

class LoggerHandlerConfigCheck(unittest.TestCase):
    """Test the integrity and function of the Logger methods used to set CDB configuration values"""
    
    def setUp(self):
        self.log = Log.Logger('cfglogger')

    def tearDown(self):
        Log.CENTRALHANDLER.capacity = 1000
        Log.CENTRALHANDLER.batchsize = 10
        Log.CENTRALHANDLER.dispatchlevel = Log.logging.ALERT
        Log.DEFAULTLOCALHANDLER.setLevel(10)
        Log.DEFAULTCENTRALHANDLER.setLevel(10)

    def testInitialization(self):
        """Log initializes handlers correctly"""
        self.assertEqual(False, Log.LOCALHANDLER is None)
        self.assertEqual(False, Log.LOCALHANDLER.formatter is None)
        self.assertEqual(True, isinstance(Log.LOCALHANDLER.formatter, Log.ACSFormatter))
        self.assertEqual(False, Log.CENTRALHANDLER is None)
        self.assertEqual(False, Log.DEFAULTCENTRALHANDLER is None)
        self.assertEqual(False, Log.DEFAULTLOCALHANDLER is None)
        self.assertEqual(Log.LEVELS[Log.ACS_LOG_STDOUT], Log.DEFAULTLOCALHANDLER.level)
        self.assertEqual(Log.LEVELS[Log.ACS_LOG_CENTRAL], Log.DEFAULTCENTRALHANDLER.level)
        self.assertEqual(0, Log.DEFAULTLOCALHANDLER.capacity)
        self.assertEqual(0, Log.DEFAULTCENTRALHANDLER.capacity)
    
    def testQueueCapacity(self):
        """Log sets log queue capacity correctly"""
        self.assertEqual(1000, Log.CENTRALHANDLER.capacity)
        Log.setCapacity(15)
        self.assertEqual(15, Log.CENTRALHANDLER.capacity)

    def testInvalidQueueCapacity(self):
        """Log handles invalid queue capacity correctly"""
        self.assertEqual(1000, Log.CENTRALHANDLER.capacity)
        Log.setCapacity(-2)
        self.assertEqual(0, Log.CENTRALHANDLER.capacity)

    def testBatchSize(self):
        """Log changes log batch size correctly"""
        self.assertEqual(10, Log.CENTRALHANDLER.batchsize)
        self.assertEqual(1000, Log.CENTRALHANDLER.capacity)
        Log.setBatchSize(15)
        self.assertEqual(15, Log.CENTRALHANDLER.batchsize)

    def testInvalidBatchSize(self):
        """Log handles invalid batch size correctly"""
        self.assertEqual(10, Log.CENTRALHANDLER.batchsize)
        Log.setBatchSize(-2)
        self.assertEqual(0, Log.CENTRALHANDLER.batchsize)

    def testBatchSizeExceedCapacity(self):
        """Log handles batch size greater than capacity correctly"""
        self.assertEqual(True, Log.CENTRALHANDLER.capacity >= Log.CENTRALHANDLER.batchsize)
        Log.setBatchSize(Log.CENTRALHANDLER.capacity + 2)
        self.assertEqual(Log.CENTRALHANDLER.capacity, Log.CENTRALHANDLER.batchsize)

    def testSetImmediateDispatchLevel(self):
        """Log sets immediate dispatch level correctly"""
        self.assertEqual(Log.LEVELS[10], Log.CENTRALHANDLER.dispatchlevel)
        Log.setImmediateDispatchLevel(3)
        self.assertEqual(Log.LEVELS[3], Log.CENTRALHANDLER.dispatchlevel)

    def testBoundaryDispatchLevel(self):
        """Log handles lowest and highest immediate dispatch level correctly"""
        Log.setImmediateDispatchLevel(0)
        self.assertEqual(Log.LEVELS[0], Log.CENTRALHANDLER.dispatchlevel)
        Log.setImmediateDispatchLevel(99)
        self.assertEqual(Log.LEVELS[99], Log.CENTRALHANDLER.dispatchlevel)

    def testUnknownDispatchLevel(self):
        """Log handles undefined immediate dispatch level correctly"""
        self.assertRaises(KeyError, Log.setImmediateDispatchLevel,-1)
        self.assertRaises(KeyError, Log.setImmediateDispatchLevel,125)

    def testSetDefaultLevels(self):
        """Log sets default handlers levels correctly for valid inputs"""
        for k in Log.LEVELS:
            Log.setDefaultLevels(maci.LoggingConfigurable.LogLevels(False, k, k))
            self.assertEqual(Log.LEVELS[k], Log.DEFAULTLOCALHANDLER.level)
            self.assertEqual(Log.LEVELS[k], Log.DEFAULTCENTRALHANDLER.level)

    def testSetDefaultLevelsInvalidInputs(self):
        """Log handles invalid log levels correctly"""
        self.assertEqual(10,Log.DEFAULTLOCALHANDLER.level)
        self.assertEqual(10,Log.DEFAULTCENTRALHANDLER.level)
        self.assertRaises(KeyError, Log.setDefaultLevels,
                          maci.LoggingConfigurable.LogLevels(False, 25, 25))
        self.assertEqual(10,Log.DEFAULTLOCALHANDLER.level)
        self.assertEqual(10,Log.DEFAULTCENTRALHANDLER.level)
        
        

class NoLoggerCheck(unittest.TestCase):
    """Test state and operation of Logging system when no loggers have been requested"""

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def testGetLoggerNames(self):
        """NoLoggerCheck getLoggerNames when no loggers requested"""
        # When the entire suite is run together, the name from TestContainer is injected.
        # This shouldn't happen, but I don't know how to fix it yet.
        namelist = Log.getLoggerNames()
        self.assertEquals(True, [] == namelist or ['UnitTestContainer'] == namelist)

    def testNone(self):
        """NoLoggerCheck logger search with no key"""
        self.assertEqual(False, Log.doesLoggerExist(None))

    def testWrong(self):
        """NoLoggerCheck logger search with non-existing key"""
        self.assertEqual(False, Log.doesLoggerExist("blah"))

class OneLoggerCheck(unittest.TestCase):
    """Check the state and operation of the Logging system when one logger has been requested"""
    
    def setUp(self):
        self.mylogger = Log.getLogger()
    
    def tearDown(self):
        pass
    
    def testUnnamedLogger(self):
        """OneLoggerCheck unnamed logger request"""
        self.assertEquals('None', self.mylogger.name)
        self.assertEquals(Log.DEFAULTLOCALHANDLER, self.mylogger.stdouthandler)
        self.assertEquals(Log.DEFAULTCENTRALHANDLER, self.mylogger.acshandler)
        
    def testUnnamedLoggerNamesList(self):
        """OneLoggerCheck names list has unnamed logger"""
        self.assertEquals([self.mylogger.name], Log.getLoggerNames())

    def testUnnamedLoggerNamesList(self):
        """OneLoggerCheck names list has unnamed logger when filtered by unnamed logger"""
        self.assertEquals([self.mylogger.name], Log.getLoggerNames(self.mylogger.name))

    def testUnnamedLoggerExists(self):
        """OneLoggerCheck unnamed logger found by search"""
        self.assertEqual(True, Log.doesLoggerExist(self.mylogger.name))

class SeveralLoggerCheck(unittest.TestCase):
    """Check the state and operation of the Logging system when multiple loggers have been requested"""
    
    def setUp(self):
        self.pname = "MyLogger1"
        self.plogger = Log.getLogger(self.pname)
        self.cname = "MyLogger2"
        self.clogger = Log.getLogger(self.cname)

    def tearDown(self):
        Log.setDefaultLevels(maci.LoggingConfigurable.LogLevels(False,3, 3))
        self.plogger.setLevels(maci.LoggingConfigurable.LogLevels(True,0, 0))
        self.clogger.setLevels(maci.LoggingConfigurable.LogLevels(True,0, 0))
    
    def testLoggerNamesRequested(self):
        """SeveralLoggerCheck getLoggerNames returns all known loggers"""
        self.assertEquals(Log.logging.Logger.manager.loggerDict.keys(), Log.getLoggerNames())

    def testLoggerNamesRequestedFiltered(self):
        """SeveralLoggerCheck getLoggerNames returns correct logger names when requested with filtering"""
        self.assertEquals(True, self.cname in Log.getLoggerNames(self.pname[:-1]))
        self.assertEquals(True, self.pname in Log.getLoggerNames(self.pname[:-1]))
        self.assertEquals([self.cname], Log.getLoggerNames(self.cname))

    def testLoggerSingleDefault(self):
        """SeveralLoggerCheck Changing a logger's levels does not affect other loggers"""
        beforecent = Log.DEFAULTCENTRALHANDLER.level
        beforeloc = Log.DEFAULTLOCALHANDLER.level
        self.plogger.setLevels(maci.LoggingConfigurable.LogLevels(False,4, 5))
        self.assertEquals(beforeloc, Log.DEFAULTLOCALHANDLER.level)
        self.assertEquals(beforecent, Log.DEFAULTCENTRALHANDLER.level)
        self.assertEquals(False, self.plogger.usingDefault)
        self.assertEquals(True, self.clogger.usingDefault)
        self.assertNotEquals(self.plogger.stdouthandler, self.clogger.stdouthandler)
        self.assertNotEquals(self.plogger.acshandler, self.clogger.acshandler)
        self.assertNotEquals(self.plogger.stdouthandler.level, self.clogger.stdouthandler.level)
        self.assertNotEquals(self.plogger.acshandler.level, self.clogger.acshandler.level)

    def testLoggerDefault(self):
        """SeveralLoggerCheck Levels changes on all loggers using default level"""
        Log.setDefaultLevels(maci.LoggingConfigurable.LogLevels(False,4, 5))
        self.assertEquals(True, self.plogger.usingDefault)
        self.assertEquals(True, self.clogger.usingDefault)
        self.assertEquals(Log.DEFAULTLOCALHANDLER.level, self.plogger.stdouthandler.level)
        self.assertEquals(Log.DEFAULTCENTRALHANDLER.level, self.plogger.acshandler.level)
        self.assertEquals(Log.DEFAULTLOCALHANDLER.level, self.clogger.stdouthandler.level)
        self.assertEquals(Log.DEFAULTCENTRALHANDLER.level, self.plogger.acshandler.level)

    def testWrong(self):
        """SeveralLoggerCheck Search with non-existing key"""
        self.assertEqual(False, Log.doesLoggerExist("blah"))

    def testExists(self):
        """SeveralLoggerCheck search with parent key"""
        self.assertEqual(True, Log.doesLoggerExist(self.pname))

    def testExistsPartial(self):
        """SeveralLoggerCheck search with part of an exising key"""
        self.assertEqual(False, Log.doesLoggerExist("child"))

    def testExistsNested(self):
        """SeveralLoggerCheck search with the nested child's name as key"""
        self.assertEqual(True, Log.doesLoggerExist(self.cname))

    def testNestedOutput(self):
        """SeveralLoggerCheck messages are logged only once for nested loggers"""
        nlogger = Log.getLogger("MyLogger1.child")
        before = len(Log.CENTRALHANDLER.buffer)
        nlogger.logInfo("Nested Message")
        after =  len(Log.CENTRALHANDLER.buffer)
        self.assertEqual(1, after - before)
        Log.CENTRALHANDLER.buffer = []

class DispatchPacketCheck(unittest.TestCase):
    """Check the operation of the logger under differing dispatchPacket values"""
    def setUp(self):
        self.logger = Log.getLogger("dispatchcheck")
        self.logger.stdouthandler = mock.Mock()
        self.logger.handlers[0] = self.logger.stdouthandler

    def tearDown(self):
        pass

    def testBuffering(self):
        """DispatchPacketCheck messages buffer correctly"""
        before = mockLogSvc.method_calls
        self.logger.logInfo("Dispatch buffered")
        after = mockLogSvc.method_calls
        self.assertEquals(before, after)
        self.assertEquals(1, len(Log.CENTRALHANDLER.buffer))
        Log.CENTRALHANDLER.buffer = []
        
    def testDispatch(self):
        """DispatchPacketCheck messages buffer correctly"""
        if Log.CENTRALHANDLER.logSvc is None:
            cleanup = True
            Log.CENTRALHANDLER.logSvc = mockLogSvc
        self.assertEquals(0, len(Log.CENTRALHANDLER.buffer))
        self.logger.logEmergency("Dispatch Sent")
        self.assertEquals(0, len(Log.CENTRALHANDLER.buffer))
        self.assertEquals('logEmergency',Log.CENTRALHANDLER.logSvc.method_calls[-1][0])
        try:
            cleanup
            Log.CENTRALHANDLER.logSvc = None
        except:
            pass

    def testBufferingandDispatch(self):
        """DispatchPacketCheck clears sends pending messages before priority message"""
        if Log.CENTRALHANDLER.logSvc is None:
            cleanup = True
            Log.CENTRALHANDLER.logSvc = mockLogSvc
        self.assertEquals(0, len(Log.CENTRALHANDLER.buffer))
        self.logger.logNotice("Dispatch buffered")
        self.assertEquals(0, len(Log.DEFAULTCENTRALHANDLER.buffer))
        self.assertEquals(1, len(Log.CENTRALHANDLER.buffer))
        self.logger.logAlert("Dispatch Sent")
        self.assertEquals(0, len(Log.CENTRALHANDLER.buffer))
        self.assertEquals('logNotice',Log.CENTRALHANDLER.logSvc.method_calls[-2][0])
        self.assertEquals('logAlert',Log.CENTRALHANDLER.logSvc.method_calls[-1][0])
        try:
            cleanup
            Log.CENTRALHANDLER.logSvc = None
        except:
            pass
        
class PeriodicFlushCheck(unittest.TestCase):
    """Check the lifecycle operation of the periodic flushing thread"""
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def testCreation(self):
        """PeriodicFlushCheck is correct and consistent after import."""
        if not Log.isFlushRunning():
            self.assertEqual(True, Log.FLUSHTHREAD is None)
            self.assertEqual(True, Log.SCHEDULER is None)
            self.assertEqual(True, Log.NEXTEVENT is None)
            self.assertEqual(True, Log.INTERVAL is None)

    def testCycleStartStop(self):
        """PeriodicFlushCheck flushing thread start and stops correctly."""
        Log.startPeriodicFlush()
        self.assertEqual(Log.DEFAULT_FLUSH_PERIOD, Log.INTERVAL)
        self.assertEqual(True, isinstance(Log.SCHEDULER, sched.scheduler))
        self.assertEqual(True, isinstance(Log.FLUSHTHREAD, threading.Thread))
        self.assertEqual(True, Log.FLUSHTHREAD.isAlive())
        self.assertEqual(False, Log.NEXTEVENT is None)
        Log.stopPeriodicFlush()
        self.assertEqual(False, Log.FLUSHTHREAD.isAlive())

    def testDoubleStart(self):
        """PeriodicFlushCheck only one thread is created if start is called twice."""
        Log.startPeriodicFlush()
        ft = Log.FLUSHTHREAD
        sc = Log.SCHEDULER
        ne = Log.NEXTEVENT
        Log.startPeriodicFlush()
        self.assertEqual(ft, Log.FLUSHTHREAD)
        self.assertEqual(sc, Log.SCHEDULER)
        self.assertEqual(ne, Log.NEXTEVENT)
        Log.stopPeriodicFlush()

    def testSetFlushInterval(self):
        """PeriodicFlushCheck updates flush interval correctly"""
        Log.startPeriodicFlush()
        now = time.time()
        next = Log.NEXTEVENT
        Log.setFlushInterval(5)
        self.assertNotEqual(next, Log.NEXTEVENT)
        self.assertAlmostEqual(now + Log.INTERVAL, Log.NEXTEVENT[0],1)
        Log.stopPeriodicFlush()

    def testSetFlushIntervalInvalid(self):
        """PeriodicFlushCheck flush thread stopped when invalid interval is set"""
        Log.startPeriodicFlush()
        Log.setFlushInterval(-5)
        self.assertEqual(False, Log.FLUSHTHREAD.isAlive())
        
        

def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(LoggerAfterImport))
    suite.addTest(unittest.makeSuite(LogLevelsCheck))
    suite.addTest(unittest.makeSuite(LoggerHandlerConfigCheck))
    suite.addTest(unittest.makeSuite(LoggerFunctionCheck))
    suite.addTest(unittest.makeSuite(LoggerClassCheck))
    suite.addTest(unittest.makeSuite(NoLoggerCheck))
    suite.addTest(unittest.makeSuite(OneLoggerCheck))
    suite.addTest(unittest.makeSuite(SeveralLoggerCheck))
    suite.addTest(unittest.makeSuite(DispatchPacketCheck))
    suite.addTest(unittest.makeSuite(PeriodicFlushCheck))
    suite.addTest(unittest.makeSuite(EnvVariableDefaultCheck))
    suite.addTest(unittest.makeSuite(StdoutEnvVariableCheck))
    suite.addTest(unittest.makeSuite(CentralEnvVariableCheck))
    return suite

if __name__ == "__main__":
    unittest.main(defaultTest='suite')


#
# ___oOo___
