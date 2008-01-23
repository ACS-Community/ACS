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
# "@(#) $Id: acspyTestUnitLog.py,v 1.1 2008/01/23 23:57:26 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# arne  2008-01-18  created
#

#------------------------------------------------------------------------------
__revision__ = "$Id: acspyTestUnitLog.py,v 1.1 2008/01/23 23:57:26 agrimstrup Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
import unittest
import mock
from os import environ
import time
#--ACS IMPORTS____-------------------------------------------------------------
import ACSLog
import Acspy.Util.ACSCorba
from Acspy.Common.TimeHelper import TimeUtil
import maci
#------------------------------------------------------------------------------

# In order to run without actually using the CORBA interfaces, we need to
# create a mock ACSLog.LogSvc object.  
methdict = {}
for meth in [x for x in ACSLog._objref_LogSvc.__methods__]:
    methdict[meth] = None
methdict['__repr__'] = "ACSLog.LogSvc"
methdict['__str__'] = "ACSLog.LogSvc"

mockLogSvc = mock.Mock(methdict)

# Replacing the acsLogSvc call ensures that we are using the mock object
# in all situations
def mockACSLogSvc():
    return mockLogSvc

Acspy.Util.ACSCorba.acsLogSvc = mockACSLogSvc

# These imports are dependent on the acsLogSvc so they could not be made
# until the replacement was completed.
import Acspy.Common.Log as Log
import Acspy.Common.ErrorTrace


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

    def tearDown(self):
        environ.pop('ACS_LOG_STDOUT')

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

    def tearDown(self):
        environ.pop('ACS_LOG_CENTRAL')

    def testStdoutSetting(self):
        """EnvVariable Central value is correct when ACS_LOG_CENTRAL environment variable is set"""
        self.assertEquals(False, environ.has_key('ACS_LOG_STDOUT'))
        self.assertEquals(True, environ.has_key('ACS_LOG_CENTRAL'))
        self.assertEquals('2', environ['ACS_LOG_CENTRAL'])
        self.assertEquals(2, Log.ACS_LOG_CENTRAL)
        self.assertEquals(3, Log.ACS_LOG_STDOUT)

class LoggerClassCheck(unittest.TestCase):
    """Test the integrity and function of the Logger class"""
    
    def setUp(self):
        self.mylogger = Log.Logger("mylogger")
        self.mylogger.acshandler.capacity = 0


    def tearDown(self):
        pass

    def testLoggerInit(self):
        """Logger class default initialization is correct"""
        self.assertEquals([],self.mylogger.error_trace_list)
        self.assertEquals("mylogger", self.mylogger.name)
        self.assertEquals(0, self.mylogger.propagate)
        self.assertNotEquals(None, self.mylogger.stdouthandler)
        self.assertNotEquals(None, self.mylogger.acshandler)
        self.assertEquals(False, self.mylogger.isdefault)
        self.assertEquals(Log.LEVELS[Log.ACS_LOG_STDOUT], self.mylogger.stdouthandler.level)
        self.assertEquals(Log.LEVELS[Log.ACS_LOG_CENTRAL], self.mylogger.acshandler.level)
        self.assertEquals([self.mylogger.stdouthandler, self.mylogger.acshandler], self.mylogger.handlers)
        self.assertEquals(None, self.mylogger.parent)

    def testLogTrace(self):
        """Logger class Trace level logging"""
        self.mylogger.setLevels(maci.LoggingConfigurable.LogLevels(False,0, 0))
        self.mylogger.logTrace("Trace Message")
        self.assertEquals('logTrace', mockLogSvc.mockGetAllCalls()[-1].getName())
        
    def testLogDebug(self):
        """Logger class Debug level logging"""
        self.mylogger.setLevels(maci.LoggingConfigurable.LogLevels(False,0, 0))
        self.mylogger.logDebug("Debug Message")
        self.assertEquals('logDebug', mockLogSvc.mockGetAllCalls()[-1].getName())
        
    def testLogInfo(self):
        """Logger class Info level logging"""
        self.mylogger.logInfo("Info Message")
        self.assertEquals('logInfo', mockLogSvc.mockGetAllCalls()[-1].getName())
        
    def testLogNotice(self):
        """Logger class Notice level logging"""
        self.mylogger.logNotice("Notice Message")
        self.assertEquals('logNotice', mockLogSvc.mockGetAllCalls()[-1].getName())
        
    def testLogWarning(self):
        """Logger class Warning level logging"""
        self.mylogger.logWarning("Warning Message")
        self.assertEquals('logWarning', mockLogSvc.mockGetAllCalls()[-1].getName())
        
    def testLogError(self):
        """Logger class Error level logging"""
        self.mylogger.logError("Error Message")
        self.assertEquals('logWithAudience', mockLogSvc.mockGetAllCalls()[-1].getName())
        self.assertEquals(ACSLog.ACS_LOG_ERROR, mockLogSvc.mockGetAllCalls()[-1].getParam(0))
        
    def testLogCritical(self):
        """Logger class Critical level logging"""
        self.mylogger.logCritical("Critical Message")
        self.assertEquals('logCritical', mockLogSvc.mockGetAllCalls()[-1].getName())
        
    def testLogAlert(self):
        """Logger class Alert level logging"""
        self.mylogger.logAlert("Alert Message")
        self.assertEquals('logAlert', mockLogSvc.mockGetAllCalls()[-1].getName())
        
    def testLogEmergency(self):
        """Logger class Emergency level logging"""
        self.mylogger.logEmergency("Emergency Message")
        self.assertEquals('logEmergency', mockLogSvc.mockGetAllCalls()[-1].getName())
        
    def testLogXML(self):
        """Logger class XML logging"""
        self.mylogger.setLevels(maci.LoggingConfigurable.LogLevels(False,0, 0))
        self.mylogger.logXML("<msg>Emergency Message</msg>")
        self.assertEquals('logDebug', mockLogSvc.mockGetAllCalls()[-1].getName())
        
    def testLogAtLevel(self):
        """Logger class User-specified level logging"""
        self.mylogger.setLevels(maci.LoggingConfigurable.LogLevels(False,0, 0))
        expected = []
        keys = Acspy.Common.Log.LEVELS.keys()
        keys.sort()
        for l in keys:
            if l == 0 or l == 99:
                self.assertRaises(ValueError, self.mylogger.logAtLevel, l, "logAtLevel Message")
            else:
                if l in [7, 8, ACSLog.ACS_LOG_ERROR]:
                    expected.append("logWithAudience")
                else:
                    expected.append("log" + Acspy.Common.Log.getLevelName(l).capitalize())
                self.mylogger.logAtLevel(l, "logAtLevel Message")
        self.assertEquals(expected, [ n.getName() for n in mockLogSvc.mockGetAllCalls()[1:]])

    def testLogErrorTrace(self):
        """Logger class ErrorTrace logging with default priority"""
        et = Acspy.Common.ErrorTrace.ErrorTrace(1,1)
        self.mylogger.logErrorTrace(et)
        logcall = mockLogSvc.mockGetAllCalls()[-1]
        self.assertEquals("logErrorWithPriority", logcall.getName())
        self.assertEquals(ACSLog.ACS_LOG_ERROR, logcall.getParam(1))

    def testLogErrorTraceInvalidPriority(self):
        """Logger class ErrorTrace logging with an invalid priority"""
        et = Acspy.Common.ErrorTrace.ErrorTrace(1,1)
        self.assertRaises(KeyError, self.mylogger.logErrorTrace, et, 25)

    def testLogErrorTraceQueuing(self):
        """Logger class ErrorTrace logging with queued messages"""
        savelogsvc = self.mylogger.acshandler.logSvc
        self.mylogger.acshandler.logSvc = None
        et = Acspy.Common.ErrorTrace.ErrorTrace(1,1)
        self.mylogger.logErrorTrace(et)
        self.assertEqual([et], self.mylogger.error_trace_list)
        self.mylogger.acshandler.logSvc = savelogsvc
        self.mylogger.logErrorTrace(et)
        for lc in mockLogSvc.mockGetAllCalls()[-2:]:
            self.assertEquals("logErrorWithPriority", lc.getName())
            self.assertEquals(ACSLog.ACS_LOG_ERROR, lc.getParam(1))

    def testLogTypeSafe(self):
        """Logger class Type-safe logging"""
        msg = "LogTypeSafe Message"
        ts = TimeUtil().py2epoch(time.time()).value
        self.mylogger.logTypeSafe(ACSLog.ACS_LOG_ERROR, ts, msg, None, None, None)
        logcall = mockLogSvc.mockGetAllCalls()[-1]
        self.assertEquals("logWithPriority", logcall.getName())
        self.assertEquals(ACSLog.ACS_LOG_ERROR, logcall.getParam(0))
        self.assertEquals(ts, logcall.getParam(1))
        self.assertEquals(msg, logcall.getParam(2))
        self.assertEquals("", logcall.getParam(6))
        self.assertEquals("", logcall.getParam(7))
        self.assertEquals("", logcall.getParam(8))
        
    def testLogTypeSafeInvalidPriority(self):
        """Logger class Type-safe logging with invalid priority"""
        msg = "LogTypeSafe Message"
        ts = TimeUtil().py2epoch(time.time()).value
        self.assertRaises(KeyError, self.mylogger.logTypeSafe, 25, ts, msg, None, None, None)

    def testLogNotSoTypeSafe(self):
        """Logger class Not So Type-safe logging"""
        msg = "LogNotSoTypeSafe Message"
        self.mylogger.logNotSoTypeSafe(ACSLog.ACS_LOG_ERROR, msg)
        logcall = mockLogSvc.mockGetAllCalls()[-1]
        self.assertEquals("logWithAudience", logcall.getName())
        self.assertEquals(ACSLog.ACS_LOG_ERROR, logcall.getParam(0))
        self.assertEquals(msg, logcall.getParam(2))
        self.assertEquals("", logcall.getParam(5))
        self.assertEquals("", logcall.getParam(6))
        self.assertEquals("", logcall.getParam(7))
        
    def testLogNotSoTypeSafeInvalidPriority(self):
        """Logger class Not So Type-safe logging with invalid priority"""
        msg = "LogTypeSafe Message"
        self.assertRaises(KeyError, self.mylogger.logNotSoTypeSafe, 25, msg)

    def testSetLevelsDefault(self):
        """Logger class set log levels to using Default"""
        self.mylogger.setLevels(maci.LoggingConfigurable.LogLevels(True,0, 0))
        self.assertEqual(True, self.mylogger.usingDefault)
        self.assertEqual(Log.LEVELS[0], self.mylogger.stdouthandler.level)
        self.assertEqual(Log.LEVELS[0], self.mylogger.acshandler.level)
        
    def testSetLevelsValidInputs(self):
        """Logger class set log levels for valid inputs"""
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

    def testGetEffectiveHandlerLevel(self):
        """Logger class effective log levels are correct"""
        self.assertEqual(Log.logging.NOTSET, self.mylogger.getEffectiveHandlerLevel('stdouthandler'))
        self.assertEqual(Log.logging.NOTSET, self.mylogger.getEffectiveHandlerLevel('acshandler'))
    
        
class NoLoggerCheck(unittest.TestCase):
    """Test state and operation of Logging system when no loggers have been requested"""

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def testGetLoggerNames(self):
        """NoLoggerCheck getLoggerNames when no loggers requested"""
        self.assertEquals([], Log.getLoggerNames())

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
        self.assertEquals(Log.defaultlogger, self.mylogger.parent)
        self.assertEquals(Log.ACS_LOG_STDOUT, Log.RLEVELS[self.mylogger.stdouthandler.level])
        self.assertEquals(Log.ACS_LOG_CENTRAL, Log.RLEVELS[self.mylogger.acshandler.level])
        
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
        self.pname = "MyLogger"
        self.plogger = Log.getLogger(self.pname)
        self.cname = "MyLogger.child"
        self.clogger = Log.getLogger(self.cname)

    def tearDown(self):
        pass

    def testNestedLogger(self):
        """SeveralLoggerCheck nested loggers"""
        self.assertEquals(self.cname, self.clogger.name)
        self.assertEquals(self.plogger, self.clogger.parent)
        self.assertEquals(self.plogger.stdouthandler.level, self.clogger.getEffectiveHandlerLevel('stdouthandler'))
        self.assertEquals(self.plogger.acshandler.level, self.clogger.getEffectiveHandlerLevel('acshandler'))

    def testNestedLoggersRequested(self):
        """SeveralLoggerCheck getLoggerNames when nested loggers requested"""
        self.assertEquals(Log.logging.Logger.manager.loggerDict.keys(), Log.getLoggerNames())

    def testNestedLoggersRequestedFiltered(self):
        """SeveralLoggerCheck getLoggerNames when nested loggers requested with filtering"""
        self.assertEquals([self.cname, self.pname], Log.getLoggerNames(self.pname))
        self.assertEquals([self.cname], Log.getLoggerNames(self.cname))

    def testNestedNamedLoggerDefaultChild(self):
        """SeveralLoggerCheck Nested Named Loggers with child using default levels"""
        self.plogger.setLevels(maci.LoggingConfigurable.LogLevels(False,4, 5))
        self.clogger.setLevels(maci.LoggingConfigurable.LogLevels(True,0, 0))
        self.assertEquals(True, self.clogger.usingDefault)
        self.assertEquals(self.plogger.stdouthandler.level, self.clogger.stdouthandler.level)
        self.assertEquals(self.plogger.acshandler.level, self.clogger.acshandler.level)

    def testNestedNamedLoggerDefaultParentChild(self):
        """SeveralLoggerCheck Nested Named Loggers with parent and child using default levels"""
        Log.defaultlogger.setLevels(maci.LoggingConfigurable.LogLevels(False,4, 5))
        self.plogger.setLevels(maci.LoggingConfigurable.LogLevels(True,0, 0))
        self.clogger.setLevels(maci.LoggingConfigurable.LogLevels(True,0, 0))
        self.assertEquals(True, self.plogger.usingDefault)
        self.assertEquals(True, self.clogger.usingDefault)
        self.assertEquals(Log.defaultlogger.stdouthandler.level, self.plogger.stdouthandler.level)
        self.assertEquals(Log.defaultlogger.acshandler.level, self.plogger.acshandler.level)
        self.assertEquals(Log.defaultlogger.stdouthandler.level, self.clogger.stdouthandler.level)
        self.assertEquals(Log.defaultlogger.acshandler.level, self.clogger.acshandler.level)

    def testNestedNamedLoggerUpdateDefaultChild(self):
        """SeveralLoggerCheck Nested Named Loggers parent level updated with children using default levels"""
        clogger1 = Log.getLogger(self.cname+".1")
        clogger2 = Log.getLogger(self.cname+".2")
        self.plogger.setLevels(maci.LoggingConfigurable.LogLevels(False,4, 5))
        clogger1.setLevels(maci.LoggingConfigurable.LogLevels(True,0, 0))
        clogger2.setLevels(maci.LoggingConfigurable.LogLevels(True,0, 0))
        self.plogger.setLevels(maci.LoggingConfigurable.LogLevels(False,6, 7))
        self.plogger.updateChildren()
        self.assertEquals(self.plogger.stdouthandler.level, self.clogger.stdouthandler.level)
        self.assertEquals(self.plogger.acshandler.level, self.clogger.acshandler.level)
        self.assertEquals(self.plogger.stdouthandler.level, clogger1.stdouthandler.level)
        self.assertEquals(self.plogger.acshandler.level, clogger1.acshandler.level)
        self.assertEquals(self.plogger.stdouthandler.level, clogger2.stdouthandler.level)
        self.assertEquals(self.plogger.acshandler.level, clogger2.acshandler.level)

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

def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(LogLevelsCheck))
    suite.addTest(unittest.makeSuite(EnvVariableDefaultCheck))
    suite.addTest(unittest.makeSuite(LoggerClassCheck))
    suite.addTest(unittest.makeSuite(NoLoggerCheck))
    suite.addTest(unittest.makeSuite(OneLoggerCheck))
    suite.addTest(unittest.makeSuite(SeveralLoggerCheck))
    return suite

if __name__ == "__main__":
    unittest.main(defaultTest='suite')


#
# ___oOo___
