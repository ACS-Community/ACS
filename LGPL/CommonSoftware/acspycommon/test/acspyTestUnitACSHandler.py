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
# "@(#) $Id: acspyTestUnitACSHandler.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# arne  2008-03-28  created
#

#------------------------------------------------------------------------------
__revision__ = "$Id: acspyTestUnitACSHandler.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
import unittest
import mock
from time import gmtime,sleep
import logging

#--ACS IMPORTS____-------------------------------------------------------------
def dummyACSLogSvc():
    return None

import ACSLog
import Acspy.Util.ACSCorba
Acspy.Util.ACSCorba.acsLogSvc = dummyACSLogSvc

import Acspy.Common.ACSHandler as ACSHandler

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


class ACSFormatterCheck(unittest.TestCase):
    """Test of the ACSFormatter class."""

    def setUp(self):
        self.f = ACSHandler.ACSFormatter()

    def testConstructor(self):
        """ACSFormatter initialized"""
        self.assertEqual("%(asctime)s.%(msecs)03d %(name)s %(message)s", self.f._fmt)
        self.assertEqual("%Y-%m-%dT%H:%M:%S", self.f.datefmt)
        self.assertEqual(gmtime, self.f.converter)

    def testFormatNoData(self):
        """ACSFormatter formats a log record that has no data attribute correctly"""
        lr = ACSHandler.ACSLogRecord("Simple", "TRACE", "/path/to/file.py", 100, "Test text", [], None)
        lr.created = 0
        lr.msecs = 0
        s = self.f.format(lr)
        self.assertEqual("1970-01-01T00:00:00.000 Simple Test text",s)

    def testFormatNoneData(self):
        """ACSFormatter formats a log record that has no data attribute correctly"""
        lr = ACSHandler.ACSLogRecord("Simple", "TRACE", "/path/to/file.py", 100, "Test text", [], None)
        lr.created = 0
        lr.msecs = 0
        lr.data = None
        s = self.f.format(lr)
        self.assertEqual("1970-01-01T00:00:00.000 Simple Test text [ ]",s)

    def testFormatEmptyDataDict(self):
        """ACSFormatter formats a log record with an empty data dictionary attribute correctly"""
        lr = ACSHandler.ACSLogRecord("Simple", "TRACE", "/path/to/file.py", 100, "Test text", [], None)
        lr.created = 0
        lr.msecs = 0
        lr.data = {}
        s = self.f.format(lr)
        self.assertEqual("1970-01-01T00:00:00.000 Simple Test text [ ]",s)

    def testFormatEmptyDataList(self):
        """ACSFormatter formats a log record with an empty data list attribute correctly"""
        lr = ACSHandler.ACSLogRecord("Simple", "TRACE", "/path/to/file.py", 100, "Test text", [], None)
        lr.created = 0
        lr.msecs = 0
        lr.data = []
        s = self.f.format(lr)
        self.assertEqual("1970-01-01T00:00:00.000 Simple Test text [ ]",s)

    def testFormatDataDict(self):
        """ACSFormatter formats a log record with a data dictionary attribute correctly"""
        lr = ACSHandler.ACSLogRecord("Simple", "TRACE", "/path/to/file.py", 100, "Test text", [], None)
        lr.created = 0
        lr.msecs = 0
        lr.data = { 'a' : 'A', 5 : '5', 'B' : 9 }
        s = self.f.format(lr)
        self.assertEqual("1970-01-01T00:00:00.000 Simple Test text [ a=A B=9 5=5 ]",s)

    def testFormatDataList(self):
        """ACSFormatter formats a log record with a data list attribute correctly"""
        lr = ACSHandler.ACSLogRecord("Simple", "TRACE", "/path/to/file.py", 100, "Test text", [], None)
        lr.created = 0
        lr.msecs = 0
        lr.data = [ ACSLog.NVPair('a', 'A'), ACSLog.NVPair('5', '5'), ACSLog.NVPair('B', '9') ]
        s = self.f.format(lr)
        self.assertEqual("1970-01-01T00:00:00.000 Simple Test text [ a=A 5=5 B=9 ]",s)

                            
class ACSLogRecordCheck(unittest.TestCase):
    """Test of the ACSLogRecord class."""

    def testSimpleConstructor(self):
        """ACSLogRecord initialized with simple name"""
        lr = ACSHandler.ACSLogRecord("Simple", "TRACE", "/path/to/file.py", 100, "Test text", [], None)
        self.assertEqual("Simple", lr.name)
        self.assertEqual(lr.name, lr.source)
        self.assertEqual("/path/to/file.py", lr.pathname)
        self.assertEqual("file.py", lr.filename)
        self.assertEqual("file", lr.module)
        self.assertEqual("Test text", lr.msg)
        self.assertEqual([], lr.args)
        self.assertEqual(None, lr.exc_info)
        self.assertEqual(None, lr.funcName)

    def testNestedConstructor(self):
        """ACSLogRecord initialized with nested name"""
        lr = ACSHandler.ACSLogRecord("Nested.Name", "TRACE", "/path/to/file.py", 100, "Test text", [], None)
        self.assertEqual("Name", lr.name)
        self.assertEqual("Nested", lr.source)


class ACSHandlerCheck(unittest.TestCase):
    """Test of the ACSHandler class."""

    def setUp(self):
        ACSHandler.acsLogSvc = mockACSLogSvc

    
    def tearDown(self):
        pass

    def testConstructorDefault(self):
        """ACSHandler initialized"""
        h = ACSHandler.ACSHandler()
        self.assertEqual(ACSHandler.DEFAULT_MAXIMUM_QUEUE, h.capacity)
        self.assertEqual(ACSHandler.DEFAULT_RECORD_CAPACITY, h.batchsize)
        self.assertEqual(ACSHandler.DEFAULT_IMMEDIATE_DISPATCH, h.dispatchlevel)
        self.assertEqual(mockLogSvc.__repr__(), h.logSvc.__repr__())

    def testConstructorNoFlushThread(self):
        """ACSHandler initialized with no periodic flush thread"""
        h = ACSHandler.ACSHandler()
        self.assertEqual(False, 'sched' in h.__dict__)

    def testConstructorNoLogSvc(self):
        """ACSHandler initialized when acsLogSvc not available"""
        ACSHandler.acsLogSvc = dummyACSLogSvc
        h = ACSHandler.ACSHandler()
        self.assertEqual(ACSHandler.DEFAULT_MAXIMUM_QUEUE, h.capacity)
        self.assertEqual(None, h.logSvc)

    def testGetCORBALoggerCache(self):
        """ACSHandler returns cached CORBA logging service"""
        h = ACSHandler.ACSHandler()
        self.assertEqual(mockLogSvc.__repr__(), h.getCORBALogger().__repr__())

    def testGetCORBALoggerCacheReload(self):
        """ACSHandler reloads CORBA logging service when cache is empty"""
        h = ACSHandler.ACSHandler()
        h.logSvc = None
        self.assertEqual(mockLogSvc.__repr__(), h.getCORBALogger().__repr__())
        self.assertEqual(mockLogSvc.__repr__(), h.logSvc.__repr__())

    def testSendLog(self):
        """ACSHandler sends Log messages at the appropriate levels"""
        import Acspy.Common.Log
        h = ACSHandler.ACSHandler()

        # Make logging calls for all known logging levels except OFF and NOTSET
        expected = []
        keys = Acspy.Common.Log.LEVELS.keys()
        keys.sort()
        for l in keys:
            if l == 0 or l == 99:
                continue
            else:
                if l in [7, 8, ACSLog.ACS_LOG_ERROR]:
                    expected.append("logWithAudience")
                else:
                    expected.append("log" + Acspy.Common.Log.getLevelName(l).capitalize())
            lr = ACSHandler.ACSLogRecord("Sample", Acspy.Common.Log.LEVELS[l], "/path/to/file.py", 100, expected[-1], None, None)
            h.sendLog(lr)

        # Append an unknown level
        expected.append("logCritical")
        lr = ACSHandler.ACSLogRecord("Sample", 75, "/path/to/file.py", 100, "Unknown", None, None)
        h.sendLog(lr)
        
        self.assertEquals(expected, [ n[0] for n in mockLogSvc.method_calls[-len(keys)+1:]])

    def testSendLogErrorTrace(self):
        """ACSHandler sends errortrace messages at the appropriate levels"""
        import Acspy.Common.Log
        import Acspy.Common.ErrorTrace as ErrorTrace
        l = Acspy.Common.Log.Logger('test')
        h = ACSHandler.ACSHandler()
        et = ErrorTrace.ErrorTrace(1,1)
        extras = { 'errortrace' : et, 'priority' : ACSLog.ACS_LOG_ERROR }
        lr = l.makeRecord("Sample", Acspy.Common.Log.LEVELS[8], "/path/to/file.py",
                                     100, 'ErrorTrace', (), None, 'dummy', extras)
        h.sendLog(lr)
        logcall = mockLogSvc.method_calls[-1]
        self.assertEquals("logErrorWithPriority", logcall[0])
        self.assertEquals(ACSLog.ACS_LOG_ERROR, logcall[1][1])

    def testSendLogWithAudience(self):
        """ACSHandler sends logWithAudience messages at the appropriate levels"""
        import Acspy.Common.Log
        l = Acspy.Common.Log.Logger('test')
        h = ACSHandler.ACSHandler()
        extras = { 'priority' : ACSLog.ACS_LOG_ERROR, 'audience' : "", 'array' : "", 'antenna' : "" }
        lr = l.makeRecord("Sample", Acspy.Common.Log.LEVELS[8], "/path/to/file.py",
                                     100, 'ErrorTrace', (), None, 'dummy', extras)
        h.sendLog(lr)
        logcall = mockLogSvc.method_calls[-1]
        self.assertEquals("logWithAudience", logcall[0])
        self.assertEquals(ACSLog.ACS_LOG_ERROR, logcall[1][0])

    def testSendLogWithPriority(self):
        """ACSHandler sends logWithPriority messages at the appropriate levels"""
        import Acspy.Common.Log
        l = Acspy.Common.Log.Logger('test')
        h = ACSHandler.ACSHandler()
        ctxt = ACSLog.RTContext('a', 'b', 'c', 'd', 'e')
        srcinf = ACSLog.SourceInfo('a', 'b', 'c')
        extras = { 'priority' : ACSLog.ACS_LOG_ERROR, 'data': [], 'rtCont' : ctxt, 'srcInfo' : srcinf, 'audience' : "", 'array' : "", 'antenna' : "" }
        lr = l.makeRecord("Sample", Acspy.Common.Log.LEVELS[8], "/path/to/file.py",
                                     100, 'ErrorTrace', (), None, 'dummy', extras)
        h.sendLog(lr)
        logcall = mockLogSvc.method_calls[-1]
        self.assertEquals("logWithPriority", logcall[0])
        self.assertEquals(ACSLog.ACS_LOG_ERROR, logcall[1][0])
        self.assertEquals(True, isinstance(logcall[1][3], ACSLog.RTContext))
        self.assertEquals(ctxt, logcall[1][3])
        self.assertEquals(True, isinstance(logcall[1][4], ACSLog.SourceInfo))
        self.assertEquals(srcinf, logcall[1][4])

    def testSendLogWithPriorityNoContextorSource(self):
        """ACSHandler sends logWithPriority messages at the appropriate levels without provided context or source"""
        import Acspy.Common.Log
        l = Acspy.Common.Log.Logger('test')
        h = ACSHandler.ACSHandler()
        extras = { 'priority' : ACSLog.ACS_LOG_ERROR, 'data': [], 'audience' : "", 'array' : "", 'antenna' : "" }
        lr = l.makeRecord("Sample", Acspy.Common.Log.LEVELS[8], "/path/to/file.py",
                                     100, 'ErrorTrace', (), None, 'dummy', extras)
        h.sendLog(lr)
        logcall = mockLogSvc.method_calls[-1]
        self.assertEquals("logWithPriority", logcall[0])
        self.assertEquals(ACSLog.ACS_LOG_ERROR, logcall[1][0])
        self.assertEquals(True, isinstance(logcall[1][3], ACSLog.RTContext))
        self.assertEquals(True, isinstance(logcall[1][4], ACSLog.SourceInfo))

    def testSendLogWithPriorityBadContextandSource(self):
        """ACSHandler sends logWithPriority messages at the appropriate levels with bad context"""
        import Acspy.Common.Log
        l = Acspy.Common.Log.Logger('test')
        h = ACSHandler.ACSHandler()
        extras = { 'priority' : ACSLog.ACS_LOG_ERROR, 'data': [], 'rtCont': None, 'srcInfo' : None, 'audience' : "", 'array' : "", 'antenna' : "" }
        lr = l.makeRecord("Sample", Acspy.Common.Log.LEVELS[8], "/path/to/file.py",
                                     100, 'ErrorTrace', (), None, 'dummy', extras)
        h.sendLog(lr)
        logcall = mockLogSvc.method_calls[-1]
        self.assertEquals("logWithPriority", logcall[0])
        self.assertEquals(ACSLog.ACS_LOG_ERROR, logcall[1][0])
        self.assertEquals(True, isinstance(logcall[1][3], ACSLog.RTContext))
        self.assertEquals(False, logcall[1][3] is None)
        self.assertEquals(True, isinstance(logcall[1][4], ACSLog.SourceInfo))
        self.assertEquals(False, logcall[1][4] is None)

    def testShouldFlushCapacity(self):
        """ACSHandler flushes when capacity is reached"""
        h = ACSHandler.ACSHandler()
        lr = ACSHandler.ACSLogRecord("Nested.Name", logging.NOTSET+1, "/path/to/file.py", 100, "Test text", [], None)
        self.assertEqual(False, h.shouldFlush(lr))
        h.buffer += [ lr, lr, lr, lr, lr, lr, lr, lr, lr, lr ]
        self.assertEqual(True, h.shouldFlush(lr))
        
    def testShouldFlushPriority(self):
        """ACSHandler flushes when high priority message is received"""
        h = ACSHandler.ACSHandler()
        hr = ACSHandler.ACSLogRecord("High.Name", logging.CRITICAL+2, "/path/to/file.py", 100, "High Test text", [], None)
        self.assertEqual(True, h.shouldFlush(hr))
        
    def testFlushLevel(self):
        """ACSHandler flushes buffer when message priority greater than threshold arrives"""
        h = ACSHandler.ACSHandler()
        lr = ACSHandler.ACSLogRecord("Nested.Name", logging.INFO, "/path/to/file.py", 100, "Test text", [], None)
        self.assertEqual(False, h.shouldFlush(lr))
        lr = ACSHandler.ACSLogRecord("Nested.Name", logging.CRITICAL+1, "/path/to/file.py", 100, "Test text", [], None)
        self.assertEqual(True, h.shouldFlush(lr))
        
    def testFlushToFile(self):
        """ACSHandler writes log messages to a file"""
        h = ACSHandler.ACSHandler()
        h.file_handler = mock.Mock(spec=logging.Handler)
        lr = ACSHandler.ACSLogRecord("Nested.Name", "TRACE", "/path/to/file.py", 100, "Test text", [], None)
        h.flushToFile(lr)
        self.assertEqual('handle',h.file_handler.method_calls[0][0])

    def testFlushToFileNoHandler(self):
        """ACSHandler creates singleton file handler when necessary"""
        def mockInitFileHandler(self):
            self.file_handler = mock.Mock(spec=logging.FileHandler)
        holdmethod = ACSHandler.ACSHandler.initFileHandler
        ACSHandler.ACSHandler.initFileHandler = mockInitFileHandler 
        h = ACSHandler.ACSHandler()
        lr = ACSHandler.ACSLogRecord("Nested.Name", "TRACE", "/path/to/file.py", 100, "Test text", [], None)
        self.assertEqual(True, h.file_handler is None)
        h.flushToFile(lr)
        self.assertEqual(False, h.file_handler is None)
        ACSHandler.ACSHandler.initFileHandler = holdmethod

    def testFlush(self):
        """ACSHandler flushes buffer correctly"""
        h = ACSHandler.ACSHandler()
        lr = ACSHandler.ACSLogRecord("Nested.Name", logging.INFO, "/path/to/file.py", 100, "Test text", None, None)
        h.buffer = [ lr, lr ]
        expected = [ 'logInfo' , 'logInfo' ]
        h.flush()
        self.assertEquals(expected, [ n[0] for n in mockLogSvc.method_calls[-2:]])

    def testFlushException(self):
        """ACSHandler handles exceptions correctly when flushing buffer"""
        def mockSendLog(self, record): raise Exception()
        holdSendLog = ACSHandler.ACSHandler.sendLog
        ACSHandler.ACSHandler.sendLog = mockSendLog
        h = ACSHandler.ACSHandler()
        h.file_handler = mock.Mock(spec=logging.Handler)
        lr = ACSHandler.ACSLogRecord("Nested.Name", logging.INFO, "/path/to/file.py", 100, "Test text", None, None)
        h.buffer = [ lr, lr ]
        expected = [ 'handle' , 'handle' ]
        h.flush()
        self.assertEquals(expected, [ n[0] for n in h.file_handler.method_calls[-2:]])
        ACSHandler.ACSHandler.sendLog = holdSendLog

    def testFullLogQueue(self):
        """ACSHandler drops new messages when pending message queue is full"""
        def mockFlush(self): return 0
        holdFlush = ACSHandler.ACSHandler.flush
        ACSHandler.ACSHandler.flush = mockFlush
        h = ACSHandler.ACSHandler(capacity=ACSHandler.DEFAULT_RECORD_CAPACITY)
        lr = ACSHandler.ACSLogRecord("Nested.Name", logging.INFO, "/path/to/file.py", 100, "Test text", None, None)
        lrn = ACSHandler.ACSLogRecord("Name", logging.INFO, "/path/to/file.py", 100, "Test text", None, None)
        h.buffer += [ lr, lr, lr, lr, lr, lr, lr, lr, lr, lr ]
        self.assertEqual([ lr, lr, lr, lr, lr, lr, lr, lr, lr, lr ], h.buffer)
        h.handle(lrn)
        self.assertEqual([ lr, lr, lr, lr, lr, lr, lr, lr, lr, lr ], h.buffer)
        ACSHandler.ACSHandler.flush = holdFlush
        

    def testFullLogQueueFilter(self):
        """ACSHandler drops low priority messages when message queue is full"""
        def mockShouldFlush(self, record): return 0
        holdShouldFlush = ACSHandler.ACSHandler.shouldFlush
        ACSHandler.ACSHandler.shouldFlush = mockShouldFlush
        h = ACSHandler.ACSHandler(capacity=ACSHandler.DEFAULT_RECORD_CAPACITY)
        lra = ACSHandler.ACSLogRecord("Nested.Name", logging.NOTSET+1, "/path/to/file.py", 100, "Test text", None, None)
        lrb = ACSHandler.ACSLogRecord("Name", logging.DEBUG, "/path/to/file.py", 100, "Test text", None, None)
        lrc = ACSHandler.ACSLogRecord("Name", logging.INFO, "/path/to/file.py", 100, "Test text", None, None)
        lrd = ACSHandler.ACSLogRecord("Name", logging.ERROR, "/path/to/file.py", 100, "Test text", None, None)
        h.buffer += [ lrd, lra, lrc, lrb, lrd, lrb, lra, lrc, lra, lrd ]
        self.assertEqual([ lrd, lra, lrc, lrb, lrd, lrb, lra, lrc, lra, lrd ], h.buffer)
        h.handle(lrd)
        self.assertEqual([ lrd, lrc, lrd, lrc, lrd, lrd ], h.buffer)
        ACSHandler.ACSHandler.shouldFlush = holdShouldFlush
        

def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(ACSFormatterCheck))
    suite.addTest(unittest.makeSuite(ACSLogRecordCheck))
    suite.addTest(unittest.makeSuite(ACSHandlerCheck))
    return suite

if __name__ == "__main__":
    unittest.main(defaultTest='suite')


#
# ___oOo___
