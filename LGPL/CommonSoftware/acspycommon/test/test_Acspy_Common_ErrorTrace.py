#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2009 
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
# "@(#) $Id: test_Acspy_Common_ErrorTrace.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2009-02-02  created
#

#--REGULAR IMPORTS-------------------------------------------------------------
import sys
import os
import os.path
from socket import gethostname
import unittest
import mock

#--ACS Imports-----------------------------------------------------------------
import ACSLog
import ACSErr
from Acspy.Common.TimeHelper import TimeUtil
from Acspy.Common.Log import Logger
import Acspy.Common.ErrorTrace as ET

class TestErrorTraceHelper(unittest.TestCase):

    def setUp(self):
        self.original = sys.stdout
        sys.stdout = mock.Mock(spec=sys.stdout)
        self.ethd = ET.ErrorTraceHelper()
        self.eth = ET.ErrorTraceHelper(mock.Mock(spec=ACSErr.ErrorTrace))

    def tearDown(self):
        sys.stdout = self.original
    
    def test_Print_empty(self):
        """ErrorTraceHelper Print produces no output for empty trace"""
        self.assertRaises(Exception, self.ethd.Print)

    def test_Print_nested(self):
        """ErrorTraceHelper Print produces output for a nested trace"""
        tu = TimeUtil()
        etg = ACSErr.ErrorTrace('myfile', 'myline', 'myroutine', 'myhost',
                               'grandson', 'mythread',
                               tu.py2epoch(1233697346).value,
                               'mySourceObj', 0, 0, 'Routine', 'Description',
                               [], [])
        ets = ACSErr.ErrorTrace('myfile', 'myline', 'myroutine', 'myhost',
                               'son', 'mythread',
                               tu.py2epoch(1233697346).value,
                               'mySourceObj', 0, 0, 'Routine', 'Description',
                               [], [etg])
        etf = ACSErr.ErrorTrace('myfile', 'myline', 'myroutine', 'myhost',
                               'father', 'mythread',
                               tu.py2epoch(1233697346).value,
                               'mySourceObj', 0, 0, 'Routine', 'Description',
                               [], [ets])
        self.eth = ET.ErrorTraceHelper(etf)
        self.eth.Print()
        self.assertEqual(6, len(sys.stdout.method_calls))

    def test_Print(self):
        """ErrorTraceHelper Print produces output for a single trace"""
        tu = TimeUtil()
        et = ACSErr.ErrorTrace('myfile', 'myline', 'myroutine', 'myhost',
                               'myprocess', 'mythread',
                               tu.py2epoch(1233697346).value,
                               'mySourceObj', 0, 0, 'Routine', 'Description',
                               [], [])
        self.eth = ET.ErrorTraceHelper(et)
        self.eth.Print()
        self.assertEqual('write', sys.stdout.method_calls[0][0])

    def test_errorTraceToString(self):
        """ErrorTraceHelper errorTraceToString creates formatted message"""
        rv = 'ErrorTrace (TimeStamp=Tue Feb  3 21:42:26 2009,\n                File=myfile,\n                Line=myline,\n                Routine=myroutine,\n                Host=myhost,\n                Process=myprocess,\n                Thread=mythread,\n                Type=0,\n                Code=0,\n                ShortDescrip=Description,\n                Severity=Routine,\n                Data: )\n'
        tu = TimeUtil()
        et = ACSErr.ErrorTrace('myfile', 'myline', 'myroutine', 'myhost',
                               'myprocess', 'mythread',
                               tu.py2epoch(1233697346).value,
                               'mySourceObj', 0, 0, 'Routine', 'Description',
                               [], None)
        self.assertEqual(rv, self.eth.errorTraceToString(et, '    '))

    def test_getData_empty(self):
        """ErrorTraceHelper getData returns expected value for empty list"""
        self.eth.error_trace.data = []
        self.assertEqual([], self.eth.getData('key'))

    def test_getData_failedsearch(self):
        """ErrorTraceHelper getData returns expected value for failed search"""
        self.eth.error_trace.data = [ACSErr.NameValue('goodkey', 'goodvalue')]
        self.assertEqual([], self.eth.getData('badkey'))

    def test_getData_mulitfind(self):
        """ErrorTraceHelper getData returns expected values for multiple match"""
        self.eth.error_trace.data = [ACSErr.NameValue('goodkey', 'goodvalue1'),
                                     ACSErr.NameValue('badkey', 'badvalue'),
                                     ACSErr.NameValue('goodkey', 'goodvalue2'),
                                     ACSErr.NameValue('badkey', 'badvalue')]
        self.assertEqual(['goodvalue1', 'goodvalue2'],
                         self.eth.getData('goodkey'))

    def test_getData(self):
        """ErrorTraceHelper getData returns expected value when matched"""
        self.eth.error_trace.data = [ACSErr.NameValue('badkey', 'badvalue'),
                                     ACSErr.NameValue('goodkey', 'goodvalue2'),
                                     ACSErr.NameValue('badkey', 'badvalue')]
        self.assertEqual(['goodvalue2'], self.eth.getData('goodkey'))

    def test_getErrorTrace_default(self):
        """ErrorTraceHelper getErrorTrace throws exception when no error_trace is defined"""
        self.assertRaises(Exception, self.ethd.getErrorTrace)

    def test_getErrorTrace(self):
        """ErrorTraceHelper getErrorTrace returns expected error trace when requested"""
        self.assertEquals(True, isinstance(self.eth.getErrorTrace(),mock.Mock))

    def test_getNext_emptylist(self):
        """ErrorTraceHelper getNext returns expected value for single error trace"""
        self.eth.error_trace.previousError = []
        self.assertEqual(True, self.eth.getNext() is None)
        self.eth.error_trace.__delattr__('previousError')

    def test_getNext(self):
        """ErrorTraceHelper getNext returns expected value for nested error trace """
        self.eth.error_trace.previousError = [ mock.Mock(spec=ACSErr.ErrorTrace)]
        self.assertEqual(True, self.eth.getNext() is not None)
        self.eth.error_trace.__delattr__('previousError')

    def test_isOK_nonzeroec(self):
        """ErrorTraceHelper isOK returns correct value for non-zero errorCode"""
        self.eth.error_trace.errorCode = 1
        self.eth.error_trace.errorType = 0
        self.assertEqual(0, self.eth.isOK())

    def test_isOK_nonzeroet(self):
        """ErrorTraceHelper isOK returns correct value for non-zero errorType"""
        self.eth.error_trace.errorCode = 0
        self.eth.error_trace.errorType = 1
        self.assertEqual(0, self.eth.isOK())

    def test_isOK(self):
        """ErrorTraceHelper isOK returns correct value for non-error report"""
        self.eth.error_trace.errorCode = 0
        self.eth.error_trace.errorType = 0
        self.assertEqual(1, self.eth.isOK())

    def test_log_empty(self):
        """ErrorTraceHelper log does nothing for empty error traces"""
        logger = mock.Mock(spec=Logger)
        self.assertRaises(Exception, self.ethd.log, logger)

    def test_log(self):
        """ErrorTraceHelper log records messages on stdout and ACS logger"""
        rv = 'ErrorTrace (TimeStamp=Tue Feb  3 21:42:26 2009,\n                File=myfile,\n                Line=myline,\n                Routine=myroutine,\n                Host=myhost,\n                Process=myprocess,\n                Thread=mythread,\n                Type=0,\n                Code=0,\n                ShortDescrip=Description,\n                Severity=Routine,\n                Data: )\n'
        logger = mock.Mock(spec=Logger)
        tu = TimeUtil()
        et = ACSErr.ErrorTrace('myfile', 'myline', 'myroutine', 'myhost',
                               'myprocess', 'mythread',
                               tu.py2epoch(1233697346).value,
                               'mySourceObj', 0, 0, 'Routine', 'Description',
                               [], [])
        self.eth = ET.ErrorTraceHelper(et)
        self.eth.log(logger, ACSLog.ACS_LOG_TRACE)
        self.assertEqual('logErrorTrace', logger.method_calls[0][0])
        self.assertEqual(2, len(sys.stdout.method_calls))

    def test_object_initialization(self):
        """ErrorTraceHelper initializes with default"""
        self.assertEquals(True, self.ethd.error_trace is None)

    def test_object_initialization_withtrace(self):
        """ErrorTraceHelper initializes with provided trace"""
        self.assertEquals(True, self.eth.error_trace is not None)

    def test_printET(self):
        """ErrorTraceHelper printET outputs formatted message"""
        rv = 'ErrorTrace (TimeStamp=Tue Feb  3 21:42:26 2009,\n                File=myfile,\n                Line=myline,\n                Routine=myroutine,\n                Host=myhost,\n                Process=myprocess,\n                Thread=mythread,\n                Type=0,\n                Code=0,\n                ShortDescrip=Description,\n                Severity=Routine,\n                Data: )\n'
        tu = TimeUtil()
        et = ACSErr.ErrorTrace('myfile', 'myline', 'myroutine', 'myhost',
                               'myprocess', 'mythread',
                               tu.py2epoch(1233697346).value,
                               'mySourceObj', 0, 0, 'Routine', 'Description',
                               [], None)
        self.eth.printET(et, '    ')
        self.assertEqual('    '+rv, sys.stdout.method_calls[0][1][0])

    def test_setData_emptylist(self):
        """ErrorTraceHelper setData adds pair to empty list"""
        self.eth.error_trace.data = []
        self.eth.setData('key', 5.5)
        self.assertEquals(['5.5'], self.eth.getData('key'))

    def test_setData_notlistmember(self):
        """ErrorTraceHelper setData adds pair to a populated list"""
        before = [ACSErr.NameValue('goodkey', 'goodvalue1'),
                  ACSErr.NameValue('badkey', 'badvalue'),
                  ACSErr.NameValue('goodkey', 'goodvalue2'),
                  ACSErr.NameValue('badkey', 'badvalue')]
        self.eth.error_trace.data = [ACSErr.NameValue('goodkey', 'goodvalue1'),
                                     ACSErr.NameValue('badkey', 'badvalue'),
                                     ACSErr.NameValue('goodkey', 'goodvalue2'),
                                     ACSErr.NameValue('badkey', 'badvalue')]
        self.eth.setData('key', 5.5)
        self.assertNotEquals(1,
                             len(set(before) - set(self.eth.error_trace.data)))

    def test_setData(self):
        """ErrorTraceHelper setData updates pair in a populated list"""
        self.eth.error_trace.data = [ACSErr.NameValue('goodkey', 'goodvalue1')]
        self.eth.setData('goodkey', 5.5)
        self.assertEqual(['5.5'], self.eth.getData('goodkey'))

    def test_setError_default(self):
        """ErrorTraceHelper setError throws exception with default values"""
        self.assertRaises(ValueError, self.ethd.setError)

    def test_setError_noec(self):
        """ErrorTraceHelper setError throws exception with undefined error code"""
        self.assertRaises(ValueError, self.eth.setError, None, '1')

    def test_setError_noet(self):
        """ErrorTraceHelper setError throws exception with undefined error type"""
        self.assertRaises(ValueError, self.eth.setError, '1', None)

    def test_setError(self):
        """ErrorTraceHelper setError sets error code and type"""
        self.eth.setError('0','0')
        self.assertEqual(0, self.eth.error_trace.errorCode)
        self.assertEqual(0, self.eth.error_trace.errorType)

class TestErrorTrace(unittest.TestCase):
    def test_object_initialization_default(self):
        """ErrorTrace initializes with default"""
        et = ET.ErrorTrace(0,0)
        self.assertEqual('unittest.py', os.path.split(et.file)[1])
        self.assertEqual(300, et.lineNum)
        self.assertEqual('__call__', et.routine)
        self.assertEqual(gethostname(), et.host)
        self.assertEqual(str(os.getpid()), et.process)
        self.assertEqual('MainThread', et.thread)
        self.assertEqual(True, isinstance(et.timeStamp,long) )
        self.assertEqual('', et.sourceObject)
        self.assertEqual(0, et.errorType)
        self.assertEqual(0, et.errorCode)
        self.assertEqual(ACSErr.Error, et.severity)
        self.assertEqual('None', et.shortDescription)
        self.assertEqual([], et.data)
        self.assertEqual([], et.previousError)

    def test_object_initialization(self):
        """ErrorTrace initializes with provided data"""
        seq = [ACSErr.NameValue('badkey', 'badvalue'),
               ACSErr.NameValue('goodkey', 'goodvalue2'),
               ACSErr.NameValue('badkey', 'badvalue')]
        try:
            raise Exception('Test Exception')
        except Exception, e:
            et = ET.ErrorTrace(0, 0, exception=e, description='Test Trace',
                               nvSeq=seq, level=1, severity=ACSErr.Alert,
                               sourceobject='Me')
        self.assertEqual('test_Acspy_Common_ErrorTrace.py',
                         os.path.split(et.file)[1])
        self.assertEqual(279, et.lineNum)
        self.assertEqual('test_object_initialization', et.routine)
        self.assertEqual(gethostname(), et.host)
        self.assertEqual(str(os.getpid()), et.process)
        self.assertEqual('MainThread', et.thread)
        self.assertEqual(True, isinstance(et.timeStamp,long) )
        self.assertEqual('Me', et.sourceObject)
        self.assertEqual(0, et.errorType)
        self.assertEqual(0, et.errorCode)
        self.assertEqual(ACSErr.Alert, et.severity)
        self.assertEqual('Test Trace', et.shortDescription)
        self.assertEqual(seq, et.data)
        self.assertEqual(True, isinstance(et.previousError[0],ET.ErrorTrace))

if __name__ == '__main__':
    unittest.main()
