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
# "@(#) $Id: test_Acspy_Common_Err.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2009-02-02  created
#

#--REGULAR IMPORTS-------------------------------------------------------------
import sys
import unittest
import mock

#--ACS Imports-----------------------------------------------------------------
import ACSErr
from ACSErrTypePythonNativeImpl import PythonExImpl
from Acspy.Common.Log import Logger
import Acspy.Common.Err as Err

class TestACSError(unittest.TestCase):
    def test_getErrorTrace_notrace(self):
        """ACSError getErrorTrace returns expected value when no trace defined"""
        er = Err.ACSError(0,0,create=0)
        self.assertEqual(True, er.getErrorTrace() is None)

    def test_getErrorTrace(self):
        """ACSError getErrorTrace returns trace when defined"""
        er = Err.ACSError(0,0)
        self.assertEqual(True, isinstance(er.errorTrace, Err.ErrorTrace))

    def test_object_initialization(self):
        """ACSError initialized with defaults"""
        er = Err.ACSError(0,0)
        self.assertEqual(True, isinstance(er.errorTrace, Err.ErrorTrace))

    def test_object_initialization_CORBA_Exception(self):
        """ACSError initialized from CORBA Exception"""
        try:
            raise Exception("ACSError Test Exception")
        except Exception, e:
            cex = Err.pyExceptionToCORBA(e)
            er = Err.ACSError(0,0,exception=cex,create=0)
            self.assertEqual(er.errorTrace,cex.errorTrace)

    def test_object_initialization_nocreate_notraceorprevious(self):
        """ACSError initialized without create and no trace or previous error"""
        try:
            raise Exception("ACSError Test Exception")
        except Exception, e:
            er = Err.ACSError(0,0,exception=5,create=0)
            self.assertEqual(False, hasattr(er,'errorTrace'))

    def test_object_initialization_Completion(self):
        """ACSError initialized from Completion"""
        cpl = ACSErr.Completion(0,0,0,[0])
        er = Err.ACSError(0,0,exception=cpl,create=0)
        self.assertEqual(cpl.previousError[0],er.errorTrace)

class TestAddComplHelperMethods(unittest.TestCase):

    def setUp(self):
        self.logger = mock.Mock(spec=Logger)
        self.original = sys.stdout
        sys.stdout = mock.Mock(spec=sys.stdout)
        er = Err.ACSError(0,0)
        self.cpl = ACSErr.Completion(0,0,0,[er.getErrorTrace()])
        Err.addComplHelperMethods(self.cpl)

    def tearDown(self):
        sys.stdout = self.original
        
    def test_add_compl_helper_methods_gettimestamp(self):
        """addComplHelperMethods getTimeStamp added and functions"""
        self.assertEqual(True, hasattr(self.cpl, 'getTimeStamp'))
        self.assertEqual(0, self.cpl.getTimeStamp())

    def test_add_compl_helper_methods_gettype(self):
        """addComplHelperMethods getType added and functions"""
        self.assertEqual(True, hasattr(self.cpl, 'getType'))
        self.assertEqual(0, self.cpl.getType())

    def test_add_compl_helper_methods_getcode(self):
        """addComplHelperMethods getCode added and functions"""
        self.assertEqual(True, hasattr(self.cpl, 'getCode'))
        self.assertEqual(0, self.cpl.getCode())

    def test_add_compl_helper_methods_iserrorfree(self):
        """addComplHelperMethods isErrorFree added and functions if error defined"""
        self.assertEqual(True, hasattr(self.cpl, 'isErrorFree'))
        self.assertEqual(0, self.cpl.isErrorFree())

    def test_add_compl_helper_methods_iserrorfree_noerror(self):
        """addComplHelperMethods getTimeStamp added and functions if no error"""
        self.cpl.previousError = []
        self.assertEqual(1, self.cpl.isErrorFree())

    def test_add_compl_helper_methods_log(self):
        """addComplHelperMethods log added and functions"""
        self.assertEqual(True, hasattr(self.cpl, 'log'))
        self.cpl.log(self.logger)
        self.assertEqual(1, len(self.logger.method_calls))
        self.assertEqual('logErrorTrace', self.logger.method_calls[0][0])
        self.assertEqual(2, len(sys.stdout.method_calls))
        self.assertEqual('write', sys.stdout.method_calls[0][0])
        self.assertEqual('write', sys.stdout.method_calls[1][0])

    def test_add_compl_helper_methods_log_noerror(self):
        """addComplHelperMethods log added and functions"""
        self.cpl.previousError = []
        self.cpl.log(self.logger)
        self.assertEqual([], self.logger.method_calls)
        self.assertEqual([], sys.stdout.method_calls)

    def test_add_compl_helper_methods_adddata(self):
        """addComplHelperMethods addData added and functions"""
        self.assertEqual(True, hasattr(self.cpl, 'addData'))
        self.cpl.addData('name', 'value')
        self.assertEqual(1,len(self.cpl.previousError[0].data)) 
        self.assertEqual(True, isinstance(self.cpl.previousError[0].data[0],
                                             ACSErr.NameValue))

class TestPyExceptionToCORBA(unittest.TestCase):
    def test_py_exception_to_corb_a(self):
        """pyExceptionToCORBA creates CORBA exception from Python exception"""
        try:
            raise Exception("ACSError Test Exception")
        except Exception, e:
            cex = Err.pyExceptionToCORBA(e)
            self.assertEqual(True, isinstance(cex, PythonExImpl))
            self.assertEqual(True, isinstance(cex.errorTrace, Err.ErrorTrace))
            self.assertEqual(7, cex.errorTrace.errorType)
            self.assertEqual(0, cex.errorTrace.errorCode)

if __name__ == '__main__':
    unittest.main()
