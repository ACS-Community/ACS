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
# "@(#) $Id: acspyTestUnitTimeHelper.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# arne  2008-04-08  created
#

#------------------------------------------------------------------------------
__revision__ = "$Id: acspyTestUnitTimeHelper.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
import unittest
import time
import acstime
#--ACS IMPORTS____-------------------------------------------------------------
import Acspy.Common.TimeHelper as TimeHelper
#------------------------------------------------------------------------------
def fixedTime():
    return 1207768989.9850370
    

class GetTimeStampCheck(unittest.TestCase):
    """Test that the getTimeStamp function is working correctly."""

    def setUp(self):
        self.savetime = time.time
        time.time = fixedTime


    def tearDown(self):
        time.time = self.savetime

    def testKnownDate(self):
        '''getTimeStamp reports correct ACS timestamp for a known value'''
        rtn = TimeHelper.getTimeStamp()
        self.assertEqual(True, isinstance(rtn, acstime.Epoch))
        self.assertEqual(134270617899850370L, rtn.value)


class TimeUtilCheck(unittest.TestCase):
    """Test that the TimeUtil class works correctly."""
    def setUp(self):
        self.th = TimeHelper.TimeUtil()
        pass

    def tearDown(self):
        pass

    def testPy2EpochEpoch(self):
        '''TimeUtil.py2epoch handles Python epoch correctly'''
        rtn = self.th.py2epoch(0)
        self.assertEqual(True, isinstance(rtn, acstime.Epoch))
        self.assertEqual(acstime.ACE_BEGIN, rtn.value)

    def testPy2EpochNegative(self):
        '''TimeUtil.py2epoch handles negative values correctly '''
        rtn = self.th.py2epoch(-1)
        self.assertEqual(10000000L, acstime.ACE_BEGIN - rtn.value)

    def testPy2EpochACSEpoch(self):
        '''TimeUtil.py2epoch handles ACS epoch correctly '''
        rtn = self.th.py2epoch(-acstime.ACE_BEGIN / 10000000L)
        self.assertEqual(0, rtn.value)

    def testEpoch2PyEpoch(self):
        '''TimeUtil.epoch2py handles ACS epoch correctly'''
        rtn = self.th.epoch2py(acstime.Epoch(0))
        self.assertEqual(True, isinstance(rtn, long))
        self.assertEqual(-acstime.ACE_BEGIN / 10000000L, rtn)

    def testEpoch2PyPyEpoch(self):
        '''TimeUtil.epoch2py handles Python epoch correctly'''
        rtn = self.th.epoch2py(acstime.Epoch(acstime.ACE_BEGIN))
        self.assertEqual(0L, rtn)

    def testEpoch2PyNegative(self):
        '''TimeUtil.epoch2py handles negative values correctly '''
        rtn = self.th.epoch2py(acstime.Epoch(acstime.ACE_BEGIN - 10000000L))
        self.assertEqual(-1L, rtn)
        pass

    def testEpoch2PyLong(self):
        '''TimeUtil.epoch2py handles long values correctly '''
        rtn = self.th.epoch2py(long(acstime.ACE_BEGIN))
        self.assertEqual(0L, rtn)

    def testPy2DurationZero(self):
        '''TimeUtil.py2duration handles 0 value correctly'''
        rtn = self.th.py2duration(0)
        self.assertEqual(True, isinstance(rtn, acstime.Duration))
        self.assertEqual(0, rtn.value)

    def testPy2DurationNegative(self):
        '''TimeUtil.py2duration handles negative values correctly '''
        rtn = self.th.py2duration(-1)
        self.assertEqual(-10000000L, rtn.value)

    def testDuration2PyZero(self):
        '''TimeUtil.duration2py handles 0 value correctly'''
        rtn = self.th.duration2py(acstime.Duration(0))
        self.assertEqual(True, isinstance(rtn, long))
        self.assertEqual(0, rtn)

    def testDuration2PyNegative(self):
        '''TimeUtil.duration2py handles negative values correctly '''
        rtn = self.th.duration2py(acstime.Duration(-1))
        self.assertEqual(-1, rtn)

    def testDuration2PyLong(self):
        '''TimeUtil.duration2py handles long values correctly '''
        rtn = self.th.duration2py(0L)
        self.assertEqual(0L, rtn)


def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(GetTimeStampCheck))
    suite.addTest(unittest.makeSuite(TimeUtilCheck))
    return suite

if __name__ == "__main__":
    unittest.main(defaultTest='suite')


#
# ___oOo___
