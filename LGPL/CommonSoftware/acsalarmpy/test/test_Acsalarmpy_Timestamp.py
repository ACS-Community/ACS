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
# "@(#) $Id: test_Acsalarmpy_Timestamp.py,v 1.2 2010/06/09 00:34:44 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-10-01  created
#

import unittest
import mock
import time
import Acsalarmpy.Timestamp as Timestamp

class TestTimestamp(unittest.TestCase):
    
    def assertWithinRange(self, first, second, delta, places=7, msg=None):
        """Fail if two objects are more than delta apart."""
        if abs(round(second-first, places)) > delta:
            raise self.failureException, \
                  (msg or '%r != %r within range of +/- %r' % (first, second, delta))

    def test_object_initialization(self):
        """Timestamp initializes with all values"""
        ts = Timestamp.Timestamp(1222887968, 813309)
        self.assertEqual(1222887968, ts.seconds)
        self.assertEqual(813309, ts.microseconds)

    @mock.patch_object(time, 'time')
    def test_object_initialization_default(self, mocktime):
        """Timestamp initializes with default values"""
        mocktime.return_value = 1222887968.813309
        ts = Timestamp.Timestamp()
        self.assertEqual(1222887968, ts.seconds)
        self.assertWithinRange(813309, ts.microseconds,1,0)

    def test_object_initialization_seconds(self):
        """Timestamp initializes with seconds provided"""
        ts = Timestamp.Timestamp(seconds=1222887968)
        self.assertEqual(1222887968, ts.seconds)
        self.assertEqual(0, ts.microseconds)

    def test_object_initialization_microseconds(self):
        """Timestamp initializes with microseconds provided"""
        ts = Timestamp.Timestamp(microSeconds=1222887968)
        self.assertEqual(1222887968, ts.microseconds)
        self.assertEqual(0, ts.seconds)

    @mock.patch_object(time, 'time')
    def test_toXML(self, mocktime):
        """Timestamp generates XML"""
        mocktime.return_value = 1222887968.813309
        ts = Timestamp.Timestamp()
        self.assertEqual('   <source-timestamp seconds="1222887968" microseconds="813308"/>\n', ts.toXML('source-timestamp', 3))

if __name__ == '__main__':
    unittest.main()
