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
# "@(#) $Id: test_AES2Py.py,v 1.1 2009/02/18 00:36:46 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2009-02-17  created
#
import sys
import unittest

if '../lib/python/site-packages' not in sys.path:
    sys.path.insert(0, '../lib/python/site-packages')

import ACSErrTypeTest
import ACSErrTypeTestImpl
import ACSErr

class TestPythonErrorFreeCompletion(unittest.TestCase):
    
    def testobjectcreation(self):
        c = ACSErrTypeTestImpl.ACSErrTestOKCompletionImpl()
        self.assertEqual(ACSErr.ACSErrTypeTest, c.type)
        self.assertEqual(ACSErrTypeTest.ACSErrTestOK, c.code)
        self.assertEqual(1, c.isOK())
        self.assertEqual(True, c.getErrorTrace() is None)

if __name__ == '__main__':
    unittest.main()

# ___oOo___
