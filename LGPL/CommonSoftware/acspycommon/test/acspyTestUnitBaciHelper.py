#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2007 
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
# "@(#) $Id: acspyTestUnitBaciHelper.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstr  2007-08-28  created
#

#------------------------------------------------------------------------------
__revision__ = "$Id: acspyTestUnitBaciHelper.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
import unittest
import mock
#--ACS IMPORTS____-------------------------------------------------------------
import acspytest__POA
import Acspy.Util.BaciHelper as BaciHelper
from maciErrTypeImpl      import CannotGetComponentExImpl
#------------------------------------------------------------------------------

class AddPropertyCheck(unittest.TestCase):
    """Tests of the addProperty() method."""

    def setUp(self):
        mockIFR = mock.Mock( {"lookup_id" : None } )
        self.IFR = BaciHelper.IFR
        BaciHelper.IFR = mockIFR
        self.victim = acspytest__POA.PyBaciTest()


    def tearDown(self):
        BaciHelper.IFR = self.IFR

    def testMockIFR(self):
        """Ensure that the mock interface repository is working correctly."""
        self.assertEqual(None, BaciHelper.IFR.lookup_id("Foo"))

    def testFailedIFRLookup(self):
        """Correct exception thrown when Component not found."""
        self.assertRaises(CannotGetComponentExImpl, BaciHelper.addProperty, self.victim, "Foo")

if __name__ == "__main__":
    unittest.main()



#
# ___oOo___
