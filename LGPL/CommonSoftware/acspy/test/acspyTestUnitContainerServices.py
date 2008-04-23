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
# "@(#) $Id: acspyTestUnitContainerServices.py,v 1.2 2008/04/23 18:28:27 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# arne  2007-08-28  created
#

#------------------------------------------------------------------------------
__revision__ = "$Id: acspyTestUnitContainerServices.py,v 1.2 2008/04/23 18:28:27 agrimstrup Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
import sys
import unittest
import mock
import CDB

# Replace CDB reference
mockCDB = mock.Mock({},CDB._objref_DAL)

def mockcdb():
    return mockCDB

import Acspy.Util.ACSCorba
Acspy.Util.ACSCorba.cdb = mockcdb

#--ACS IMPORTS____-------------------------------------------------------------
import Acspy.Servants.ContainerServices
from maciErrTypeImpl          import CannotGetComponentExImpl
#------------------------------------------------------------------------------
mockLogger = mock.Mock( { "logWarning" : None } )

def mockGetManager():
    return mock.Mock( { "get_component_info" : [] } )

def mockGetLogger(n):
    return mockLogger

def mockacsPrintExcDebug():
    pass


class ImportComponentStubsCheck(unittest.TestCase):
    """Test of the __importComponentStubs() method."""

    def setUp(self):
        self._getManager = Acspy.Servants.ContainerServices.getManager
        Acspy.Servants.ContainerServices.getManager = mockGetManager
        self._getLogger = Acspy.Servants.ContainerServices.getLogger
        Acspy.Servants.ContainerServices.getLogger = mockGetLogger
        self._printDebug = Acspy.Servants.ContainerServices.acsPrintExcDebug
        Acspy.Servants.ContainerServices.acsPrintExcDebug = mockacsPrintExcDebug
        self.victim = Acspy.Servants.ContainerServices.ContainerServices()
        self.victim.getLogger()

    def tearDown(self):
        Acspy.Servants.ContainerServices.getManager = self._getManager
        Acspy.Servants.ContainerServices.getLogger = self._getLogger
        Acspy.Servants.ContainerServices.acsPrintExcDebug = self._printDebug

    def testGetManager(self):
        """Mock Manager returned empty component list"""
        self.assertEquals([], Acspy.Servants.ContainerServices.getManager().get_component_info())

    def testGetLogger(self):
        """Mock Logger recorded correct log message"""
        self.assertEquals(None, Acspy.Servants.ContainerServices.getLogger("a").logWarning("Spoon!"))
        self.assertEquals("Spoon!", mockLogger.mockGetNamedCalls("logWarning")[-1].getParam(0))

    def testUnknownComponent(self):
        """Unknown Component throws CannotGetComponentEx"""
        self.assertRaises(CannotGetComponentExImpl, self.victim._ContainerServices__importComponentStubs,"Foo", None)
        self.assertEquals("Unable to import 'Foo' component's module: Component type unavailable!",
                          mockLogger.mockGetNamedCalls("logWarning")[-1].getParam(0))


    def testImportFailure(self):
        """Import of unknown module throws CannotGetComponentEx"""
        self.assertRaises(CannotGetComponentExImpl, self.victim._ContainerServices__importComponentStubs, "Foo", "IDL:/alma/Foo/Bar:1.0")
        self.assertEquals("Unable to import 'Foo' component's module: No module named Foo",
                          mockLogger.mockGetNamedCalls("logWarning")[-1].getParam(0))


if __name__ == "__main__":
    unittest.main()


#
# ___oOo___
