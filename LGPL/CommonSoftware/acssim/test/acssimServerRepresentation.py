#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2005 
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
# "@(#) $Id: acssimServerRepresentation.py,v 1.2 2010/06/03 03:34:40 agrimstrup Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# rhiriart  2007-03-05  created
#

import sys
import unittest

from Acspy.Clients.SimpleClient import PySimpleClient
from demo import LampUnavailable

#-------------------------------------------------------------------------------
class ServerRepresentationTest(unittest.TestCase):
    """
    Test cases for the Server representation.
    """

    #---------------------------------------------------------------------------
    def setUp(self):
        """
        Test case fixture.
        """
        self.client = PySimpleClient("ServerRepresentationTest")
        self.simulator = self.client.getComponent("SIMULATION_SERVER")
        self.lamp = self.client.getComponent("LAMP_ACCESS")
        
        
    #---------------------------------------------------------------------------
    def tearDown(self):
        """
        Test case fixture cleanup.
        """
        self.client.releaseComponent("SIMULATION_SERVER")
        self.client.releaseComponent("LAMP_ACCESS")
        self.client.disconnect()

    #---------------------------------------------------------------------------
    def testSetupBehavior(self):
        """
        Modify the behavior of the simulated LAMP_ACCESS component.
        Setup the getLampBrightness function so it returns a constant 
        number.
        """

        code = """LOGGER.logInfo('getLampBrightness called.')
6.8"""
        self.simulator.setMethod('LAMP_ACCESS', 'getLampBrightness', code, 0.0)

        brightness = self.lamp.getLampBrightness()
        assert abs(brightness - 6.8) <= 0.01

    #---------------------------------------------------------------------------
    def testPersistentData(self):
        """
        Data can be left in a global buffer located in the Goodies module.
        This allows to get input data to persist between calls to the simulated
        component.
        As this test case shows, this can be useful to simulate get/set methods.
        """
        code = """LOGGER.logInfo('setLampBrightness called; brightness='+str(parameters[0]))
from Acssim.Goodies import setGlobalData
setGlobalData('brightness', parameters[0])
None"""
        self.simulator.setMethod('LAMP_ACCESS', 'setLampBrightness', code, 0.0)
        code = """LOGGER.logInfo('getLampBrightness called.')
from Acssim.Goodies import setGlobalData
getGlobalData('brightness')"""
        self.simulator.setMethod('LAMP_ACCESS', 'getLampBrightness', code, 0.0)

        self.lamp.setLampBrightness(3.14)
        brightness = self.lamp.getLampBrightness()
        assert abs(brightness - 3.14) <= 0.01


    #---------------------------------------------------------------------------
    def testGetGlobalData(self):
        """
        The SIMULATION_SERVER component implements also a getGlobalData() function,
        which is tested here.
        """
        code = """LOGGER.logInfo('setLampBrightness called; brightness='+str(parameters[0]))
from Acssim.Goodies import setGlobalData
setGlobalData('brightness', parameters[0])
None"""
        self.simulator.setMethod('LAMP_ACCESS', 'setLampBrightness', code, 0.0)
        self.lamp.setLampBrightness(3.14)
        brightness = self.simulator.getGlobalData('brightness')
        assert brightness == '3.14'

    #---------------------------------------------------------------------------
    def testRaiseException(self):
        """
        Modify the behaviour of the LAMP_ACCESS component so it raises the
        LampUnavailable exception when the getLampBrightness() function is
        called.
        """
        code = """from demo import LampUnavailable
raise LampUnavailable()"""
        self.simulator.setMethod('LAMP_ACCESS', 'getLampBrightness', code, 0.0)

        try:
            b = self.lamp.getLampBrightness()
        except LampUnavailable, ex:
            return # Correct exception was raised.
        except:
            ei = sys.exc_info()
            fail("Wrong exception raised: '" + ei[0] + "'")

        fail('No exception was raised')        

def suite():
    suite = unittest.TestSuite()
    suite.addTest(ServerRepresentationTest("testSetupBehavior"))
    suite.addTest(ServerRepresentationTest("testPersistentData"))
    suite.addTest(ServerRepresentationTest("testGetGlobalData"))
    suite.addTest(ServerRepresentationTest("testRaiseException"))
    return suite

if __name__ == '__main__':
    unittest.main(defaultTest='suite')


#
# ___oOo___
