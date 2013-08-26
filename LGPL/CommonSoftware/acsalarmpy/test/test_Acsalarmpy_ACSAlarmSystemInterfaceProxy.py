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
# "@(#) $Id: test_Acsalarmpy_ACSAlarmSystemInterfaceProxy.py,v 1.3 2010/06/09 00:34:44 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-09-29  created
#

import unittest
import mock
import Acspy.Common.Log
import ACSAlarmSystemInterfaceProxy as ACSInterface
import Acsalarmpy.FaultState as FaultState

class TestACSAlarmSystemInterfaceProxy(unittest.TestCase):
    
    @mock.patch_object(Acspy.Common.Log, 'getLogger')
    def setUp(self, mocklog):
        mockLogger = mock.Mock(spec=Acspy.Common.Log.Logger)
        mocklog.return_value = mockLogger
        self.asi = ACSInterface.ACSAlarmSystemInterfaceProxy()

    def test_object_initialization(self):
        """ACSAlarmSystemInterfaceProxy default initializer"""
        self.assertEqual(True, self.asi.sourceName is None)
        self.assertEqual(True, self.asi.hostName is None)
        self.assertEqual(True, isinstance(self.asi.configuration, ACSInterface.ASInterface.ASI.ASIConfiguration))

    def test_close(self):
        """ACSAlarmSystemInterfaceProxy close interface"""
        self.assertRaises(Exception,self.asi.close)

    def test_publishMessage(self):
        """ACSAlarmSystemInterfaceProxy publish message to alarm system"""
        self.assertRaises(Exception,self.asi.publishMessage)

    def test_push_single(self):
        """ACSAlarmSystemInterfaceProxy push a single fault"""
        self.asi.logger.reset_mock()
        fault = FaultState.FaultState("Family","Member",1)
        base = 'Alarm sent: <%s, %s, %d> %s' % (fault.family, fault.member, fault.code, fault.descriptor)
        self.asi.push(fault)
        mesg = self.asi.logger.method_calls[1:-1]
        self.assertEqual(1, len(mesg))
        self.assertEqual('logAlert',mesg[0][0])
        self.assertEqual(base, mesg[0][1][0])


    def test_push_multi(self):
        """ACSAlarmSystemInterfaceProxy push multiple faults"""
        self.asi.logger.reset_mock()
        faults = [FaultState.FaultState("Family","Member",2),FaultState.FaultState("Family","Member",3)]
        self.asi.push(faults)
        mesg = self.asi.logger.method_calls[1:-1]
        self.assertEqual(2, len(mesg))
        for m in mesg:
            self.assertEqual('logAlert',m[0])

    def test_pushActiveList(self):
        """ACSAlarmSystemInterfaceProxy push a list of active faults"""
        self.asi.logger.reset_mock()
        faultList = [FaultState.FaultState("Family","Member",4),FaultState.FaultState("Family","Member",5)]
        self.asi.pushActiveList(faultList)
        mesg = self.asi.logger.method_calls[1:-1]
        self.assertEqual(2, len(mesg))
        for m in mesg:
            self.assertEqual('logAlert',m[0])

if __name__ == '__main__':
    unittest.main()
