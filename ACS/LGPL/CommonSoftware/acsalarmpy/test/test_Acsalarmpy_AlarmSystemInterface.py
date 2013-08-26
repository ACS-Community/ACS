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
# "@(#) $Id: test_Acsalarmpy_AlarmSystemInterface.py,v 1.3 2010/06/09 00:34:44 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-09-29  created
#

import unittest
import mock
import Acspy.Common.Log

mockLogger = mock.Mock(spec=Acspy.Common.Log.Logger)
def mockGetLogger(name=None):
    return mockLogger

Acspy.Common.Log.getLogger = mockGetLogger

import Acsalarmpy.AlarmSystemInterface as ASInterface
import Acsalarmpy.FaultState as FaultState


def mockPublishMessage(self, msg):
    self.outmsg = msg

class TestAlarmSystemInterface(unittest.TestCase):
    def setUp(self):
        self.asi = ASInterface.AlarmSystemInterface()
        
    def test_close(self):
        """AlarmSystemInterface close interface"""
        self.assertRaises(Exception, self.asi.close) 

    def test_object_initialization(self):
        """AlarmSystemInterface default initializer"""
        self.assertEqual(True, self.asi.sourceName is None)
        self.assertEqual(True, self.asi.hostName is None)
        self.assertEqual(True, isinstance(self.asi.configuration, ASInterface.ASI.ASIConfiguration))

    def test_publishMessage(self):
        """AlarmSystemInterface publish message to alarm system"""
        self.assertRaises(Exception, self.asi.publishMessage) 

    @mock.patch('Acsalarmpy.AlarmSystemInterface.AlarmSystemInterface.publishMessage', new=mock.Mock())
    def test_push_single(self):
        """Push a single fault"""
        self.asi.logger.reset_mock()
        fault = FaultState.FaultState("Family","Member",1)
        self.asi.push(fault)
        mesg = ASInterface.AlarmSystemInterface.publishMessage.call_args[0]
        self.assertEqual(1, len(mesg))
        self.assertEqual(True, isinstance(mesg[0], ASInterface.ASI.ASIMessage))
        self.assertEqual(fault, mesg[0].faultStates[0])
        self.assertEqual(5, len(self.asi.logger.method_calls))
        

    @mock.patch('Acsalarmpy.AlarmSystemInterface.AlarmSystemInterface.publishMessage', new=mock.Mock())
    def test_push_multi(self):
        """Push multiple faults"""
        self.asi.logger.reset_mock()
        faults = [FaultState.FaultState("Family","Member",1),FaultState.FaultState("Family","Member",2)]
        self.asi.push(faults)
        mesg = ASInterface.AlarmSystemInterface.publishMessage.call_args[0]
        self.assertEqual(1, len(mesg))
        self.assertEqual(True, isinstance(mesg[0], ASInterface.ASI.ASIMessage))
        self.assertEqual(faults, mesg[0].faultStates)
        self.assertEqual(6, len(self.asi.logger.method_calls))
        

    @mock.patch('Acsalarmpy.AlarmSystemInterface.AlarmSystemInterface.publishMessage', new=mock.Mock())
    def test_pushActiveList(self):
        """Push a list of active faults"""
        self.asi.logger.reset_mock()
        faultList = [FaultState.FaultState("Family","Member",1),FaultState.FaultState("Family","Member",2)] 
        self.asi.pushActiveList(faultList)
        mesg = ASInterface.AlarmSystemInterface.publishMessage.call_args[0]
        self.assertEqual(1, len(mesg))
        self.assertEqual(True, isinstance(mesg[0], ASInterface.ASI.ASIMessage))
        self.assertEqual(faultList, mesg[0].faultStates)
        self.assertEqual(6, len(self.asi.logger.method_calls))

if __name__ == '__main__':
    unittest.main()
