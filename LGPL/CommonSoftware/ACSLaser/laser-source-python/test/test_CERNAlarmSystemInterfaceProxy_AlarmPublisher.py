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
# "@(#) $Id$"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-10-21  created
#

import unittest
import mock
import Acspy.Common.Log

mockLogger = mock.Mock(spec=Acspy.Common.Log.Logger)
def mockGetLogger(name=None):
    return mockLogger

Acspy.Common.Log.getLogger = mockGetLogger

import Acspy.Nc.Supplier as Supplier

mockSupplier = mock.Mock(spec=Supplier.Supplier)
def getMock(channel, component=None, domain=None):
    return mockSupplier

Supplier.Supplier = getMock

import CERNAlarmSystemInterfaceProxy.AlarmPublisher as AlarmPublisher

class TestAlarmPublisher(unittest.TestCase):

    def test_object_initialization(self):
        """AlarmPublisher default initialization"""
        ap = AlarmPublisher.AlarmPublisher()
        self.assertEqual(False, ap.supplier is None)

    def test_object_initialization_with_channel(self):
        """AlarmPublisher initialization with given channel name"""
        ap = AlarmPublisher.AlarmPublisher("ChannelName")
        self.assertEqual(False, ap.supplier is None)


    def test_publishAlarm(self):
        """AlarmPublisher alarm publishing"""
        import Acsalarmpy.FaultState as FS
        import Acsalarmpy.ASI as ASI
        import Acsalarmpy.Timestamp as TS

        mockSupplier.reset_mock()
        fs = FS.FaultState("Family", "Member", 1)
        fs.descriptor = ""
        fs.userTimestamp = TS.Timestamp()
        msg = ASI.ASIMessage([fs])
        msg.sourceTimestamp = TS.Timestamp()
        baseline = msg.toXML()
        ap = AlarmPublisher.AlarmPublisher()
        ap.publishAlarm(msg)
        self.assertEqual('publishEvent', mockSupplier.method_calls[0][0])
        self.assertEqual(True, isinstance(mockSupplier.method_calls[0][2]['simple_data'],AlarmPublisher.ACSJMSMessageEntity_idl._0_com.cosylab.acs.jms.ACSJMSMessageEntity))
        self.assertEqual(baseline, mockSupplier.method_calls[0][2]['simple_data'].text)


if __name__ == '__main__':
    unittest.main()
