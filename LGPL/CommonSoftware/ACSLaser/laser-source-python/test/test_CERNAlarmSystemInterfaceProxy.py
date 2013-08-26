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
# agrimstrup  2008-10-15  created
#

import unittest
import mock
import Acspy.Common.Log

mockLogger = mock.Mock(spec=Acspy.Common.Log.Logger)
def mockGetLogger(name=None):
    return mockLogger

Acspy.Common.Log.getLogger = mockGetLogger

from Acsalarmpy.ASI import ASIMessage
from Acsalarmpy.FaultState import FaultState
import CERNAlarmSystemInterfaceProxy.AlarmPublisher as AP

mockAlarmPublisher = mock.Mock(spec=AP.AlarmPublisher)
def getMockAlarmPublisher(topic=None):
    return mockAlarmPublisher

from CERNAlarmSystemInterfaceProxy import CERNAlarmSystemInterfaceProxy

class TestCERNAlarmSystemInterfaceProxy(unittest.TestCase):

    def test_close(self):
        """CERNAlarmSystemInterfaceProxy closes after default initialization"""
        p = CERNAlarmSystemInterfaceProxy()
        self.assertEqual(None, p.close())
        self.assertEqual(True, p.sourceName is None)
        self.assertEqual(True, p.hostName is None)
        self.assertEqual(True, p.publisher is None)
        self.assertEqual(True, p.topic is None)

    def test_object_initialization(self):
        """CERNAlarmSystemInterfaceProxy default initialization"""
        p = CERNAlarmSystemInterfaceProxy()
        self.assertEqual(True, p.sourceName is None)
        self.assertEqual(False, p.hostName is None)
        self.assertEqual(True, p.publisher is None)
        self.assertEqual(True, p.topic is None)

    @mock.patch('CERNAlarmSystemInterfaceProxy.AlarmPublisher.AlarmPublisher',new=getMockAlarmPublisher)
    def test_publishMessage(self):
        """CERNAlarmSystemInterfaceProxy publishes alarm after default initialization"""
        p = CERNAlarmSystemInterfaceProxy()
        msg = ASIMessage([FaultState('Family','Member',1)])
        p.publishMessage(msg)
        self.assertEqual('CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES',p.topic)
        self.assertEqual(True, p.publisher is not None)
        self.assertEqual('publishAlarm', p.publisher.method_calls[0][0])
        self.assertEqual(msg, p.publisher.method_calls[0][1][0])

    @mock.patch('CERNAlarmSystemInterfaceProxy.AlarmPublisher.AlarmPublisher',new=getMockAlarmPublisher)
    def test_publishMessage_with_source(self):
        """CERNAlarmSystemInterfaceProxy publishes alarm after initialization with source"""
        mockAlarmPublisher.reset_mock()
        p = CERNAlarmSystemInterfaceProxy(sourceName='Bonzo')
        msg = ASIMessage([FaultState('Family','Member',1)])
        msg.sourceName = 'msgSource'
        p.publishMessage(msg)
        self.assertEqual('CMW.ALARM_SYSTEM.ALARMS.SOURCES.msgSource',p.topic)
        self.assertEqual(True, p.publisher is not None)
        self.assertEqual('publishAlarm', p.publisher.method_calls[0][0])
        self.assertEqual(msg, p.publisher.method_calls[0][1][0])

    def test_creation_with_Bonzo(self):
        """CERNAlarmSystemInterfaceProxy initialize with given source name"""
        p = CERNAlarmSystemInterfaceProxy(sourceName='Bonzo')
        self.assertEqual(False, p.sourceName is None)
        self.assertEqual('Bonzo', p.sourceName)
        self.assertEqual(False, p.hostName is None)
        self.assertEqual(True, p.publisher is None)
        self.assertEqual(True, p.topic is None)
        
    def test_close_returns_None_after_creation_with_source(self):
        """CERNAlarmSystemInterfaceProxy closes after source name initialization"""
        p = CERNAlarmSystemInterfaceProxy(sourceName='Bonzo')
        self.assertEqual(None, p.close())
        self.assertEqual(True, p.sourceName is None)
        self.assertEqual(True, p.hostName is None)
        self.assertEqual(True, p.publisher is None)
        self.assertEqual(True, p.topic is None)

    @mock.patch('CERNAlarmSystemInterfaceProxy.AlarmPublisher.AlarmPublisher',new=getMockAlarmPublisher)
    def test_close_returns_None_after_message_published(self):
        """CERNAlarmSystemInterfaceProxy closes after source name initialization"""
        p = CERNAlarmSystemInterfaceProxy(sourceName='Bonzo')
        p.publisher = mock.Mock()
        p.topic = 'Topic'
        self.assertEqual(None, p.close())
        self.assertEqual(True, p.sourceName is None)
        self.assertEqual(True, p.hostName is None)
        self.assertEqual(True, p.publisher is None)
        self.assertEqual(True, p.topic is None)

if __name__ == '__main__':
    unittest.main()
