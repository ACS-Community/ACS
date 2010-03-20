#! /usr/bin/env python
#******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2010 
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
# "@(#) $Id: test_Acspy_Nc_CDBProperties.py,v 1.2 2010/03/20 22:46:40 agrimstrup Exp $"
#
# who         when        what
# --------    --------    ----------------------------------------------
# agrimstrup  2010-02-05  created
#

#------------------------------------------------------------------------------
__revision__ = "$Id: test_Acspy_Nc_CDBProperties.py,v 1.2 2010/03/20 22:46:40 agrimstrup Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
import unittest
import mock
import CORBA
import CosNotification
import Acspy.Nc.CDBProperties as CDBP

class TestBase(unittest.TestCase):

    def setUp(self):
        CDBP._cdb_access = mock.Mock(spec=CDBP.CDBaccess)

    def tearDown(self):
        CDBP._cdb_access = None

    def set_element_return_value(self, value):
        CDBP._cdb_access.getElement.return_value = value

    def set_element_exception(self, exception):
        def raiser():
            raise exception()

        CDBP._cdb_access.getElement.side_effect = raiser

    def set_field_return_value(self, value):
        CDBP._cdb_access.getField.return_value = value

    def set_field_exception(self, exception):
        def raiser():
            raise exception()

        CDBP._cdb_access.getField.side_effect = raiser


class TestGetNotificationServiceMapping(TestBase):
    
    def test_ok(self):
        self.set_element_return_value(['Stuff'])
        self.assertEqual(['Stuff'],
                         CDBP.get_notification_service_mapping('Channel'))

    def test_exception_handling(self):
        self.set_element_exception(Exception)
        self.assertEqual([], CDBP.get_notification_service_mapping('Channel'))


class TestChannelConfigExists(TestBase):
    
    def test_ok(self):
        self.set_field_return_value(['Stuff'])
        self.assertEqual(1, CDBP.cdb_channel_config_exists('Channel'))

    def test_exception_handling(self):
        self.set_field_exception(Exception)
        self.assertEqual(0, CDBP.cdb_channel_config_exists('Channel'))


class TestGetIntegrationLogs(TestBase):

    def test_log_exists(self):
        CDBP.INTEGRATION_LOGS = { 'Key':True }
        self.assertEqual(True, CDBP.get_integration_logs('Key'))
        CDBP.INTEGRATION_LOGS = {}

    def test_no_channel(self):
        self.set_field_exception(Exception)
        self.assertEqual(0, CDBP.get_integration_logs('Key'))

    def test_channel_false(self):
        self.set_element_return_value([{"IntegrationLogs":"false"}])
        self.set_field_return_value(True)
        self.assertEqual(0, CDBP.get_integration_logs('Key'))
        CDBP.INTEGRATION_LOGS = {}
        
    def test_channel_true(self):
        self.set_element_return_value([{"IntegrationLogs":"true"}])
        self.set_field_return_value(True)
        self.assertEqual(1, CDBP.get_integration_logs('Key'))
        CDBP.INTEGRATION_LOGS = {}
        
    def test_channel_unknown(self):
        self.set_element_return_value([{"IntegrationLogs":"frob"}])
        self.set_field_return_value(True)
        self.assertEqual(0, CDBP.get_integration_logs('Key'))
        CDBP.INTEGRATION_LOGS = {}
        

class TestGetChannelQofSProps(TestBase):

    def test_qofs_start_false_stop_false(self):
        self.set_element_return_value([{"DiscardPolicy":"AnyOrder",
                                        "EventReliability":"BestEffort",
                                        "ConnectionReliability":"BestEffort",
                                        "Priority":"0",
                                        "Timeout":"0",
                                        "OrderPolicy":"AnyOrder",
                                        "StartTimeSupported":"false",
                                        "StopTimeSupported":"false",
                                        "MaxEventsPerConsumer":"0"}])
        self.set_field_return_value(True)
        self.assertEqual(5,
                         len(CDBP.get_channel_qofs_props('Key')))
        
    def test_qofs_start_true_stop_true(self):
        self.set_element_return_value([{"DiscardPolicy":"AnyOrder",
                                        "EventReliability":"BestEffort",
                                        "ConnectionReliability":"BestEffort",
                                        "Priority":"0",
                                        "Timeout":"0",
                                        "OrderPolicy":"AnyOrder",
                                        "StartTimeSupported":"true",
                                        "StopTimeSupported":"true",
                                        "MaxEventsPerConsumer":"0"}])
        self.set_field_return_value(True)
        self.assertEqual(5,
                            len(CDBP.get_channel_qofs_props('Key')))
        
    def test_empty_config(self):
        self.set_element_return_value([{}])
        self.set_field_return_value(True)
        self.assertRaises(KeyError,
                          CDBP.get_channel_qofs_props,
                          'Key')
        
class TestGetChannelAdminProps(TestBase):

    def test_reject_false(self):
        self.set_element_return_value([{"MaxQueueLength":"0",
                                        "MaxConsumers":"0",
                                        "MaxSuppliers":"0",
                                        "RejectNewEvents":"false"}])
        self.set_field_return_value(True)
        self.assertEqual(4,
                         len(CDBP.get_channel_admin_props('Key')))
        
    def test_reject_true(self):
        self.set_element_return_value([{"MaxQueueLength":"0",
                                        "MaxConsumers":"0",
                                        "MaxSuppliers":"0",
                                        "RejectNewEvents":"true"}])
        self.set_field_return_value(True)
        self.assertEqual(4,
                            len(CDBP.get_channel_admin_props('Key')))
        
    def test_empty_config(self):
        self.set_element_return_value([{}])
        self.set_field_return_value(True)
        self.assertRaises(KeyError,
                          CDBP.get_channel_admin_props,
                          'Key')
        

class TestGetEventHandlerTimeoutDict(TestBase):
    
    def test_no_channel(self):
        self.set_field_exception(Exception)
        self.assertEqual({}, CDBP.getEventHandlerTimeoutDict('Key'))

    def test_no_events(self):
        self.set_field_return_value( \
            '<?xml version="1.0" encoding="ISO-8859-1"?>' \
            '<EventChannel xmlns="urn:schemas-cosylab-com:EventChannel:1.0"' \
            ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' \
            ' IntegrationLogs="true">' \
            '</EventChannel>')
        self.assertEqual({}, CDBP.getEventHandlerTimeoutDict('Key'))
	
    def test_events(self):
        self.set_field_return_value( \
            '<?xml version="1.0" encoding="ISO-8859-1"?>' \
            '<EventChannel xmlns="urn:schemas-cosylab-com:EventChannel:1.0"' \
            ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' \
            ' IntegrationLogs="true">' \
            '  <Events>' \
            '    <_ Name="EventDescription" MaxProcessTime="1.25" />' \
            '  </Events>' \
            '</EventChannel>')
        self.assertEqual({'EventDescription': 1.25},
                         CDBP.getEventHandlerTimeoutDict('Key'))
	
    
if __name__ == "__main__":
    unittest.main()


#
# ___oOo___
