#! /usr/bin/env python
#*******************************************************************************
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
# "@(#) $Id: test_Acspy_Clients_BaseClient.py,v 1.2 2010/06/08 01:55:25 agrimstrup Exp $"
#
# who         when        what
# --------    --------    ----------------------------------------------
# agrimstrup  2010-01-18  created
#

import unittest
import mock
import maci
import Acspy.Common.Log
import Acspy.Common.TimeHelper
import Acspy.Clients.BaseClient
from ACSErrTypeCommonImpl import CORBAProblemExImpl

def manager_login_value_builder():
    mockmanager = mock.Mock(spec=maci._objref_Manager)
    mockmanager.login.return_value = 1
    return mockmanager

def manager_login_none_builder():
    mockmanager = mock.Mock(spec=maci._objref_Manager)
    mockmanager.login.return_value = None
    return mockmanager

def manager_exception_builder():
    def raiser(*args):
        raise Exception()
    
    mockmanager = mock.Mock(spec=maci._objref_Manager)
    mockmanager.login.side_effect = raiser
    mockmanager.logout.side_effect = raiser
    return mockmanager

def manager_logout_exception_builder():
    def raiser(*args):
        raise Exception()
    
    mockmanager = mock.Mock(spec=maci._objref_Manager)
    mockmanager.logout.side_effect = raiser
    return mockmanager

def logger_builder(name):
    return mock.Mock(spec=Acspy.Common.Log.Logger)

def timestamp_builder():
    mockts = mock.Mock(spec=Acspy.Common.TimeHelper.TimeUtil)
    mockts.value = 134831582610597120L
    return mockts
    
def corba_builder():
    def raiser(*args):
        raise Exception()
    
    mockservant = mock.Mock()
    mockservant.side_effect = raiser
    return mockservant

class TestBaseClient(unittest.TestCase):

    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger')
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_login_value_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    def test_init_no_logger(self, myrefmock, logmock):
        logmock.return_value = True
        myrefmock.return_value = True
        try:
            bc = Acspy.Clients.BaseClient.BaseClient()
        except:
            pass
        self.assertEqual(True, bc.logger)

    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger')
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_exception_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    def test_init_manager_login_exception(self, myrefmock, logmock):
        logmock.return_value = True
        myrefmock.return_value = True
        self.assertRaises(CORBAProblemExImpl, Acspy.Clients.BaseClient.BaseClient)

    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger')
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_login_none_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    def test_init_manager_login_no_token(self, myrefmock, logmock):
        logmock.return_value = True
        myrefmock.return_value = True
        self.assertRaises(CORBAProblemExImpl, Acspy.Clients.BaseClient.BaseClient)

#     @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger')
#     @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
#                        manager_login_value_builder)
#     @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
#     def test_init_name_setting(self, myrefmock, logmock):
#         logmock.return_value = True
#         myrefmock.return_value = True
#         try:
#             bc = Acspy.Clients.BaseClient.BaseClient('Foo')
#         except:
#             pass
#         self.assertEqual('Foo', bc._get_name())

    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger', logger_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_login_value_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    def test_disconnect_ok(self, myrefmock):
        myrefmock.return_value = True
        bc = Acspy.Clients.BaseClient.BaseClient('Foo')
        self.assertEqual(True, bc.disconnect() is None)

    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger', logger_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_logout_exception_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    def test_disconnect_fault(self, myrefmock):
        myrefmock.return_value = True
        bc = Acspy.Clients.BaseClient.BaseClient('Foo')
        self.assertEqual(True, bc.disconnect() is None)
        self.assertEqual(True, bc.logger.logInfo.called)

    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger', logger_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_logout_exception_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Clients.BaseClient, 'getTimeStamp', timestamp_builder)
    def test_authenticate(self, myrefmock):
        myrefmock.return_value = True
        bc = Acspy.Clients.BaseClient.BaseClient('Foo')
        expected = maci.AuthenticationData(answer='CFoo',
                                                 client_type=maci.CLIENT_TYPE,
                                                 impl_lang=maci.PYTHON,
                                                 recover=False,
                                                 timestamp=134831582610597120L,
                                                 execution_id=15)
        returned = bc.authenticate(15, None)
        self.assertEqual(expected.answer, returned.answer)
        self.assertEqual(expected.client_type, returned.client_type)
        self.assertEqual(expected.impl_lang, returned.impl_lang)
        self.assertEqual(expected.recover, returned.recover)
        self.assertEqual(expected.timestamp, returned.timestamp)
        self.assertEqual(expected.execution_id, returned.execution_id)

    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger', logger_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_logout_exception_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    def test_message_error(self, myrefmock):
        myrefmock.return_value = True
        bc = Acspy.Clients.BaseClient.BaseClient('Foo')
        bc.message(maci.Client.MSG_ERROR, 'Error')
        self.assertEqual(True, bc.logger.logWarning.called)
        self.assertEqual('Error message from the manager: Error',
                         bc.logger.logWarning.call_args[0][0])
        
    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger', logger_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_logout_exception_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    def test_message_information(self, myrefmock):
        myrefmock.return_value = True
        bc = Acspy.Clients.BaseClient.BaseClient('Foo')
        bc.message(maci.Client.MSG_INFORMATION, 'Info')
        self.assertEqual(True, bc.logger.logInfo.called)
        self.assertEqual('Info message from the manager: Info',
                         bc.logger.logInfo.call_args[0][0])
        
    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger', logger_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_logout_exception_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    def test_message_unknown(self, myrefmock):
        myrefmock.return_value = True
        bc = Acspy.Clients.BaseClient.BaseClient('Foo')
        bc.message(42, 'Blah')
        self.assertEqual(True, bc.logger.logInfo.called)
        self.assertEqual('Message of unknown type from the manager: Blah',
                         bc.logger.logInfo.call_args[0][0])

    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger', logger_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_logout_exception_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    def test_message_unknown(self, myrefmock):
        myrefmock.return_value = True
        bc = Acspy.Clients.BaseClient.BaseClient('Foo')
        bc.message(42, 'Blah')
        self.assertEqual(True, bc.logger.logInfo.called)
        self.assertEqual('Message of unknown type from the manager: Blah',
                         bc.logger.logInfo.call_args[0][0])

    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger', logger_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_logout_exception_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    def test_components_unavailable_empty_empty(self, myrefmock):
        myrefmock.return_value = True
        bc = Acspy.Clients.BaseClient.BaseClient('Foo')
        bc.components_unavailable([])
        self.assertEqual([], bc.managerComponents)

    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger', logger_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_logout_exception_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    def test_components_unavailable_empty_data(self, myrefmock):
        myrefmock.return_value = True
        bc = Acspy.Clients.BaseClient.BaseClient('Foo')
        bc.components_unavailable([])
        bc.managerComponents = ['foo', 'bar', 'quux']
        self.assertEqual(['foo', 'bar', 'quux'], bc.managerComponents)

    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger', logger_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_logout_exception_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    def test_components_unavailable_data_empty(self, myrefmock):
        myrefmock.return_value = True
        bc = Acspy.Clients.BaseClient.BaseClient('Foo')
        bc.components_unavailable(['foo', 'bar', 'quux'])
        self.assertEqual([], bc.managerComponents)

    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger', logger_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_logout_exception_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    def test_components_unavailable_data_data_nocommon(self, myrefmock):
        myrefmock.return_value = True
        bc = Acspy.Clients.BaseClient.BaseClient('Foo')
        for i in ['bletch', 'sed', 'awk']:
            t = mock.Mock()
            t.name = i
            bc.managerComponents.append(t)
        bc.components_unavailable(['foo', 'bar', 'quux'])
        self.assertEqual(['bletch', 'sed', 'awk'],
                         [ t.name for t in bc.managerComponents])

    @mock.patch_object(Acspy.Clients.BaseClient, 'getLogger', logger_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_logout_exception_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    def test_components_unavailable_data_data_common(self, myrefmock):
        myrefmock.return_value = True
        bc = Acspy.Clients.BaseClient.BaseClient('Foo')
        for i in ['bletch', 'bar', 'sed', 'awk']:
            t = mock.Mock()
            t.name = i
            bc.managerComponents.append(t)
        bc.components_unavailable(['foo', 'bar', 'quux'])
        self.assertEqual(['bletch', 'sed', 'awk'],
                         [ t.name for t in bc.managerComponents])

    
if __name__ == '__main__':
    unittest.main()

#
# ___oOo___
