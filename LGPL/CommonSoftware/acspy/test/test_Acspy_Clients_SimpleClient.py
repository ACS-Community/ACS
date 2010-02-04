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
# "@(#) $Id: test_Acspy_Clients_SimpleClient.py,v 1.1 2010/02/04 21:45:16 agrimstrup Exp $"
#
# who         when        what
# --------    --------    ----------------------------------------------
# agrimstrup  2010-01-25  created
#

import unittest
import mock
import maci
import Acspy.Util.ACSCorba
import Acspy.Common.Log
import Acspy.Clients.BaseClient
import Acspy.Servants.ContainerServices
import Acspy.Clients.SimpleClient as SimpleClient

def manager_login_value_builder():
    mockmanager = mock.Mock(spec=maci._objref_Manager)
    mocktoken = mock.Mock(spec=maci.ClientInfo)
    mocktoken.h = 1
    mockmanager.login.return_value = mocktoken 
    return mockmanager

class TestSimpleClient(unittest.TestCase):

    @mock.patch_object(Acspy.Util.ACSCorba, 'getManager',
                       manager_login_value_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_login_value_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Servants.ContainerServices, 'CDBaccess')
    @mock.patch_object(Acspy.Common.Log, 'getLogger')
    def test_init_no_name(self, lgmock, cdbmock, refmock):
        refmock.return_value = True
        sc = SimpleClient.PySimpleClient()
        self.assertEqual('Python Client', sc.name)

    @mock.patch_object(Acspy.Util.ACSCorba, 'getManager',
                       manager_login_value_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_login_value_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Servants.ContainerServices, 'CDBaccess')
    @mock.patch_object(Acspy.Common.Log, 'getLogger')
    def test_init_name(self, lgmock, cdbmock, refmock):
        refmock.return_value = True
        sc = SimpleClient.PySimpleClient('Foo')
        self.assertEqual('Foo', sc.name)

    @mock.patch_object(Acspy.Util.ACSCorba, 'getManager',
                       manager_login_value_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_login_value_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Servants.ContainerServices, 'CDBaccess')
    @mock.patch_object(Acspy.Common.Log, 'getLogger')
    def test_getInstance_first(self, lgmock, cdbmock, refmock):
        refmock.return_value = True
        self.assertEqual(True, SimpleClient._instance is None)
        sc = SimpleClient.PySimpleClient.getInstance('Foo')
        self.assertEqual(SimpleClient._instance, sc)
        self.assertEqual(1, SimpleClient._myInstanceCount)

    @mock.patch_object(Acspy.Util.ACSCorba, 'getManager',
                       manager_login_value_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_login_value_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Servants.ContainerServices, 'CDBaccess')
    @mock.patch_object(Acspy.Common.Log, 'getLogger')
    def test_getInstance_later(self, lgmock, cdbmock, refmock):
        refmock.return_value = True
        SimpleClient._instance = True
        SimpleClient._myInstanceCount = 1 
        sc = SimpleClient.PySimpleClient.getInstance('Foo')
        self.assertEqual(sc, SimpleClient._instance)
        self.assertEqual(2, SimpleClient._myInstanceCount)
        SimpleClient._instance = None
        SimpleClient._myInstanceCount = 0

    @mock.patch_object(Acspy.Util.ACSCorba, 'getManager',
                       manager_login_value_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_login_value_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Servants.ContainerServices, 'CDBaccess')
    @mock.patch_object(Acspy.Common.Log, 'getLogger')
    def test_disconnect_first(self, lgmock, cdbmock, refmock):
        refmock.return_value = True
        sc = SimpleClient.PySimpleClient.getInstance('Foo')
        sc.disconnect()
        self.assertEqual(0, SimpleClient._myInstanceCount)
        self.assertEqual(True, SimpleClient._instance is None)
        self.assertEqual(False, lgmock.logInfo.called)

    @mock.patch_object(Acspy.Util.ACSCorba, 'getManager',
                       manager_login_value_builder)
    @mock.patch_object(Acspy.Clients.BaseClient, 'getManager',
                       manager_login_value_builder)
    @mock.patch_object(Acspy.Clients.BaseClient.BaseClient, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Servants.ContainerServices, 'CDBaccess')
    @mock.patch_object(Acspy.Common.Log, 'getLogger')
    def test_disconnect_later(self, lgmock, cdbmock, refmock):
        refmock.return_value = True
        sc = SimpleClient.PySimpleClient.getInstance('Foo')
        SimpleClient._myInstanceCount = 2
        sc.disconnect()
        self.assertEqual(True, SimpleClient._instance is not None)
        self.assertEqual(1, SimpleClient._myInstanceCount)
        self.assertEqual(False, lgmock.logInfo.called)
        SimpleClient._instance = None
        SimpleClient._myInstanceCount = 0


if __name__ == '__main__':
    unittest.main()

#
# ___oOo___
