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
# "@(#) $Id: test_Acspy_Clients_SimpleClient.py,v 1.3 2012/07/04 16:45:46 acaproni Exp $"
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
        self.assertEqual(True, sc.name.startswith('Python Client'))
        sc.disconnect()

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
        self.assertEqual(True, sc.name.startswith('Foo'))
        sc.disconnect()

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
        sc = SimpleClient.PySimpleClient.getInstance('Foo')
        self.assertEqual(sc, SimpleClient._instance)
        SimpleClient._instance = None

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
        self.assertEqual(True, sc.isLoggedIn())
        sc.disconnect()
        self.assertEqual(False, sc.isLoggedIn())
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
        sc.disconnect()
        self.assertEqual(True, SimpleClient._instance is not None)
        self.assertEqual(False, lgmock.logInfo.called)
        SimpleClient._instance = None

if __name__ == '__main__':
    unittest.main()

#
# ___oOo___
