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
# "@(#) $Id: test_Acspy_Util_ACSCorba.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who         when        what
# --------    --------    ----------------------------------------------
# agrimstrup  2010-02-09  created
#

#------------------------------------------------------------------------------
__revision__ = "$Id: test_Acspy_Util_ACSCorba.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
import sys
import unittest
import mock
import new
import omniORB
#--ACS IMPORTS-----------------------------------------------------------------
import Acspy.Util.ACSCorba as ACSCorba
#------------------------------------------------------------------------------

class TestGetManagerCorbaLoc(unittest.TestCase):

    def setUp(self):
        ACSCorba.MGR_CORBALOC = None

    def tearDown(self):
        ACSCorba.MGR_CORBALOC = None

    def test_manager_exists(self):
        ACSCorba.MGR_CORBALOC = 'foo'
        self.assertEqual('foo', ACSCorba.getManagerCorbaloc())

    def test_new_corbaloc(self):
        self.assertEqual('foo', ACSCorba.getManagerCorbaloc('foo'))

    @mock.patch_object(ACSCorba, 'getIP')
    @mock.patch_object(ACSCorba, 'getManagerPort')
    def test_no_args_or_env(self, managerportmock, ipmock):
        ipmock.return_value = '127.0.0.1'
        managerportmock.return_value = '3000'
        self.assertEqual('corbaloc::127.0.0.1:3000/Manager',
                         ACSCorba.getManagerCorbaloc())
        
    def test_environ(self):
        saveenv = ACSCorba.environ
        ACSCorba.environ = { 'MANAGER_REFERENCE':'corbaloc::127.0.0.2:3000/Manager'}
        self.assertEqual('corbaloc::127.0.0.2:3000/Manager',
                         ACSCorba.getManagerCorbaloc())
        ACSCorba.environ = saveenv

    def test_args(self):
        saveargv = ACSCorba.argv
        ACSCorba.argv = [ '-m', 'corbaloc::127.0.0.3:3000/Manager' ]
        self.assertEqual('corbaloc::127.0.0.3:3000/Manager',
                         ACSCorba.getManagerCorbaloc())
        ACSCorba.argv = saveargv


class TestGetORB(unittest.TestCase):

    def setUp(self):
        ACSCorba.ORB = None
        ACSCorba.POA_MANAGER = None

    def tearDown(self):
        ACSCorba.ORB = None
        ACSCorba.POA_MANAGER = None

    def test_orb_exists(self):
        ACSCorba.ORB = 'foo'
        self.assertEqual('foo', ACSCorba.getORB())

    @mock.patch_object(ACSCorba.CORBA, 'ORB_init')
    def test_init_fault(self, initmock):
        def raiser(*args):
            raise Exception("Boom!")

        initmock.side_effect = raiser
        self.assertEqual(True, ACSCorba.getORB() is None)
        
    @mock.patch_object(ACSCorba.CORBA, 'ORB_init')
    @mock.patch_object(ACSCorba, 'getPOARoot')
    def test_ok(self, initmock, rootmock):
        self.assertEqual(False, ACSCorba.getORB() is None)
        

class TestGetPOARoot(unittest.TestCase):

    def setUp(self):
        ACSCorba.POA_ROOT = None

    def tearDown(self):
        ACSCorba.POA_ROOT = None

    def test_root_exists(self):
        ACSCorba.POA_ROOT = 'foo'
        self.assertEqual('foo', ACSCorba.getPOARoot())

    @mock.patch_object(ACSCorba, 'getORB')
    def test_init_fault(self, getorbmock):
        def raiser(*args):
            raise Exception("Boom!")

        orbmock = mock.Mock(spec=ACSCorba.CORBA.ORB)
        orbmock.resolve_initial_references.side_effect = raiser
        getorbmock.return_value = orbmock
        self.assertEqual(True, ACSCorba.getPOARoot() is None)
        
    @mock.patch_object(ACSCorba, 'getORB')
    def test_ok(self, getorbmock):
        orbmock = mock.Mock(spec=ACSCorba.CORBA.ORB)
        getorbmock.return_value = orbmock
        self.assertEqual(False, ACSCorba.getPOARoot() is None)
        

class TestGetPOAManager(unittest.TestCase):

    def setUp(self):
        ACSCorba.POA_MANAGER = None

    def tearDown(self):
        ACSCorba.POA_MANAGER = None

    def test_manager_exists(self):
        ACSCorba.POA_MANAGER = 'foo'
        self.assertEqual('foo', ACSCorba.getPOAManager())

    @mock.patch_object(ACSCorba, 'getPOARoot')
    def test_init_fault(self, getrootmock):
        def raiser(*args):
            raise Exception("Boom!")

        rootmock = mock.Mock(spec=omniORB.PortableServer.POA)
        rootmock._get_the_POAManager.side_effect = raiser
        getrootmock.return_value = rootmock
        self.assertEqual(True, ACSCorba.getPOAManager() is None)
        
    @mock.patch_object(ACSCorba, 'getPOARoot')
    def test_ok(self, getrootmock):
        rootmock = mock.Mock(spec=omniORB.PortableServer.POA)
        getrootmock.return_value = rootmock
        self.assertEqual(False, ACSCorba.getPOAManager() is None)
        
class TestGetManager(unittest.TestCase):

    def setUp(self):
        ACSCorba.MGR_REF = None
        ACSCorba.MGR_CORBALOC = 'Here'

    def tearDown(self):
        ACSCorba.MGR_REF = None

    def test_manager_exists(self):
        ACSCorba.MGR_REF = 'foo'
        self.assertEqual('foo', ACSCorba.getManager())

    @mock.patch_object(ACSCorba, 'getORB')
    @mock.patch_object(ACSCorba, 'getManagerCorbaloc')
    def test_orb_fault(self, getorbmock, corbalocmock):
        def raiser(*args):
            raise Exception("Boom!")

        orbmock = mock.Mock(spec=ACSCorba.CORBA.ORB)
        orbmock.string_to_object.side_effect = raiser
        getorbmock.return_value = orbmock
        self.assertEqual(True, ACSCorba.getManager() is None)
        
    @mock.patch_object(ACSCorba, 'getORB')
    @mock.patch_object(ACSCorba, 'getManagerCorbaloc')
    def test_orb_no_object(self, corbalocmock, getorbmock):
        orbmock = mock.Mock(spec=ACSCorba.CORBA.ORB)
        orbmock.string_to_object.return_value = None
        getorbmock.return_value = orbmock
        self.assertEqual(True, ACSCorba.getManager() is None)
        
    @mock.patch_object(ACSCorba, 'getManagerCorbaloc')
    @mock.patch_object(ACSCorba.CORBA, 'is_nil')
    @mock.patch_object(ACSCorba, 'getORB')
    def test_obj_fault(self, getorbmock, nilmock, corbalocmock):
        def raiser(*args):
            raise Exception("Boom!")

        nilmock.return_value = False
        objmock = mock.Mock(spec=ACSCorba.CORBA.Object)
        objmock._non_existent.side_effect = raiser
        orbmock = mock.Mock(spec=ACSCorba.CORBA.ORB)
        orbmock.string_to_object.return_value = objmock
        getorbmock.return_value = orbmock
        self.assertEqual(True, ACSCorba.getManager() is None)
        
    @mock.patch_object(ACSCorba, 'getORB')
    @mock.patch_object(ACSCorba.CORBA, 'is_nil')
    @mock.patch_object(ACSCorba, 'getManagerCorbaloc')
    def test_ok(self, getorbmock, nilmock, corbalocmock):
        nilmock.return_value = False
        objmock = mock.Mock(spec=ACSCorba.CORBA.Object)
        orbmock = mock.Mock(spec=ACSCorba.CORBA.ORB)
        orbmock.string_to_object.return_value = objmock
        getorbmock.return_value = orbmock
        self.assertEqual(False, ACSCorba.getManager() is None)
        

class TestGetClient(unittest.TestCase):

    def setUp(self):
        ACSCorba.SINGLETON_CLIENT = None

    def tearDown(self):
        ACSCorba.SINGLETON_CLIENT = None

    def test_client_exists(self):
        ACSCorba.SINGLETON_CLIENT = 'foo'
        self.assertEqual('foo', ACSCorba.getClient())

    @mock.patch_object(ACSCorba, '_Client')
    def test_create_fault(self, clientmock):
        def raiser(*args):
            raise Exception("Boom!")

        clientmock.side_effect = raiser
        self.assertEqual(True, ACSCorba.getClient() is None)
        
    @mock.patch_object(ACSCorba, '_Client')
    def test_ok(self, clientmock):
        self.assertEqual(False, ACSCorba.getClient() is None)

mockManager = mock.Mock()
mockManager.return_value = None

def manager_side_effect(*args):
    global mockManager

    mockManager.return_value = mock.Mock(spec=ACSCorba.maci._objref_Manager)
    mockManager.side_effect = None

class Test_Client(unittest.TestCase):

    def setUp(self):
        self.original = sys.stdout
        sys.stdout = mock.Mock(spec=sys.stdout) 

    def tearDown(self):
        sys.stdout = self.original

    @mock.patch_object(ACSCorba, 'getManager', mockManager)
    @mock.patch_object(ACSCorba, 'getPOAManager')
    @mock.patch_object(ACSCorba._Client, '_this')
    def test_client_init_without_manager(self, corbaRef, poaManager):
        global mockManager

        mockManager.side_effect = manager_side_effect
        client = ACSCorba._Client()
        self.assertEqual(mockManager.return_value, client.mgr)

    @mock.patch_object(ACSCorba, 'getManager')
    @mock.patch_object(ACSCorba, 'getPOAManager')
    @mock.patch_object(ACSCorba._Client, '_this')
    def test_client_init_pydoc(self, corbaRef, poaManager, manager):
        manager.return_value = None

        sys.argv.insert(0, 'pydoc')
        mockManager.side_effect = manager_side_effect
        client = ACSCorba._Client()
        sys.argv.pop()
        self.assertEqual(True , client.token is None)

    
class Test_ClientMethods(unittest.TestCase):

    @mock.patch_object(ACSCorba, 'getManager')
    @mock.patch_object(ACSCorba, 'getPOAManager')
    @mock.patch_object(ACSCorba._Client, '_this')
    def setUp(self, corbaRef, poaManager, manager):
        manager.return_value = mock.Mock(spec=ACSCorba.maci._objref_Manager)
        corbaRef.return_value = mock.Mock(spec=ACSCorba.maci__POA.omniORB.PortableServer.POA)
        self.client = ACSCorba._Client()

    def tearDown(self):
        pass

    def test_client_getService(self):
        comp = self.client.getService('curl://component')
        self.assertEqual(False ,  comp is None)

    def test_client_getservice_none_component(self):
        self.client.mgr.get_service.return_value = None
        
        comp = self.client.getService('curl://component')
        self.assertEqual(True ,  comp is None)
        
    def test_client_getservice_none_manager(self):
        self.client.mgr = None
        
        comp = self.client.getService('curl://component')
        self.assertEqual(True ,  comp is None)


if __name__ == "__main__":
    unittest.main()


#
# ___oOo___
