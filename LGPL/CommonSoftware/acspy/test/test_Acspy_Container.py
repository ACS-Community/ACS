#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2009 
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
# "@(#) $Id: test_Acspy_Container.py,v 1.4 2010/06/08 01:55:25 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2009-01-09  created
#

#--REGULAR IMPORTS-------------------------------------------------------------
import unittest
import mock
import new
from os import environ
import sys
import omniORB
import CDB


#--ACS Imports-----------------------------------------------------------------
from Acspy.Util import LoggingConfig_xsd
import Acspy.Util.ACSCorba
import maci
import Acspy.Common.CDBAccess
import Acspy.Container


class TestInitialization(unittest.TestCase):

    def setUp(self):
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = mock.Mock(spec=Acspy.Util.ACSCorba._Client)

    def tearDown(self):
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = None

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    def test_container_poa_object_fault(self, confcorbamock):
        def raiser(*args):
            raise Acspy.Container.CouldntCreateObjectExImpl()

        confcorbamock.side_effect = raiser
        self.assertRaises(Acspy.Container.CouldntCreateObjectExImpl,
                          Acspy.Container.Container,
                          'TestContainer')

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    def test_corba_ref_fault(self, confcorbamock):
        def raiser(*args):
            raise Acspy.Container.CORBAProblemExImpl()

        confcorbamock.side_effect = raiser
        self.assertRaises(Acspy.Container.CORBAProblemExImpl,
                          Acspy.Container.Container,
                          'TestContainer')

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container, 'BaseClient')
    def test_client_init_fault(self, confcofbamock, baseclientmock):
        def raiser(*args):
            raise Acspy.Container.CORBAProblemExImpl()

        baseclientmock.side_effect = raiser
        self.assertRaises(Acspy.Container.CORBAProblemExImpl,
                          Acspy.Container.Container,
                          'TestContainer')

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container, 'CDBaccess')
    def test_cdbaccess_fault(self, cdbmock, baseclientmock, confcorbamock):
        def raiser(*args):
            raise Acspy.Container.CORBAProblemExImpl()

        cdbmock.side_effect = raiser
        self.assertRaises(Acspy.Container.CORBAProblemExImpl,
                          Acspy.Container.Container,
                          'TestContainer')



    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container, 'CDBaccess')
    def test_getCDBInfo_no_container_info(self, cdbmock, baseclientmock,
                                          confcorbamock):
        def raiser(*args):
            raise Acspy.Container.CORBAProblemExImpl()

        cdbifmock = mock.Mock(spec=Acspy.Common.CDBAccess.CDBaccess)
        cdbifmock.getElement.side_effect = raiser
        cdbmock.return_value = cdbifmock
        c = Acspy.Container.Container('TestContainer')
        self.assertEqual({}, c.cdbContainerInfo)

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container, 'CDBaccess')
    def test_getCDBInfo_no_package_info(self, cdbmock, baseclientmock,
                                          confcorbamock):
        cdbifmock = mock.Mock(spec=Acspy.Common.CDBAccess.CDBaccess)
        cdbifmock.getElement.return_value = []
        cdbmock.return_value = cdbifmock
        c = Acspy.Container.Container('TestContainer')
        self.assertEqual([], c.autoLoadPackages)

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container, 'CDBaccess')
    @mock.patch_object(Acspy.Container, 'findFile')
    def test_getCDBInfo_no_package_found(self, findfilemock, cdbmock,
                                          baseclientmock, confcorbamock):

        findfilemock.return_value = ("","")
        cdbifmock = mock.Mock(spec=Acspy.Common.CDBAccess.CDBaccess)
        cdbifmock.getElement.return_value = [{ 'string':'package1'},
                                             {'string':'package2'}]
        cdbmock.return_value = cdbifmock
        c = Acspy.Container.Container('TestContainer')
        self.assertEqual([], c.autoLoadPackages)

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container, 'CDBaccess')
    @mock.patch_object(Acspy.Container, 'findFile')
    def test_getCDBInfo_package_found(self, findfilemock, cdbmock,
                                      baseclientmock, confcorbamock):

        findfilemock.return_value = ("bin/package1","")
        cdbifmock = mock.Mock(spec=Acspy.Common.CDBAccess.CDBaccess)
        cdbifmock.getElement.return_value = [{ 'string':'package1'}]
        cdbmock.return_value = cdbifmock
        c = Acspy.Container.Container('TestContainer')
        self.assertEqual(["bin/package1"], c.autoLoadPackages)
        Acspy.Container.Log.stopPeriodicFlush()


class TestGetMyCorbaRef(unittest.TestCase):

    def setUp(self):
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = mock.Mock(spec=Acspy.Util.ACSCorba._Client)


    def tearDown(self):
        Acspy.Container.Log.stopPeriodicFlush()
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = None

    @mock.patch_object(Acspy.Container.ACSCorba, 'getPOARoot')
    @mock.patch_object(Acspy.Container, 'CDBaccess')
    def test_getMyCorbaRef_no_ref(self, cdbmock, poarootmock):
        def raiser(*args):
            raise Exception()

        contpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        contpoamock.servant_to_reference.side_effect = raiser
        contpoamock.activate_object_with_id.return_value = None

        poaobjmock = mock.Mock(spec=omniORB.PortableServer.POA)
        poaobjmock.create_id_assignment_policy.return_value = \
              mock.Mock(spec=omniORB.PortableServer.IdAssignmentPolicy)
        poaobjmock.create_lifespan_policy.return_value = \
              mock.Mock(spec=omniORB.PortableServer.LifespanPolicy)
        poaobjmock.create_request_processing_policy.return_value = \
              mock.Mock(spec=omniORB.PortableServer.RequestProcessingPolicy)
        poaobjmock.create_servant_retention_policy.return_value = \
              mock.Mock(spec=omniORB.PortableServer.ServantRetentionPolicy)
        poaobjmock.create_POA.return_value = contpoamock

        poarootmock.return_value = poaobjmock
        
        self.assertRaises(Acspy.Container.CORBAProblemExImpl,
                          Acspy.Container.Container,
                          'TestContainer')


    @mock.patch_object(Acspy.Container.ACSCorba, 'getPOARoot')
    @mock.patch_object(Acspy.Container, 'CDBaccess')
    def test_getMyCorbaRef_bad_ref(self, cdbmock, poarootmock):
        def raiser(*args):
            raise Exception()

        contpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        contpoamock.servant_to_reference.return_value = None
        contpoamock.activate_object_with_id.return_value = None

        poaobjmock = mock.Mock(spec=omniORB.PortableServer.POA)
        poaobjmock.create_id_assignment_policy.return_value = \
              mock.Mock(spec=omniORB.PortableServer.IdAssignmentPolicy)
        poaobjmock.create_lifespan_policy.return_value = \
              mock.Mock(spec=omniORB.PortableServer.LifespanPolicy)
        poaobjmock.create_request_processing_policy.return_value = \
              mock.Mock(spec=omniORB.PortableServer.RequestProcessingPolicy)
        poaobjmock.create_servant_retention_policy.return_value = \
              mock.Mock(spec=omniORB.PortableServer.ServantRetentionPolicy)
        poaobjmock.create_POA.return_value = contpoamock

        poarootmock.return_value = poaobjmock
        
        self.assertRaises(Acspy.Container.CORBAProblemExImpl,
                          Acspy.Container.Container,
                          'TestContainer')



class TestActivateComponent(unittest.TestCase):

    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def setUp(self, confcorbamock, refmock, cdbmock, baseclientmock):
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = mock.Mock(spec=Acspy.Util.ACSCorba._Client)
        self.tc = Acspy.Container.Container('TestContainer')
        self.tc.token = mock.Mock()
        self.tc.token.h = 123435
        self.tc.createPOAForComponent = mock.Mock()

    def tearDown(self):
        Acspy.Container.Log.stopPeriodicFlush()
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = None


    def test_already_exists(self):
        self.tc.components['TestComponent'] = { 'COMPONENTINFO':True }
        self.assertEqual(True, self.tc.activate_component(12345,
                                                      12,
                                                      'TestComponent',
                                                      'TestComponent',
                                                      'testcomponent.idl'))

    def test_poa_create_fault(self):
        def raiser(*args):
            raise Exception('Boom!')
        self.tc.createPOAForComponent.side_effect = raiser
        self.assertEqual(True, self.tc.activate_component(12345,
                                                     12,
                                                     'Test', 'Test',
                                                     'test.idl') is None)

    def test_offshoot_poa_create_fault(self):
        def raiser(*args):
            raise Exception('Boom!')
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.side_effect = raiser
        self.tc.createPOAForComponent.return_value = offshootpoamock 
        self.assertEqual(True, self.tc.activate_component(12345,
                                                     12,
                                                     'Test', 'Test',
                                                     'test.idl') is None)

    def test_bad_type_name_fault(self):
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        self.tc.createPOAForComponent.return_value = offshootpoamock 
        self.assertEqual(True, self.tc.activate_component(12345,
                                                     12,
                                                     'Test', 'Test',
                                                     'test.idl') is None)

    def test_import_fault(self):
        mockmodule = new.module('MockModule')
        sys.modules['MockModule'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        self.tc.createPOAForComponent.return_value = offshootpoamock 
        self.assertRaises(Acspy.Container.CannotActivateComponentExImpl,
                          self.tc.activate_component,
                          12345, 12, 'Test', 'Test',
                          'IDL:alma/acspytest/MockModule:1.0')
        del sys.modules['MockModule']

    def test_bad_class_fault(self):
        mockmodule = new.module('MockClass')
        sys.modules['MockModule'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        self.tc.createPOAForComponent.return_value = offshootpoamock
        self.assertRaises(Acspy.Container.CannotActivateComponentExImpl,
                          self.tc.activate_component,
                          12345, 12, 'Test', 'MockClass',
                          'IDL:alma/acspytest/MockClass:1.0')
        del sys.modules['MockModule']

    @mock.patch_object(Acspy.Container, 'instance')
    def test_bad_class_type_fault(self, instancemock):
        def raiser(*args):
            raise TypeError("boom!")
            
        mockmodule = new.module('MockClass')
        sys.modules['MockClass'] = mockmodule
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (object,), {})
        instancemock.side_effect = raiser
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        self.tc.createPOAForComponent.return_value = offshootpoamock
        self.assertRaises(Acspy.Container.CannotActivateComponentExImpl,
                          self.tc.activate_component,
                          12345, 12, 'Test', 'MockClass',
                          'IDL:alma/acspytest/MockClass:1.0')
        del sys.modules['MockClass']

    def test_constructor_fault(self):
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass', (), {})
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        self.tc.createPOAForComponent.return_value = offshootpoamock
        self.assertEqual(True, 
                         self.tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

    def test_dummy_class(self):
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices,
                                                         Acspy.Container.ACSComponent,
                                                         Acspy.Container.ComponentLifecycle), {})
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        self.tc.createPOAForComponent.return_value = offshootpoamock
        self.assertEqual(True, 
                         self.tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

    def test_bad_initialization(self):
        def initialize(self):
            self.componentState = Acspy.Container.ACS.COMPSTATE_NEW
            
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices,
                                                         Acspy.Container.ACSComponent,
                                                         Acspy.Container.ComponentLifecycle), {})
        mockmodule.__dict__['MockClass'].initialize = initialize
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        self.tc.createPOAForComponent.return_value = offshootpoamock
        self.assertEqual(True, 
                         self.tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

    def test_init_lifecycle_exception(self):
        def initialize(self):
            raise Acspy.Container.ComponentLifecycleException('Boom!')
            
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices,
                                                         Acspy.Container.ACSComponent,
                                                         Acspy.Container.ComponentLifecycle), {})
        mockmodule.__dict__['MockClass'].initialize = initialize
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        self.tc.createPOAForComponent.return_value = offshootpoamock
        self.assertEqual(True, 
                         self.tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

    def test_init_unexpected_exception(self):
        def initialize(self):
            raise Exception('Boom!')
            
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices,
                                                         Acspy.Container.ACSComponent,
                                                         Acspy.Container.ComponentLifecycle), {})
        mockmodule.__dict__['MockClass'].initialize = initialize
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        self.tc.createPOAForComponent.return_value = offshootpoamock
        self.assertEqual(False, 
                         self.tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

    def test_exec_lifecycle_exception(self):
        def execute(self):
            raise Acspy.Container.ComponentLifecycleException('Boom!')
            
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices,
                                                         Acspy.Container.ACSComponent,
                                                         Acspy.Container.ComponentLifecycle), {})
        mockmodule.__dict__['MockClass'].execute = execute
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        self.tc.createPOAForComponent.return_value = offshootpoamock
        self.assertEqual(True, 
                         self.tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

    def test_exec_unexpected_exception(self):
        def execute(self):
            raise Exception('Boom!')
            
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices,
                                                         Acspy.Container.ACSComponent,
                                                         Acspy.Container.ComponentLifecycle), {})
        mockmodule.__dict__['MockClass'].execute = execute
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        self.tc.createPOAForComponent.return_value = offshootpoamock
        self.assertEqual(False, 
                         self.tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

    def test_corba_servant_fault(self):
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices,
                                                         Acspy.Container.ACSComponent,
                                                         Acspy.Container.ComponentLifecycle), {})
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.servant_to_reference.return_value = None
        self.tc.createPOAForComponent.return_value = offshootpoamock
        self.assertEqual(True, 
                         self.tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is None)
        del sys.modules['MockClass']

    def test_corba_exception(self):
        def raiser(*args):
            raise Exception("Boom!")
        
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices,
                                                         Acspy.Container.ACSComponent,
                                                         Acspy.Container.ComponentLifecycle), {})
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.activate_object_with_id.side_effect = raiser
        self.tc.createPOAForComponent.return_value = offshootpoamock
        self.assertEqual(True, 
                         self.tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is None)
        del sys.modules['MockClass']

    def test_existing_module(self):
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices,
                                                         Acspy.Container.ACSComponent,
                                                         Acspy.Container.ComponentLifecycle), {})
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        self.tc.createPOAForComponent.return_value = offshootpoamock
        self.tc.compModuleCount[mockmodule] = 1
        self.assertEqual(True, 
                         self.tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

class TestDeactivation(unittest.TestCase):

    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def setUp(self, confcorbamock, refmock, cdbmock, baseclientmock):
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = mock.Mock(spec=Acspy.Util.ACSCorba._Client)
        self.testcontainer = Acspy.Container.Container('TestContainer')
        self.testcontainer.token = mock.Mock()
        self.testcontainer.token.h = 123435
        self.testcontainer.__init__ = mock.Mock()

    def tearDown(self):
        Acspy.Container.Log.stopPeriodicFlush()
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = None
        

    def test_not_found(self):
        self.assertEqual(True, self.testcontainer.deactivate_components([1]) is None)


    def test_not_component(self):
        componentdata = {}
        componentdata[Acspy.Container.HANDLE] = 1
        componentdata[Acspy.Container.NAME] = 'TestComponent'
        componentdata[Acspy.Container.CORBAREF] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.POAOFFSHOOT] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.POA] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.PYREF] = new.classobj('MockClass', (), {})() 
        componentdata[Acspy.Container.COMPMODULE] = new.module('MockClass')
        self.testcontainer.compHandles[1] = componentdata[Acspy.Container.NAME]
        self.testcontainer.components[componentdata[Acspy.Container.NAME]] = componentdata
        self.testcontainer.compModuleCount[componentdata[Acspy.Container.COMPMODULE]] = 2
        self.assertEqual(True, self.testcontainer.deactivate_components([1]) is None)


    def test_component(self):
        componentdata = {}
        componentdata[Acspy.Container.HANDLE] = 1
        componentdata[Acspy.Container.NAME] = 'TestComponent'
        componentdata[Acspy.Container.CORBAREF] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.POAOFFSHOOT] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.POA] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.PYREF] = new.classobj('MockClass', (Acspy.Container.ContainerServices,
                                                                          Acspy.Container.ACSComponent,
                                                                          Acspy.Container.ComponentLifecycle), {})()
        componentdata[Acspy.Container.COMPMODULE] = new.module('MockClass')
        self.testcontainer.compHandles[1] = componentdata[Acspy.Container.NAME]
        self.testcontainer.components[componentdata[Acspy.Container.NAME]] = componentdata
        self.testcontainer.compModuleCount[componentdata[Acspy.Container.COMPMODULE]] = 2
        self.assertEqual(True, self.testcontainer.deactivate_components([1]) is None)

    def test_module_reload(self):
        componentdata = {}
        componentdata[Acspy.Container.HANDLE] = 1
        componentdata[Acspy.Container.NAME] = 'TestComponent'
        componentdata[Acspy.Container.CORBAREF] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.POAOFFSHOOT] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.POA] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.PYREF] = new.classobj('MockClass', (Acspy.Container.ContainerServices,
                                                                          Acspy.Container.ACSComponent,
                                                                          Acspy.Container.ComponentLifecycle), {})()
        componentdata[Acspy.Container.COMPMODULE] = new.module('MockClass')
        self.testcontainer.compHandles[1] = componentdata[Acspy.Container.NAME]
        self.testcontainer.components[componentdata[Acspy.Container.NAME]] = componentdata
        self.testcontainer.compModuleCount[componentdata[Acspy.Container.COMPMODULE]] = 1
        self.assertEqual(True, self.testcontainer.deactivate_components([1]) is None)

class TestGetComponentInfo(unittest.TestCase):
    
    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def setUp(self, confcorbamock, refmock, cdbmock, baseclientmock):
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = mock.Mock(spec=Acspy.Util.ACSCorba._Client)
        self.testcontainer = Acspy.Container.Container('TestContainer')
        self.testcontainer.token = mock.Mock()
        self.testcontainer.token.h = 123435
        self.testcontainer.__init__ = mock.Mock()

    def tearDown(self):
        Acspy.Container.Log.stopPeriodicFlush()
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = None

    def test_no_components(self):
        self.assertEqual([], self.testcontainer.get_component_info(None))

    def test_components_no_keys(self):
        componentdata = {}
        componentdata[Acspy.Container.NAME] = 'TestComponent'
        componentdata[Acspy.Container.COMPONENTINFO] = 'Info Here!'
        self.testcontainer.components[componentdata[Acspy.Container.NAME]] = componentdata
        self.assertEqual(['Info Here!'], self.testcontainer.get_component_info(None))

    def test_components_keys(self):
        componentdata = {}
        componentdata[Acspy.Container.NAME] = 'TestComponent'
        componentdata[Acspy.Container.COMPONENTINFO] = 'Info Here!'
        self.testcontainer.components['TestComponent1'] = { Acspy.Container.NAME:'TestComponent1',
                                                       Acspy.Container.COMPONENTINFO:'Info 1' }
        self.testcontainer.components['TestComponent2'] = { Acspy.Container.NAME:'TestComponent2',
                                                       Acspy.Container.COMPONENTINFO:'Info 2' }
        self.testcontainer.components['TestComponent3'] = { Acspy.Container.NAME:'TestComponent3',
                                                       Acspy.Container.COMPONENTINFO:'Info 3' }
        self.testcontainer.compHandles = { 1:'TestComponent1', 2:'TestComponent2', 3:'TestComponent3' }
        self.assertEqual(['Info 2'], self.testcontainer.get_component_info([2]))

    def test_components_key_not_found(self):
        componentdata = {}
        componentdata[Acspy.Container.NAME] = 'TestComponent'
        componentdata[Acspy.Container.COMPONENTINFO] = 'Info Here!'
        self.testcontainer.components['TestComponent1'] = { Acspy.Container.NAME:'TestComponent1',
                                                       Acspy.Container.COMPONENTINFO:'Info 1' }
        self.testcontainer.components['TestComponent2'] = { Acspy.Container.NAME:'TestComponent2',
                                                       Acspy.Container.COMPONENTINFO:'Info 2' }
        self.testcontainer.components['TestComponent3'] = { Acspy.Container.NAME:'TestComponent3',
                                                       Acspy.Container.COMPONENTINFO:'Info 3' }
        self.testcontainer.compHandles = { 1:'TestComponent1', 2:'TestComponent2', 3:'TestComponent3' }
        self.assertEqual([], self.testcontainer.get_component_info([4]))


class TestOffShootActivation(unittest.TestCase):
    
    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def setUp(self, confcorbamock, refmock, cdbmock, baseclientmock):
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = mock.Mock(spec=Acspy.Util.ACSCorba._Client)
        self.testcontainer = Acspy.Container.Container('TestContainer')
        self.testcontainer.token = mock.Mock()
        self.testcontainer.token.h = 123435
        self.testcontainer.__init__ = mock.Mock()

    def tearDown(self):
        Acspy.Container.Log.stopPeriodicFlush()
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = None
        

    def test_component_not_exist(self):
        self.assertEqual(True, self.testcontainer.activateOffShoot('TestComponent',
                                                              None) is None)

    def test_object_not_offshoot_poa_fault(self):
        self.testcontainer.components['TestComponent'] = { Acspy.Container.NAME:'TestComponent',
                                                      Acspy.Container.COMPONENTINFO:'Info' }
        self.assertEqual(True, self.testcontainer.activateOffShoot('TestComponent',
                                                              object) is None)

    def test_object_not_offshoot_poa_ok(self):
        self.testcontainer.components['TestComponent'] = {
            Acspy.Container.NAME:'TestComponent',
            Acspy.Container.COMPONENTINFO:'Info',
            Acspy.Container.POAOFFSHOOT:mock.Mock(spec=omniORB.PortableServer.POA)
            }
        self.assertEqual(True,
                         self.testcontainer.activateOffShoot('TestComponent', object) is not None)

class TestDestruction(unittest.TestCase):

    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def setUp(self, confcorbamock, refmock, cdbmock, baseclientmock):
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = mock.Mock(spec=Acspy.Util.ACSCorba._Client)
        self.testcontainer = Acspy.Container.Container('TestContainer')
        self.testcontainer.token = mock.Mock()
        self.testcontainer.token.h = 123435
        self.testcontainer.__init__ = mock.Mock()

    def tearDown(self):
        Acspy.Container.Log.stopPeriodicFlush()
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = None
        

    def test_shutdown_unknown(self):
        self.testcontainer.shutdown(3 << 8)
        self.assertEqual(1, self.testcontainer.running)

    @mock.patch_object(Acspy.Container.ACSCorba, 'getManager')
    def test_shutdown_reload(self, managermock):
        self.testcontainer.shutdown(Acspy.Container.ACTIVATOR_RELOAD << 8)
        self.assertEqual(1, self.testcontainer.running)
        self.assertEqual(True, self.testcontainer.__init__.called)

    @mock.patch_object(Acspy.Container.ACSCorba, 'getManager')
    def test_shutdown_reboot(self, managermock):
        self.testcontainer.shutdown(Acspy.Container.ACTIVATOR_REBOOT << 8)
        self.assertEqual(1, self.testcontainer.running)
        self.assertEqual(True, self.testcontainer.__init__.called)

    @mock.patch_object(Acspy.Common.Log, 'stopPeriodicFlush')
    @mock.patch_object(Acspy.Container.ACSCorba, 'getManager')
    def test_shutdown_exit(self, logmock, managermock):
        self.testcontainer.shutdown(Acspy.Container.ACTIVATOR_EXIT << 8)
        self.assertEqual(0, self.testcontainer.running)
        self.assertEqual(True, logmock.called)
        self.assertEqual(False, self.testcontainer.__init__.called)


class TestOperations(unittest.TestCase):
    pass
##     def test_taggedmessage(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_set_component_shutdown_order(self):
##         self.fail("Not implemented") # TODO: implement your test here


class TestLoggingConfigurableInterface(unittest.TestCase):

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container, 'CDBaccess')
    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def setUp(self, cdbinfomock, baseclientmock, cdbaccessmock, corbamock):
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = mock.Mock(spec=Acspy.Util.ACSCorba._Client)
        self.tc = Acspy.Container.Container('UnitTestContainer')

    def tearDown(self):
        Acspy.Util.ACSCorba.SINGLETON_CLIENT = None
        Acspy.Container.Log.stopPeriodicFlush()
        Acspy.Container.Log.FLUSHTHREAD = None
        
    def test_configureComponentLogger(self):
        """Defined logger is configured to the CDB values."""
        self.tc.cdbAccess.getElement.return_value = [{ 'Name':'DefinedTestComponent',
                                                               'minLogLevel':'8',
                                                               'minLogLevelLocal':'9' }]
        self.tc.configureComponentLogger('DefinedTestComponent')
        ll = self.tc.get_logLevels('DefinedTestComponent')
        self.assertEqual(False, ll.useDefault)
        self.assertEqual(8, ll.minLogLevel)
        self.assertEqual(9, ll.minLogLevelLocal)

    def test_configureComponentLogger_unknown(self):
        """Unknown logger is configured to the CDB values."""
        self.tc.cdbAccess.getElement.return_value = [{ 'Name':'DefinedTestComponent',
                                                       'minLogLevel':'8',
                                                       'minLogLevelLocal':'9' }]
        self.tc.configureComponentLogger('UnknownTestComponent')
        ll = self.tc.get_logLevels('UnknownTestComponent')
        self.assertEqual(True, ll.useDefault)
        self.assertEqual(2, ll.minLogLevel)
        self.assertEqual(2, ll.minLogLevelLocal)

    def test_configureComponentLogger_partialLocal(self):
        """Partially-defined (local) logger is configured to the CDB values."""
        self.tc.cdbAccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentA', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentB', 'minLogLevel':'8'}]
        self.tc.configureComponentLogger('PartiallyDefinedTestComponentA')
        ll = self.tc.get_logLevels('PartiallyDefinedTestComponentA')
        self.assertEqual(False, ll.useDefault)
        self.assertEqual(2, ll.minLogLevel)
        self.assertEqual(9, ll.minLogLevelLocal)

    def test_configureComponentLogger_partialCentral(self):
        """Partially-defined (local) logger is configured to the CDB values."""
        self.tc.cdbAccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentA', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentB', 'minLogLevel':'8'}]
        self.tc.configureComponentLogger('PartiallyDefinedTestComponentB')
        ll = self.tc.get_logLevels('PartiallyDefinedTestComponentB')
        self.assertEqual(False, ll.useDefault)
        self.assertEqual(8, ll.minLogLevel)
        self.assertEqual(2, ll.minLogLevelLocal)

    def test_configureComponentLogger_localEnv(self):
        """Partially-defined (local) logger is configured to the CDB values and not STDOUT environment value."""
        self.tc.cdbAccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentA', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentB', 'minLogLevel':'8'}]
        environ['ACS_LOG_STDOUT'] = '5'
        self.tc.configureComponentLogger('PartiallyDefinedTestComponentA')
        ll = self.tc.get_logLevels('PartiallyDefinedTestComponentA')
        self.assertEqual(False, ll.useDefault)
        self.assertEqual(2, ll.minLogLevel)
        self.assertEqual(9, ll.minLogLevelLocal)
        del environ['ACS_LOG_STDOUT']

    def test_configureComponentLogger_centralEnv(self):
        """Partially-defined (local) logger is configured to the CDB values and not CENTRAL environment value."""
        self.tc.cdbAccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentA', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentB', 'minLogLevel':'8'}]
        environ['ACS_LOG_CENTRAL'] = '5'
        self.tc.configureComponentLogger('PartiallyDefinedTestComponentB')
        ll = self.tc.get_logLevels('PartiallyDefinedTestComponentB')
        self.assertEqual(False, ll.useDefault)
        self.assertEqual(8, ll.minLogLevel)
        if 'ACS_LOG_STDOUT' in environ:
            self.assertEqual(int(environ['ACS_LOG_STDOUT']), ll.minLogLevelLocal)
        else:
            self.assertEqual(2, ll.minLogLevelLocal)
        del environ['ACS_LOG_CENTRAL']

    @mock.patch_object(Acspy.Container.Log, 'getLogger')
    def test_get_logLevels_defined(self, mockGetLogger):
        mockLogger = mock.Mock(spec=Acspy.Common.Log.Logger)
        mockLogger.getLevels.return_value = True
        mockGetLogger.return_value = mockLogger
        cll = self.tc.get_logLevels('UnitTestContainer')
        self.assertEqual(True, cll)

    def test_get_logLevels_undefined(self):
        """Exception is thrown when log levels are requested for undefined logger"""
        self.assertRaises(Acspy.Container.LoggerDoesNotExistExImpl,
                          self.tc.get_logLevels,"Phantom")

    def test_get_logLevels_nocdb(self):
        """Container log levels are correct when no CDB info present"""
        dll = self.tc.get_default_logLevels()
        cll = self.tc.get_logLevels('UnitTestContainer')
        self.assertEqual(dll.useDefault, cll.useDefault)
        self.assertEqual(dll.minLogLevel, cll.minLogLevel)
        self.assertEqual(dll.minLogLevelLocal, cll.minLogLevelLocal)

    def test_refresh_logging_config(self):
        """Container log levels are set correctly to CDB values."""
        self.tc.cdbAccess.getElement.return_value = [{'centralizedLogger':"Log",
                                                  'minLogLevel':"5",
                                                  'minLogLevelLocal':"6",
                                                  'maxLogQueueSize':"0",
                                                  'dispatchPacketSize':"0",
                                                  'immediateDispatchLevel':"99",
                                                  'flushPeriodSeconds':"100"}]
        lcfg = LoggingConfig_xsd.LoggingConfig()
        self.tc.refresh_logging_config()
        self.assertNotEqual(Acspy.Container.Log.LEVELS[lcfg.minLogLevel],
                         Acspy.Container.Log.DEFAULTCENTRALHANDLER.level)
        self.assertNotEqual(Acspy.Container.Log.LEVELS[lcfg.minLogLevelLocal],
                         Acspy.Container.Log.DEFAULTLOCALHANDLER.level)
        self.assertNotEqual(lcfg.maxLogQueueSize, Acspy.Container.Log.CENTRALHANDLER.capacity)
        self.assertNotEqual(lcfg.dispatchPacketSize, Acspy.Container.Log.CENTRALHANDLER.batchsize)
        self.assertNotEqual(Acspy.Container.Log.LEVELS[lcfg.immediateDispatchLevel],
                         Acspy.Container.Log.CENTRALHANDLER.dispatchlevel)
        self.assertEqual(True, Acspy.Container.Log.isFlushRunning())
        self.assertNotEqual(lcfg.flushPeriodSeconds, Acspy.Container.Log.INTERVAL)

    def test_refresh_logging_config_nocdb_withcentral(self):
        """Container log levels are reset correctly when no CDB info and ACS_LOG_CENTRAL is present"""
        environ['ACS_LOG_CENTRAL'] = '5'
        lcfg = LoggingConfig_xsd.LoggingConfig()
        self.tc.refresh_logging_config()
        self.assertEqual(Acspy.Container.Log.LEVELS[5],
                         Acspy.Container.Log.DEFAULTCENTRALHANDLER.level)
        del environ['ACS_LOG_CENTRAL']

    def test_refresh_logging_config_nocdb_withlocal(self):
        """Container log levels are reset correctly when no CDB info and ACS_LOG_STDOUT is present"""
        environ['ACS_LOG_STDOUT'] = '5'
        lcfg = LoggingConfig_xsd.LoggingConfig()
        self.tc.refresh_logging_config()
        self.assertEqual(Acspy.Container.Log.LEVELS[5],
                         Acspy.Container.Log.DEFAULTLOCALHANDLER.level)
        del environ['ACS_LOG_STDOUT']

    def test_refresh_logging_config_nocdb_or_env(self):
        """Container log levels are reset correctly when no CDB info or environment vars are present"""
        lcfg = LoggingConfig_xsd.LoggingConfig()
        self.tc.refresh_logging_config()
        self.assertEqual(Acspy.Container.Log.LEVELS[lcfg.minLogLevel],
                         Acspy.Container.Log.DEFAULTCENTRALHANDLER.level)
        self.assertEqual(Acspy.Container.Log.LEVELS[lcfg.minLogLevelLocal],
                         Acspy.Container.Log.DEFAULTLOCALHANDLER.level)
        self.assertEqual(lcfg.maxLogQueueSize, Acspy.Container.Log.CENTRALHANDLER.capacity)
        self.assertEqual(lcfg.dispatchPacketSize, Acspy.Container.Log.CENTRALHANDLER.batchsize)
        self.assertEqual(Acspy.Container.Log.LEVELS[lcfg.immediateDispatchLevel],
                         Acspy.Container.Log.CENTRALHANDLER.dispatchlevel)
        self.assertEqual(lcfg.flushPeriodSeconds, Acspy.Container.Log.INTERVAL)
        self.assertEqual(True, Acspy.Container.Log.isFlushRunning())

    def test_set_logLevels(self):
        """Log levels for a named logger are set correctly"""
        sll =  self.tc.get_default_logLevels()
        sll.useDefault = False
        sll.minLogLevel = 5
        sll.minLogLevelLocal = 6
        self.tc.set_logLevels('UnitTestContainer',sll)
        ll = self.tc.get_logLevels('UnitTestContainer')
        self.assertEqual(sll.useDefault, ll.useDefault)
        self.assertEqual(sll.minLogLevel, ll.minLogLevel)
        self.assertEqual(sll.minLogLevelLocal, ll.minLogLevelLocal)

    def test_set_logLevels_unknown(self):
        """Exception is thrown when attempt to set log levels on an undefined logger"""
        self.assertRaises(Acspy.Container.LoggerDoesNotExistExImpl,
                          self.tc.set_logLevels, 'Phantom', None)

if __name__ == '__main__':
    unittest.main()
