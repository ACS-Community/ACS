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
# "@(#) $Id: test_Acspy_Container.py,v 1.2 2010/02/04 21:45:16 agrimstrup Exp $"
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

# In order to remove the dependency on ACS being up, the client and
# manager have to be replaced.  We use mock objects so that the Container
# will function normally.
Acspy.Util.ACSCorba.SINGLETON_CLIENT = mock.Mock(spec=Acspy.Util.ACSCorba._Client)

def mockGetManager():
    mockMan = mock.Mock(spec=Acspy.Util.ACSCorba.maci._objref_Manager)
    mockMan.login.return_value = maci.ClientInfo(12345, 'foo', [], 'Name', [])
    return mockMan

Acspy.Util.ACSCorba.getManager = mockGetManager

import Acspy.Common.CDBAccess
import Acspy.Container


#--Global Objects
# Test Container
C = Acspy.Container.Container("UnitTestContainer")

class TestInitialization(unittest.TestCase):

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    def test_container_poa_object_fault(self, confcorbamock):
        def raiser():
            raise Acspy.Container.CouldntCreateObjectExImpl()

        confcorbamock.side_effect = raiser
        self.assertRaises(Acspy.Container.CouldntCreateObjectExImpl,
                          Acspy.Container.Container,
                          'TestContainer')

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    def test_corba_ref_fault(self, confcorbamock):
        def raiser():
            raise Acspy.Container.CORBAProblemExImpl()

        confcorbamock.side_effect = raiser
        self.assertRaises(Acspy.Container.CORBAProblemExImpl,
                          Acspy.Container.Container,
                          'TestContainer')

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container, 'BaseClient')
    def test_client_init_fault(self, baseclientmock, confcofbamock):
        def raiser():
            raise Acspy.Container.CORBAProblemExImpl()

        baseclientmock.side_effect = raiser
        self.assertRaises(Acspy.Container.CORBAProblemExImpl,
                          Acspy.Container.Container,
                          'TestContainer')

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container, 'CDBaccess')
    def test_cdbaccess_fault(self, confcorbamock, baseclientmock, cdbmock):
        def raiser():
            raise Acspy.Container.CORBAProblemExImpl()

        cdbmock.side_effect = raiser
        self.assertRaises(Acspy.Container.CORBAProblemExImpl,
                          Acspy.Container.Container,
                          'TestContainer')



    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container, 'CDBaccess')
    def test_getCDBInfo_no_container_info(self, confcorbamock, baseclientmock,
                                          cdbmock):
        def raiser():
            raise Acspy.Container.CORBAProblemExImpl()

        cdbifmock = mock.Mock(spec=Acspy.Common.CDBAccess.CDBaccess)
        cdbifmock.getElement.side_effect = raiser
        cdbmock.return_value = cdbifmock
        c = Acspy.Container.Container('TestContainer')
        self.assertEqual({}, c.cdbContainerInfo)

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container, 'CDBaccess')
    def test_getCDBInfo_no_package_info(self, confcorbamock, baseclientmock,
                                          cdbmock):

        cdbifmock = mock.Mock(spec=Acspy.Common.CDBAccess.CDBaccess)
        cdbifmock.getElement.return_value = []
        cdbmock.return_value = cdbifmock
        c = Acspy.Container.Container('TestContainer')
        self.assertEqual([], c.autoLoadPackages)

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container, 'BaseClient')
    @mock.patch_object(Acspy.Container, 'CDBaccess')
    @mock.patch_object(Acspy.Container, 'findFile')
    def test_getCDBInfo_no_package_found(self, confcorbamock, baseclientmock,
                                          cdbmock, findfilemock):

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
    def test_getCDBInfo_package_found(self, confcorbamock, baseclientmock,
                                          cdbmock, findfilemock):

        findfilemock.return_value = ("bin/package1","")
        cdbifmock = mock.Mock(spec=Acspy.Common.CDBAccess.CDBaccess)
        cdbifmock.getElement.return_value = [{ 'string':'package1'}]
        cdbmock.return_value = cdbifmock
        c = Acspy.Container.Container('TestContainer')
        self.assertEqual(["bin/package1"], c.autoLoadPackages)


class TestGetMyCorbaRef(unittest.TestCase):

    @mock.patch_object(Acspy.Container.ACSCorba, 'getPOARoot')
    @mock.patch_object(Acspy.Container, 'CDBaccess')
    def test_getMyCorbaRef_no_ref(self, poarootmock, cdbmock):
        def raiser():
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
    def test_getMyCorbaRef_bad_ref(self, poarootmock, cdbmock):
        def raiser():
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

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_already_exists(self, confcorbamock, refmock, cdbmock):
        tc = Acspy.Container.Container('TestContainer')
        tc.components['TestComponent'] = { 'COMPONENTINFO':True }
        self.assertEqual(True, tc.activate_component(12345,
                                                      12,
                                                      'TestComponent',
                                                      'TestComponent',
                                                      'testcomponent.idl'))

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_poa_create_fault(self, confcorbamock, refmock, cdbmock,
                              createpoamock):
        def raiser():
            raise Exception('Boom!')
        createpoamock.side_effect = raiser
        tc = Acspy.Container.Container('TestContainer')
        self.assertEqual(True, tc.activate_component(12345,
                                                     12,
                                                     'Test', 'Test',
                                                     'test.idl') is None)

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_offshoot_poa_create_fault(self, confcorbamock, refmock, cdbmock,
                                       createpoamock):
        def raiser():
            raise Exception('Boom!')
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.side_effect = raiser
        createpoamock.return_value = offshootpoamock 
        tc = Acspy.Container.Container('TestContainer')
        self.assertEqual(True, tc.activate_component(12345,
                                                     12,
                                                     'Test', 'Test',
                                                     'test.idl') is None)

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_bad_type_name_fault(self, confcorbamock, refmock, cdbmock,
                                 createpoamock):
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        createpoamock.return_value = offshootpoamock 
        tc = Acspy.Container.Container('TestContainer')
        self.assertEqual(True, tc.activate_component(12345,
                                                     12,
                                                     'Test', 'Test',
                                                     'test.idl') is None)

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_import_fault(self, confcorbamock, refmock, cdbmock,
                          createpoamock):
        mockmodule = new.module('MockModule')
        sys.modules['MockModule'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        createpoamock.return_value = offshootpoamock 
        tc = Acspy.Container.Container('TestContainer')
        self.assertRaises(Acspy.Container.CannotActivateComponentExImpl,
                          tc.activate_component,
                          12345, 12, 'Test', 'Test',
                          'IDL:alma/acspytest/MockModule:1.0')
        del sys.modules['MockModule']

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_bad_class_fault(self, confcorbamock, refmock, cdbmock,
                             createpoamock):
        mockmodule = new.module('MockClass')
        sys.modules['MockModule'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        createpoamock.return_value = offshootpoamock
        tc = Acspy.Container.Container('TestContainer')
        self.assertRaises(Acspy.Container.CannotActivateComponentExImpl,
                          tc.activate_component,
                          12345, 12, 'Test', 'MockClass',
                          'IDL:alma/acspytest/MockClass:1.0')
        del sys.modules['MockModule']

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    @mock.patch_object(Acspy.Container, 'instance')
    def test_bad_class_type_fault(self, confcorbamock, refmock, cdbmock,
                                  createpoamock, instancemock):
        def raiser():
            raise TypeError("boom!")
            
        mockmodule = new.module('MockClass')
        sys.modules['MockClass'] = mockmodule
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (object,), {})
        instancemock.side_effect = raiser
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        createpoamock.return_value = offshootpoamock
        tc = Acspy.Container.Container('TestContainer')
        self.assertRaises(Acspy.Container.CannotActivateComponentExImpl,
                          tc.activate_component,
                          12345, 12, 'Test', 'MockClass',
                          'IDL:alma/acspytest/MockClass:1.0')
        del sys.modules['MockClass']

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_constructor_fault(self, confcorbamock, refmock, cdbmock,
                               createpoamock):
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass', (), {})
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        createpoamock.return_value = offshootpoamock
        tc = Acspy.Container.Container('TestContainer')
        self.assertEqual(True, 
                         tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_dummy_class(self, confcorbamock, refmock, cdbmock,
                         createpoamock):
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices, Acspy.Container.ACSComponent, Acspy.Container.ComponentLifecycle), {})
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        createpoamock.return_value = offshootpoamock
        tc = Acspy.Container.Container('TestContainer')
        self.assertEqual(True, 
                         tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_bad_initialization(self, confcorbamock, refmock, cdbmock,
                                createpoamock):
        def initialize(self):
            self.componentState = Acspy.Container.ACS.COMPSTATE_NEW
            
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices, Acspy.Container.ACSComponent, Acspy.Container.ComponentLifecycle), {})
        mockmodule.__dict__['MockClass'].initialize = initialize
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        createpoamock.return_value = offshootpoamock
        tc = Acspy.Container.Container('TestContainer')
        self.assertEqual(True, 
                         tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_init_lifecycle_exception(self, confcorbamock, refmock, cdbmock,
                                      createpoamock):
        def initialize(self):
            raise Acspy.Container.ComponentLifecycleException('Boom!')
            
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices, Acspy.Container.ACSComponent, Acspy.Container.ComponentLifecycle), {})
        mockmodule.__dict__['MockClass'].initialize = initialize
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        createpoamock.return_value = offshootpoamock
        tc = Acspy.Container.Container('TestContainer')
        self.assertEqual(True, 
                         tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_init_unexpected_exception(self, confcorbamock, refmock, cdbmock,
                                       createpoamock):
        def initialize(self):
            raise Exception('Boom!')
            
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices, Acspy.Container.ACSComponent, Acspy.Container.ComponentLifecycle), {})
        mockmodule.__dict__['MockClass'].initialize = initialize
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        createpoamock.return_value = offshootpoamock
        tc = Acspy.Container.Container('TestContainer')
        self.assertEqual(False, 
                         tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_exec_lifecycle_exception(self, confcorbamock, refmock, cdbmock,
                                      createpoamock):
        def execute(self):
            raise Acspy.Container.ComponentLifecycleException('Boom!')
            
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices, Acspy.Container.ACSComponent, Acspy.Container.ComponentLifecycle), {})
        mockmodule.__dict__['MockClass'].execute = execute
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        createpoamock.return_value = offshootpoamock
        tc = Acspy.Container.Container('TestContainer')
        self.assertEqual(True, 
                         tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_exec_unexpected_exception(self, confcorbamock, refmock, cdbmock,
                                       createpoamock):
        def execute(self):
            raise Exception('Boom!')
            
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices, Acspy.Container.ACSComponent, Acspy.Container.ComponentLifecycle), {})
        mockmodule.__dict__['MockClass'].execute = execute
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        createpoamock.return_value = offshootpoamock
        tc = Acspy.Container.Container('TestContainer')
        self.assertEqual(False, 
                         tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_corba_servant_fault(self, confcorbamock, refmock, cdbmock,
                                 createpoamock):
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices, Acspy.Container.ACSComponent, Acspy.Container.ComponentLifecycle), {})
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.servant_to_reference.return_value = None
        createpoamock.return_value = offshootpoamock
        tc = Acspy.Container.Container('TestContainer')
        self.assertEqual(True, 
                         tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is None)
        del sys.modules['MockClass']

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_corba_exception(self, confcorbamock, refmock, cdbmock,
                             createpoamock):
        def raiser():
            raise Exception("Boom!")
        
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices, Acspy.Container.ACSComponent, Acspy.Container.ComponentLifecycle), {})
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.activate_object_with_id.side_effect = raiser
        createpoamock.return_value = offshootpoamock
        tc = Acspy.Container.Container('TestContainer')
        self.assertEqual(True, 
                         tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is None)
        del sys.modules['MockClass']

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Container.Container, 'createPOAForComponent')
    def test_existing_module(self, confcorbamock, refmock, cdbmock,
                             createpoamock):
        mockmodule = new.module('MockClass')
        mockmodule.__dict__['MockClass'] = new.classobj('MockClass',
                                                        (Acspy.Container.ContainerServices, Acspy.Container.ACSComponent, Acspy.Container.ComponentLifecycle), {})
        sys.modules['MockClass'] = mockmodule
        offshootpoamock = mock.Mock(spec=omniORB.PortableServer.POA)
        offshootpoamock.create_POA.return_value = \
                                     mock.Mock(spec=omniORB.PortableServer.POA)
        createpoamock.return_value = offshootpoamock
        tc = Acspy.Container.Container('TestContainer')
        tc.compModuleCount[mockmodule] = 1
        self.assertEqual(True, 
                         tc.activate_component(12345, 12, 'Test',
                                               'MockClass',
                                               'IDL:alma/acspytest/MockClass:1.0') is not None)
        del sys.modules['MockClass']

class TestDeactivation(unittest.TestCase):

#     def setUp(self):
        
    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_not_found(self, confcorbamock, refmock, cdbmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        self.assertEqual(True, testcontainer.deactivate_components([1]) is None)


    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_not_component(self, confcorbamock, refmock, cdbmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        componentdata = {}
        componentdata[Acspy.Container.HANDLE] = 1
        componentdata[Acspy.Container.NAME] = 'TestComponent'
        componentdata[Acspy.Container.CORBAREF] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.POAOFFSHOOT] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.POA] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.PYREF] = new.classobj('MockClass', (), {})() 
        componentdata[Acspy.Container.COMPMODULE] = new.module('MockClass')
        testcontainer.compHandles[1] = componentdata[Acspy.Container.NAME]
        testcontainer.components[componentdata[Acspy.Container.NAME]] = componentdata
        testcontainer.compModuleCount[componentdata[Acspy.Container.COMPMODULE]] = 2
        self.assertEqual(True, testcontainer.deactivate_components([1]) is None)


    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_component(self, confcorbamock, refmock, cdbmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        componentdata = {}
        componentdata[Acspy.Container.HANDLE] = 1
        componentdata[Acspy.Container.NAME] = 'TestComponent'
        componentdata[Acspy.Container.CORBAREF] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.POAOFFSHOOT] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.POA] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.PYREF] = new.classobj('MockClass', (Acspy.Container.ContainerServices, Acspy.Container.ACSComponent, Acspy.Container.ComponentLifecycle), {})()
        componentdata[Acspy.Container.COMPMODULE] = new.module('MockClass')
        testcontainer.compHandles[1] = componentdata[Acspy.Container.NAME]
        testcontainer.components[componentdata[Acspy.Container.NAME]] = componentdata
        testcontainer.compModuleCount[componentdata[Acspy.Container.COMPMODULE]] = 2
        self.assertEqual(True, testcontainer.deactivate_components([1]) is None)

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_module_reload(self, confcorbamock, refmock, cdbmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        componentdata = {}
        componentdata[Acspy.Container.HANDLE] = 1
        componentdata[Acspy.Container.NAME] = 'TestComponent'
        componentdata[Acspy.Container.CORBAREF] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.POAOFFSHOOT] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.POA] = mock.Mock(spec=omniORB.PortableServer.POA)
        componentdata[Acspy.Container.PYREF] = new.classobj('MockClass', (Acspy.Container.ContainerServices, Acspy.Container.ACSComponent, Acspy.Container.ComponentLifecycle), {})()
        componentdata[Acspy.Container.COMPMODULE] = new.module('MockClass')
        testcontainer.compHandles[1] = componentdata[Acspy.Container.NAME]
        testcontainer.components[componentdata[Acspy.Container.NAME]] = componentdata
        testcontainer.compModuleCount[componentdata[Acspy.Container.COMPMODULE]] = 1
        self.assertEqual(True, testcontainer.deactivate_components([1]) is None)

class TestGetComponentInfo(unittest.TestCase):
    
    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_no_components(self, confcorbamock, refmock, cdbmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        self.assertEqual([], testcontainer.get_component_info(None))

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_components_no_keys(self, confcorbamock, refmock, cdbmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        componentdata = {}
        componentdata[Acspy.Container.NAME] = 'TestComponent'
        componentdata[Acspy.Container.COMPONENTINFO] = 'Info Here!'
        testcontainer.components[componentdata[Acspy.Container.NAME]] = componentdata
        self.assertEqual(['Info Here!'], testcontainer.get_component_info(None))

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_components_keys(self, confcorbamock, refmock, cdbmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        componentdata = {}
        componentdata[Acspy.Container.NAME] = 'TestComponent'
        componentdata[Acspy.Container.COMPONENTINFO] = 'Info Here!'
        testcontainer.components['TestComponent1'] = { Acspy.Container.NAME:'TestComponent1', Acspy.Container.COMPONENTINFO:'Info 1' }
        testcontainer.components['TestComponent2'] = { Acspy.Container.NAME:'TestComponent2', Acspy.Container.COMPONENTINFO:'Info 2' }
        testcontainer.components['TestComponent3'] = { Acspy.Container.NAME:'TestComponent3', Acspy.Container.COMPONENTINFO:'Info 3' }
        testcontainer.compHandles = { 1:'TestComponent1', 2:'TestComponent2', 3:'TestComponent3' }
        self.assertEqual(['Info 2'], testcontainer.get_component_info([2]))

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_components_key_not_found(self, confcorbamock, refmock, cdbmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        componentdata = {}
        componentdata[Acspy.Container.NAME] = 'TestComponent'
        componentdata[Acspy.Container.COMPONENTINFO] = 'Info Here!'
        testcontainer.components['TestComponent1'] = { Acspy.Container.NAME:'TestComponent1', Acspy.Container.COMPONENTINFO:'Info 1' }
        testcontainer.components['TestComponent2'] = { Acspy.Container.NAME:'TestComponent2', Acspy.Container.COMPONENTINFO:'Info 2' }
        testcontainer.components['TestComponent3'] = { Acspy.Container.NAME:'TestComponent3', Acspy.Container.COMPONENTINFO:'Info 3' }
        testcontainer.compHandles = { 1:'TestComponent1', 2:'TestComponent2', 3:'TestComponent3' }
        self.assertEqual([], testcontainer.get_component_info([4]))


class TestOffShootActivation(unittest.TestCase):
    
    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_component_not_exist(self, confcorbamock, refmock, cdbmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        self.assertEqual(True, testcontainer.activateOffShoot('TestComponent',
                                                              None) is None)

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_object_not_offshoot_poa_fault(self, confcorbamock, refmock,
                                           cdbmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        testcontainer.components['TestComponent'] = { Acspy.Container.NAME:'TestComponent', Acspy.Container.COMPONENTINFO:'Info' }
        self.assertEqual(True, testcontainer.activateOffShoot('TestComponent',
                                                              object) is None)

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_object_not_offshoot_poa_ok(self, confcorbamock, refmock,
                                           cdbmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        testcontainer.components['TestComponent'] = {
            Acspy.Container.NAME:'TestComponent',
            Acspy.Container.COMPONENTINFO:'Info',
            Acspy.Container.POAOFFSHOOT:mock.Mock(spec=omniORB.PortableServer.POA)
            }
        self.assertEqual(True,
                         testcontainer.activateOffShoot('TestComponent', object) is not None)

class TestDestruction(unittest.TestCase):

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_shutdown_unknown(self, confcorbamock, refmock, cdbmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        testcontainer.shutdown(3 << 8)
        self.assertEqual(1, testcontainer.running)

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_shutdown_reload(self, confcorbamock, refmock, cdbmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        testcontainer.shutdown(Acspy.Container.ACTIVATOR_RELOAD << 8)
        self.assertEqual(1, testcontainer.running)

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    def test_shutdown_reboot(self, confcorbamock, refmock, cdbmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        testcontainer.shutdown(Acspy.Container.ACTIVATOR_REBOOT << 8)
        self.assertEqual(1, testcontainer.running)

    @mock.patch_object(Acspy.Container.Container, 'configCORBA')
    @mock.patch_object(Acspy.Container.Container, 'getMyCorbaRef')
    @mock.patch_object(Acspy.Container.Container, 'getCDBInfo')
    @mock.patch_object(Acspy.Common.Log, 'stopPeriodicFlush')
    def test_shutdown_exit(self, confcorbamock, refmock, cdbmock, logmock):
        testcontainer = Acspy.Container.Container('TestContainer')
        testcontainer.shutdown(Acspy.Container.ACTIVATOR_EXIT << 8)
        self.assertEqual(0, testcontainer.running)
        self.assertEqual(True, logmock.called)


class TestOperations(unittest.TestCase):
    pass
##     def test_taggedmessage(self):
##         self.fail("Not implemented") # TODO: implement your test here

##     def test_set_component_shutdown_order(self):
##         self.fail("Not implemented") # TODO: implement your test here


class TestLoggingConfigurableInterface(unittest.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        C.cdbAccess.cache = {}
        C.refresh_logging_config()
        C.cdbAccess.cache = {}
        
    @mock.patch_object(C, 'cdbAccess')
    def test_configureComponentLogger(self, mockCDBaccess):
        """Defined logger is configured to the CDB values."""
        mockCDBaccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9' }]
        C.configureComponentLogger('DefinedTestComponent')
        ll = C.get_logLevels('DefinedTestComponent')
        self.assertEqual(False, ll.useDefault)
        self.assertEqual(8, ll.minLogLevel)
        self.assertEqual(9, ll.minLogLevelLocal)

    @mock.patch_object(C,'cdbAccess')
    def test_configureComponentLogger_unknown(self, mockCDBaccess):
        """Unknown logger is configured to the CDB values."""
        mockCDBaccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9' }]
        C.configureComponentLogger('UnknownTestComponent')
        ll = C.get_logLevels('UnknownTestComponent')
        self.assertEqual(True, ll.useDefault)
        self.assertEqual(2, ll.minLogLevel)
        self.assertEqual(2, ll.minLogLevelLocal)

    @mock.patch_object(C,'cdbAccess')
    def test_configureComponentLogger_partialLocal(self, mockCDBaccess):
        """Partially-defined (local) logger is configured to the CDB values."""
        mockCDBaccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentA', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentB', 'minLogLevel':'8'}]
        C.configureComponentLogger('PartiallyDefinedTestComponentA')
        ll = C.get_logLevels('PartiallyDefinedTestComponentA')
        self.assertEqual(False, ll.useDefault)
        self.assertEqual(2, ll.minLogLevel)
        self.assertEqual(9, ll.minLogLevelLocal)

    @mock.patch_object(C,'cdbAccess')
    def test_configureComponentLogger_partialCentral(self, mockCDBaccess):
        """Partially-defined (local) logger is configured to the CDB values."""
        mockCDBaccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentA', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentB', 'minLogLevel':'8'}]
        C.configureComponentLogger('PartiallyDefinedTestComponentB')
        ll = C.get_logLevels('PartiallyDefinedTestComponentB')
        self.assertEqual(False, ll.useDefault)
        self.assertEqual(8, ll.minLogLevel)
        self.assertEqual(2, ll.minLogLevelLocal)

    @mock.patch_object(C,'cdbAccess')
    def test_configureComponentLogger_localEnv(self, mockCDBaccess):
        """Partially-defined (local) logger is configured to the CDB values and not STDOUT environment value."""
        mockCDBaccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentA', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentB', 'minLogLevel':'8'}]
        environ['ACS_LOG_STDOUT'] = '5'
        C.configureComponentLogger('PartiallyDefinedTestComponentA')
        ll = C.get_logLevels('PartiallyDefinedTestComponentA')
        self.assertEqual(False, ll.useDefault)
        self.assertEqual(2, ll.minLogLevel)
        self.assertEqual(9, ll.minLogLevelLocal)
        del environ['ACS_LOG_STDOUT']

    @mock.patch_object(C,'cdbAccess')
    def test_configureComponentLogger_centralEnv(self, mockCDBaccess):
        """Partially-defined (local) logger is configured to the CDB values and not CENTRAL environment value."""
        mockCDBaccess.getElement.return_value = [{ 'Name':'DefinedTestComponent', 'minLogLevel':'8', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentA', 'minLogLevelLocal':'9'},
                                                 { 'Name':'PartiallyDefinedTestComponentB', 'minLogLevel':'8'}]
        environ['ACS_LOG_CENTRAL'] = '5'
        C.configureComponentLogger('PartiallyDefinedTestComponentB')
        ll = C.get_logLevels('PartiallyDefinedTestComponentB')
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
        cll = C.get_logLevels('UnitTestContainer')
        self.assertEqual(True, cll)

    def test_get_logLevels_undefined(self):
        """Exception is thrown when log levels are requested for undefined logger"""
        self.assertRaises(Acspy.Container.LoggerDoesNotExistExImpl,
                          C.get_logLevels,"Phantom")

    def test_get_logLevels_nocdb(self):
        """Container log levels are correct when no CDB info present"""
        dll = C.get_default_logLevels()
        cll = C.get_logLevels('UnitTestContainer')
        self.assertEqual(dll.useDefault, cll.useDefault)
        self.assertEqual(dll.minLogLevel, cll.minLogLevel)
        self.assertEqual(dll.minLogLevelLocal, cll.minLogLevelLocal)

    @mock.patch_object(C,'cdbAccess')
    def test_refresh_logging_config(self, mockCDBaccess):
        """Container log levels are set correctly to CDB values."""
        mockCDBaccess.getElement.return_value = [{'centralizedLogger':"Log",
                                                  'minLogLevel':"5",
                                                  'minLogLevelLocal':"6",
                                                  'maxLogQueueSize':"0",
                                                  'dispatchPacketSize':"0",
                                                  'immediateDispatchLevel':"99",
                                                  'flushPeriodSeconds':"100"}]
        lcfg = LoggingConfig_xsd.LoggingConfig()
        C.refresh_logging_config()
        self.assertNotEqual(Acspy.Container.Log.LEVELS[lcfg.minLogLevel],
                         Acspy.Container.Log.DEFAULTCENTRALHANDLER.level)
        self.assertNotEqual(Acspy.Container.Log.LEVELS[lcfg.minLogLevelLocal],
                         Acspy.Container.Log.DEFAULTLOCALHANDLER.level)
        self.assertNotEqual(lcfg.maxLogQueueSize, Acspy.Container.Log.CENTRALHANDLER.capacity)
        self.assertNotEqual(lcfg.dispatchPacketSize, Acspy.Container.Log.CENTRALHANDLER.batchsize)
        self.assertNotEqual(Acspy.Container.Log.LEVELS[lcfg.immediateDispatchLevel],
                         Acspy.Container.Log.CENTRALHANDLER.dispatchlevel)
        self.assertNotEqual(lcfg.flushPeriodSeconds, Acspy.Container.Log.INTERVAL)

    def test_refresh_logging_config_nocdb_withcentral(self):
        """Container log levels are reset correctly when no CDB info and ACS_LOG_CENTRAL is present"""
        environ['ACS_LOG_CENTRAL'] = '5'
        lcfg = LoggingConfig_xsd.LoggingConfig()
        C.refresh_logging_config()
        self.assertEqual(Acspy.Container.Log.LEVELS[5],
                         Acspy.Container.Log.DEFAULTCENTRALHANDLER.level)
        del environ['ACS_LOG_CENTRAL']

    def test_refresh_logging_config_nocdb_withlocal(self):
        """Container log levels are reset correctly when no CDB info and ACS_LOG_STDOUT is present"""
        environ['ACS_LOG_STDOUT'] = '5'
        lcfg = LoggingConfig_xsd.LoggingConfig()
        C.refresh_logging_config()
        self.assertEqual(Acspy.Container.Log.LEVELS[5],
                         Acspy.Container.Log.DEFAULTLOCALHANDLER.level)
        del environ['ACS_LOG_STDOUT']

    def test_refresh_logging_config_nocdb_or_env(self):
        """Container log levels are reset correctly when no CDB info or environment vars are present"""
        lcfg = LoggingConfig_xsd.LoggingConfig()
        C.refresh_logging_config()
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
        sll =  C.get_default_logLevels()
        sll.useDefault = False
        sll.minLogLevel = 5
        sll.minLogLevelLocal = 6
        C.set_logLevels('UnitTestContainer',sll)
        ll = C.get_logLevels('UnitTestContainer')
        self.assertEqual(sll.useDefault, ll.useDefault)
        self.assertEqual(sll.minLogLevel, ll.minLogLevel)
        self.assertEqual(sll.minLogLevelLocal, ll.minLogLevelLocal)

    def test_set_logLevels_unknown(self):
        """Exception is thrown when attempt to set log levels on an undefined logger"""
        self.assertRaises(Acspy.Container.LoggerDoesNotExistExImpl,
                          C.set_logLevels, 'Phantom', None)

if __name__ == '__main__':
    unittest.main()
