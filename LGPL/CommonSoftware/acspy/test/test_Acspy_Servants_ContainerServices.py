#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2007 
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
# "@(#) $Id: test_Acspy_Servants_ContainerServices.py,v 1.2 2010/06/08 01:55:25 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# arne  2007-08-28  created
#

#------------------------------------------------------------------------------
__revision__ = "$Id: test_Acspy_Servants_ContainerServices.py,v 1.2 2010/06/08 01:55:25 agrimstrup Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
import sys
import unittest
import mock
import new
import CDB
#--ACS IMPORTS-----------------------------------------------------------------
import Acspy.Common.Log
import Acspy.Servants.ContainerServices as CS
from maciErrTypeImpl          import CannotGetComponentExImpl
#------------------------------------------------------------------------------


class TestGetLogger(unittest.TestCase):

    @mock.patch_object(CS, 'getLogger')
    @mock.patch_object(CS, 'CDBaccess')
    def test_undefined(self, cdbaccessmock, getloggermock):
        loggermock = mock.Mock(spec=Acspy.Common.Log.Logger)
        getloggermock.return_value = loggermock
        containerservices = CS.ContainerServices()
        self.assertEqual(True, containerservices.getLogger() is not None)
        self.assertEqual(loggermock,
                         containerservices._ContainerServices__logger)

    @mock.patch_object(CS, 'CDBaccess')
    @mock.patch_object(CS, 'getLogger')
    def test_defined(self, cdbaccessmock, getloggermock):
        loggermock = mock.Mock(spec=Acspy.Common.Log.Logger)
        getloggermock.return_value = loggermock
        containerservices = CS.ContainerServices()
        containerservices._ContainerServices__logger = True
        self.assertEqual(True, containerservices.getLogger())


class TestGetComponent(unittest.TestCase):

    @mock.patch_object(CS, 'getManager')
    @mock.patch_object(CS, 'CDBaccess')
    @mock.patch_object(CS.ContainerServices,
                       '_ContainerServices__narrowComponentReference')
    @mock.patch_object(CS.ContainerServices,
                       '_ContainerServices__importComponentStubs')
    def test_by_name(self, stubsmock, refmock, cdbaccessmock, getmanagermock):
        managermock = mock.Mock(spec=CS.maci._objref_Manager)
        getmanagermock.return_value = managermock
        containerservices = CS.ContainerServices()
        component = containerservices.getComponent('TestComponent')
        self.assertEqual(True, stubsmock.called)
        self.assertEqual(True, refmock.called)
        self.assertEqual(True, managermock.get_component.called)

    @mock.patch_object(CS.ContainerServices, 'getDefaultComponent')
    @mock.patch_object(CS, 'CDBaccess')
    def test_by_type(self, defaultmock, cdbaccessmock):
        containerservices = CS.ContainerServices()
        component = containerservices.getComponent(comp_idl_type='TestComponent')
        self.assertEqual(True, defaultmock.called)

    @mock.patch_object(CS.ContainerServices, 'getDynamicComponent')
    @mock.patch_object(CS, 'CDBaccess')
    def test_by_code(self, dynamicmock, cdbaccessmock):
        containerservices = CS.ContainerServices()
        component = containerservices.getComponent(comp_code='TestComponent')
        self.assertEqual(True, dynamicmock.called)

    @mock.patch_object(CS.ContainerServices, 'getDynamicComponent')
    @mock.patch_object(CS, 'CDBaccess')
    def test_by_container(self, dynamicmock, cdbaccessmock):
        containerservices = CS.ContainerServices()
        component = containerservices.getComponent(container_name='TestComponent')
        self.assertEqual(True, dynamicmock.called)
    

class TestImportComponentStubs(unittest.TestCase):

    @mock.patch_object(CS, 'CDBaccess')
    def test_no_name_no_type(self, cdbaccessmock):
        loggermock = mock.Mock(spec=Acspy.Common.Log.Logger)
        containerservices = CS.ContainerServices()
        containerservices._ContainerServices__logger = loggermock
        self.assertRaises(CS.CannotGetComponentExImpl,
                          containerservices._ContainerServices__importComponentStubs)
        self.assertEqual(True, loggermock.logWarning.called)

    @mock.patch_object(CS.ContainerServices, 'availableComponents')
    @mock.patch_object(CS, 'CDBaccess')
    def test_name_not_found_no_type(self, cdbaccessmock, availablemock):
        availablemock.return_value = []
        loggermock = mock.Mock(spec=Acspy.Common.Log.Logger)
        containerservices = CS.ContainerServices()
        containerservices._ContainerServices__logger = loggermock
        self.assertRaises(CS.CannotGetComponentExImpl,
                          containerservices._ContainerServices__importComponentStubs, 'TestComponent')
        self.assertEqual(True, loggermock.logWarning.called)
        

    @mock.patch_object(CS.ContainerServices, 'availableComponents')
    @mock.patch_object(CS, 'CDBaccess')
    def test_name_found_no_type(self, cdbaccessmock, availablemock):
        availablemock.return_value = [CS.maci.ComponentInfo(None, None, None,
                                                            'TestComponent',
                                                            None, None, None,
                                                            None, None, None)]
        loggermock = mock.Mock(spec=Acspy.Common.Log.Logger)
        containerservices = CS.ContainerServices()
        containerservices._ContainerServices__logger = loggermock
        self.assertRaises(CS.CannotGetComponentExImpl, \
                  containerservices._ContainerServices__importComponentStubs,
                  'TestComponent')
        self.assertEqual(True, loggermock.logWarning.called)
        
    @mock.patch_object(CS.ContainerServices, 'availableComponents')
    @mock.patch_object(CS, 'CDBaccess')
    def test_name_found_with_unknown_type(self, cdbaccessmock, availablemock):
        availablemock.return_value = [CS.maci.ComponentInfo( \
            'IDL:alma/MockUnknown/MockClass:1.0', None, None,
            'TestComponent', None, None, None, None, None, None)]
        loggermock = mock.Mock(spec=Acspy.Common.Log.Logger)
        containerservices = CS.ContainerServices()
        containerservices._ContainerServices__logger = loggermock
        self.assertRaises(CS.CannotGetComponentExImpl, \
                  containerservices._ContainerServices__importComponentStubs,
                  'TestComponent')
        self.assertEqual(True, loggermock.logWarning.called)

    @mock.patch_object(CS, 'CDBaccess')
    def test_known_type_unknown_class(self, cdbaccessmock):
        sys.modules['MockModule'] = new.module('MockModule')
        loggermock = mock.Mock(spec=Acspy.Common.Log.Logger)
        containerservices = CS.ContainerServices()
        containerservices._ContainerServices__logger = loggermock
        self.assertEqual(True, \
                  containerservices._ContainerServices__importComponentStubs( \
                  None, 'IDL:alma/MockModule/MockClass:1.0') is None)
        self.assertEqual(False, loggermock.logWarning.called)
        del sys.modules['MockModule']

    @mock.patch_object(CS, 'CDBaccess')
    def test_known_type_known_class(self, cdbaccessmock):
        sys.modules['MockModule'] = new.module('MockModule')
        sys.modules['MockModule'].__dict__['MockClass'] = \
                            new.classobj('MockClass', (object,), {})
        loggermock = mock.Mock(spec=Acspy.Common.Log.Logger)
        containerservices = CS.ContainerServices()
        containerservices._ContainerServices__logger = loggermock
        self.assertEqual(False, \
                  containerservices._ContainerServices__importComponentStubs( \
                  None, 'IDL:alma/MockModule/MockClass:1.0') is None)
        self.assertEqual(False, loggermock.logWarning.called)
        del sys.modules['MockModule']


class TestNarrowComponentReference(unittest.TestCase):

    @mock.patch_object(CS, 'CDBaccess')
    def test_ok(self, cdbaccessmock):
        loggermock = mock.Mock(spec=Acspy.Common.Log.Logger)
        containerservices = CS.ContainerServices()
        containerservices._ContainerServices__logger = loggermock
        corbamock = mock.Mock(spec=CS.CORBA.Object)
        corbamock._narrow.return_value = True
        self.assertEqual(True, containerservices._ContainerServices__narrowComponentReference(corbamock, "foo"))
        self.assertEqual(False, loggermock.logWarning.called)
        self.assertEqual(False, loggermock.logCritical.called)


    @mock.patch_object(CS, 'CDBaccess')
    def test_none_corba_obj(self, cdbaccessmock):
        loggermock = mock.Mock(spec=Acspy.Common.Log.Logger)
        containerservices = CS.ContainerServices()
        containerservices._ContainerServices__logger = loggermock
        self.assertEqual(True, containerservices._ContainerServices__narrowComponentReference(None, "foo") is None)
        self.assertEqual(True, loggermock.logCritical.called)

    @mock.patch_object(CS, 'CDBaccess')
    def test_corba_obj(self, cdbaccessmock):
        def raiser():
            raise Exception("Boom!")

        corbamock = mock.Mock(spec=CS.CORBA.Object)
        corbamock._narrow.side_effect = raiser
        loggermock = mock.Mock(spec=Acspy.Common.Log.Logger)
        containerservices = CS.ContainerServices()
        containerservices._ContainerServices__logger = loggermock
        self.assertEqual(corbamock, containerservices._ContainerServices__narrowComponentReference(corbamock, "foo"))
        self.assertEqual(True, loggermock.logWarning.called)

    
if __name__ == "__main__":
    unittest.main()


#
# ___oOo___
