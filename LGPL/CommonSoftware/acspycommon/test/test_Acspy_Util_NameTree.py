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
# "@(#) $Id: test_Acspy_Util_NameTree.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who         when        what
# --------    --------    ----------------------------------------------
# agrimstrup  2010-02-12  created
#

#------------------------------------------------------------------------------
__revision__ = "$Id: test_Acspy_Util_NameTree.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
import sys
import unittest
import mock
import new
import CORBA
import omniORB
#--ACS IMPORTS-----------------------------------------------------------------
import Acspy.Util.NameTree as NT
#------------------------------------------------------------------------------

class TestGetNode(unittest.TestCase):

    def test_resolve_fault(self):
        def raiser():
            raise Exception("Boom!")
        workingdirmock = mock.Mock(spec=NT.CosNaming._objref_NamingContext)
        workingdirmock.resolve.side_effect = raiser
        self.assertEqual(True, NT.getnode('foo', workingdirmock) is None)

    def test_narrow_fault(self):
        def raiser():
            raise Exception("Boom!")
        contextmock = mock.Mock(spec=CORBA.Object)
        contextmock._narrow.side_effect = raiser
        workingdirmock = mock.Mock(spec=NT.CosNaming._objref_NamingContext)
        workingdirmock.resolve.return_value = contextmock
        self.assertEqual(True, NT.getnode('foo', workingdirmock) is None)

    def test_normal_operation(self):
        contextmock = mock.Mock(spec=CORBA.Object)
        workingdirmock = mock.Mock(spec=NT.CosNaming._objref_NamingContext)
        workingdirmock.resolve.return_value = contextmock
        self.assertEqual(False, NT.getnode('foo', workingdirmock) is None)


class TestInitialize(unittest.TestCase):

    @mock.patch_object(NT, 'nameService')
    def test_no_nameservice(self, nsmock):
        nsmock.return_value = None
        nt = NT.nameTree(None)
        self.assertEqual(False, 'top' in nt.__dict__)

    @mock.patch_object(NT, 'nameService')
    def test_ok(self, nsmock):
        nsmock.return_value = mock.Mock(spec=CORBA.Object)
        nt = NT.nameTree(None)
        self.assertEqual(True, 'top' in nt.__dict__)


class TestNameTreeMethods(unittest.TestCase):

    @mock.patch_object(NT, 'nameService')
    def setUp(self, nsmock):
        topmock = mock.Mock(spec=NT.CosNaming._objref_NamingContext)
        nsmock._narrow.return_value = mock.Mock(spec=CORBA.Object)
        self.nt = NT.nameTree(None)

    @mock.patch_object(NT, 'getnode')
    def test_mkdir_absolute_path_no_node(self, getnodemock):
        getnodemock.return_value = None
        self.nt.mkdir('/path/to/somewhere')
        self.assertEqual(True, self.nt.top.bind_new_context.called)
        
    @mock.patch_object(NT, 'getnode')
    def test_mkdir_absolute_path_existing_nodes(self, getnodemock):
        self.nt.mkdir('/path/to/somewhere')
        self.assertEqual(False, self.nt.top.bind_new_context.called)
        
    @mock.patch_object(NT, 'getnode')
    def test_mkdir_absolute_path_bind_fault(self, getnodemock):
        def raiser():
            raise Exception("Boom!")
        getnodemock.return_value = None
        self.nt.top.bind_new_context.side_effect = raiser
        self.nt.mkdir('/path/to/somewhere')
        self.assertEqual(True, self.nt.top.bind_new_context.called)
        
    @mock.patch_object(NT, 'getnode')
    def test_mkdir_relative_path_no_node(self, getnodemock):
        getnodemock.return_value = None
        self.nt.mkdir('path/to/somewhere')
        self.assertEqual(True, self.nt.top.bind_new_context.called)
        
    @mock.patch_object(NT, 'getnode')
    def test_mkdir_relative_path_existing_nodes(self, getnodemock):
        self.nt.mkdir('path/to/somewhere')
        self.assertEqual(False, self.nt.top.bind_new_context.called)
        
    @mock.patch_object(NT, 'getnode')
    def test_mkdir_relativee_path_bind_fault(self, getnodemock):
        def raiser():
            raise Exception("Boom!")
        getnodemock.return_value = None
        self.nt.top.bind_new_context.side_effect = raiser
        self.nt.mkdir('path/to/somewhere')
        self.assertEqual(True, self.nt.top.bind_new_context.called)
        
    def test_getObject_not_found(self):
        def raiser(*args):
            raise NT.CosNaming.NamingContext.NotFound("Boom!","Boom!")
        self.nt.top.resolve.side_effect = raiser
        self.assertRaises(NT.CosNaming.NamingContext.NotFound,
                          self.nt.getObject, 'foo', 'Foo')
        
    def test_getObject_other_fault(self):
        def raiser():
            raise Exception("Boom!")
        self.nt.top.resolve.side_effect = raiser
        self.assertEqual(True, self.nt.getObject('foo', 'Foo') is None)
        
    def test_pwd_default(self):
        self.assertEqual('', self.nt.pwd())

    def test_pwd_nested(self):
        self.nt.path.append(('foo', mock.Mock()))
        self.assertEqual('/foo', self.nt.pwd())

    def test_cd_relative_default(self):
        self.assertEqual(1, self.nt.cd('..'))

    def test_cd_relative_nested(self):
        self.nt.path.append(('foo', mock.Mock()))
        self.assertEqual(1, self.nt.cd('..'))

    def test_cd_relative_nested_forward(self):
        self.nt.path.append(('foo', mock.Mock()))
        self.assertEqual(1, self.nt.cd('../bar'))

    @mock.patch_object(NT, 'getnode')
    def test_cd_relative_nested_forward_not_exist(self, getnodemock):
        getnodemock.return_value = None
        self.assertEqual(0, self.nt.cd('bar'))


if __name__ == "__main__":
    unittest.main()

#
# ___oOo___
