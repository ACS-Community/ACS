#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2009 
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
# "@(#) $Id: test_generateXsdPythonBinding.py,v 1.1 2009/12/18 22:47:32 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2009-12-17  created
#

import sys
import unittest
import mock
import generateXsdPythonBinding

class TestFindSchemaFiles(unittest.TestCase):
    def test_no_content(self):
        ebs = generateXsdPythonBinding.xmlpybind.EntitybuilderSettings.EntitybuilderSettings()
        self.assertEqual([], generateXsdPythonBinding.find_schema_files(ebs))

    @mock.patch('os.path.isfile')
    def test_no_file(self, isfile_mock):
        xml = '<?xml version="1.0" encoding="ISO-8859-1"?>\n<EntitybuilderSettings>\n<EntitySchema schemaName="TestSchemaOtherDir.xsd" relativePathSchemafile="../config/test_idl" xmlNamespace="TestSchemaOtherDir"/>\n</EntitybuilderSettings>\n'
        isfile_mock.return_value = False
        ebs = generateXsdPythonBinding.xmlpybind.EntitybuilderSettings.CreateFromDocument(xml)
        self.assertRaises(IOError, generateXsdPythonBinding.find_schema_files,ebs)

    @mock.patch('os.path.isfile')
    def test_with_content(self, isfile_mock):
        xml = '<?xml version="1.0" encoding="ISO-8859-1"?>\n<EntitybuilderSettings>\n<EntitySchema schemaName="TestSchemaOtherDir.xsd" relativePathSchemafile="../config/test_idl" xmlNamespace="TestSchemaOtherDir"/>\n</EntitybuilderSettings>\n'
        isfile_mock.return_value = True
        ebs = generateXsdPythonBinding.xmlpybind.EntitybuilderSettings.CreateFromDocument(xml)
        self.assertEqual([(u'../config/test_idl/TestSchemaOtherDir.xsd', u'TestSchemaOtherDir')],
                         generateXsdPythonBinding.find_schema_files(ebs))


class TestGenerateBindings(unittest.TestCase):

    @mock.patch('generateXsdPythonBinding.call')
    def test_command_correct(self, call_mock):
        call_mock.return_value = 0
        self.assertEqual(0, generateXsdPythonBinding.generate_bindings('foo', [('../bar/bar.xsd', 'Bar')]))
        self.assertEqual('pyxbgen --module-prefix=foo --binding-root=../lib/python/site-packages --archive-to-file=../lib/python/site-packages/foo.wxs --archive-path=$PYTHONPATH -u ../bar/bar.xsd -m Bar', call_mock.call_args[0][0])

    @mock.patch('generateXsdPythonBinding.call')
    def test_call_failed(self, call_mock):
        call_mock.return_value = -1
        self.assertEqual(-1, generateXsdPythonBinding.generate_bindings('foo', [('../bar/bar.xsd', 'Bar')]))


class TestMain(unittest.TestCase):

    def exception_thrower(self):
        raise IOError('File Not Found')

    def test_no_arg(self):
        self.assertEqual(0, generateXsdPythonBinding.main([]))

    @mock.patch('__builtin__.open')
    def test_file_not_found(self, open_mock):
        open_mock.side_effect = self.exception_thrower
        self.assertEqual(-1, generateXsdPythonBinding.main(['foo']))

    @mock.patch('__builtin__.open')
    def test_file_empty(self, open_mock):
        file_mock = mock.Mock()
        file_mock.read.return_value = ''
        open_mock.return_value = file_mock
        self.assertEqual(-1, generateXsdPythonBinding.main(['foo']))

    @mock.patch('__builtin__.open')
    @mock.patch('generateXsdPythonBinding.xmlpybind.EntitybuilderSettings')
    @mock.patch('generateXsdPythonBinding.find_schema_files')
    def test_no_files_in_schema(self, open_mock, ebs_mock, fsf_mock):
        fsf_mock.return_value = []
        self.assertEqual(0, generateXsdPythonBinding.main(['foo']))

    @mock.patch('__builtin__.open')
    @mock.patch('generateXsdPythonBinding.xmlpybind.EntitybuilderSettings')
    @mock.patch('generateXsdPythonBinding.find_schema_files')
    @mock.patch('generateXsdPythonBinding.generate_bindings')
    def test_generation_fault(self, open_mock, ebs_mock, fsf_mock, gen_mock):
        fsf_mock.return_value = [1]
        gen_mock.return_value = 1
        self.assertEqual(1, generateXsdPythonBinding.main(['foo']))



class TestMain(unittest.TestCase):

    def exception_thrower(self):
        raise IOError('File Not Found')

    def test_no_arg(self):
        self.assertEqual(0, generateXsdPythonBinding.main([]))

    @mock.patch('__builtin__.open')
    def test_file_not_found(self, open_mock):
        open_mock.side_effect = self.exception_thrower
        self.assertEqual(-1, generateXsdPythonBinding.main(['foo']))

    @mock.patch('__builtin__.open')
    def test_file_empty(self, open_mock):
        file_mock = mock.Mock()
        file_mock.read.return_value = ''
        open_mock.return_value = file_mock
        self.assertEqual(-1, generateXsdPythonBinding.main(['foo']))

    @mock.patch('__builtin__.open')
    @mock.patch('generateXsdPythonBinding.xmlpybind.EntitybuilderSettings')
    @mock.patch('generateXsdPythonBinding.find_schema_files')
    def test_no_files_in_schema(self, open_mock, ebs_mock, fsf_mock):
        fsf_mock.return_value = []
        self.assertEqual(0, generateXsdPythonBinding.main(['foo']))

    @mock.patch('__builtin__.open')
    @mock.patch('generateXsdPythonBinding.xmlpybind.EntitybuilderSettings')
    @mock.patch('generateXsdPythonBinding.find_schema_files')
    @mock.patch('generateXsdPythonBinding.generate_bindings')
    def test_main_ok(self, open_mock, ebs_mock, fsf_mock, gen_mock):
        fsf_mock.return_value = [('b', 'a')]
        gen_mock.return_value = 0
        self.assertEqual(0, generateXsdPythonBinding.main(['foo']))

if __name__ == '__main__':
    unittest.main()
