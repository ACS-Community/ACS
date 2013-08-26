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
# "@(#) $Id: testContainerPortLock.py,v 1.6 2010/06/15 00:55:35 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2009-04-15  created
#
from __future__ import with_statement
import os
import sys
import time
import fcntl
import socket
import subprocess
import unittest
import mock

from AcsutilPy.ACSPorts import getIP
import acsstartupContainerPort

mockfile = mock.Mock(spec=file)
mockstderr = mock.Mock(spec=file)


class ContainerPortLockCleanupTest(unittest.TestCase):
    
    def setUp(self):
        mockfile.fileno.return_value = 4
        acsstartupContainerPort.container_file = mockfile
        acsstartupContainerPort.BASESLEEPTIME = 1
            
    def tearDown(self):
        mockfile.reset_mock()
        mockstderr.reset_mock()

    def excthrower(self):
        raise Exception('Test Exception')

    def test_no_file(self):
        acsstartupContainerPort.container_file = None
        acsstartupContainerPort.cleanUp()
        self.assertEqual(True, acsstartupContainerPort.container_file is None)

    @mock.patch_object(acsstartupContainerPort, 'stderr', mockstderr)
    @mock.patch_object(acsstartupContainerPort, 'sleep', mock.Mock)
    @mock.patch_object(acsstartupContainerPort, 'exit', mock.Mock)
    def test_unlock_exception(self):
        acsstartupContainerPort.cleanUp()
        self.assertEqual(True, acsstartupContainerPort.container_file is None)
        self.assertEqual(True, mockstderr.write.called)
        self.assertEqual(2, len(mockstderr.method_calls))
    
    @mock.patch_object(acsstartupContainerPort, 'stderr', mockstderr)
    @mock.patch_object(acsstartupContainerPort, 'sleep', mock.Mock)
    @mock.patch_object(acsstartupContainerPort, 'exit', mock.Mock)
    @mock.patch_object(acsstartupContainerPort, 'container_file', mockfile)
    @mock.patch_object(acsstartupContainerPort, 'rename')
    @mock.patch_object(acsstartupContainerPort, 'remove')
    def test_close_exception(self, renamemock, removemock):
        mockfile.close.side_effect = self.excthrower
        acsstartupContainerPort.cleanUp()
        self.assertEqual(True, mockfile.close.called)
        self.assertEqual([], mockfile.call_args_list)
        self.assertEqual(True, acsstartupContainerPort.container_file is None)
        self.assertEqual(True, mockstderr.write.called)
        self.assertEqual(1, len(mockstderr.method_calls))


class ContainerPortLockTest(unittest.TestCase):

    def setUp(self):
        self.oldacstmp = os.environ['ACS_TMP']
        os.environ['ACS_TMP'] = os.getcwd()
        self.tmpdirpath = 'ACS_INSTANCE.0'
        os.makedirs(self.tmpdirpath)
        self.testfilename = self.tmpdirpath + '/USED_CONTAINER_PORTS'
        os.system('touch ' + self.testfilename)


    def tearDown(self):
        os.remove(self.testfilename)
        os.removedirs(self.tmpdirpath)
        os.environ['ACS_TMP'] = self.oldacstmp

    def check_lock(self):
        with open(self.testfilename, 'r+') as f:
            try:
                fcntl.lockf(f.fileno(), fcntl.LOCK_EX|fcntl.LOCK_NB)
                fcntl.lockf(f.fileno(), fcntl.LOCK_UN)
                return True
            except IOError:
                return False

    def exec_helper(self, cmdline, errorcase=True):
        self.assertEqual(True, self.check_lock())
        out,err = subprocess.Popen(cmdline, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
        if not errorcase:
            self.assertNotEqual('', out)
            self.assertEqual('', err)
        else:
            self.assertEqual('', out)
            self.assertNotEqual('', err)
        self.assertEqual(True, self.check_lock())

    def test_normal(self):
        self.exec_helper(['acsstartupContainerPort', '--py', 'testContainer'], errorcase=False)

    def test_port_allocated(self):
        with open(self.testfilename, 'w') as f:
            f.write('testContainer 4000 %s\n' % getIP())
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.bind((getIP(), 4000))
        s.listen(1)
        self.exec_helper(['acsstartupContainerPort', '--py', 'testContainer'])
        s.close()

    def test_multiple_container_types(self):
        self.exec_helper(['acsstartupContainerPort', '--cpp', '--py', 'testContainer'])

    def test_no_container_type(self):
        self.exec_helper(['acsstartupContainerPort', 'testContainer'])

    def test_different_host_name(self):
        with open(self.testfilename, 'w') as f:
            f.write('testContainer 4000 172.16.70.128\n')
        self.exec_helper(['acsstartupContainerPort', '--py', 'testContainer'])

    def test_port_change(self):
        with open(self.testfilename, 'w') as f:
            f.write('testContainer 4000 %s\n' % getIP())
        self.exec_helper(['acsstartupContainerPort', '--port 4002', '--py', 'testContainer'])

    def test_port_assigned(self):
        with open(self.testfilename, 'w') as f:
            f.write('testContainer2 4000 %s\n' % getIP())
        self.exec_helper(['acsstartupContainerPort', '--port 4000', '--py', 'testContainer'])

    def test_port_in_use(self):
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.bind((getIP(), 4000))
        s.listen(1)
        self.exec_helper(['acsstartupContainerPort', '--port 4000', '--py', 'testContainer'])
        s.close()
    
        
if __name__ == "__main__":
    unittest.main()

#
# ___oOo___
