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
# "@(#) $Id: testContainerPortLock.py,v 1.1 2009/04/15 23:28:26 agrimstrup Exp $"
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

from AcsutilPy.ACSPorts import getIP

class ContainerPortLockTest(unittest.TestCase):

    def setUp(self):
        self.oldacsdata = os.environ['ACSDATA']
        os.environ['ACSDATA'] = os.getcwd()
        self.tmpdirpath = 'tmp/ACS_INSTANCE.0'
        os.makedirs(self.tmpdirpath)
        self.testfilename = self.tmpdirpath + '/USED_CONTAINER_PORTS'
        os.system('touch ' + self.testfilename)


    def tearDown(self):
        os.remove(self.testfilename)
        os.removedirs(self.tmpdirpath)
        os.environ['ACSDATA'] = self.oldacsdata

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
