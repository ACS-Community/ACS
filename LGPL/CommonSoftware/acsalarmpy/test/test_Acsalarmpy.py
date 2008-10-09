#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2008 
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
# "@(#) $Id: test_Acsalarmpy.py,v 1.2 2008/10/09 19:13:20 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-09-29  created
#

import unittest
import mock
import CDB

mockCDB = mock.Mock(spec=CDB._objref_DAL)

def mockcdb():
    return mockCDB

import Acspy.Util.ACSCorba
Acspy.Util.ACSCorba.cdb = mockcdb

import Acspy.Common.Log

mockLogger = mock.Mock(spec=Acspy.Common.Log.Logger)
def mockGetLogger(name=None):
    return mockLogger

Acspy.Common.Log.getLogger = mockGetLogger

import Acsalarmpy

class TestAlarmSystemInterfaceFactory(unittest.TestCase):
    def test_createFaultState(self):
        """AlarmSystemInterfaceFactory creates FaultState correctly"""
        mockCDB.get_DAO.return_value = '<?xml version="1.0" encoding="ISO-8859-1"?><alarm-system-configuration xmlns="urn:schemas-cosylab-com:AcsAlarmSystem:1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><configuration-property name="Implementation">CERN</configuration-property></alarm-system-configuration>'
        mgr = mock.Mock()
        Acsalarmpy.AlarmSystemInterfaceFactory.init(mgr)
        fs = Acsalarmpy.AlarmSystemInterfaceFactory.createFaultState()
        self.assertEqual(True, isinstance(fs, Acsalarmpy.FaultState.FaultState))
        Acsalarmpy.AlarmSystemInterfaceFactory.done()

    def test_createSource_noinit(self):
        """AlarmSystemInterfaceFactory does not create source when not initialized"""
        self.assertRaises(RuntimeError, Acsalarmpy.AlarmSystemInterfaceFactory.createSource)
        
    def test_createSource_ACS(self):
        """AlarmSystemInterfaceFactory creates ACS source correctly"""
        mockCDB.get_DAO.return_value = '<?xml version="1.0" encoding="ISO-8859-1"?><alarm-system-configuration xmlns="urn:schemas-cosylab-com:AcsAlarmSystem:1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><configuration-property name="Implementation">ACS</configuration-property></alarm-system-configuration>'
        mgr = mock.Mock()
        Acsalarmpy.AlarmSystemInterfaceFactory.init(mgr)
        asi = Acsalarmpy.AlarmSystemInterfaceFactory.createSource()
        self.assertEqual(True, isinstance(asi, Acsalarmpy.ACSAlarmSystemInterfaceProxy.ACSAlarmSystemInterfaceProxy))
        Acsalarmpy.AlarmSystemInterfaceFactory.done()

    def test_createSource_CERN(self):
        """AlarmSystemInterfaceFactory creates CERN source correctly"""
        mockCDB.get_DAO.return_value = '<?xml version="1.0" encoding="ISO-8859-1"?><alarm-system-configuration xmlns="urn:schemas-cosylab-com:AcsAlarmSystem:1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><configuration-property name="Implementation">CERN</configuration-property></alarm-system-configuration>'
        mgr = mock.Mock()
        Acsalarmpy.AlarmSystemInterfaceFactory.init(mgr)
        Acsalarmpy.AlarmSystemInterfaceFactory.registry['CERN'] = mock.Mock
        asi = Acsalarmpy.AlarmSystemInterfaceFactory.createSource()
        self.assertEqual(True, isinstance(asi, mock.Mock))
        Acsalarmpy.AlarmSystemInterfaceFactory.done()

    def test_createFaultState_noinit(self):
        """AlarmSystemInterfaceFactory does not create FaultStates when not initialized"""
        self.assertRaises(RuntimeError, Acsalarmpy.AlarmSystemInterfaceFactory.createFaultState)

    def test_done(self):
        """AlarmSystemInterfaceFactory cleared correctly"""
        Acsalarmpy.AlarmSystemInterfaceFactory.done()
        self.assertEqual(True, Acsalarmpy.AlarmSystemInterfaceFactory.manager is None)
        self.assertEqual(False, Acsalarmpy.AlarmSystemInterfaceFactory.initialized)
        self.assertEqual(True, Acsalarmpy.AlarmSystemInterfaceFactory.systemtype is None)

    def test_init_cern(self):
        """AlarmSystemInterfaceFactory initialized correctly with CERN configuration"""
        mockCDB.get_DAO.return_value = '<?xml version="1.0" encoding="ISO-8859-1"?><alarm-system-configuration xmlns="urn:schemas-cosylab-com:AcsAlarmSystem:1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><configuration-property name="Implementation">CERN</configuration-property></alarm-system-configuration>'
        mgr = mock.Mock()
        Acsalarmpy.AlarmSystemInterfaceFactory.init(mgr)
        self.assertEqual(mgr, Acsalarmpy.AlarmSystemInterfaceFactory.manager)
        self.assertEqual(True, Acsalarmpy.AlarmSystemInterfaceFactory.initialized)
        self.assertEqual('CERN', Acsalarmpy.AlarmSystemInterfaceFactory.systemtype)
        Acsalarmpy.AlarmSystemInterfaceFactory.done()

    def test_init_acs(self):
        """AlarmSystemInterfaceFactory initialized correctly with ACS configuration"""
        mockCDB.get_DAO.return_value = '<?xml version="1.0" encoding="ISO-8859-1"?><alarm-system-configuration xmlns="urn:schemas-cosylab-com:AcsAlarmSystem:1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><configuration-property name="Implementation">ACS</configuration-property></alarm-system-configuration>'
        mgr = mock.Mock()
        Acsalarmpy.AlarmSystemInterfaceFactory.init(mgr)
        self.assertEqual(mgr, Acsalarmpy.AlarmSystemInterfaceFactory.manager)
        self.assertEqual(True, Acsalarmpy.AlarmSystemInterfaceFactory.initialized)
        self.assertEqual('ACS', Acsalarmpy.AlarmSystemInterfaceFactory.systemtype)
        Acsalarmpy.AlarmSystemInterfaceFactory.done()

if __name__ == '__main__':
    unittest.main()
