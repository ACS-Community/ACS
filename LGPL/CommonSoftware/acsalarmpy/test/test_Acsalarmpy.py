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
# "@(#) $Id: test_Acsalarmpy.py,v 1.7 2010/06/09 00:34:44 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-09-29  created
#

import unittest
import mock
import Acspy.Common.CDBAccess
import Acspy.Common.Log
import Acsalarmpy
import ACSAlarmSystemInterfaceProxy as ACSProxy

class TestAlarmSystemInterfaceFactory(unittest.TestCase):

    @mock.patch_object(Acsalarmpy, 'CDBAccess')
    @mock.patch_object(Acsalarmpy.AlarmSystemInterfaceFactory,'logger')
    def test_createFaultState(self, mocklog, mockcdb):
        """AlarmSystemInterfaceFactory creates FaultState correctly"""
        mockcdba = mock.Mock(spec=Acspy.Common.CDBAccess.CDBaccess)
        mockcdba.getField.return_value = '<?xml version="1.0" encoding="ISO-8859-1"?><alarm-system-configuration xmlns="urn:schemas-cosylab-com:acsalarm-alarmservice:1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><configuration-property name="Implementation">CERN</configuration-property></alarm-system-configuration>'
        mockcdb.CDBaccess.return_value = mockcdba
        mgr = mock.Mock()
        self.assertEqual(Acsalarmpy.AlarmSystemInterfaceFactory.logger, mocklog)
        Acsalarmpy.AlarmSystemInterfaceFactory.init(mgr)
        fs = Acsalarmpy.AlarmSystemInterfaceFactory.createFaultState()
        self.assertEqual(True, isinstance(fs, Acsalarmpy.FaultState.FaultState))
        Acsalarmpy.AlarmSystemInterfaceFactory.done()

    def test_createSource_noinit(self):
        """AlarmSystemInterfaceFactory does not create source when not initialized"""
        self.assertRaises(Acsalarmpy.ErrFactory.ACSASFactoryNotInitedExImpl, Acsalarmpy.AlarmSystemInterfaceFactory.createSource)

    @mock.patch_object(Acsalarmpy, 'CDBAccess')
    @mock.patch_object(Acsalarmpy.AlarmSystemInterfaceFactory,'logger')
    def test_createSource_ACS(self, mocklog, mockcdb):
        """AlarmSystemInterfaceFactory creates ACS source correctly"""
        mockcdba = mock.Mock(spec=Acspy.Common.CDBAccess.CDBaccess)
        mockcdba.getField.return_value = '<?xml version="1.0" encoding="ISO-8859-1"?><alarm-system-configuration xmlns="urn:schemas-cosylab-com:acsalarm-alarmservice:1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><configuration-property name="Implementation">ACS</configuration-property></alarm-system-configuration>'
        mockcdb.CDBaccess.return_value = mockcdba
        mgr = mock.Mock()
        Acsalarmpy.AlarmSystemInterfaceFactory.init(mgr)
        asi = Acsalarmpy.AlarmSystemInterfaceFactory.createSource()
        self.assertEqual(True, isinstance(asi, ACSProxy.ACSAlarmSystemInterfaceProxy))
        Acsalarmpy.AlarmSystemInterfaceFactory.done()

    @mock.patch_object(Acsalarmpy, 'CDBAccess')
    @mock.patch_object(Acsalarmpy.AlarmSystemInterfaceFactory,'logger')
    def test_createSource_CERN(self, mocklog, mockcdb):
        """AlarmSystemInterfaceFactory creates CERN source correctly"""
        mockcdba = mock.Mock(spec=Acspy.Common.CDBAccess.CDBaccess)
        mockcdba.getField.return_value = '<?xml version="1.0" encoding="ISO-8859-1"?><alarm-system-configuration xmlns="urn:schemas-cosylab-com:acsalarm-alarmservice:1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><configuration-property name="Implementation">CERN</configuration-property></alarm-system-configuration>'
        mockcdb.CDBaccess.return_value = mockcdba
        mgr = mock.Mock()
        Acsalarmpy.AlarmSystemInterfaceFactory.init(mgr)
        Acsalarmpy.AlarmSystemInterfaceFactory.registry['CERN'] = mock.Mock
        asi = Acsalarmpy.AlarmSystemInterfaceFactory.createSource()
        self.assertEqual(True, isinstance(asi, mock.Mock))
        Acsalarmpy.AlarmSystemInterfaceFactory.done()

    @mock.patch_object(Acsalarmpy.AlarmSystemInterfaceFactory,'logger')
    def test_createFaultState_noinit(self, mocklog):
        """AlarmSystemInterfaceFactory does not create FaultStates when not initialized"""
        self.assertRaises(Acsalarmpy.ErrFactory.ACSASFactoryNotInitedExImpl, Acsalarmpy.AlarmSystemInterfaceFactory.createFaultState)

    @mock.patch_object(Acsalarmpy.AlarmSystemInterfaceFactory,'logger')
    def test_done(self, mocklog):
        """AlarmSystemInterfaceFactory cleared correctly"""
        Acsalarmpy.AlarmSystemInterfaceFactory.done()
        self.assertEqual(True, Acsalarmpy.AlarmSystemInterfaceFactory.manager is None)
        self.assertEqual(False, Acsalarmpy.AlarmSystemInterfaceFactory.initialized)
        self.assertEqual(True, Acsalarmpy.AlarmSystemInterfaceFactory.systemtype is None)

    @mock.patch_object(Acsalarmpy, 'CDBAccess')
    @mock.patch_object(Acsalarmpy.AlarmSystemInterfaceFactory,'logger')
    def test_init_cern(self, mocklog, mockcdb):
        """AlarmSystemInterfaceFactory initialized correctly with CERN configuration"""
        mockcdba = mock.Mock(spec=Acspy.Common.CDBAccess.CDBaccess)
        mockcdba.getField.return_value = '<?xml version="1.0" encoding="ISO-8859-1"?><alarm-system-configuration xmlns="urn:schemas-cosylab-com:acsalarm-alarmservice:1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><configuration-property name="Implementation">CERN</configuration-property></alarm-system-configuration>'
        mockcdb.CDBaccess.return_value = mockcdba
        mgr = mock.Mock()
        Acsalarmpy.AlarmSystemInterfaceFactory.init(mgr)
        self.assertEqual(mgr, Acsalarmpy.AlarmSystemInterfaceFactory.manager)
        self.assertEqual(True, Acsalarmpy.AlarmSystemInterfaceFactory.initialized)
        self.assertEqual('CERN', Acsalarmpy.AlarmSystemInterfaceFactory.systemtype)
        Acsalarmpy.AlarmSystemInterfaceFactory.done()

    @mock.patch_object(Acsalarmpy, 'CDBAccess')
    @mock.patch_object(Acsalarmpy.AlarmSystemInterfaceFactory,'logger')
    def test_init_acs(self, mocklog, mockcdb):
        """AlarmSystemInterfaceFactory initialized correctly with ACS configuration"""
        mockcdba = mock.Mock(spec=Acspy.Common.CDBAccess.CDBaccess)
        mockcdba.getField.return_value = '<?xml version="1.0" encoding="ISO-8859-1"?><alarm-system-configuration xmlns="urn:schemas-cosylab-com:acsalarm-alarmservice:1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><configuration-property name="Implementation">ACS</configuration-property></alarm-system-configuration>'
        mockcdb.CDBaccess.return_value = mockcdba
        mgr = mock.Mock()
        Acsalarmpy.AlarmSystemInterfaceFactory.init(mgr)
        self.assertEqual(mgr, Acsalarmpy.AlarmSystemInterfaceFactory.manager)
        self.assertEqual(True, Acsalarmpy.AlarmSystemInterfaceFactory.initialized)
        self.assertEqual('ACS', Acsalarmpy.AlarmSystemInterfaceFactory.systemtype)
        Acsalarmpy.AlarmSystemInterfaceFactory.done()

if __name__ == '__main__':
    unittest.main()
