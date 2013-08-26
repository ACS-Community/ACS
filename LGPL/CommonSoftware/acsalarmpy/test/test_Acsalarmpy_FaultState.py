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
# "@(#) $Id: test_Acsalarmpy_FaultState.py,v 1.1 2008/10/09 16:11:10 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-09-30  created
#

import unittest
import Acsalarmpy.Timestamp as Timestamp
import Acsalarmpy.FaultState as FaultState

class TestProperties(unittest.TestCase):
    def test_toXML_default(self):
        """Properties creates XML for default values"""
        p = FaultState.Properties()
        self.assertEquals('', p.toXML())

    def test_toXML_loaded(self):
        """Properties creates XML for given values"""
        p = FaultState.Properties()
        p['Name1'] = 'Value1'
        p['Name2'] = 'Value2'
        self.assertEquals('<user-properties>\n   <property name="Name2" value="Value2"/>\n   <property name="Name1" value="Value1"/>\n</user-properties>\n', p.toXML(0))
        
class TestFaultState(unittest.TestCase):

    def test_object_default_initialization(self):
        """FaultState initializes with default values"""
        fs = FaultState.FaultState()
        self.assertEquals(True, fs.family is None)
        self.assertEquals(True, fs.member is None)
        self.assertEquals(True, fs.code is None)
        self.assertEquals(FaultState.Properties(), fs.userProperties)
        self.assertEquals(True, fs.userTimestamp is None)
        self.assertEquals(True, fs.descriptor is None)
        self.assertEquals(True, fs.activatedByBackup is None)
        self.assertEquals(True, fs.terminatedByBackup is None)

    def test_object_initialization(self):
        """FaultState initializes with all required values"""
        fs = FaultState.FaultState("Family", "Member", 1)
        self.assertEquals(False, fs.family is None)
        self.assertEquals("Family", fs.family)
        self.assertEquals(False, fs.member is None)
        self.assertEquals("Member", fs.member)
        self.assertEquals(False, fs.code is None)
        self.assertEquals(1, fs.code)
        self.assertEquals(FaultState.Properties(), fs.userProperties)
        self.assertEquals(True, fs.userTimestamp is None)
        self.assertEquals(True, fs.descriptor is None)
        self.assertEquals(True, fs.activatedByBackup is None)
        self.assertEquals(True, fs.terminatedByBackup is None)

    def test_object_partial_initialization(self):
        """FaultState initializes with only a few values"""
        fs = FaultState.FaultState("Family")
        self.assertEquals(True, fs.family is None)
        self.assertEquals(True, fs.member is None)
        self.assertEquals(True, fs.code is None)
        self.assertEquals(FaultState.Properties(), fs.userProperties)
        self.assertEquals(True, fs.userTimestamp is None)
        self.assertEquals(True, fs.descriptor is None)
        self.assertEquals(True, fs.activatedByBackup is None)
        self.assertEquals(True, fs.terminatedByBackup is None)

    def test_toXML_default(self):
        """FaultState creates XML for default values"""
        fs = FaultState.FaultState()
        self.assertRaises(TypeError, fs.toXML)

    def test_toXML(self):
        """FaultState creates XML for all values"""
        fs = FaultState.FaultState("Family", "Member", 1)
        self.assertEquals('<fault-state family="Family" member="Member" code="1">\n</fault-state>\n', fs.toXML())

    def test_toXML_descriptor(self):
        """FaultState creates XML with descriptor"""
        fs = FaultState.FaultState("Family", "Member", 1)
        fs.descriptor = "Descriptor"
        self.assertEquals('<fault-state family="Family" member="Member" code="1">\n   <descriptor>Descriptor</descriptor>\n</fault-state>\n', fs.toXML())
            
    def test_toXML_properties(self):
        """FaultState creates XML with properties"""
        fs = FaultState.FaultState("Family", "Member", 1)
        fs.userProperties['Name'] = 'Value'
        self.assertEquals('<fault-state family="Family" member="Member" code="1">\n   <user-properties>\n      <property name="Name" value="Value"/>\n   </user-properties>\n</fault-state>\n', fs.toXML())

    def test_toXML_timestamp(self):
        """FaultState creates XML with timestamp"""
        fs = FaultState.FaultState("Family", "Member", 1)
        fs.userTimestamp = Timestamp.Timestamp(1222887968, 813309)
        self.assertEquals('<fault-state family="Family" member="Member" code="1">\n   <user-timestamp seconds="1222887968" microseconds="813309"/>\n</fault-state>\n', fs.toXML())

if __name__ == '__main__':
    unittest.main()
