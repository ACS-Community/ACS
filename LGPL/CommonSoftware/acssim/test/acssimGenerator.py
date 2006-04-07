#!/usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2002 
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration)
# and Cosylab 2002, All rights reserved
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307  USA
#
# @(#) $Id$
#------------------------------------------------------------------------------
__revision__ = "@(#) $Id$"
'''
Tests Acssim.Corba.Generator
'''
import CORBA
from Acssim.Corba.Generator import getRandomValue
from Acssim.Corba.Utilities import getTypeCode
#---------------------------------------------------------------
class MockComponent:
    def __init__(self, name): self.name = name
    def _get_name(self): return self.name
    def activateOffShoot(self, os): return os
    
FAKE_MS = MockComponent("MS1")
#---------------------------------------------------------------
def testType(idl_type):
    '''
    Helper function completely tests a single IDL type
    '''
    #get the typecode
    tc = getTypeCode(idl_type)
    
    #get a random instance
    rv = getRandomValue(tc,
                        FAKE_MS)
                        
    print "Created an instance of '", idl_type, "':", type(rv)

#---------------------------------------------------------------
if __name__=="__main__":
    print "--Testing simple CORBA types--"
    testType("IDL:omg.org/CORBA/Boolean:1.0")
    testType("IDL:omg.org/CORBA/Char:1.0")
    testType("IDL:omg.org/CORBA/Octet:1.0")
    testType("IDL:omg.org/CORBA/Short:1.0")
    testType("IDL:omg.org/CORBA/UShort:1.0")
    testType("IDL:omg.org/CORBA/Long:1.0")
    testType("IDL:omg.org/CORBA/ULong:1.0")
    testType("IDL:omg.org/CORBA/LongLong:1.0")
    testType("IDL:omg.org/CORBA/ULongLong:1.0")
    testType("IDL:omg.org/CORBA/Float:1.0")
    testType("IDL:omg.org/CORBA/Double:1.0")
    testType("IDL:omg.org/CORBA/LongDouble:1.0")
    testType("IDL:omg.org/CORBA/String:1.0")
    testType("IDL:omg.org/CORBA/Void:1.0")
    testType("IDL:omg.org/CORBA/Null:1.0")
    print
    print "-----------------------------------------------------"
    print "--Testing simple sequence types--" 
    testType("IDL:omg.org/CORBA/BooleanSeq:1.0")
    testType("IDL:omg.org/CORBA/CharSeq:1.0")
    testType("IDL:omg.org/CORBA/OctetSeq:1.0")
    testType("IDL:omg.org/CORBA/ShortSeq:1.0")
    testType("IDL:omg.org/CORBA/UShortSeq:1.0")
    testType("IDL:omg.org/CORBA/LongSeq:1.0")
    testType("IDL:omg.org/CORBA/ULongSeq:1.0")
    testType("IDL:omg.org/CORBA/LongLongSeq:1.0")
    testType("IDL:omg.org/CORBA/ULongLongSeq:1.0")
    testType("IDL:omg.org/CORBA/FloatSeq:1.0")
    testType("IDL:omg.org/CORBA/DoubleSeq:1.0")
    testType("IDL:omg.org/CORBA/StringSeq:1.0")
    print
    print "-----------------------------------------------------"
    print "--Testing everything from acssimTest"
    testType("IDL:alma/acssimTest/OnOffStates:1.0")
    print
    testType("IDL:alma/acssimTest/garbage:1.0")
    testType("IDL:alma/acssimTest/JunkCharComp:1.0")
    testType("IDL:alma/acssimTest/JunkBaciProp:1.0")
    testType("IDL:alma/acssimTest/complexStruct:1.0")
    testType("IDL:alma/acssimTest/JunkControl:1.0")
    print
    testType("IDL:alma/acssimTest/JunkControl/charSeq:1.0")
    testType("IDL:alma/acssimTest/JunkControl/charArray:1.0")
    testType("IDL:alma/acssimTest/JunkControl/longSeq:1.0")
    testType("IDL:alma/acssimTest/JunkControl/longArray:1.0")
    testType("IDL:alma/acssimTest/JunkControl/OnOffStatesSeq:1.0")
    testType("IDL:alma/acssimTest/JunkControl/OnOffStatesArray:1.0")
    testType("IDL:alma/acssimTest/JunkControl/garbageSeq:1.0")
    testType("IDL:alma/acssimTest/JunkControl/garbageArray:1.0")
    testType("IDL:alma/acssimTest/JunkControl/JunkCharCompSeq:1.0")
    testType("IDL:alma/acssimTest/JunkControl/JunkCharCompArray:1.0")
    testType("IDL:alma/acssimTest/JunkControl/JunkBaciPropSeq:1.0")
    testType("IDL:alma/acssimTest/JunkControl/JunkBaciPropArray:1.0")
    testType("IDL:alma/acssimTest/JunkControl/complexStructSeq:1.0")
    testType("IDL:alma/acssimTest/JunkControl/complexStructArray:1.0")
    