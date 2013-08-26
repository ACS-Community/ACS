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
Tests API
'''
from Acssim.Servants.Representations.CDB import CDB
        
if __name__=="__main__":
    
    #get the IFR ID of the component
    if_list = ["IDL:alma/RampedPS/RampedPowerSupply:1.0",
               "IDL:alma/PS/PowerSupply:1.0",
               "IDL:alma/ACS/CharacteristicComponent:1.0",
               "IDL:alma/ACS/ACSComponent:1.0"]
    
    print "--TEST_RPS_1--"
    cdb = CDB("TEST_RPS_1", if_list)
    #from RampedPowerSupply
    print "cdb.getMethod('startRamping'):",  cdb.getMethod('startRamping')['Timeout']             
    #from PowerSuppy
    print "cdb.getMethod('_get_readback'):",  cdb.getMethod('_get_readback')
    print
    
    print "--HELLOWORLD1--"
    cdb = CDB("HELLOWORLD1", ["IDL:alma/acsexmplHelloWorld/HelloWorld:1.0",
                              "IDL:alma/ACS/ACSComponent:1.0"])
    print "cdb.getMethod('displayMessage'):",  cdb.getMethod('displayMessage')['Timeout']          
    print "cdb.getMethod('nonexistentMethod'):",  cdb.getMethod('nonexistentMethod')
    print
    
    print "--BADCOMPONENT--"
    cdb = CDB("BADCOMPONENT", [ "crap", "morecrap"])
    print dir(cdb)