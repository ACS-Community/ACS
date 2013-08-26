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
Tests Dynamic
'''
import CORBA
from Acssim.Servants.Representations.Dynamic import Dynamic
from Acssim.Goodies import addComponent

class MockComponent:
    def __init__(self, name): self.name = name
    def _get_name(self): return self.name
    def activateOffShoot(self, os): return os

addComponent("TEST_RPS_1", MockComponent("TEST_RPS_1"))
addComponent("HELLOWORLD1", MockComponent("HELLOWORLD1"))
addComponent("BADCOMPONENT", MockComponent("BADCOMPONENT"))
       
if __name__=="__main__":
    
    print "--TEST_RPS_1--"
    dyn = Dynamic("TEST_RPS_1", "IDL:alma/RampedPS/RampedPowerSupply:1.0")
    #from RampedPowerSupply
    print "dyn.getMethod('startRamping'):",  dyn.getMethod('startRamping')                
    #from PowerSuppy
    print "dyn.getMethod('_get_readback'):",  dyn.getMethod('_get_readback')
    print
    
    print "--HELLOWORLD1--"
    dyn = Dynamic("HELLOWORLD1", "IDL:alma/acsexmplHelloWorld/HelloWorld:1.0")
    print "dyn.getMethod('displayMessage'):",  dyn.getMethod('displayMessage')
    try:
        print "dyn.getMethod('nonexistentMethod'):",  dyn.getMethod('nonexistentMethod')
    except CORBA.NO_RESOURCES, exception:
        print "Good...cannot invoke 'nonexistent' which does not exist in the IDL"           
    print
    
    print "--BADCOMPONENT--"
    try:
        dyn = Dynamic("BADCOMPONENT", "crap")
        print dir(dyn)
    except CORBA.NO_RESOURCES, exception:
        print "Good...cannot create 'BADCOMPONENT' of no real type"
