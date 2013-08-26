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
Tests BehaviorProxy
'''
from Acssim.Servants.Representations.BehaviorProxy import BehaviorProxy
from Acssim.Goodies import addComponent
import CORBA

class MockComponent:
    def __init__(self, name): self.name = name
    def _get_name(self): return self.name
    def activateOffShoot(self, os): return os

if __name__=="__main__":
    bp = BehaviorProxy("TEST_RPS_1")
    print "dir(TEST_RPS_1):", dir(bp)
    print
    
    addComponent("TEST_RPS_1", MockComponent("TEST_RPS_1"))
    try:
        print "bp.getMethod('on'):", bp.getMethod('on')
        print "Good!"
    except:
        print "Bad...cannot get 'on' method after a MockComponent has been created"
    print
    
    try:
        print "bp.getMethod('nonexistentMethod'):", bp.getMethod('nonexistentMethod')
        print "Bad!"
    except CORBA.NO_RESOURCES, temp_ex:
        print "Good...got a NO_RESOURCES exception for a nonexistent method"
    print
    
    try:
        bp = BehaviorProxy("NON_EXISTENT")
        print "Bad!"
    except CORBA.NO_RESOURCES, temp_ex:
        print "Good...could not create a 'NON_EXISTENT' proxy"
    print
    
    