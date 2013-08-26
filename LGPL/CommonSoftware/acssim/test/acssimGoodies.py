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
Tests Acssim.Goodies
'''
from Acssim.Goodies import *
#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "--Testing----------------------------------------------------------"
    print "getComponent('dkfjkdf'):", getComponent('dkfjkdf')
    
    addComponent("my", 7)
    print "Good:", getComponent("my")

    removeComponent("my")
    try:
        removeComponent("dfdfd")
        print "Should not see this!"
    except:
        print "Good."
             
    print "Done."
    print
    
    print "--Testing----------------------------------------------------------"
    
    print "getSimProxy('TEST_PS_1'):", getSimProxy('TEST_PS_1')
    print
    print "getComponentXMLObj('TEST_PS_1'):", getComponentXMLObj('TEST_PS_1')
    print "getComponentXMLObj('HELLOWORLD1'):", getComponentXMLObj('HELLOWORLD1')
    print "getComponentXMLObj(''):", getComponentXMLObj('')
    print
    print "setGlobalData('stuff',1L):", setGlobalData('stuff', 1L)
    print "getGlobalData():", getGlobalData('stuff')
    print "removeGlobalData('stuff'):", removeGlobalData('stuff')
    print "getGlobalData():", getGlobalData('stuff')
    print
    print "getCompLocalNS('TEST_PS_1'):", getCompLocalNS('TEST_PS_1')
    print "getCompLocalNS('HELLOWORLD1'):", getCompLocalNS('HELLOWORLD1')
    print "getCompLocalNS(''):", getCompLocalNS('')
    print
    print "getCHARS():", getCHARS()
    print "setCHARS(('a', 'b')):", setCHARS(('a', 'b'))
    print "getCHARS():", getCHARS()
    print
    print "getStandardTimeout():", getStandardTimeout()
    print "setStandardTimeout(5.0):", setStandardTimeout(5.0)
    print "getStandardTimeout():", getStandardTimeout()
    print
    print "getExceptProb():", getExceptProb()
    print "setExceptProb(0.5):", setExceptProb(0.5)
    print "getExceptProb():", getExceptProb()
    print
    print "getMaxSeqSize():", getMaxSeqSize()
    print "setMaxSeqSize(50):", setMaxSeqSize(50)
    print "getMaxSeqSize():", getMaxSeqSize()
    
    
    
    