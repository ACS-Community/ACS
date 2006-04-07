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

'''
Tests DynamicImplementation.
'''
from Acssim.Corba.DynamicImplementation import DynamicImplementation

if __name__ == "__main__":
    print "-----------------------------------------------------------------"
    g = DynamicImplementation("IDL:alma/acssimTest/JunkControl:1.0")
    g.onewayOn()
    g.on()
    g._set_charplainAttribute('a')
    g._set_charplainSeqAttribute(('a', 'b', 'c'))
    g._set_charplainArrayAttribute(('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l'))

    print "-----------------------------------------------------------------"
    h = DynamicImplementation("IDL:alma/PS/PowerSupply:1.0")
    print "PowerSupply dict:", dir(h)

    print "-----------------------------------------------------------------"
    i = DynamicImplementation("IDL:alma/RampedPS/RampedPowerSupply:1.0")
    print "PowerSupply dict:", dir(i)
    print "-----------------------------------------------------------------"
    try:
        j =  DynamicImplementation("IDL:alma/doesnt/Exist:1.0")
    except ImportError, ex:
        print "Good...ImportError when trying to create 'DL:alma/doesnt/Exist:1.0'"
    
    try:
        k =  DynamicImplementation("puregarbage")
    except IndexError, ex:
        print "Good...ImportError when trying to create 'DL:alma/doesnt/Exist:1.0'"
    print "-----------------------------------------------------------------"
    print "Done..."
