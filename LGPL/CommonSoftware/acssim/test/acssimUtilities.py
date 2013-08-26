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
Tests Acssim.Corba.Utilities
'''
from Acssim.Corba.Utilities import *

def test_name(name):
    '''
    Tests a component name
    '''
    try:
        id = getCompIfrID(name)
    except:
        id = None
        
    try:
        interfaces = getSuperIDs(id)
    except:
        interfaces = []
        
    print "NAME:", name, "; ID:", id, "; INTERFACES:", interfaces

if __name__ == "__main__":
    test_name("TEST_PS_1")
    test_name("TEST_RPS_1")
    test_name("blah")
    test_name("")
    