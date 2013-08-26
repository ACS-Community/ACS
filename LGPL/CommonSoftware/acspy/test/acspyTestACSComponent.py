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
# @(#) $Id: acspyTestACSComponent.py,v 1.2 2004/04/21 22:36:36 dfugate Exp $
###############################################################################
'''
Tests the Python ACSComponent.
'''
###############################################################################
from Acspy.Servants.ACSComponent import ACSComponent
import ACS
###############################################################################
if __name__ == "__main__":
    g = ACSComponent()
    g.setName("blar")
    print g._get_name()
    g.setComponentState(ACS.COMPSTATE_INITIALIZING)
    print g._get_componentState()
    print "Done..."
