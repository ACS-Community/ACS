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
# @(#) $Id: acspyTestCompReload.py,v 1.1 2005/04/29 21:15:40 dfugate Exp $
###############################################################################
'''
Tests reloading components
'''
from Acspy.Clients.SimpleClient import PySimpleClient
from time import sleep
###############################################################################
if __name__ == "__main__":
    simpleClient = PySimpleClient()
    #this bit a code should make the container print out one special statement
    print "...should see 1 module message now..."
    c1 = simpleClient.getComponent("RELOADCOMP1")
    sleep(4)
    #releasing it should do the same thing
    print "...and another one after it's released..."
    simpleClient.releaseComponent("RELOADCOMP1")
    sleep(4)
    
    print "...there should not be one here though..."
    c1 = simpleClient.getComponent("RELOADCOMP1")
    sleep(4)
    print "...or here..."
    c2 = simpleClient.getComponent("RELOADCOMP2")
    sleep(4)
    print "...or even here"
    simpleClient.releaseComponent("RELOADCOMP1")
    sleep(4)
    print "...but there should be one here..."
    simpleClient.releaseComponent("RELOADCOMP2")
    sleep(4)
    simpleClient.disconnect()
    
