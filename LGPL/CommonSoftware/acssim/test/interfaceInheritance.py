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
Tests interface inheritance using the CDB
'''
from sys import argv
import ACS
from Acspy.Clients.SimpleClient import PySimpleClient
from Acspy.Common.Callbacks     import CBvoid
from ACSImpl.DevIO import DevIO

#-------------------------------------------------------
class ReadbackDevIO(DevIO):

    def __init__(self):
        DevIO.__init__(self, 3.14)

    def read(self):
        return 3.14
#-------------------------------------------------------
class MyCallback(CBvoid):
     def working (self, completion, desc):
         '''
         '''
         print "Ramped PowerSupply startRamping CB: working method called"

#-------------------------------------------------------
from time import sleep
if __name__=="__main__":
    compName = "TEST_RPS_1"

    # Make an instance of the PySimpleClient
    simpleClient = PySimpleClient()
    comp = simpleClient.getComponent(compName)

    myCB = MyCallback()
    myCorbaCB = simpleClient.activateOffShoot(myCB)
    myDescIn = ACS.CBDescIn(0L, 0L, 0L)
    
    print "Ramped PowerSupply readback value:", comp._get_readback().get_sync()[0]

    comp.startRamping(1L, myCorbaCB, myDescIn)
    
    simpleClient.releaseComponent(compName)
    simpleClient.disconnect()
