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
#------------------------------------------------------------------------------
'''
TODO:
- All!!!
'''
__version__ = "$Id: BasePerfComp.py,v 1.2 2004/10/21 16:55:57 dfugate Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from time import sleep
#--CORBA STUBS-----------------------------------------------------------------
import perftest__POA
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.CharacteristicComponent import CharacteristicComponent
from Acspy.Servants.ContainerServices       import ContainerServices
from Acspy.Servants.ComponentLifecycle      import ComponentLifecycle
#--GLOBALS---------------------------------------------------------------------
#------------------------------------------------------------------------------
class BasePerfComp(perftest__POA.BasePerfComp,
                   CharacteristicComponent,
                   ContainerServices,
                   ComponentLifecycle):
    #------------------------------------------------------------------------------
    def __init__(self):
        '''
        Just call superclass constructors here.
        '''
        CharacteristicComponent.__init__(self)
        ContainerServices.__init__(self)

        self.count = 0
        self.size = 0
        self.waitTime = 0L
        return
    #------------------------------------------------------------------------------
    def method(self):
        '''
        void method();
        '''
        pass
    #------------------------------------------------------------------------------
    def setup(self, count, size, waitTime):
        '''
        void setup(in unsigned long count, in unsigned long size);
        '''
        self.count = count
        self.size = size
        self.waitTime = waitTime/10000000.0 #convert into seconds
        return
    #------------------------------------------------------------------------------
    def waitAwhile(self):
        '''
        Helper function
        '''
        sleep(self.waitTime)
    
