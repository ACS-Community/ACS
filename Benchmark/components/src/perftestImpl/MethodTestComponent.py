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
__version__ = "$Id: MethodTestComponent.py,v 1.4 2006/05/12 20:39:56 dfugate Exp $" 

#--REGULAR IMPORTS-------------------------------------------------------------
#--CORBA STUBS-----------------------------------------------------------------
import perftest__POA
#--ACS Imports-----------------------------------------------------------------
from perftestImpl.BasePerfComp import BasePerfComp
#--GLOBALS---------------------------------------------------------------------
#------------------------------------------------------------------------------
class MethodTestComponent(perftest__POA.MethodTestComponent,
                          BasePerfComp):
    #--------------------------------------------------------------------------
    def __init__(self):
        '''
        '''
        BasePerfComp.__init__(self)
        self.retVal = ""
    #------------------------------------------------------------------------------
    def setup(self, count, size, waitTime):
        '''
        void setup(in unsigned long count, in unsigned long size);
        '''
        BasePerfComp.setup(self, count, size, waitTime)
        self.retVal = ""

        for i in range(0, self.size):
            self.retVal = self.retVal + "*"
            
        return
    #------------------------------------------------------------------------------
    def testReturnSize(self):
        '''
        charSeq testReturnSize();
        '''
        return self.retVal

    def testInParam(self, chars):
        '''
        void testInParam(in charSeq chars);
        '''
        return
    #------------------------------------------------------------------------------
