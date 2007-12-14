#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2007 
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id: TestLogLevelsComp.py,v 1.1 2007/12/14 16:53:32 eallaert Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# arne  2007-11-14  created
#

#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------
import contLogTest__POA
import contLogTest
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Servants.ACSComponent       import ACSComponent
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class TestLogLevelsComp(contLogTest__POA.TestLogLevelsComp,  #CORBA stubs for IDL interface
                ACSComponent,  #Base IDL interface
                ContainerServices,  #Developer niceties
                ComponentLifecycle):  #HLA stuff
    '''
    Simple component implementation provided as a reference for developers.
    '''
    def __init__(self):
        '''
        Just call superclass constructors here.
        '''
        ACSComponent.__init__(self)
        ContainerServices.__init__(self)
        return
    #------------------------------------------------------------------------------
    #--Override ComponentLifecycle methods-----------------------------------------
    #------------------------------------------------------------------------------
    def initialize(self):
        '''
        Override this method inherited from ComponentLifecycle
        '''
        self.getLogger().logTrace("called...")

    #------------------------------------------------------------------------------
    def cleanUp(self):
        '''
        Override this method inherited from ComponentLifecycle
        '''
        self.getLogger().logInfo("called...") 
    #------------------------------------------------------------------------------
    #--Implementation of IDL methods-----------------------------------------------
    #------------------------------------------------------------------------------
    def getLevels(self):
        '''
        Python implementation of IDL method.
        LongSeq getLevels();
        '''
        self.getLogger().logInfo("called...") 
        return [1, 2, 3, 4, 5]

    #------------------------------------------------------------------------------
    def logDummyMessages(self, levels):
        '''
        Python implementation of IDL method.
        void logDummyMessages(in LongSeq levels);
        '''
        self.getLogger().logInfo("called...") 
        
#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Creating an object"
    g = TestLogLevelsComp()
    print "Done..."












#
# ___oOo___
