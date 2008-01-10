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
# "@(#) $Id: TestLogLevelsComp.py,v 1.5 2008/01/10 16:12:06 eallaert Exp $"
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
from Acspy.Common.Log                  import getLevelName
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
        self.getLogger().logTrace("TestLogLevelsComp.TestLogLevelsComp")

    #------------------------------------------------------------------------------
    def cleanUp(self):
        '''
        Override this method inherited from ComponentLifecycle
        '''
        self.getLogger().logInfo("Destroying " + self.name + "...") 
    #------------------------------------------------------------------------------
    #--Implementation of IDL methods-----------------------------------------------
    #------------------------------------------------------------------------------
    def getLevels(self):
        '''
        Python implementation of IDL method.
        LongSeq getLevels();
        '''
        mylogger = self.getLogger()
        mylogger.logInfo("called...")
        levels = mylogger.getLevels()
        
        return [2, 2, min(levels.minLogLevel, levels.minLogLevelLocal), levels.minLogLevel, levels.minLogLevelLocal]

    #------------------------------------------------------------------------------
    def logDummyMessages(self, levels):
        '''
        Python implementation of IDL method.
        void logDummyMessages(in LongSeq levels);
        '''
        mylogger = self.getLogger()
        for l in levels:
            mylogger.logAtLevel(l, "dummy log message for core level %d/%s" % (l, getLevelName(l)))
        mylogger.logAtLevel(levels[-2], "===last log message===")
        # Python seems to sends logs in packets of 10 logs, so add 9 messages to
        # ensure all the above logs get sent across right now.
        for i in range(1,10):
            mylogger.logAtLevel(levels[-2], "===packet fill-up message===")
           
        
#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Creating an object"
    g = TestLogLevelsComp()
    print "Done..."












#
# ___oOo___
