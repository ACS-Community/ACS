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
__version__ = "$Id: LogTestComponent.py,v 1.5 2005/05/20 21:47:56 dfugate Exp $" 

#--REGULAR IMPORTS-------------------------------------------------------------
from os import environ
#--CORBA STUBS-----------------------------------------------------------------
#--ACS Imports-----------------------------------------------------------------
from perftestImpl.BasePerfComp import BasePerfComp
from Acspy.Util.Profiler       import Profiler
#--GLOBALS---------------------------------------------------------------------
#------------------------------------------------------------------------------
class LogTestComponent(BasePerfComp):
    #--------------------------------------------------------------------------
    def __init__(self):
        '''
        '''
        BasePerfComp.__init__(self)
    #------------------------------------------------------------------------------
    def method(self):
        '''
        void method();
        '''
        
        profiler = Profiler()
        
        tString=""
        for i in range(0, self.size):
            tString = tString + "*"

        for i in range(0, self.count):
            profiler.start()
            self.getLogger().logInfo(tString)
            profiler.stop()
            self.waitAwhile()

        if environ.has_key("ACS_LOG_STDOUT"):
            profiler.addData("ACS_LOG_STDOUT", environ["ACS_LOG_STDOUT"])
        else:
            profiler.addData("ACS_LOG_STDOUT", "None")
        profiler.fullDescription("ACS Log of Size '" + str(self.size) + "' Bytes from within a CharacteristicComponent")
        return 
    #------------------------------------------------------------------------------
