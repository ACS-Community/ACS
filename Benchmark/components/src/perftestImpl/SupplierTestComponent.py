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
__version__ = "$Id: SupplierTestComponent.py,v 1.3 2004/10/21 18:40:38 dfugate Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
#--CORBA STUBS-----------------------------------------------------------------
import perftest
#--ACS Imports-----------------------------------------------------------------
from perftestImpl.BasePerfComp import BasePerfComp
from Acspy.Util.Profiler       import Profiler
from Acspy.Nc.Supplier         import Supplier
#--GLOBALS---------------------------------------------------------------------
#------------------------------------------------------------------------------
class SupplierTestComponent(BasePerfComp):
    #------------------------------------------------------------------------------
    def initialize(self):
        '''
        '''
        self.supplier = Supplier("perf channel")
        return
    #------------------------------------------------------------------------------
    def method(self):
        '''
        void method();
        '''
        profiler = Profiler()
        
        tString=""
        for i in range(0, self.size):
            tString = tString + "*"

        joe = perftest.charSeqStruct(tString)

        for i in range(0, self.count):
            profiler.start()
            self.supplier.publishEvent(joe)
            profiler.stop()
            self.waitAwhile()

        profiler.fullDescription("Event Channel Event of Size '" + str(self.size) + "' Bytes from within a CharacteristicComponent")
        return
    #------------------------------------------------------------------------------
