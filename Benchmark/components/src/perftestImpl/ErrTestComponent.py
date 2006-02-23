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
__version__ = "$Id: ErrTestComponent.py,v 1.2 2004/09/24 21:15:46 dfugate Exp $" 

#--REGULAR IMPORTS-------------------------------------------------------------
#--CORBA STUBS-----------------------------------------------------------------
import perftest__POA
#--ACS Imports-----------------------------------------------------------------
from perftestImpl.BasePerfComp import BasePerfComp
import BenchmarkErrTypeImpl
#--GLOBALS---------------------------------------------------------------------
#------------------------------------------------------------------------------
class ErrTestComponent(perftest__POA.ErrTestComponent,
                       BasePerfComp):
    #--------------------------------------------------------------------------
    def __init__(self):
        '''
        '''
        BasePerfComp.__init__(self)
    #--------------------------------------------------------------------------
    def getException(self, depth, e):
        '''
        '''
        if depth==0:
            return e
        else:
            return self.getException(depth-1,
                                     BenchmarkErrTypeImpl.BenchmarkErr0ExImpl(exception=e, create=1))
    #------------------------------------------------------------------------------
    def testExceptions(self, depth, err):
        '''
        void testExceptions(in long depth, in boolean err) raises (ACSErr::ACSException, BenchmarkErrType::BenchmarkErr0Ex);
        '''
        if depth < 0:
            print "Bad depth"
            return
            
        e = self.getException(depth,
                              BenchmarkErrTypeImpl.BenchmarkErr0ExImpl())

        if err==1:
            raise e
        return
    #------------------------------------------------------------------------------
