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
__version__ = "$Id: SimpleBACIComponent.py,v 1.1 2004/09/22 22:46:51 dfugate Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
#--CORBA STUBS-----------------------------------------------------------------
import perftest__POA
from ACSErr               import Completion
from ACS                  import CBDescOut
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.ContainerServices       import ContainerServices
from Acspy.Servants.ComponentLifecycle      import ComponentLifecycle

from perftestImpl.BasePerfComp import BasePerfComp
from Acspy.Util.BaciHelper     import addProperty
from Acspy.Util.Scheduler      import Scheduler
from Acspy.Common.TimeHelper   import getTimeStamp
#--GLOBALS---------------------------------------------------------------------
#------------------------------------------------------------------------------
class SimpleBACIComponent(perftest__POA.SimpleBACIComponent,
                          BasePerfComp):
    #------------------------------------------------------------------------------
    def initialize(self):
        '''
        '''
        self.scheduler = Scheduler()
        
        addProperty(self, "property")
        return
    #------------------------------------------------------------------------------
    def action(self, cb, desc):
        '''
        void action(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def actionImpl(self, cb, desc):
        '''
        '''
        print "Aysnc. action finished"
        compl = Completion(long(getTimeStamp().value),  #unsigned long long timeStamp;
                           0L,  #ACSErr::CompletionType type;
                           0L,  #ACSErr::CompletionCode code;
                           ())  #ErrorLinkedList  previousError;
        
        cb.done(compl, CBDescOut(0L, desc.id_tag))
        return
    #------------------------------------------------------------------------------  
