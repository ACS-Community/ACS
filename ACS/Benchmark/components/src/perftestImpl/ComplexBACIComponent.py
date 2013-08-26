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
__version__ = "$Id: ComplexBACIComponent.py,v 1.2 2004/09/24 21:19:53 dfugate Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
#--CORBA STUBS-----------------------------------------------------------------
import perftest__POA
from ACSErr               import Completion
from ACS                  import CBDescOut
#--ACS Imports-----------------------------------------------------------------
from perftestImpl.BasePerfComp import BasePerfComp
from Acspy.Util.BaciHelper     import addProperty
from Acspy.Util.Scheduler      import Scheduler
from Acspy.Common.TimeHelper   import getTimeStamp
#--GLOBALS---------------------------------------------------------------------
#------------------------------------------------------------------------------
class ComplexBACIComponent(perftest__POA.ComplexBACIComponent,
                           BasePerfComp):
    #------------------------------------------------------------------------------
    def initialize(self):
        '''
        '''
        self.scheduler = Scheduler()
        
        addProperty(self, "property01")
        addProperty(self, "property02")
        addProperty(self, "property03")
        addProperty(self, "property04")
        addProperty(self, "property05")
        addProperty(self, "property06")
        addProperty(self, "property07")
        addProperty(self, "property08")
        addProperty(self, "property09")
        addProperty(self, "property10")
        addProperty(self, "property11")
        addProperty(self, "property12")
        addProperty(self, "property13")
        addProperty(self, "property14")
        addProperty(self, "property15")
        addProperty(self, "property16")
        return
    #------------------------------------------------------------------------------
    def action01(self, cb, desc):
        '''
        void action01(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action02(self, cb, desc):
        '''
        void action02(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action03(self, cb, desc):
        '''
        void action03(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action04(self, cb, desc):
        '''
        void action04(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action05(self, cb, desc):
        '''
        void action05(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action06(self, cb, desc):
        '''
        void action(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action07(self, cb, desc):
        '''
        void action(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action08(self, cb, desc):
        '''
        void action(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action09(self, cb, desc):
        '''
        void action(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action10(self, cb, desc):
        '''
        void action(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action11(self, cb, desc):
        '''
        void action(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action12(self, cb, desc):
        '''
        void action(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action13(self, cb, desc):
        '''
        void action(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action14(self, cb, desc):
        '''
        void action(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action15(self, cb, desc):
        '''
        void action(in ACS::CBvoid cb, in ACS::CBDescIn desc);
        '''
        self.scheduler.scheduleTimeout(self.actionImpl,
                                       0,
                                       0,
                                       (cb, desc))
        return
    #------------------------------------------------------------------------------
    def action16(self, cb, desc):
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
