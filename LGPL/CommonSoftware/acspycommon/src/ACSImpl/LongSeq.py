# @(#) $Id: LongSeq.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
#
# Copyright (C) 2001
# Associated Universities, Inc. Washington DC, USA.
#
# Produced for the ALMA project
#
# This library is free software; you can redistribute it and/or modify it under
# the terms of the GNU Library General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your option) any
# later version.
#
# This library is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
# details.
#
# You should have received a copy of the GNU Library General Public License
# along with this library; if not, write to the Free Software Foundation, Inc.,
# 675 Massachusetts Ave, Cambridge, MA 02139, USA.  Correspondence concerning
# ALMA should be addressed as follows:
#
# Internet email: alma-sw-admin@nrao.edu
# "@(#) $Id: LongSeq.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004/07/21  Created.
#------------------------------------------------------------------------------

'''
This module provides an implementation of the PlongSeq IDL interface:
'''

__version__ = "$Id: LongSeq.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
import ACS__POA
from ACS                  import CBDescOut
#--ACS Imports-----------------------------------------------------------------
from ACSImpl.GenericProperty     import GenericProperty
from ACSImpl.Monitors            import Monitorlong
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
#--P property------------------------------------------------------------------
#------------------------------------------------------------------------------
class PlongSeq(GenericProperty):
    '''
    Properties can be derived from PlongSeq only if their IDL derives from
    ACS::PlongSeq.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, name, charCompRef, devIORef):
        '''
        Constructor

        Params:
        - name is the quite literally the name of the property
        - charCompRef is the characteristic component object which contains this
        property
        - devIORef is a reference to a DevIO to be used with this property

        Returns: Nothing

        Raises: Nothing.
        '''
        GenericProperty.__init__(self, name, charCompRef, devIORef)
        return
    #--------------------------------------------------------------------------
    def coerceToPropertyType(self, value=None):
        '''
        Overriden.
        '''
        #something went wrong. Return default value
        if value==None:
            return []
        
        try:
            retVal = eval("[" + value + "]")
            #coerce into an int type
            for i in range(0, len(retVal)):
                retVal[i] = int(retVal[i])
            retVal = tuple(retVal)
            return retVal
        except:
            #warn them about CDB access
            self.getLogger().logAlert("Unble to coerce '" + str(value) + "' into the correct type!")
            print_exc()
            #return an acceptable default value instead...an empty sequence
            return []
    #--------------------------------------------------------------------------
    def getMonitorObject(self, scheduler, timeoutID):
        '''
        Helper method returns a monitor object of the correct type.
        '''
        return Monitorlong(scheduler, timeoutID)
    #--------------------------------------------------------------------------
    #--Overriden methods because BACI is inconsistent--------------------------
    #--------------------------------------------------------------------------
    def _get_min_delta_trigger(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) min_delta_trigger;
        '''
        try:
            return int(str(self.getCDBDict()['min_delta_trig']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead. It's up to the overriden
            #coerceToPropertyType method to decide what an acceptable default
            #value is!
            return 0
    #--------------------------------------------------------------------------
    def _get_default_value(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) default_value;
        '''
        try:
            return int(str(self.getCDBDict()['default_value']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead. It's up to the overriden
            #coerceToPropertyType method to decide what an acceptable default
            #value is!
            return 0
    #--------------------------------------------------------------------------
    def _get_graph_min(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) graph_min;
        '''
        try:
            return int(str(self.getCDBDict()['graph_min']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 0
    #--------------------------------------------------------------------------
    def _get_graph_max(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) graph_max;
        '''
        try:
            return int(str(self.getCDBDict()['graph_max']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 1000
    #--------------------------------------------------------------------------
    def _get_min_step(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) min_step;
        '''
        try:
            return int(str(self.getCDBDict()['min_step']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead. It's up to the overriden
            #coerceToPropertyType method to decide what an acceptable default
            #value is!
            return 0
#------------------------------------------------------------------------------
#--RO property-----------------------------------------------------------------
#------------------------------------------------------------------------------
class ROlongSeq(ACS__POA.ROlongSeq, PlongSeq):
    '''
    Properties can be derived from ROlongSeq only if their IDL derives from
    ACS::ROlongSeq.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, name, charCompRef, devIORef=None):
        '''
        Constructor

        Params:
        - name is the quite literally the name of the property
        - charCompRef is the characteristic component object which contains this
        property
        - devIORef is a reference to a DevIO to be used with this property

        Returns: Nothing

        Raises: Nothing.
        '''
        PlongSeq.__init__(self, name, charCompRef, devIORef)
        return
    #--------------------------------------------------------------------------
    #--Overriden methods because BACI is inconsistent--------------------------
    #--------------------------------------------------------------------------
    def _get_alarm_low_on(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) alarm_low_on;
        '''
        try:
            return int(str(self.getCDBDict()['alarm_low_on']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 0
    #--------------------------------------------------------------------------
    def _get_alarm_low_off(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) alarm_low_off;
        '''
        try:
            return int(str(self.getCDBDict()['alarm_low_off']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 0
    #--------------------------------------------------------------------------
    def _get_alarm_high_on(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) alarm_high_on;
        '''
        try:
            return int(str(self.getCDBDict()['alarm_high_on']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 0
    #--------------------------------------------------------------------------
    def _get_alarm_high_off(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) alarm_high_off;
        '''
        try:
            return int(str(self.getCDBDict()['alarm_high_off']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 0
    #--------------------------------------------------------------------------
    def get_sync(self):
        '''
        Overriden.
        '''
        retVal = list(GenericProperty.get_sync(self))
        try:
            retVal[0] = list(retVal[0])
        except:
            retVal[0] = [retVal[0]]
        return tuple(retVal)
#-----------------------------------------------------------------------------
#--RW property----------------------------------------------------------------
#-----------------------------------------------------------------------------
class RWlongSeq(ACS__POA.RWlongSeq, ROlongSeq):
    '''
    Properties can be derived from ROlongSeq only if their IDL derives from
    ACS::ROlongSeq.
    '''
    #-------------------------------------------------------------------------
    def __init__(self, name, charCompRef, devIORef=None):
        '''
        Constructor

        Params:
        - name is the quite literally the name of the property
        - charCompRef is the characteristic component object which contains this
        property
        - devIORef is a reference to a DevIO to be used with this property

        Returns: Nothing

        Raises: Nothing.
        '''
        ROlongSeq.__init__(self, name, charCompRef, devIORef)
        return
    #--------------------------------------------------------------------------
    #--Overriden methods because BACI is inconsistent--------------------------
    #--------------------------------------------------------------------------
    def _get_min_value(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) min_value;
        '''
        try:
            return int(str(self.getCDBDict()['min_value']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 0
    #--------------------------------------------------------------------------
    def _get_max_value(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) max_value;
        '''
        try:
            return int(str(self.getCDBDict()['max_value']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 1000
    #--------------------------------------------------------------------------
    def increment(self, cb, desc):
        '''
        Implementation of the IDL method.
        
        void increment (in CBvoid cb, in CBDescIn desc);
        '''
        compl = self.set_sync(map(lambda x: x+1,self.get_sync()[0]))
        cb.done(compl, CBDescOut(0L, desc.id_tag))
        return 
    #--------------------------------------------------------------------------
    def decrement(self, cb, desc):
        '''
        Implementation of the IDL method.
        
        void decrement (in CBvoid cb, in CBDescIn desc);
        '''
        compl = self.set_sync(map(lambda x: x-1,self.get_sync()[0]))
        cb.done(compl, CBDescOut(0L, desc.id_tag))
        return
#---------------------------------------------------------------------------
if __name__ == "__main__":
    pass
