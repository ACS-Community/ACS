# @(#) $Id: FloatSeq.py,v 1.1 2007/10/03 20:44:03 agrimstrup Exp $
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
# "@(#) $Id: FloatSeq.py,v 1.1 2007/10/03 20:44:03 agrimstrup Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004/07/21  Created.
#------------------------------------------------------------------------------

'''
This module provides an implementation of the PdoubleSeq IDL interface:
'''

__version__ = "$Id: FloatSeq.py,v 1.1 2007/10/03 20:44:03 agrimstrup Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
import ACS__POA
from ACS                  import CBDescOut
#--ACS Imports-----------------------------------------------------------------
from ACSImpl.GenericProperty     import GenericProperty
from Acssim.Monitors            import Monitorfloat
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
#--P property------------------------------------------------------------------
#------------------------------------------------------------------------------
class PfloatSeq(GenericProperty):
    '''
    Properties can be derived from PdoubleSeq only if their IDL derives from
    ACS::PdoubleSeq.
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
            #coerce into a double type (for floats specified like "123")
            for i in range(0, len(retVal)):
                retVal[i] = float(retVal[i])
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
        Helper method used to return a monitor of the correct type.
        '''
        return Monitorfloat(scheduler, timeoutID)
    #--------------------------------------------------------------------------
    #--Overriden methods because BACI is inconsistent--------------------------
    #--------------------------------------------------------------------------
    def _get_min_delta_trigger(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) min_delta_trigger;
        '''
        try:
            return float(str(self.getCDBDict()['min_delta_trig']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead. It's up to the overriden
            #coerceToPropertyType method to decide what an acceptable default
            #value is!
            return 0.0
    #--------------------------------------------------------------------------
    def _get_default_value(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) default_value;
        '''
        try:
            return float(str(self.getCDBDict()['default_value']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead. It's up to the overriden
            #coerceToPropertyType method to decide what an acceptable default
            #value is!
            return 0.0
    #--------------------------------------------------------------------------
    def _get_graph_min(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) graph_min;
        '''
        try:
            return float(str(self.getCDBDict()['graph_min']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 0.0
    #--------------------------------------------------------------------------
    def _get_graph_max(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) graph_max;
        '''
        try:
            return float(str(self.getCDBDict()['graph_max']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 1000.0
    #--------------------------------------------------------------------------
    def _get_min_step(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) min_step;
        '''
        try:
            return float(str(self.getCDBDict()['min_step']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead. It's up to the overriden
            #coerceToPropertyType method to decide what an acceptable default
            #value is!
            return 0.0
#------------------------------------------------------------------------------
#--RO property-----------------------------------------------------------------
#------------------------------------------------------------------------------
class ROfloatSeq(ACS__POA.ROfloatSeq, PfloatSeq):
    '''
    Properties can be derived from ROdoubleSeq only if their IDL derives from
    ACS::ROdoubleSeq.
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
        PfloatSeq.__init__(self, name, charCompRef, devIORef)
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
            return float(str(self.getCDBDict()['alarm_low_on']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 0.0
    #--------------------------------------------------------------------------
    def _get_alarm_low_off(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) alarm_low_off;
        '''
        try:
            return float(str(self.getCDBDict()['alarm_low_off']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 0.0
    #--------------------------------------------------------------------------
    def _get_alarm_high_on(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) alarm_high_on;
        '''
        try:
            return float(str(self.getCDBDict()['alarm_high_on']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 0.0
    #--------------------------------------------------------------------------
    def _get_alarm_high_off(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) alarm_high_off;
        '''
        try:
            return float(str(self.getCDBDict()['alarm_high_off']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 0.0
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
class RWfloatSeq(ACS__POA.RWfloatSeq, ROfloatSeq):
    '''
    Properties can be derived from ROdoubleSeq only if their IDL derives from
    ACS::ROdoubleSeq.
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
        ROdoubleSeq.__init__(self, name, charCompRef, devIORef)
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
            return float(str(self.getCDBDict()['min_value']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 0.0
    #--------------------------------------------------------------------------
    def _get_max_value(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) max_value;
        '''
        try:
            return float(str(self.getCDBDict()['max_value']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return 1000.0
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
