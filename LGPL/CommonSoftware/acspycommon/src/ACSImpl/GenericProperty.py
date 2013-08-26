# @(#) $Id: GenericProperty.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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
# "@(#) $Id: GenericProperty.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004/07/21  Created.
#------------------------------------------------------------------------------

'''
This module provides an implementation of the TypelessProperty IDL interface:

TODO:
- alarms dont work
- asynchronous methods are not really asynchronous
'''

__version__ = "$Id: GenericProperty.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
from CORBA                import NO_IMPLEMENT
from ACSErr               import Completion
from ACS                  import CBDescOut
from ACSErrTypeCommonImpl import CouldntAccessPropertyCompletionImpl
#--ACS Imports-----------------------------------------------------------------
from ACSImpl.CharacteristicModel  import CharacteristicModel
from Acspy.Common.CDBAccess       import CDBaccess
from ACSImpl.DevIO                import DevIO
from Acspy.Common.TimeHelper      import getTimeStamp
from Acspy.Util.Scheduler         import Scheduler
from Acspy.Util.XmlObjectifier    import XmlObject
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
#Scheduler which is responsible for handling monitors/alarms
GLOBAL_SCHEDULER = Scheduler()

class Property(CharacteristicModel):
    '''
    Properties can be derived from Property only if their IDL derives from
    ACS::Property.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, name, charCompRef):
        '''
        Constructor

        Params:
        - name is the quite literally the name of the property
        - charCompRef is the characteristic component object which contains this
        property

        Returns: Nothing

        Raises: Nothing.
        '''

        #In a perfect world CDB config files would be named similarly to
        #the IDL interface...but this is not the case. Therefore the following
        #is not going to work.
        #now it looks like ['IDL', 'alma/demo/HelloDemo', '1.0']
        #compInterfaceName = charCompRef._NP_RepositoryId.split(':')
        #take the second to last element; split that based on '/'; then take the list element
        #should be the interface name...
        #compInterfaceName = compInterfaceName[len(compInterfaceName)-2].split('/').pop()
        #combine the component (IDL) name and the property name
        #name = compInterfaceName + ":" + name

        #call the superclass constructor
        CharacteristicModel.__init__(self, name)

        #save a copy of the reference to the characteristic component this
        #property lives in
        self.__compRef = charCompRef

        #DevIO instance
        self.value = None

        self.getLogger().logDebug("Property baseclass initialized")
        return
    #--------------------------------------------------------------------------
    def _get_name(self):
        '''
        Implementation of the IDL attribute.

        readonly attribute string name;
        '''
        return self.name
    #--------------------------------------------------------------------------
    def _get_characteristic_component_name(self):
        '''
        Implementation of the IDL attribute.

        readonly attribute string characteristic_component_name;
        '''
        #just invoke a method of the component
        return self.__compRef._get_name()
    #--------------------------------------------------------------------------
    def getLogger(self):
        '''
        Helper method returns a reference to the components (this property lives
        within) logger.
        '''
        return self.__compRef.getLogger()
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
class TypelessProperty(Property):
    '''
    Properties can be derived from TypelessProperty only if their IDL derives from
    ACS::TypelessProperty.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, name, charCompRef):
        '''
        Constructor

        Params:
        - name is the quite literally the name of the property
        - charCompRef is the characteristic component object which contains this
        property

        Returns: Nothing

        Raises: Nothing.
        '''
        #just call the superclass constructor
        Property.__init__(self, name, charCompRef)

        #setup a default value for all attributes
        self.cdbDict = {}

        try:
            #need access to the CDB to determine characteristics of this property
            cdbAccess = CDBaccess()
            
            #make sure the entry exists first of all...
            t_xml = cdbAccess.getField("alma/" + self._get_characteristic_component_name())
            
            #create an xml helper object
            xml_obj = XmlObject(xmlString = t_xml)

            #get the top-level element
            xml_obj = xml_obj.firstChild

            t_prop_name = self._get_name().split('/')[1]

            #get the property we're looking for
            xml_obj = xml_obj.getElementsByTagName(t_prop_name)[0]

            #setup the CDB dict using attributes found within the CDB
            for attr in xml_obj.attributes.keys():
                self.cdbDict[attr] = xml_obj.getAttribute(attr)
        except:
            #print_exc()
            self.getLogger().logWarning("Some problem occurred when attempting to retrieve data from the ACS CDB for: alma/" +
                                        self._get_characteristic_component_name()  + "/" + self._get_name().split('/')[1])
        
        return
    #--------------------------------------------------------------------------
    def _get_description(self):
        '''
        Implementation of the IDL attribute.

        readonly attribute string description;
        '''
        try:
            return str(self.getCDBDict()['description'])
        except:
            #warn them about CDB access
            self.getLogger().logWarning("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead
            return "No description available"
    #--------------------------------------------------------------------------
    def _get_format(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute string format;
        '''
        try:
            return str(self.getCDBDict()['format'])
        except:
            #warn them about CDB access
            self.getLogger().logWarning("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead
            return "No format available"
    #--------------------------------------------------------------------------
    def _get_units(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute string units;
        '''
        try:
            return str(self.getCDBDict()['units'])
        except:
            #warn them about CDB access
            self.getLogger().logWarning("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead
            return "No units available"
    #--------------------------------------------------------------------------
    def _get_resolution(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute pattern resolution;
        '''
        try:
            return long(self.getCDBDict()['resolution'])
        except:
            #warn them about CDB access
            self.getLogger().logWarning("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead
            return 65535L
    #--------------------------------------------------------------------------
    def getCDBDict(self):
        '''
        Helper method returns the CDB dictionary associated with this property.
        '''
        
        return self.cdbDict
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
class GenericProperty(TypelessProperty):
    '''
    This intermediary class is used so certain methods in TypelessProperty-derived
    IDL interfaces do not have to be needlessly duplicated.
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
        TypelessProperty.__init__(self, name, charCompRef)
        self.monitors=[]

        if devIORef==None:
            self.value = DevIO(self._get_default_value())
        else:
            self.value = devIORef
        return
    #--------------------------------------------------------------------------
    #--Helper methods to be overriden in subclasses----------------------------
    #--------------------------------------------------------------------------
    def coerceToPropertyType(self, value=None):
        '''
        This helper method MUST be overriden in subclasses. Basically it is used
        to coerce a stringified value of the property type to its correct type.
        
        '''
        del value  #to make pychecker happy
        self.getLogger().logCritical("Looks like this method was never overriden in a subclass!")
        raise NO_IMPLEMENT()
    #--------------------------------------------------------------------------
    def getMonitorObject(self, scheduler, timeoutID):
        '''
        Helper method.
        '''
        del scheduler  #to make pychecker happy
        del timeoutID  #to make pychecker happy
        
        self.getLogger().logCritical("Looks like this method was never overriden in a subclass!")
        raise NO_IMPLEMENT()
    #--------------------------------------------------------------------------
    #--From "P" properties-----------------------------------------------------
    #--------------------------------------------------------------------------
    def _get_default_timer_trigger(self):
        '''
        Implementation of the IDL attribute.

        readonly attribute TimeInterval default_timer_trigger;
        '''
        try:
            return long(float(self.getCDBDict()['default_timer_trig']) * 10000000.0)
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve default timer trigger data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead... 0.1 seconds
            return 1000000L
    #--------------------------------------------------------------------------
    def _get_min_timer_trigger(self):
        '''
        Implementation of the IDL attribute.

        readonly attribute TimeInterval min_timer_trigger;
        '''
        try:
            return long(float(self.getCDBDict()['min_timer_trig']) * 10000000.0)
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve minimum timer trigger data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead... 0.001 seconds
            return 10000L
    #--------------------------------------------------------------------------
    def _get_min_delta_trigger(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) min_delta_trigger;
        '''
        try:
            return self.coerceToPropertyType(str(self.getCDBDict()['min_delta_trig']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead. It's up to the overriden
            #coerceToPropertyType method to decide what an acceptable default
            #value is!
            return self.coerceToPropertyType(str(0))
    #--------------------------------------------------------------------------
    def _get_default_value(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) default_value;
        '''
        try:
            return self.coerceToPropertyType(str(self.getCDBDict()['default_value']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead. It's up to the overriden
            #coerceToPropertyType method to decide what an acceptable default
            #value is!
            return self.coerceToPropertyType(None)
    #--------------------------------------------------------------------------
    def _get_graph_min(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) graph_min;
        '''
        try:
            return self.coerceToPropertyType(str(self.getCDBDict()['graph_min']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return self.coerceToPropertyType(str(0))
    #--------------------------------------------------------------------------
    def _get_graph_max(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) graph_max;
        '''
        try:
            return self.coerceToPropertyType(str(self.getCDBDict()['graph_max']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return self.coerceToPropertyType(str(1000))
    #--------------------------------------------------------------------------
    def _get_min_step(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) min_step;
        '''
        try:
            return self.coerceToPropertyType(str(self.getCDBDict()['min_step']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead. It's up to the overriden
            #coerceToPropertyType method to decide what an acceptable default
            #value is!
            return self.coerceToPropertyType(str(0))
    #--------------------------------------------------------------------------
    def get_sync(self):
        '''
        Implementation of the IDL method.

        stringSeq get_sync (out ACSErr::Completion c);
        '''
        try:
            #first just try to get the value from the DevIO...
            retVal = self.value.read()

            #in order to provide the same interface as the C++ DevIOs,
            #a Python DevIO is expected to pass back a list of value
            #and timestamp.  To preserve backward compatablity, if the
            #DevIO does not return a tuple, a timestamp will be generated
            #as before.
            if isinstance(retVal,tuple):
                ts = retVal[-1]
                retVal = retVal[0]
            else:
                ts = long(getTimeStamp().value)

            #succeeded! now just create a "no-error" completion
            compl = Completion(ts,  #unsigned long long timeStamp;
                               0L,  #ACSErr::CompletionType type;
                               0L,  #ACSErr::CompletionCode code;
                               ())  #ErrorLinkedList  previousError;
        except Exception, e:
            self.getLogger().logAlert("Some problem occurred when accessing the DevIO's read method")
            print_exc()
            #whoops...something failed. use the default value as a return
            #value instead. this is bad to do but get_sync does not throw
            #IDL exceptions!
            retVal = self._get_default_value()

            #let's see if it raised an ACS Error
            #System exception first...
            try:
                compl = CouldntAccessPropertyCompletionImpl(exception=e)
            except:
                #a native Python exception was raised...not much we can do
                #here
                compl = CouldntAccessPropertyCompletionImpl()

                
        return (retVal, compl)
    #--------------------------------------------------------------------------
    def get_async(self, cb, desc):
        '''
        Implementation of the IDL method.
        
        void get_async (in CBstringSeq cb, in CBDescIn desc);
        '''
        self.getLogger().logInfo('Not really asynchronous for now...')
        retVal, compl = self.get_sync()
        cb.done(retVal, compl, CBDescOut(0L, desc.id_tag))
        return 
    #--------------------------------------------------------------------------
    def get_history(self, n_last_values):
        '''
        Implementation of the IDL method.
        
        long get_history (in long n_last_values,
                          out stringSeqSeq vs,
			  out TimeSeq ts);
        '''
        del n_last_values   #to make pychecker happy
        raise NO_IMPLEMENT()
        return
    #--------------------------------------------------------------------------
    def create_monitor(self, cb, desc):
        '''
        Implementation of the IDL method.
        
        MonitorunknownType create_monitor (in CBunknownType cb, in CBDescIn desc);
        '''
        self.monitorRunner(cb, desc)
        return self.create_postponed_monitor(0L, cb, desc)
    #--------------------------------------------------------------------------
    def monitorRunner(self, cb, desc):
        '''
        Helper method
        '''
        (value, compl) = self.get_sync()
        cb.working(value, compl, CBDescOut(0L, desc.id_tag))
        return
    #--------------------------------------------------------------------------
    def create_postponed_monitor(self, start_time, cb, desc):
        '''
        Implementation of the IDL method.
        
        MonitorstringSeq create_postponed_monitor (in Time start_time,
                                                   in CBstringSeq cb,
			                           in CBDescIn desc);
        '''
        
        #if they want to use the default timeout value, use it
        #if desc.normal_timeout==0L:
        frequency=self._get_default_timer_trigger()
        #else:
        #    frequency=desc.normal_timeout
        
        timeoutID = GLOBAL_SCHEDULER.scheduleTimeout(self.monitorRunner,
                                                     start_time,
                                                     frequency,
                                                     (cb, desc))

        monitorObject = self.getMonitorObject(GLOBAL_SCHEDULER, timeoutID)
        monitorCORBAObject = monitorObject._this()

        tDict = {'start_time': start_time,
                 'cb':cb,
                 'desc':desc,
                 'timeoutID':timeoutID,
                 'monitorObject':monitorObject,
                 'monitorCORBAObject':monitorCORBAObject
                 }
        
        self.monitors.append(tDict)
        return monitorCORBAObject
    #--------------------------------------------------------------------------
    #--From RO properties------------------------------------------------------
    #--------------------------------------------------------------------------
    def _get_alarm_low_on(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) alarm_low_on;
        '''
        try:
            return self.coerceToPropertyType(str(self.getCDBDict()['alarm_low_on']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return self.coerceToPropertyType(str(0))
    #--------------------------------------------------------------------------
    def _get_alarm_low_off(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) alarm_low_off;
        '''
        try:
            return self.coerceToPropertyType(str(self.getCDBDict()['alarm_low_off']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return self.coerceToPropertyType(str(0))
    #--------------------------------------------------------------------------
    def _get_alarm_high_on(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) alarm_high_on;
        '''
        try:
            return self.coerceToPropertyType(str(self.getCDBDict()['alarm_high_on']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return self.coerceToPropertyType(str(0))
    #--------------------------------------------------------------------------
    def _get_alarm_high_off(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) alarm_high_off;
        '''
        try:
            return self.coerceToPropertyType(str(self.getCDBDict()['alarm_high_off']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return self.coerceToPropertyType(str(0))
    #--------------------------------------------------------------------------
    def new_subscription_Alarm(self, cb, desc):
        '''
        Implementation of the IDL method.

        Subscription new_subscription_Alarm(in AlarmunknownType cb,in CBDescIn desc);
        '''
        raise NO_IMPLEMENT
        return
    #--------------------------------------------------------------------------
    #--From RW properties------------------------------------------------------
    #--------------------------------------------------------------------------
    def _get_min_value(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) min_value;
        '''
        try:
            return self.coerceToPropertyType(str(self.getCDBDict()['min_value']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return self.coerceToPropertyType(str(0))
    #--------------------------------------------------------------------------
    def _get_max_value(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) max_value;
        '''
        try:
            return self.coerceToPropertyType(str(self.getCDBDict()['max_value']))
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return self.coerceToPropertyType(str(1000))
    #--------------------------------------------------------------------------
    def set_sync(self, newVal):
        '''
        Implementation of the IDL method.

        ACSErr::Completion set_sync (in double value);
        '''
        try:
            #first just try to get the value from the DevIO...
            compl = self.value.write(newVal)

            #developer didn't want to create a completion.
            if compl == None:
                #succeeded! now just create a "no-error" completion
                compl = Completion(long(getTimeStamp().value),  #unsigned long long timeStamp;
                                   0L,  #ACSErr::CompletionType type;
                                   0L,  #ACSErr::CompletionCode code;
                                   ())  #ErrorLinkedList  previousError;
        except Exception, e:
            self.getLogger().logAlert("Some problem occurred when using the DevIO's write method")
            print_exc()
            #let's see if it raised an ACS Error
            #System exception first...
            try:
                compl = CouldntAccessPropertyCompletionImpl(exception=e)
            except:
                #a native Python exception was raised...not much we can do
                #here
                compl = CouldntAccessPropertyCompletionImpl()
                
        return compl
    #--------------------------------------------------------------------------
    def set_async(self, newVal, cb, desc):
        '''
        Implementation of the IDL method.
        
        void set_async (in double value, in CBvoid cb, in CBDescIn desc);
        '''
        self.getLogger().logInfo('Not really asynchronous for now...')
        compl = self.set_sync(newVal)
        cb.done(compl, CBDescOut(0L, desc.id_tag))
        return 
    #--------------------------------------------------------------------------
    def set_nonblocking(self, newVal):
        '''
        Implementation of the IDL method.
        
        void set_nonblocking (in double value);
        '''
        try:
            self.value.write(newVal)
        except:
            self.getLogger().logAlert("Some problem occurred when using the DevIO's write method")
            print_exc()
    #--------------------------------------------------------------------------
    def increment(self, cb, desc):
        '''
        Implementation of the IDL method.
        
        void increment (in CBvoid cb, in CBDescIn desc);
        '''
        compl = self.set_sync(self.get_sync()[0] + 1)
        cb.done(compl, CBDescOut(0L, desc.id_tag))
        return 
    #--------------------------------------------------------------------------
    def decrement(self, cb, desc):
        '''
        Implementation of the IDL method.
        
        void decrement (in CBvoid cb, in CBDescIn desc);
        '''
        compl = self.set_sync(self.get_sync()[0] - 1)
        cb.done(compl, CBDescOut(0L, desc.id_tag))
        return
    #--------------------------------------------------------------------------

    
#-------------------------------------------------------------------------
