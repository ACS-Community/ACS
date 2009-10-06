# @(#) $Id: Supplier.py,v 1.20 2009/10/06 09:04:56 javarias Exp $
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
'''
This module provides all classes dealing with publishing CORBA Notification
Service structured events.

TODO:
- nada
'''

__revision__ = "$Id: Supplier.py,v 1.20 2009/10/06 09:04:56 javarias Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
from omniORB                      import CORBA
from omniORB                      import any
from ACSErrTypeCommonImpl         import CORBAProblemExImpl
from ACSErrTypeCommonImpl         import CouldntPerformActionExImpl
from ACSErrTypeCommonImpl         import TypeNotSupportedExImpl
from ACSErr                       import NameValue
import CosNotifyChannelAdmin
import CosNotifyComm__POA
import CosNotification
import acsnc
import acscommon
#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log         import getLogger
from Acspy.Common.TimeHelper  import getTimeStamp
from Acspy.Util.ACSCorba      import getORB
from Acspy.Util               import NameTree
from Acspy.Nc.CDBProperties   import cdb_channel_config_exists
from Acspy.Nc.CDBProperties   import get_channel_qofs_props
from Acspy.Nc.CDBProperties   import get_channel_admin_props
from Acspy.Nc.CDBProperties   import get_integration_logs
from Acspy.Nc.CommonNC        import CommonNC
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class Supplier (CosNotifyComm__POA.StructuredPushSupplier, CommonNC):
    '''
    Class Supplier is the implementation of a CORBA structured push supplier. It
    is provided to hide details of the CORBA Notificiation Service
    from the developer.  Furthermore, one can reconfigure the quality of service
    properties and administrator properties for a given notification channel by
    overriding the correct methods in this class.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, channelname, component=None, domain=None):
        '''
        Constructor.

        Params:
        - channelName is the channel name
        - component is the component this supplier has been instantiated from
        (if applicable). This parameter is likely to become mandatory in future
        version of ACS
        - domain is the name of the domain of notification channels the channel
        belongs to


        Returns: Nothing

        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''
        
        #Logger for this supplier
        self.logger = getLogger(str(channelname) + "-Supplier")

        #Call the super's constructor
        CommonNC.__init__(self, channelname, component, domain)
        
        #CORBA ref to the channels supplier admin
        self.supplierAdmin = None  
        #CORBA ref to this suppliers structured proxy push consumer
        self.sppc = None
        #number of events sent so far
        self.count = 0

        #Handle all of the CORBA stuff now
        CommonNC.initCORBA(self)
        self.initCORBA()
    #------------------------------------------------------------------------------
    def initCORBA(self):
        '''
        Handles all the CORBA involved in creating a Supplier.

        Parameters: None

        Returns: Nothing

        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''        
        #Get admin object. 
        try:
            (self.supplierAdmin, adminid) = self.evtChan.new_for_suppliers(CosNotifyChannelAdmin.AND_OP)
            #to make the NRI happy
            adminid = 0
            
        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("reason",
                                                      "Unable to obtain supplier admin"),
                                            NameValue("exception",
                                                      str(e))])

        #Get proxy consumer; activate this supplier instance;
        try:
            (self.sppc, pid) = self.supplierAdmin.obtain_notification_push_consumer(CosNotifyChannelAdmin.STRUCTURED_EVENT)
            self.sppc = self.sppc._narrow(CosNotifyChannelAdmin.StructuredProxyPushConsumer)
            self.sppc.connect_structured_push_supplier(self._this())
            self.connected = 1
            
            #to make the NRI happy
            pid = 0
        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("reason",
                                                      "Bad proxy or admin"),
                                            NameValue("exception",
                                                      str(e))])
    #------------------------------------------------------------------------------
    def disconnect(self):
        '''
        User code MUST call this method when the Supplier is no longer useful.
        Failure to do so can result in remote memory leaks!

        Params: None

        Returns: Nothing

        Raises: Nothing
        '''
        try:
            if self.connected:
                #Destroy the remote objects first
                self.sppc.disconnect_structured_push_consumer()
                self.supplierAdmin.destroy()
                #DWF-deactivate the CORBA object!!!
                #Let the Python garbage collector take care of anything else.
        except Exception, e:
            self.logger.logWarning(str(e))
            print_exc()

        self.callback.disconnect()
        self.callback = None
        self.evtChan = None
        self.supplierAdmin = None  
        self.sppc = None  
        self.nt = None
        #Set this back to false
        self.connected = 0 
        return
    #------------------------------------------------------------------------------
    def disconnect_structured_push_supplier(self):
        '''
        Override this method to do something when a consumer unsubscribes from the
        channel. Developer code must never invoke this method!

        Params: Nothing

        Returns: Nothing

        Raises: Nothing
        '''
        self.logger.logTrace('')
        return
    #------------------------------------------------------------------------------
    def subscription_change(self, added, removed):
        '''
        Override this method so a "smart" Supplier subclass can publish (or not
        pubish) events based on Consumer demands.  Not very useful when there is
        more than one Supplier instance for a given channel.

        Params:
        - added subscription list
        - removed subscription list

        Returns: Nothing
        
        Raises:
        - InvalidEventType Throw this exception when a consumer subscribes (or
        unsubscribes) to a bad domain/type
        '''
        #to make pychecker happy
        added = None

        #to make pychecker happy
        removed = None
        
        self.logger.logTrace('')
        return
    #------------------------------------------------------------------------------
    def publishEvent (self,
                      simple_data=None,
                      event_callback=None,
                      type_name=None,
                      event_name="",
                      se=None,
                      supplier_name=None):
        '''
        publishEvent is the one method developers have to use.

        publishEvent() is designed so Supplier does not have to be subclassed.  

        Params:
        - simple_data is a user-defined IDL struct. If this parameter is not
        specified by the developer, se MUST be used.  99% of the time developers
        should specify the simple_data parameter and NOTHING ELSE!
        - event_callback is a reference to the user implemented eventProcessCallback
        the user must implements: eventSent(self, simple_data), 
        eventDropped(self, simple_data) and eventStoredInQueue(self, simple_data)
        methods.
        - type_name is literally the type_name field of a structured event. This
        is an optional parameter and should not be specified under normal
        circumstances. If unspecified, the name of the simple_data object is used.
        - event_name is the event_name field of the structured event. Not really
        useful.
        - se is a fully-defined structured event. If this parameter is specified,
        all other parameters will be completely ignored. A check is made to ensure
        that this object is really what it claims to be. This parameter is
        reserved for ACS internal usage.
        - suppier_name is the name of the supplier publishing the event. This
        parameter is reserved for ACS internal usage.

        Returns: Nothing

        Raises:
        - ACSErrTypeCommonImpl.CORBAProblemExImpl
        - ACSErrTypeCommonImpl.CouldntPerformActionExImpl
        - ACSErrTypeCommonImpl.TypeNotSupportedExImpl
        '''
        #Whoah, user passed in the entire structured event...I'm impressed.
        if se != None:
            #check to make sure it's a structured event first.
            if isinstance(se, CosNotification.StructuredEvent):
                #publish it
                try:
                    self.sppc.push_structured_event(se)
                    return
                except Exception, e:
                    print_exc()
                    raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                              self.channelName),
                                                    NameValue("exception",
                                                              str(e))])
            else:
                raise TypeNotSupportedExImpl(nvSeq=[NameValue("channelname",
                                                              self.channelName),
                                                    NameValue("reason",
                                                              "Not a structured event")])
            
        #User didn't specify type_name.  Assume it's the name of the
        #repository ID. If that doesn't work either, must be a simple
        #CORBA type.
        if (type_name == None) and (simple_data != None):
            try:
                type_name = str(simple_data.__class__.__name__)
            except Exception, e:
                self.logger.logWarning(str(e))
                print_exc()
                type_name = str(CORBA.id(simple_data))
        elif (simple_data == None):
            raise CouldntPerformActionExImpl(nvSeq=[NameValue("channelname",
                                                              self.channelName),
                                                    NameValue("reason",
                                                              "Empty data")])
        
        #create the CORBA Any in the "normal" manner first.  If this
        #fails, try omniORB's any helper module designed for simple types.
        try:
            corba_any = CORBA.Any(CORBA.TypeCode(CORBA.id(simple_data)),
                                  simple_data)
        except Exception, e:
            self.logger.logTrace(str(e))
            try:
                corba_any = any.to_any(simple_data)
            except Exception, e:
                print_exc()
                raise TypeNotSupportedExImpl(nvSeq=[NameValue("channelname",
                                                              self.channelName),
                                                    NameValue("exception",
                                                              str(e)),
                                                    NameValue("reason",
                                                              "Data not a CORBA type")])
    
        #Create the real structured event.  Look at CosNotification.idl to see these definitions
        fixed_header = CosNotification.FixedEventHeader(CosNotification.EventType(str(self.getChannelDomain()),
                                                                                  str(type_name)),
                                                        str(event_name))
        header = CosNotification.EventHeader (fixed_header,
                                              [])
        
        # create list of Property to pass in the event
        new_list = [CosNotification.Property(acscommon.DEFAULTDATANAME,
                                             corba_any)]

        #Create the event dscription
        #get the component/client's name
        if supplier_name==None:
            try:
                component_name = self.component._get_name()
            except Exception, e:
                component_name = "Unknown"
        else:
            component_name = supplier_name
                
        #get the time the event is being sent
        time_stamp = getTimeStamp()
        
        event_descrip = acsnc.EventDescription(component_name,
                                               long(time_stamp.value),
                                               long(self.count))
        event_descrip = CORBA.Any(CORBA.TypeCode(CORBA.id(event_descrip)),
                                  event_descrip)

        # create the structured event
        se = CosNotification.StructuredEvent(header,
                                             new_list,
                                             event_descrip)
        
        # publish the event
        try:
            self.sppc.push_structured_event(se)
            if event_callback != None:
                event_calback.eventSent(simple_data)

            #For HLA/ITS - to be removed later!
            if get_integration_logs(self.channelName)==1:
                try:
                    comp_name = self.component.getName()
                except:
                    comp_name = "Unknown"
                    
                self.logger.logNotice("Publisher:" + comp_name +
                                      ", Event Type:" + type_name)
            
            self.count = self.count + 1
        except (CORBA.COMM_FAILURE, CORBA.TRANSIENT):
            #Notify Service is down
            if event_callback != None:
                event_calback.eventDropped(simple_data)

        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("exception",
                                                      str(e))])
        return
#------------------------------------------------------------------------------
    def reconnect(self, ecf):
        self.evtChan.set_qos(self.configQofS())
        self.supplierAdmin.set_qos(self.configAdminProps())