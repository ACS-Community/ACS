# @(#) $Id: Consumer.py,v 1.23 2011/10/12 15:24:25 javarias Exp $
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
#------------------------------------------------------------------------------
'''
This module includes classes to be used as Consumers for the CORBA Notification
service.
'''

__revision__ = "$Id: Consumer.py,v 1.23 2011/10/12 15:24:25 javarias Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
import CosNotifyChannelAdmin
import CosNotifyComm__POA
import CosNotification
import CosNotifyFilter
import acsnc
import Queue
from threading import Thread
from ACSErrTypeCommonImpl         import CORBAProblemExImpl
from ACSErr                       import NameValue
from AcsNCTraceLogLTS import LOG_NC_ReceiverTooSlow
#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log         import getLogger
from Acspy.Nc.CommonNC        import CommonNC
from Acspy.Nc.CDBProperties   import get_integration_logs
from Acspy.Nc.CDBProperties   import getEventHandlerTimeoutDict
from Acspy.Util.Profiler      import Profiler
#--GLOBALS---------------------------------------------------------------------
#the default maximum amount of time an event handler is given to process event
#before an exception is logged. this is used when an enduser does *not* define
#the appropriate XML elements within the ACS CDB. see the inline doc on EventChannel.xsd
#for more info
DEFAULT_MAX_PROCESS_TIME = 2.0

#------------------------------------------------------------------------------
class Consumer (CosNotifyComm__POA.StructuredPushConsumer, CommonNC):
    #--------------------------------------------------------------------------
    '''
    Class Consumer is the implementation of a CORBA Structured Push Consumer.
    It is provided to hide details of the CORBA Notification Service from
    the developer.  It can be instantiated as-is or it can be subclassed and
    the developer should override the processEvent method.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name, component=None, domain=None):
        '''
        Constructor.

        Params:
        - name is the channel name in string format
        - component is the component this supplier has been instantiated from
        (if applicable). This parameter is likely to become mandatory in future
        version of ACS
        - domain is the name of the domain of notification channels the channel
        belongs to

        Returns: Nothing

        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''
        self.logger = getLogger(str(name) + "-Consumer")

        #Call the super's constructor
        CommonNC.__init__(self, name, component, domain)
        
        #CORBA ref to the channels consumer admin
        self.consumerAdmin = None  
        #CORBA ref to this consumers structured proxy push supplier
        self.spps = None
        #Dictionary containing handler functions for different object types
        self.handlers = {}
        #dictionary mapping events to the total amount of time they have
        #to finish
        self.handlerTimeoutDict = getEventHandlerTimeoutDict(name)
        #profiler to time how long it takes events to be processed
        self.profiler = Profiler()
       
        #Handle all of the CORBA stuff now
        CommonNC.initCORBA(self)
        self.initCORBA()
        self.__buffer_max_size = 100
        self.__buffer = Queue.Queue(self.__buffer_max_size);
        self.__stop_thread = False
        self.__thread = Thread(target=self.__process_event)
        self.__thread.setDaemon(True)
        self.__thread.start()
    #--------------------------------------------------------------------------
    def initCORBA(self):
        '''
        Handles all the CORBA involved in creating a Consumer.

        Parameters: None

        Returns: Nothing

        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''
            
        #Get admin object. 
        try:
            # get admin object for the consumer and the proxy supplier from
            # admin
            (self.consumerAdmin, adminid) = self.evtChan.new_for_consumers(CosNotifyChannelAdmin.AND_OP)
            (self.spps, pid) = self.consumerAdmin.obtain_notification_push_supplier(CosNotifyChannelAdmin.STRUCTURED_EVENT)
            #unfortunately this HAS to be narrowed.
            self.spps = self.spps._narrow(CosNotifyChannelAdmin.StructuredProxyPushSupplier)

            # to make NRI happy
            pid = 0
            adminid = 0
            
        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("reason",
                                                      "Bad proxy or admin"),
                                            NameValue("exception", str(e))])
        return
    #--------------------------------------------------------------------------
    def push_structured_event (self, event):
        '''
        CORBA method that is invoked by suppliers. If a correct handler function
        has been provided via the addSubscription method, this is used to do
        something useful with the event. Furthermore, if the user-defined handler
        function raises an exception, this is caught and the processEvent
        method is also invoked. Otherwise, its up to the developer to subclass
        Consumer and override the processEvent method.

        Parameters: event is a CosNotification.StructuredEvent
        
        Returns: Nothing

        Raises: Nothing
        '''
        #For HLA/ITS - to be removed later!
        comp_name = "Unknown"
        if get_integration_logs(self.channelName)==1:
            try:
                comp_name = self.component.getName()
            except:
                pass
                
            self.logger.logNotice("Receiver:" + comp_name +
                                  ", Event Type:" + event.header.fixed_header.event_type.type_name)

        if self.__buffer.qsize() >= self.__buffer_max_size / 2:
            log = LOG_NC_ReceiverTooSlow()
            log.setClientName(comp_name)
            log.setEventName(event.header.fixed_header.event_type.type_name)
            log.setChannelName(self.channelName)
            if self.__buffer.full():
                log.setNumEventsDiscarded(1);
                self.__buffer.get(block=False)
            log.log()
        
        self.__buffer.put(event, block=False)
#        #For HLA - maximum amount of time the consumer has to process the event
#        if self.handlerTimeoutDict.has_key(event.header.fixed_header.event_type.type_name) == 0:
#            #give it the default timeout if it's undefined by the CDB
#            self.handlerTimeoutDict[event.header.fixed_header.event_type.type_name] = DEFAULT_MAX_PROCESS_TIME
#
#        #maximum amount of time the event handler has to process the event.
#        #if it takes too long, a message is logged.
#        max_process_time = self.handlerTimeoutDict[event.header.fixed_header.event_type.type_name]
#        
#        #check to see if the developer has provided a handler first
#        if self.handlers.has_key(event.header.fixed_header.event_type.type_name):
#            try:
#                #get the function
#                temp_func = self.handlers[event.header.fixed_header.event_type.type_name]
#
#                #convert the CORBA any into a "normal" Python object
#                real_obj = event.filterable_data[0].value
#                real_obj = real_obj.value()
#
#                #invoke the user-defined function on it.
#                self.profiler.start() #start the timer
#                temp_func(real_obj) #invoke the handler
#                #stop the timer and convert the time it took to run to floating
#                #point seconds
#                time_to_run = self.profiler.stop() / 1000.0
#                #reset the timer to keep memory consumption low
#                self.profiler.reset()
#
#                #check if the event took too long to be processed
#                if time_to_run > max_process_time:
#                    self.logger.logCritical("Took too long to handle an '" +
#                                            event.header.fixed_header.event_type.type_name +
#                                            "' event: " + str(time_to_run) + " seconds.")
#                    self.logger.logInfo("Maximum time to process an event is: " + str(max_process_time) + " seconds.")
#
#                #ignore everything else
#                return
#            
#            except Exception, e:
#                self.logger.logCritical('Unable to use handler function...' +
#                                        str(e))
#                print_exc()
#
#        #either a handler is not used or it failed...pass the se to
#        #processEvent(...) and hope the developer has overriden that method.
#        try:
#            #start the timer
#            self.profiler.start()
#            self.processEvent(type_name=event.header.fixed_header.event_type.type_name,
#                              event_name=event.header.fixed_header.event_name,
#                              corba_any=event.filterable_data[0].value,
#                              se=event)
#            #stop the timer and convert the time it took to run to floating
#            #point seconds
#            time_to_run = self.profiler.stop() / 1000.0
#            #reset the timer to keep memory consumption low
#            self.profiler.reset()
#
#            #check if the event took too long to be processed
#            if time_to_run > max_process_time:
#                self.logger.logCritical("Took too long to handle an '" +
#                                        event.header.fixed_header.event_type.type_name +
#                                        "' event: " + str(time_to_run) + " seconds.")
#                self.logger.logInfo("Maximum time to process an event is: " + str(max_process_time) + " seconds.")
#            
#        except Exception, e:
#            self.logger.logCritical('processEvent(...)...' + str(e))
#            print_exc()

    #--------------------------------------------------------------------------
    def suspend(self):
        '''
        Stop receiving structured events.  All events published on this
        channel will not be processed by this Consumer instance, but will
        be queued for later delivery by the Supplier.  

        Parameters: None
        
        Returns: Nothing

        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''
        try:
            self.spps.suspend_connection()
        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("exception",
                                                      str(e))])
    #--------------------------------------------------------------------------
    def resume(self):
        '''
        Resume receiving structured events.  All events queued by the Supplier
        will be forwarded to this Consumer instance.

        Parameters: None
        
        Returns: Nothing

        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''
        try:
            self.spps.resume_connection()
        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("exception",
                                                      str(e))])
    #--------------------------------------------------------------------------
    def consumerReady(self):
        '''
        Consumer can now begin receiving events after invoking consumerReady.

        Parameters: None
        
        Returns: Nothing

        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''
        try:
            self.spps.connect_structured_push_consumer(self._this())
        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("exception",
                                                      str(e))])
    #--------------------------------------------------------------------------
    def addSubscription (self, name, handler_function=None):
        '''
        add a subscription to a given type.

        Parameters
        - name is the actual Python class for the IDL struct.
        - handler_function If a structured event is received and the
        registerHandler method has been defined on that SEs type_name, the
        handler_function must be able to process filterable_data[0].value.value().
        In other words every time an event of this nature is received, the
        Consumer object will first try to invoke the handler_function
        using the REAL object extracted from the CORBA Any in the structured
        event located at the the first position of the filterable_data list.
        IF THIS FAILS, the Consumer simply passes the event to the
        processEvent(...) method which will have hopefully been overriden by
        the developer.
        
        Returns: Nothing

        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''
        #two cases here...either they're passing a string or the Python IDL
        #struct class handle both regardles.
        if not isinstance(name, str):
            #assume it's the class for the IDL struct
            name = str(name.__name__)

        #save the handler for future use
        if handler_function != None:
            self.handlers[name] = handler_function

        self.logger.logInfo(name)
        try:
            self.consumerAdmin.subscription_change([CosNotification.EventType(self.getChannelDomain(),
                                                                              name)],
                                                   [])
        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("exception",
                                                      str(e))])
    #--------------------------------------------------------------------------
    def removeSubscription (self, name):
        '''
        remove a subscription type.
        
        Parameters: name is the actual Python class for the IDL struct.
        
        Returns: Nothing
        
        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''
        #assume it's the class for the IDL struct
        if not isinstance(name, str):
            name = str(name.__name__)
            
        
        self.logger.logInfo(name)
        try:
            self.consumerAdmin.subscription_change([],
                                                   [CosNotification.EventType(self.getChannelDomain(),
                                                                              name)])
        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("exception",
                                                      str(e))])
    #--------------------------------------------------------------------------
    def addFilter(self, type_of_event, filter_exp):
        '''
        add a filter.

        Parameters:
        - type_of_event is the actual Python class for the IDL struct.
        - filter_exp is an EXTENDED_TCL string.
        
        Returns: The ID of the newly created filter (positive integer?)

        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''
        #assume it's the class for the IDL struct
        if not isinstance(type_of_event, str):
            eventtype = str(type_of_event.__name__)
        else:
            eventtype = type_of_event
            
        try:
            t_filter_factory = self.evtChan._get_default_filter_factory()
            #create the filter
            t_filter = t_filter_factory.create_filter(self.getFilterLanguage())
            t_event_type = [CosNotification.EventType(self.getChannelDomain(),
                                                      str(eventtype))]
            t_cexp = [CosNotifyFilter.ConstraintExp(t_event_type,
                                                    str(filter_exp))]
            t_filter.add_constraints(t_cexp)
            return self.spps.add_filter(t_filter)
        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("exception",
                                                      str(e))])
    #--------------------------------------------------------------------------
    def removeFilter(self, filter_id):
        '''
        remove a filter defined by filter_id.

        Parameters: filter_id should be what is returned by the addFilter
        method
        
        Returns: 1 on success and 0 on failure.

        Raises: Nothing
        '''
        try:
            self.spps.remove_filter(filter_id)
            return 1
        except Exception, e:
            self.logger.logWarning('ID ' + str(filter_id) + ' failed...' +
                                   str(e))
            print_exc()
            return 0
    #--------------------------------------------------------------------------
    def getFilterLanguage(self):
        '''
        This method returns a constant character pointer to the type of filter
        constraint language to be used for filtering events which is normally
        equivalent to acsnc::FILTER_LANGUAGE_NAME. Override to change this
        behavior.

        Parameters: None

        Returns: string

        Raises: nothing
        '''
        return acsnc.FILTER_LANGUAGE_NAME
    #--------------------------------------------------------------------------
    def disconnect(self):
        '''
        Consumer client must call this to disconnect from notification channel.
        Once disconnect has been called, the Consumer object should be treated
        as if it has been deleted.

        Parameters: None
        
        Returns: Nothing

        Raises: Nothing
        '''
        #Disconnect
        self.connected = 0
        self.callback.disconnect()
        self.callback = None
        
        self.__stop_thread = True
        self.__thread.join()
        try:
            #suspend all subscriptions
            self.suspend()
            #unsubscribe from everything
            self.consumerAdmin.subscription_change([],
                                                   [CosNotification.EventType('*',
                                                                              '*')])
            #disconnect from the proxy supplier.
            self.spps.disconnect_structured_push_supplier()
            self.consumerAdmin.destroy()
            
        except Exception, e:
            self.logger.logWarning(str(e))
            print_exc()
        
        return
    #--------------------------------------------------------------------------
    def disconnect_structured_push_consumer (self):
        '''
        Called by a supplier to inform this consumer it is disconnecting from
        the channel. Developer code must never call this.

        Parameters: None
        
        Returns: Nothing

        Raises: Nothing
        '''
        self.logger.logTrace('')
        return
    #--------------------------------------------------------------------------
    def offer_change (self, added, removed):
        '''
        Called by a supplier to inform this consumer its offering a change in the
        events in publishing. Developer code must never call this.

        Parameters:
        - added is a list of domain/type pairs a supplier will begin publishing
        - removed is a list of domain/type pairs a supplier will stop
        publishing
        
        Returns: Nothing

        Raises: Nothing
        '''
        print "Consumer.offer_change(...): added=", added, ", removed=", removed
        return
    #--------------------------------------------------------------------------
    def processEvent (self,
                      type_name=None,
                      event_name=None,
                      corba_any=None,
                      se=None):
        '''
        Developer should override this method if handlers are not used.  It does
        NOT narrow the corba_any for the developer as there is no guarantee the
        correct Python CORBA stub module has been imported!

        Parameters:
        - type_name corresponds to the type_name field of a structured event (i.e.,
        string)
        - event_name is the event_name field of a structured event (i.e., string)
        - corba_any is filterable_data[0].value (i.e., CORBA any).  Most
        developers are only interested in this.
        - se is the entire structured event.

        Returns: Nothing

        Raises: Developer is free to raise any exception.
        '''
        #to make pychecker happy
        event_name = 0
        corba_any = 0
        se = 0
        
        self.logger.logInfo('Someone forgot to override this method (\'type_name\'=' +
                            str(type_name) + ')')
        return
#------------------------------------------------------------------------------

    def reconnect(self, ecf):
        self.evtChan.set_qos(self.configQofS())
        self.supplierAdmin.set_qos(self.configAdminProps())
        
    def __process_event(self):
        while not self.__stop_thread:
            event = None
            try:
                event = self.__buffer.get(block = True, timeout = 2)
            except Queue.Empty, e:
                continue
            #For HLA - maximum amount of time the consumer has to process the event
            if self.handlerTimeoutDict.has_key(event.header.fixed_header.event_type.type_name) == 0:
            #give it the default timeout if it's undefined by the CDB
                self.handlerTimeoutDict[event.header.fixed_header.event_type.type_name] = DEFAULT_MAX_PROCESS_TIME
    
            #maximum amount of time the event handler has to process the event.
            #if it takes too long, a message is logged.
            max_process_time = self.handlerTimeoutDict[event.header.fixed_header.event_type.type_name]
            
            #check to see if the developer has provided a handler first
            if self.handlers.has_key(event.header.fixed_header.event_type.type_name):
                try:
                    #get the function
                    temp_func = self.handlers[event.header.fixed_header.event_type.type_name]
    
                    #convert the CORBA any into a "normal" Python object
                    real_obj = event.filterable_data[0].value
                    real_obj = real_obj.value()
    
                    #invoke the user-defined function on it.
                    self.profiler.start() #start the timer
                    temp_func(real_obj) #invoke the handler
                    #stop the timer and convert the time it took to run to floating
                    #point seconds
                    time_to_run = self.profiler.stop() / 1000.0
                    #reset the timer to keep memory consumption low
                    self.profiler.reset()
    
                    #check if the event took too long to be processed
                    if time_to_run > max_process_time:
                        self.logger.logCritical("Took too long to handle an '" + 
                                                event.header.fixed_header.event_type.type_name + 
                                                "' event: " + str(time_to_run) + " seconds.")
                        self.logger.logInfo("Maximum time to process an event is: " + str(max_process_time) + " seconds.")
    
                    #ignore everything else
                    continue
                
                except Exception, e:
                    self.logger.logCritical('Unable to use handler function...' + 
                                            str(e))
                    print_exc()
    
            #either a handler is not used or it failed...pass the se to
            #processEvent(...) and hope the developer has overriden that method.
            try:
                #start the timer
                self.profiler.start()
                self.processEvent(type_name=event.header.fixed_header.event_type.type_name,
                                  event_name=event.header.fixed_header.event_name,
                                  corba_any=event.filterable_data[0].value,
                                  se=event)
                #stop the timer and convert the time it took to run to floating
                #point seconds
                time_to_run = self.profiler.stop() / 1000.0
                #reset the timer to keep memory consumption low
                self.profiler.reset()
    
                #check if the event took too long to be processed
                if time_to_run > max_process_time:
                    self.logger.logCritical("Took too long to handle an '" + 
                                            event.header.fixed_header.event_type.type_name + 
                                            "' event: " + str(time_to_run) + " seconds.")
                    self.logger.logInfo("Maximum time to process an event is: " + str(max_process_time) + " seconds.")
                
            except Exception, e:
                self.logger.logCritical('processEvent(...)...' + str(e))
                print_exc()
            #Just to make happy the Python queue API
            self.__buffer.task_done()

        