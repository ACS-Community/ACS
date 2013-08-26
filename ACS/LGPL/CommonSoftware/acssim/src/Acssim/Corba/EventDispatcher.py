# @(#) $Id$
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
# "@(#) $Id$"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/12/09  Created.
#------------------------------------------------------------------------------
'''
This module includes the implementation of an object which periodically sends
events and also sends events in response to receiving a particular type of 
event.

TODO LIST:
    - generating the event using user-defined code does not work yet
    - there is some problem with the Scheduler where events are being sent
    too quickly.
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from operator import isSequenceType
from traceback import print_exc
from random import randrange
from time import sleep
#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acssim.Goodies import supplyEventByType, supplyEventByInstance
from Acspy.Util.Scheduler    import Scheduler
from Acssim.Goodies import getComponentXMLObj
from Acssim.Goodies import getCompLocalNSList
from Acssim.Corba.Utilities  import  listToCodeObj
from Acspy.Common.Log        import getLogger
from Acspy.Common.Log        import acsPrintExcDebug
from Acspy.Nc.Consumer       import Consumer
#--GLOBALS---------------------------------------------------------------------
#a single scheduler will publish all events
SCHEDULER = Scheduler()
#------------------------------------------------------------------------------
class EventDispatcher:
    '''
    EventDispatcher dispatches events at given frequencies and also sends
    events in reponse to receiving an event of a particular type.
    '''
    def __init__(self, comp_ref):
        '''
        Constructor
        
        Parameters: comp_ref - reference to the component
        
        Raises: ???
        '''
        #component reference
        self.comp_ref = comp_ref
        
        #the component's name which we need for the CDB
        self.comp_name = self.comp_ref._get_name()
        
        #our own personal logger
        self.logger = getLogger("EventDispatcher (" + 
                                 self.comp_name + ")")
        
        #list of all timeouts we have scheduled. used at destruction
        self.timeout_ids = []
        
        #maps consumer objects to channels.
        self.consumers = {}
        
        #delegate the logic determining when events will be sent
        #to this helper method
        self.setupEventDispatching()
        
        
    def destroy(self):
        '''
        Destroys this object: cancels all timeouts and destroys all consumers.
        
        Params: None
        
        Returns: Nothing
        
        Raises: ???
        '''
        #first cancel all the scheduled timeouts
        for timeout_id in self.timeout_ids:
            SCHEDULER.cancelTimeout(timeout_id)
        
        #next destroy all the consumers
        for consumer in self.consumers.values():
            consumer.disconnect()
        
    def setupEventDispatching(self):
        '''
        Helper method sets up event dispatching using info found in the
        ACS CDB.
        '''
        self.logger.logInfo("Setting up event dispatching.")
        
        #get the xml object which describes the event frequencies and
        #reponses
        xml_obj = getComponentXMLObj(self.comp_name)
        
        #sanity check
        if xml_obj == None:
            self.logger.logDebug("No CDB entry found. Bailing.")
            return
        
        #just delegate to other helper methods
        self.handleFrequencies(xml_obj)
        self.handleResponses(xml_obj)
    
    def handleFrequencies(self, xml_obj):
        '''
        Helper method used to setup events sent at certain
        frequencies.
        
        Params: xml_obj - an XMLObjectifier object conforming to the 
        SimulatedComponent.xsd schema.
        
        Returns: Nothing
        
        Raises: ???
        '''
        #events is the somewhat formatted data taken from the XML. not really
        #nice enough to work with yet.
        try:
            events = xml_obj.SimulatedComponent._almaEvent
            if isSequenceType(events)==0:
                events = [ events ]
        except:
            self.logger.logDebug("No event frequencies defined.")
            return
        
        #cycle through all the events
        for event in events:
            #extract the channel name
            channel_name = event.getAttribute('Channel')
            
            #extract the IFR ID
            ifr_id = event.getAttribute('ID')
            
            #extract the rate at which events will be sent at
            frequency = float(event.getAttribute('Frequency'))
            
            
            event_instance = self.eventFunctionHelper(event, ifr_id)
            
            if event_instance != None:
                self.scheduleEventByInstance(channel_name, event_instance, frequency)
            else:
                #this is perfectly OK. end-user did not define a function to 
                #generate events
                self.scheduleEventByType(channel_name, ifr_id, frequency)
                 
    def handleResponses(self, xml_obj):
        '''
        Helper method used to send events in response to receiving events
        of a certain type.
        
        Params: xml_obj - an XMLObjectifier object conforming to the 
        SimulatedComponent.xsd schema.
        
        Returns: Nothing
        
        Raises: ???
        '''
        #events is the somewhat formatted data taken from the XML. not really
        #nice enough to work with yet.
        try:
            events = xml_obj.SimulatedComponent._almaEventResponse
            if isSequenceType(events)==0:
                events = [ events ]
        except:
            self.logger.logDebug("No event frequencies defined.")
            return
        
        #cycle through all the events
        for event in events:
            
            #extract the incoming channel name
            incoming_channel_name = event.getAttribute('IncomingChannel')
            
            #extract the incoming IFR ID
            incoming_ifr_id = event.getAttribute('IncomingID')
            
            #extract the outgoing channel name
            outgoing_channel_name = event.getAttribute('OutgoingChannel')
            
            #extract the outgoing IFR ID
            outgoing_ifr_id = event.getAttribute('OutgoingID')
            
            #how long to wait beforse sending the event
            delay = float(event.getAttribute('Delay'))
            
            #the chance of any given event not being sent
            missed_event_chance = float(event.getAttribute('MissedEventChance'))
            
            self.logger.logInfo("'" + outgoing_ifr_id + 
                                "' events will be sent to the '" + 
                                outgoing_channel_name + "' channel after '" +
                                str(delay) + "' seconds with a '" + 
                                str(missed_event_chance) + 
                                "' probabily of being skipped when events of type '" +
                                incoming_ifr_id + 
                                "' are received on the '" + 
                                incoming_channel_name + "' channel.")
            
            #get an instance of the event
            event_instance = self.eventFunctionHelper(event, incoming_ifr_id)
            #delegate to another helper method
            self.responseHelper(incoming_channel_name, incoming_ifr_id,
                               outgoing_channel_name, outgoing_ifr_id,
                               delay,
                               missed_event_chance,
                               event_instance)
        return
                               
    
    def eventFunctionHelper(self, event, ifr_id):
        '''
        Returns an event instance or None based off the contents of 
        an _almaEvent or _almaEventReponse XML element (DOM).
        '''
        #here comes the fun part...it might be necessary to dynamically
        #create the object now!
        #get the code to be executed yielding a return value
        try:
            #if the following line of code throws an exception,
            #it's not really a big deal. it just means that 
            #no function was defined within the XML element to 
            #generate the event instance
            value = event.getValue().rstrip().lstrip().split('\n')
            
            #this next block is wrapped in a separate try/except
            #because it's possible that the end-user has problems
            #in their function definition.
            try:
                _locals = {}
                #attach all imports to the function definition
                value = getCompLocalNSList(self.comp_name) + value
                #make the code list a function in the _locals namespace
                value = listToCodeObj(value, _locals)
                #create the function
                exec value in globals(), _locals
                #execute the function as well to get the event instance
                exec "joe = stringFunction([])" in globals(), _locals
                event_instance = _locals['joe']
                
            except Exception, e:
                #the function definition given by the end-user was bad!
                #warn them and schedule dynamic events instead
                acsPrintExcDebug()
                self.logger.logCritical("Something was wrong within the function definition for the '" + 
                                        ifr_id + 
                                        "' event type.")
                self.logger.logInfo("Will try dynamically creating the event instead.")                    
                #just rethrow e so the next block catches it
                raise e

        except:
            event_instance = None
            
        return event_instance
    
    def responseHelper(self, incoming_channel_name, incoming_ifr_id, 
                      outgoing_channel_name, outgoing_ifr_id, 
                      delay, missed_event_chance, 
                      event_instance):
        '''
        A fairly complex helper method which:
            - adds a subscription to a consumer for the incoming_ifr_id
            event type
            - whenever an event is received by the consumer, an event of
            outgoing_ifr_id type is PROBABLY published on the 
            outgoing_channel_name channel after 'delay' seconds
            - there's a chance the event will not be published at all
            if 'missed_event_chance' is close to 1.0.
        '''
        
        #sanity check to ensure a consumer is around
        if not self.consumers.has_key(incoming_channel_name):
            #add a consumer
            consumer = Consumer(incoming_channel_name)
            consumer.consumerReady()
            self.consumers[incoming_channel_name] = consumer
        
        #consumer
        cons = self.consumers[incoming_channel_name]
        
        #define the event handler method
        def eventHandler(data):
            '''
            This event handler method:
                - checks the probability to ensure an event should actually
                be published.
                - pauses for a set amount of time
                - sends another event presumably of a different type
            '''
            #first check the probability to see if we can
            #ignore the call entirely
            ran_float = (randrange(0,100))/100.0
            if ran_float > missed_event_chance:
                #bail
                self.logger.logDebug("Randomly skipped an event: " + 
                                      str(ran_float))
                return
            
            self.logger.logDebug("Publishing a '" + outgoing_ifr_id +
                                "' event on the '" + outgoing_channel_name +
                                "' in response to receiving an event of type'" +
                                 incoming_ifr_id + "' on the '" + 
                                 incoming_channel_name)
            
            #first we sleep
            sleep(delay)
            
            #send an event in response...
            if event_instance==None:
                supplyEventByType(self.comp_name, outgoing_channel_name, outgoing_ifr_id)
            else:
                supplyEventByInstance(self.comp_name, outgoing_channel_name, event_instance)
            
            return #ends the function definition
        
        #add the subscription; first getting at the event type
        event_type = incoming_ifr_id.split(":")[1].split('/').pop()
        #now add the subscription with the function we just defined
        cons.addSubscription(event_type, eventHandler)
    
    
    def scheduleEventByType(self, channel_name, ifr_id, frequency):
        '''
        Schedules event transmissions by type.
        '''
        self.logger.logInfo("Sending '" + ifr_id +
                            "' events on the '" + channel_name + "' channel" +
                            " at a rate of '" + str(frequency) + 
                            "' per second.")
                            
        id = SCHEDULER.scheduleTimeout(supplyEventByType,
                                       0L,
                                       frequency * 10000000.0,
                                       (self.comp_name, channel_name, ifr_id))
        
        self.timeout_ids.append(id)
        
        return
    
    def scheduleEventByInstance(self, channel_name, event_instance, frequency):
        '''
        Schedules event transmissions by type.
        '''
        self.logger.logInfo("Sending '" + event_instance._NP_RepositoryId +
                            "' events on the '" + channel_name + "' channel" +
                            " at a rate of '" + str(frequency) + 
                            "' per second.")
        id = SCHEDULER.scheduleTimeout(supplyEventByInstance,
                                       0L,
                                       frequency * 10000000.0,
                                       (self.comp_name, channel_name, event_instance)) 

        self.timeout_ids.append(id)
        return
        
        