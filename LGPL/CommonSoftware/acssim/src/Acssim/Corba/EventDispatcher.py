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
TODO LIST:
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from operator import isSequenceType
from traceback import print_exc
from random import randrange
from time import sleep
#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acssim.Servants.Goodies import supplyEventByType, supplyEventByInstance
from Acspy.Util.Scheduler          import Scheduler
from Acssim.Servants.Goodies import getComponentXMLObj
from Acssim.Servants.Goodies import getCompLocalNSList
from Acssim.Corba.Utilities import  listToFunction
from Acspy.Common.Log import getLogger
from Acspy.Nc.Consumer import Consumer
#--GLOBALS---------------------------------------------------------------------
SCHEDULER = Scheduler()
#------------------------------------------------------------------------------
class EventDispatcher:
    '''
    EventDispatcher dispatches events at given frequencies.
    '''
    def __init__(self, comp_ref):
        '''
        Constructor
        
        Parameters: comp_ref - reference to the component
        '''
        self.comp_ref = comp_ref
        self.comp_name = self.comp_ref._get_name()
        
        self.logger = getLogger("EventDispatcher - " + 
                                 self.comp_name)
        
        self.timeout_ids = []
        
        #maps consumer objects to channels
        self.consumers = {}
        
        self.setupEventDispatching()
        
        
    def setupEventDispatching(self):
        '''
        Helper method sets up event dispatching using info found in the
        ACS CDB.
        '''
        self.logger.logInfo("Setting up event dispatching.")
        
        xml_obj = getComponentXMLObj(self.comp_name)
        
        #sanity check
        if xml_obj == None:
            self.logger.logDebug("No XML found. Bailing.")
            return
        
        #just delegate to other helper methods
        self.handleFrequencies(xml_obj)
        self.handleResponses(xml_obj)
    
    def handleFrequencies(self, xml_obj):
        '''
        Helper method used to setup events sent at certain
        frequencies.
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
            
            #here comes the fun part...it might be necessary to dynamically
            #create the object now!
            #get the code to be executed yielding a return value
            try:
                _locals = {}
                value = event.getValue().rstrip().lstrip().split('\n')
                value = getCompLocalNSList(self.comp_name) + value
                value = listToFunction(value, _locals)
                exec value in globals(), _locals
                exec "joe = stringFunction([])" in globals(), _locals
                event_instance = _locals['joe']
                self.scheduleEventByInstance(channel_name, event_instance, frequency)
                return
            except:
                #TODO: fix me
                #print_exc()
                self.scheduleEventByType(channel_name, ifr_id, frequency)
            
                    
    def handleResponses(self, xml_obj):
        '''
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
            #extract the channel name
            incoming_channel_name = event.getAttribute('IncomingChannel')
            
            #extract the IFR ID
            incoming_ifr_id = event.getAttribute('IncomingID')
            
            #extract the channel name
            outgoing_channel_name = event.getAttribute('OutgoingChannel')
            
            #extract the IFR ID
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
            
            #here comes the fun part...it might be necessary to dynamically
            #create the object now!
            #get the code to be executed yielding a return value
            try:
                _locals = {}
                value = event.getValue().rstrip().lstrip().split('\n')
                value = getCompLocalNSList(self.comp_name) + value
                value = listToFunction(value, _locals)
                exec value in globals(), _locals
                exec "joe = stringFunction([])" in globals(), _locals
                event_instance = _locals['joe']
                return
            except:
                event_instance = None
                
            self.responseHelper(incoming_channel_name, incoming_ifr_id,
                               outgoing_channel_name, outgoing_ifr_id,
                               delay,
                               missed_event_chance,
                               event_instance)
        return
                               
    
    def responseHelper(self, incoming_channel_name, incoming_ifr_id, 
                      outgoing_channel_name, outgoing_ifr_id, 
                      delay, missed_event_chance, 
                      event_instance):
        '''
        Helper method.
        '''
        #sanity check
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
            '''
            #first check the probability to see if we can
            #ignore the call entirely
            ran_float = (randrange(0,100))/100.0
            
            if ran_float > missed_event_chance:
                #bail
                self.logger.logDebug("Randomly skipped an event: " + 
                                      str(ran_float))
                return
            
            #nope, now it's necessary to send an event in 
            #response...
            #first we sleep though
            self.logger.logDebug("Publishing a '" + outgoing_ifr_id +
                                "' event on the '" + outgoing_channel_name +
                                "' in response to receiving an event of type'" +
                                 incoming_ifr_id + "' on the '" + 
                                 incoming_channel_name)
            sleep(delay)
            
            if event_instance==None:
                supplyEventByType(self.comp_name, outgoing_channel_name, outgoing_ifr_id)
            else:
                supplyEventByInstance(self.comp_name, outgoing_channel_name, event_instance)
        
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
                                       frequency * 1000000L,
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
        id = SCHEDULER.scheduleTimeout(supplyEventByType,
                                       0L,
                                       frequency * 1000000L,
                                       (self.comp_name, channel_name, event_instance)) 

        self.timeout_ids.append(id)
        return
        
        