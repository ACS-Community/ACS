#!/usr/bin/env python
# @(#) $Id: CounterConsumer.py,v 1.1 2008/05/23 12:53:45 eallaert Exp $
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
DESCRIPTION
This trivial example shows how to publish notification channel events in
Python using the COUNTER::statusBlockEvent example. In particular,
it will send a number of events, the number and frequency of which depend
on the initial settings:
- the initial value of the counter is passed as an optional argument to the
  Consumer. Default: 1
- the last value of the counter is passed as an optional argument to the
  Consumer. Default: 20
- the period with which the counter will be incremented is passed as an
  optional argument. Default: 0.5 (units are seconds)
- an additional optional argument indicates at what counter value a boolean,
  part of the event-block, will transition from false to true. Default: same
  as last value of counter. Note that in any case this flag will be set to
  true for the last event sent by the Consumer

WHAT CAN I GAIN FROM THIS EXAMPLE?
- Consumer class usage.
- Publishing events.

LINKS
- 
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from time import sleep
import getopt
import sys
#--CORBA STUBS-----------------------------------------------------------------
import COUNTER__POA
import COUNTER
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Servants.ACSComponent       import ACSComponent
from Acspy.Nc.Consumer      import Consumer
from Acspy.Common.Log       import getLogger
#--GLOBALS---------------------------------------------------------------------
#------------------------------------------------------------------------------
class CounterConsumer(COUNTER__POA.CounterConsumer,  #CORBA stubs for IDL interface
                ACSComponent,  #Base IDL interface
                ContainerServices,  #Developer niceties
                ComponentLifecycle):  #HLA stuff
    '''
    Simple component implementation provided as a reference for developers.
    '''
    

    def __init__(self):
        '''
        Just call superclass constructors here.
        '''
        ACSComponent.__init__(self)
        ContainerServices.__init__(self)

        self.eventCount = 0
        self.contFlag = True
        self.LOGGER = getLogger("CounterConsumer")

        return
    #------------------------------------------------------------------------------
    #--Override ComponentLifecycle methods-----------------------------------------
    #------------------------------------------------------------------------------
    def initialize(self):
        '''
        Override this method inherited from ComponentLifecycle
        '''

        self.LOGGER.logTrace("CounterConsumer.CounterConsumer")

    #------------------------------------------------------------------------------
    def cleanUp(self):
        '''
        Override this method inherited from ComponentLifecycle
        '''

        if self.name == None:
            self.LOGGER.logInfo("Stopping main ...") 
        else:
            self.LOGGER.logInfo("Destroying " + self.name + "...") 

        #cleanly disconnect from the channel
        if self.Consumer != None:
            self.Consumer.disconnect()
            self.Consumer = None
       
    #------------------------------------------------------------------------------
    def counterDataHandler(self,someParam):
        '''
        This function serves only one purpose...it must do something with the extracted
        data from the structured event.  That is, it must be capable of processing
        filterable_data[0].any in the structured event.  We can be certain of the real
        type of someParam because handlers are registered only for specific
        types (i.e., the type_name field of a structured event).
    
        Parameters: someParam is the real CORBA type extracted from the CORBA Any at
        filterable_data[0].  In this case, it will always be a COUNTER.temperatureDataBlockEvent.
    
        Returns: event handler functions return nothing.
    
        Raises: If any exception is thrown by this function, the Consumer class will
        catch it and call processEvent(...) which will hopefully have been overriden.
        '''
    
        if self.contFlag:
            #pattern  = someParam.status
            onOff    = someParam.onOff
            myString = someParam.myString
            counter1 = someParam.counter1
            counter2 = someParam.counter2
            counter3 = someParam.counter3
            lastFlag = someParam.flipFlop
            period   = someParam.period
    
            if (onOff == COUNTER.ON) and (not lastFlag):
                self.LOGGER.logInfo('Counter now %d (max %d), flag  will flip at %d' % (counter1, counter2, counter3))
                #self.LOGGER.logInfo('bitmask: %x' % pattern)
            else:
                self.LOGGER.logInfo(myString + ' received, counter is now ' + str(counter1))
                #self.LOGGER.logInfo('bitmask: %x' % pattern)
                self.contFlag = False
                
        self.eventCount = self.eventCount + 1
        
        return
    #------------------------------------------------------------------------------
    #--Implementation of IDL methods-----------------------------------------------
    #------------------------------------------------------------------------------
    def getBlocks(self):

        #Create a Consumer
        self.LOGGER.logInfo('Creating an instance of Consumer')
        self.Consumer = Consumer(COUNTER.CHANNELNAME_COUNTER)
        
        #Subscribe to statusBlockEvent events (see nctest_IF.idl) and register
        #this handler to process those events
        self.Consumer.addSubscription(COUNTER.statusBlockEvent, self.counterDataHandler)

        #Let the Notification Service know we are ready to start processing events.
        #After consumerReady() is invoked, receive(...) is invoked
        #by the notification channel.  That is, we have no control over when
        #that method is called.
        self.Consumer.consumerReady()
        self.LOGGER.logInfo("CounterConsumer is ready to receive 'status' events.")

        return
    
    def waitTillDone(self):

        while self.contFlag and (self.Consumer != None):
            self.LOGGER.logInfo("CounterConsumer received %d blocks so far ..." % self.eventCount)
            sleep(1.0)

        self.LOGGER.logInfo("CounterConsumer received total of %d blocks" % self.eventCount)
        #cleanly disconnect from the channel
        if self.Consumer != None :
            self.Consumer.disconnect()
            self.Consumer = None
        
        return self.eventCount
        
#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":

    print "Creating an object"
    pyConsumer = CounterConsumer()
    pyConsumer.initialize()

    #Get the events
    pyConsumer.getBlocks()
    count = pyConsumer.waitTillDone() 

    pyConsumer.cleanUp()
    
    print "Done..."

#------------------------------------------------------------------------------











