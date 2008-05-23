#!/usr/bin/env python
# @(#) $Id: contNcTestCounterConsumer.py,v 1.1 2008/05/23 12:53:45 eallaert Exp $
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

'''
DESCRIPTION
This trivial example shows how to consume events from a notification channel in 
Python using the COUNTER::temperatureDataBlockEvent example. This particular
example does not return control until five events have been received.

WHAT CAN I GAIN FROM THIS EXAMPLE?
- Consumer class usage.
- Consuming events.
- Implementation of so-called handler functions to process event data.

LINKS
- 
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from time import sleep
#--CORBA STUBS-----------------------------------------------------------------
import COUNTER
#--ACS Imports-----------------------------------------------------------------
from Acspy.Nc.Consumer          import Consumer
from Acspy.Common.Log           import getLogger
#--GLOBALS---------------------------------------------------------------------
#count is a global integer used to make sure we don't print more than five
#events to standard out.  really this is done just to make this module's modular
# test happy.
count = 0
contFlag = True
LOGGER = getLogger("contNcTestCounterConsumer")
#------------------------------------------------------------------------------
def counterDataHandler(someParam):
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
    global count
    global contFlag
    
    if contFlag:
        onOff    = someParam.onOff
        counter1 = someParam.counter1
        counter2 = someParam.counter2
        counter3 = someParam.counter3
        lastFlag = someParam.flipFlop
        period   = someParam.period

        if (onOff == COUNTER.ON) and (not lastFlag):
            LOGGER.logInfo('Counter now %d (max %d), flag  will flip at %d' % (counter1, counter2, counter3))
        else:
            LOGGER.logInfo('Last event from supplier received, counter is now ' + str(counter1))
            contFlag = False
        count = count + 1
        
    return
#------------------------------------------------------------------------------
if __name__ == "__main__":

    #Create a contNcTestCounterConsumer
    LOGGER.logInfo('Creating contNcTestCounterConsumer')
    g = Consumer(COUNTER.CHANNELNAME_COUNTER)

    #Subscribe to statusBlockEvent events (see contNcTest_IF.idl) and register
    #this handler to process those events
    g.addSubscription(COUNTER.statusBlockEvent, counterDataHandler)

    #Let the Notification Service know we are ready to start processing events.
    g.consumerReady()

    #After five events have been received, disconnect from the channel
    LOGGER.logInfo("Waiting for events . . .")
    while(contFlag):
        sleep(0.5)

    LOGGER.logInfo("Events all done (%d) . . . exiting" % count)
    g.disconnect()
#------------------------------------------------------------------------------
