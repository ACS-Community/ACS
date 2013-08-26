#!/usr/bin/env python
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
# @(#) $Id$
#------------------------------------------------------------------------------
__revision__ = "@(#) $Id$"
'''
Tests EventDispatcher
'''
from Acssim.Corba.EventDispatcher import EventDispatcher
from Acspy.Nc.Consumer import Consumer
from time import sleep

class MockComponent:
    def __init__(self, name): self.name = name
    def _get_name(self): return self.name
    def activateOffShoot(self, os): return os
    
FAKE_MS = MockComponent("MS1")


EVENT_COUNTER = {}

def eventHandler(event):
    '''
    Consumer event handler
    '''
    ifr_id = event._NP_RepositoryId
    
    #sanity check
    if not EVENT_COUNTER.has_key(ifr_id):
        EVENT_COUNTER[ifr_id] = 0
    
    #increment
    EVENT_COUNTER[ifr_id] = EVENT_COUNTER[ifr_id] + 1


if __name__=="__main__":
    
    ec_cons = Consumer("ALMA_EVENT_CHANNEL")
    ec_cons.addSubscription("temperatureDataBlockEvent", eventHandler)
    ec_cons.addSubscription("XmlEntityStruct", eventHandler)
    ec_cons.consumerReady()
    
    erc_cons = Consumer("ALMA_EVENT_RESPONSE_CHANNEL")
    erc_cons.addSubscription("Duration", eventHandler)
    erc_cons.consumerReady()
    
    #create the event dispatcher
    ed = EventDispatcher(FAKE_MS)
    
    #sleep for awhile giving consumers a chance to process a few 
    #events
    sleep(60)
    
    ec_cons.disconnect()
    erc_cons.disconnect()
    ed.destroy()

    if EVENT_COUNTER["IDL:alma/FRIDGE/temperatureDataBlockEvent:1.0"] > 10:
        print "Good...enough temperatureDataBlockEvent's"
    else:
        print "Bad...not enough temperatureDataBlockEvent's"
    
    if EVENT_COUNTER["IDL:alma/xmlentity/XmlEntityStruct:1.0"] > 4:
        print "Good...enough XmlEntityStruct's"
    else:
        print "Bad...not enough XmlEntityStruct's"
        
    if EVENT_COUNTER["IDL:alma/acstime/Duration:1.0"] > 1 and EVENT_COUNTER["IDL:alma/acstime/Duration:1.0"] < EVENT_COUNTER["IDL:alma/FRIDGE/temperatureDataBlockEvent:1.0"]:
        print "Good...enough Durations have been received"
    else:
        print "Bad...the wrong number of Durations were received"
    
    #print EVENT_COUNTER
    #sleep(20) 
    
