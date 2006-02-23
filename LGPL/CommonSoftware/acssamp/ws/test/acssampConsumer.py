#!/usr/bin/env python
# @(#) $Id: acssampConsumer.py,v 1.3 2004/04/21 23:07:05 dfugate Exp $
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
This Python script is designed to provide the developer with a sample implementation
of an event consumer in Python.  This particular script does not force events
to be supplied as acsexmplFridgeNCClient.py does.  This particular script will
only print five events to standard out and then disconnect from the event channel.
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from time import sleep
from sys  import argv
#--CORBA STUBS-----------------------------------------------------------------
import ACSSamp
#--ACS Imports-----------------------------------------------------------------
from Acspy.Clients.SimpleClient import PySimpleClient
from Acspy.Nc.Consumer          import Consumer
#--GLOBALS---------------------------------------------------------------------
#count is a global integer used to make sure we don't print more than five
#events to standard out.  really this is done just to make this module's modular
# test happy.
count = 0
#------------------------------------------------------------------------------
def dataHandler(someParam):
    '''
    This function serves only one purpose...it must do something with the extracted
    data from the structured event.  That is, it must be capable of processing
    filterable_data[0].any in the structured event.  We can be certain of the real
    type of someParam because handlers are registered only for specific
    types (i.e., the type_name field of a structured event).

    Parameters: someParam is the real CORBA type extracted from the CORBA Any at
    filterable_data[0].

    Returns: event handler functions return nothing.

    Raises: If any exception is thrown by this function, the Consumer class will
    catch it and call processEvent(...) which will hopefully have been overriden.
    '''
    global count

    count = count + 1
    if count < 5:
        for data in someParam:
            print "TIME STAMP: ", data.sampTime
            print "VALUE: ", data.sampVal.value()
    print "Received: ", count
    return
#------------------------------------------------------------------------------
if __name__ == "__main__":

    client = PySimpleClient()
    samp = client.getComponent(argv[1])
    sampObj = samp.initSampObj("LAMP1", "brightness", 1000000, 10000000)

    print "acssampConsumer entering ..."
    g = Consumer("NC_LAMP1_brightness_1000000_10000000")
    g.addSubscription("SampDataBlockSeq", handlerFunction=dataHandler)
    g.consumerReady()

    sampObj.start()
    print " ACS sampling started"
    sleep(5)
    sampObj.suspend()
    print "ACS sampling suspended"
    
    sleep(5)
    sampObj.resume()
    print "ACS sampling resumed"
    
    sleep(6)
    sampObj.stop()
    print "ACS sampling stopped"
    
    sleep(2)
    sampObj.destroy()
    print "ACS sampling destroyed"
    

    #After five events have been received, disconnect from the channel
    print "Waiting for events . . ."
    while(count<5):
        sleep(1)
        
    client.releaseComponent(argv[1])
    g.disconnect()
    client.disconnect()
#------------------------------------------------------------------------------
