#!/usr/bin/env python
# @(#) $Id: acscourseMountConsumer.py,v 1.4 2005/07/04 16:51:58 dfugate Exp $
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
of an event consumer in Python.
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from time import sleep
#--CORBA STUBS-----------------------------------------------------------------
import ACSCOURSE_MOUNT
#--ACS Imports-----------------------------------------------------------------
from Acspy.Nc.Consumer          import Consumer
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
def mountDataHandler(someParam):
    '''
    This function serves only one purpose...it must do something with the extracted
    data from the structured event.

    Parameters: someParam is the real CORBA type extracted from the event.
    In this case, it will always be a ACSCOURSE_MOUNT.MountEventData.

    Returns: event handler functions return nothing.

    Raises: If any exception is thrown by this function, the Consumer class will
    catch it and call processEvent(...) which will hopefully have been overriden.
    '''
    print "The commanded Az/El received by this consumer are:", someParam.Azimuth, ",", someParam.Elevation
    return
#------------------------------------------------------------------------------
if __name__ == "__main__":
    #Create a Consumer
    g = Consumer(ACSCOURSE_MOUNT.MOUNT_CHANNEL)

    #Subscribe to MountEventData events (see the IDL for a definition) and register
    #this handler to process those events
    g.addSubscription(ACSCOURSE_MOUNT.MountEventData, handler_function=mountDataHandler)

    #Let the Notification Service know we are ready to start processing events.
    g.consumerReady()

    #Give suppliers 50 seconds to send events.
    print "Waiting for events . . ."
    for i in range(0,50):
        sleep(1)
        
    #cleanly disconnect the consumer
    g.disconnect()
#------------------------------------------------------------------------------
