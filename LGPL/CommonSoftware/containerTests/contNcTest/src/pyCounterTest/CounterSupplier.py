#!/usr/bin/env python
# @(#) $Id: CounterSupplier.py,v 1.1 2008/05/23 12:53:45 eallaert Exp $
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
  supplier. Default: 1
- the last value of the counter is passed as an optional argument to the
  supplier. Default: 20
- the period with which the counter will be incremented is passed as an
  optional argument. Default: 0.5 (units are seconds)
- an additional optional argument indicates at what counter value a boolean,
  part of the event-block, will transition from false to true. Default: same
  as last value of counter. Note that in any case this flag will be set to
  true for the last event sent by the supplier

WHAT CAN I GAIN FROM THIS EXAMPLE?
- Supplier class usage.
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
from Acspy.Nc.Supplier      import Supplier
from Acspy.Common.Log       import getLogger
#--GLOBALS---------------------------------------------------------------------
#------------------------------------------------------------------------------
class CounterSupplier(COUNTER__POA.CounterSupplier,  #CORBA stubs for IDL interface
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

        self.LOGGER = getLogger("CounterSupplier")
        #LOGGER.logInfo('Passed through __init__')
        return
    #------------------------------------------------------------------------------
    #--Override ComponentLifecycle methods-----------------------------------------
    #------------------------------------------------------------------------------
    def initialize(self):
        '''
        Override this method inherited from ComponentLifecycle
        '''

        self.LOGGER.logTrace("CounterSupplier.CounterSupplier")

        #Create a supplier
        self.LOGGER.logInfo('Creating an instance of Supplier')
        self.supplier = Supplier(COUNTER.CHANNELNAME_COUNTER)


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
        self.supplier.disconnect()
        
    #------------------------------------------------------------------------------
    #--Implementation of IDL methods-----------------------------------------------
    #------------------------------------------------------------------------------
    def sendBlocks(self, initialVal, lastVal, changeVal, period):
        '''
        Python implementation of IDL method.
        '''
        
        self.LOGGER.logInfo("called...")
        
        
        #Send the events
        self.LOGGER.logInfo("Ready to send NC events...")
        myString = "Python supplier"
        val = initialVal
        eventCount = 0

        while val < lastVal:
            if val < changeVal:
                flag = False
            else:
                flag = True
            
            self.supplier.publishEvent(COUNTER.statusBlockEvent(COUNTER.ON, myString, val, lastVal, changeVal, flag, period))
            eventCount = eventCount + 1
            self.LOGGER.logInfo("Counting ongoing with period %.3fs up to %d, now %d" % (period,  lastVal, val) )
            val = val + 1
            sleep(period)
        
        # Tell consumers this is the last event
        myString = "Last event from Python supplier"
        self.supplier.publishEvent(COUNTER.statusBlockEvent(COUNTER.OFF, myString, lastVal, lastVal, changeVal, True, period))
        eventCount = eventCount + 1
        self.LOGGER.logInfo("Counter stopped, last value %d" % val)


        return eventCount

#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":

    # set the defaults
    initVal = 1
    lastVal = 20
    period = 0.25
    changeValSet = False
 
    try:                                
        opts, args = getopt.getopt(sys.argv[1:], "c:dhi:l:p:d", ["changeVal=", "debug", "help", "initVal=", "lastVal=", "period="])
    except getopt.GetoptError:          
        usage()                         
        sys.exit(2)
        
    # There should be lon arguments left
    if len(args) > 0:
        usage()
        sys.exit()
                       
    for opt, arg in opts:                
        if opt in ("-c", "--changeVal"): 
            changeVal = int(arg)
            changeValSet = True             
        elif opt == '-d':                
            global _debug               
            _debug = 1                  
        elif opt in ("-h", "--help"):      
            usage()                     
            sys.exit()                  
        elif opt in ("-i", "--initVal"): 
            initVal = int(arg)               
        elif opt in ("-l", "--lastVal"): 
            lastVal = int(arg)               
        elif opt in ("-p", "--period"): 
            period = float(arg)               

    if not changeValSet:
        changeVal = lastVal
        
    print "Creating an object"
    pySupplier = CounterSupplier()
    pySupplier.initialize()

    #Send the events
    eventCount = pySupplier.sendBlocks(initVal, lastVal, changeVal, period)

    pySupplier.cleanUp()
    
    print "Done..."

def usage():
    '''
    '''
    print "Usage: \
        CounterSupplier [--initialVal=<val>] [--lastVal=<val>] [--period=<period>] [--changeVal=<val>]"
    
    return

#------------------------------------------------------------------------------











