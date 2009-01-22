#!/usr/bin/env python
# @(#) $Id: contNcTestCounterSupplier.py,v 1.2 2009/01/22 19:55:05 jslopez Exp $
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
import COUNTER
#--ACS Imports-----------------------------------------------------------------
from Acspy.Nc.Supplier      import Supplier
from Acspy.Common.Log       import getLogger
#--GLOBALS---------------------------------------------------------------------
LOGGER = getLogger("contNcTestCounterSupplier")
#------------------------------------------------------------------------------
def usage():
    '''
    '''
    print "Usage: \
        contNcTestCounterSupplier [--initialVal=<val>] [--lastVal=<val>] [--period=<period>] [--changeVal=<val>]"
    
    return

#------------------------------------------------------------------------------

if __name__ == "__main__":

    # set the defaults
    myString = "Dummy content"
    initVal = 1
    lastVal = 20
    period = 0.5
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
        
    #Create a supplier
    LOGGER.logInfo('Creating an instance of Supplier')
    g = Supplier(COUNTER.CHANNELNAME_COUNTER)

    #Send the events
    LOGGER.logInfo("Ready to send NC events...")
    val = initVal

    while val < lastVal:
        if val < changeVal:
            flag = False
        else:
            flag = True
	g.publishEvent(COUNTER.statusBlockEvent(COUNTER.ON, myString, val, lastVal, changeVal, flag, period))
        LOGGER.logInfo("Counting ongoing with period %.3fs up to %d, now %d" % (period,  lastVal, val) )
        val = val + 1
        sleep(period)
        
    # Tell consumers this is the last event
    g.publishEvent(COUNTER.statusBlockEvent(COUNTER.OFF, myString, lastVal, lastVal, changeVal, True, period))
    LOGGER.logInfo("Counter stopped, last value %d" % val)
    # As this will be the main process in the tat-test, we should take care that we
    # don't exit earlier than the consumer process has had an opportunity to log
    # whatever it wants before exiting - tat will stop collecting output as
    # soon as the main process stops. A simple short sleep should be enough.
    sleep(1)
    
    LOGGER.logInfo("Events all done . . . exiting")
    #cleanly disconnect from the channel
    g.disconnect()
#------------------------------------------------------------------------------
