#!/usr/bin/env python
# @(#) $Id: TConsumer.py,v 1.4 2015/01/23 16:51:58 pcolomer Exp $
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
from Acspy.Nc.Consumer import Consumer
from time import sleep
import TEST_NS_STATISTICS_SERVICE
import sys

def dataHandler1(params):
    print "Consumer1 - ", params.strVal
    sleep(1)
    return

def dataHandler2(params):
    print "Consumer2 - ", params.strVal
    sleep(1)
    return

def dataHandler3(params):
    print "Consumer3 - ", params.strVal
    sleep(1)
    return

def dataHandler4(params):
    print "Consumer4 - ", params.strVal
    sleep(1)
    return
    
def main(argv):
	consumers = []
	wait_sec = int(argv[0])
	for ch in argv[1:]:
		ch = int(ch)
		
		print "Creating channel %d" % (ch)
    	
		consumer = None
		if ch == 1:
			consumer = Consumer(TEST_NS_STATISTICS_SERVICE.CHANNEL_1)
			consumer.addSubscription(TEST_NS_STATISTICS_SERVICE.Test1EventData,handler_function=dataHandler1)
		elif ch == 2:
			consumer = Consumer(TEST_NS_STATISTICS_SERVICE.CHANNEL_2)
			consumer.addSubscription(TEST_NS_STATISTICS_SERVICE.Test1EventData,handler_function=dataHandler2)
		elif ch == 3:
			consumer = Consumer(TEST_NS_STATISTICS_SERVICE.CHANNEL_3)
			consumer.addSubscription(TEST_NS_STATISTICS_SERVICE.Test1EventData,handler_function=dataHandler3)
		elif ch == 4:
			consumer = Consumer(TEST_NS_STATISTICS_SERVICE.CHANNEL_4)
    		consumer.addSubscription(TEST_NS_STATISTICS_SERVICE.Test1EventData,handler_function=dataHandler4)
        	
		if consumer is None:
			raise BaseException("Unknown channel. Allowed values are from 1 to 4: %d"%(ch))		
		else:
			consumers.append(consumer)		
		  		
	print "Enable %d consumers"%(len(consumers))
	for consumer in consumers:
		consumer.consumerReady()

	if wait_sec > 0:        
		print "Wait %d seconds"%(wait_sec)
		sleep(wait_sec)
    
	# Disconnect consumers
	print "Disconnect %d consumers"%(len(consumers))
	for consumer in consumers:
		consumer.disconnect()


if __name__ == "__main__":
	# > TConsumer.py wait_sec ch_id_1 ch_id_2 ... ch_id_N
	# Where ch_id can be 1, 2, 3, 4
	main(sys.argv[1:])    
