#!/usr/bin/env python
# @(#) $Id: TSupplier.py,v 1.4 2015/01/23 16:51:58 pcolomer Exp $
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
from Acspy.Nc.Supplier import Supplier
from time import sleep
import TEST_NS_STATISTICS_SERVICE
import sys

def main(argv):

	suppliers = []
	suppliers_params = []
	n_events = int(argv[0])
	wait_sec = int(argv[1])
	for ch in argv[2:]:
		ch = int(ch)
		channel = None
		if ch == 1:
			channel = TEST_NS_STATISTICS_SERVICE.CHANNEL_1
		elif ch == 2:
			channel = TEST_NS_STATISTICS_SERVICE.CHANNEL_2
		elif ch == 3:
			channel = TEST_NS_STATISTICS_SERVICE.CHANNEL_3
		elif ch == 4:
			channel = TEST_NS_STATISTICS_SERVICE.CHANNEL_4
		else:
			raise BaseException("Wrong channel. Must be between 1 and 4") 
		
		# Create a supplier
		suppliers.append(Supplier(channel))
		
		# Create the parameters of the supplier
		suppliers_params.append({'channel_id': ch})

	for j in xrange(0,n_events):
		for i in xrange(0,len(suppliers)):
			supplier = suppliers[i]
			channel_id = suppliers_params[i]['channel_id']
			data = TEST_NS_STATISTICS_SERVICE.Test1EventData(j, j, "%d Event in channel %d"%(j,channel_id))
			supplier.publishEvent(data)
			print "Published the event number %d to channel %d" % (j, channel_id)
			
	if wait_sec > 0:
		print "Wait %d seconds" % (wait_sec)
		sleep(wait_sec)
		
	print "Disconnect %d suppliers" % (len(suppliers))		
	for supplier in suppliers:
		supplier.disconnect()

if __name__ == "__main__":
	# > TSupplier.py n_events wait_sec ch_id_1 ch_id_2 ... ch_id_N
	# Where ch_id can be 1, 2, 3, 4
	main(sys.argv[1:])