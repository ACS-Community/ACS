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

import sys
from omniORB import CORBA
import CosNaming
import NotifyMonitoringExt


nsCorbaloc = None
notify_service = None
domain = "DEFAULT"
ch_name = None
kind = "channels"

for i in range(0, len(sys.argv) - 1):
	if sys.argv[i] == '-name_service' or sys.argv[i] == '-n':
		nsCorbaloc = sys.argv[i + 1]
	elif sys.argv[i] == '-notify_service':
		notify_service = sys.argv[i + 1]
	elif sys.argv[i] == '-channel_name':
		ch_name = sys.argv[i + 1]
	elif sys.argv[i] == '-domain' or sys.argv[i] == '-d':
		domain = sys.argv[i + 1]


if notify_service is None:
	print "Notify Service is not set!"
	sys.exit(1)

if nsCorbaloc is None:
	print "Name Service is not set!"
	sys.exit(1)

if ch_name is None:
	print "Channel name is not set!"
	sys.exit(1)

ch_and_domain_name = ch_name + "@" + domain

orb = CORBA.ORB_init(sys.argv)
poa = orb.resolve_initial_references("RootPOA")
poa._get_the_POAManager().activate()


nameRoot = None
try:
	nameRoot = orb.string_to_object(nsCorbaloc)
	nameRoot = nameRoot._narrow(CosNaming.NamingContextExt)
	if nameRoot is None:
		print "NameService narrow failed"
		sys.exit(1)
except CORBA.ORB.InvalidName, ex:
	print "Got an InvalidName exception when resolving NameService!"
	sys.exit(1)

except CORBA.NO_RESOURCES, ex:
	print "No NameService configured!"
	sys.exit(1)

def create_channel():
	# Notify Service: NotifyEventChannelFactory
	name = [CosNaming.NameComponent(notify_service,"")]
	channel_factory = nameRoot.resolve(name)
	channel_factory = channel_factory._narrow(NotifyMonitoringExt.EventChannelFactory)
	(channel, channel_id) = channel_factory.create_named_channel([], [], ch_name)
	channel_id = None

	name = [CosNaming.NameComponent(ch_and_domain_name, kind)]
	nameRoot.rebind(name, channel)

	channel_ref = nameRoot.resolve(name)
	channel_ref = channel_ref._narrow(NotifyMonitoringExt.EventChannel)

try:
	create_channel()
except NotifyMonitoringExt.NameAlreadyUsed, ex:
	print "Channel %s already exists" % (ch_and_domain_name)
	sys.exit(1)

print "Channel %s created!" % (ch_and_domain_name)
#orb.run()
