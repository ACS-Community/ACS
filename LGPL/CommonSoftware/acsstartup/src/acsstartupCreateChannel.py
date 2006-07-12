#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2006 
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id: acsstartupCreateChannel.py,v 1.3 2006/07/12 21:36:02 dfugate Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# dfugate  2006-06-20  created
#

#************************************************************************
#   NAME
# 
#   SYNOPSIS
# 
#   DESCRIPTION
#
#   FILES
#
#   ENVIRONMENT
#
#   RETURN VALUES
#
#   CAUTIONS
#
#   EXAMPLES
#
#   SEE ALSO
#
#   BUGS     
#
#------------------------------------------------------------------------
#

'''
This script is designed to create notification channels from the command-line
with the assigned quality of service and administrative properties.

Assumptions:
- the CORBA Naming Service is up and running
- the Notify_Service process which will host the channel is up and running
- the Notify_Service process is registered with the CORBA Naming Service
- the end-user knows which Q of S and admin properties cause the TAO Notify Service
to fail to create channels. This varies with each release of TAO

TODO:
'''

from optparse import OptionParser


#------------------------------------------------------------------------------
#--Parse the command-line options.

usage_msg='''
This script is used to create notification channels.

For quality of service and administrative properties, it accepts EXACTLY the same
type of values as defined by the ACS CDB ($ACS_CDB/MACI/Channels/channelName/channelName.xml
section). Aside from this, just remember that boolean values should be entered as
"true" or "false" as opposed to "0" or "1".
'''
parser = OptionParser(usage=usage_msg)

#naming service corbaloc
parser.add_option("--name_service",
                  dest="name_service",
                  help="Corbaloc of the CORBA Naming Service. Required.")

#notify service id
parser.add_option("--notify_service_id",
                  dest="notify_service_id",
                  help="ID of the Notification Service as registered with the CORBA Naming Service. Required.")

#notify service kind
parser.add_option("--notify_service_kind",
                  dest="notify_service_kind",
                  default="",
                  help="Kind of the Notification Service as registered with the CORBA Naming Service. Optional.")

#channel id
parser.add_option("--channel_id",
                  dest="channel_id",
                  help="ID of the Notification Channel to be created as registered with the CORBA Naming Service. Required.")

#channel kind
parser.add_option("--channel_kind",
                  dest="channel_kind",
                  default="",
                  help="Kind of the Notification Channel to be created as registered with the CORBA Naming Service. Optional.")

#-----------------------------
#channel admin props

#max queue length
parser.add_option("--max_queue_length",
                  dest="max_queue_length",
                  help="See ACS Notification Channel tutorial for description.")

#max consumers
parser.add_option("--max_consumers",
                  dest="max_consumers",
                  help="See ACS Notification Channel tutorial for description.")

#max suppliers
parser.add_option("--max_suppliers",
                  dest="max_suppliers",
                  help="See ACS Notification Channel tutorial for description.")

#reject new events
parser.add_option("--reject_new_events",
                  dest="reject_new_events",
                  help="See ACS Notification Channel tutorial for description.")

#-----------------------------
#Channel quality of service props

#discard policy
parser.add_option("--discard_policy",
                  dest="discard_policy",
                  help="See ACS Notification Channel tutorial for description.")

#event reliability
parser.add_option("--event_reliability",
                  dest="event_reliability",
                  help="See ACS Notification Channel tutorial for description.")

#connection reliability
parser.add_option("--connection_reliability",
                  dest="connection_reliability",
                  help="See ACS Notification Channel tutorial for description.")

#priority
parser.add_option("--priority",
                  dest="priority",
                  help="See ACS Notification Channel tutorial for description.")

#timeout
parser.add_option("--timeout",
                  dest="timeout",
                  help="See ACS Notification Channel tutorial for description.")

#order policy
parser.add_option("--order_policy",
                  dest="order_policy",
                  help="See ACS Notification Channel tutorial for description.")

#start time supported
parser.add_option("--start_time_supported",
                  dest="start_time_supported",
                  help="See ACS Notification Channel tutorial for description.")

#stop time supported
parser.add_option("--stop_time_supported",
                  dest="stop_time_supported",
                  help="See ACS Notification Channel tutorial for description.")

#max events per consumer
parser.add_option("--max_events_per_consumer",
                  dest="max_events_per_consumer",
                  help="See ACS Notification Channel tutorial for description.")

(options, parsed_argv) = parser.parse_args()

#------------------------------------------------------------------------------
#--GLOBALS
from omniORB import CORBA
import sys

ORB = CORBA.ORB_init(sys.argv)
POA_ROOT = ORB.resolve_initial_references("RootPOA")
POA_MANAGER = POA_ROOT._get_the_POAManager()            
POA_MANAGER.activate()
CHANNEL_ADMIN_PROPS=[]
CHANNEL_QOS_PROPS=[]

#------------------------------------------------------------------------------
#--Sanity checks

import CosNotification
from omniORB import any

#name_service
if options.name_service==None:
    print "name_service is a required paramter!"
    sys.exit(1)
else:
    cl_name_service = options.name_service

#notify_service_id
if options.notify_service_id==None:
    print "notify_service_id is a required paramter!"
    sys.exit(1)
else:
    cl_notify_service_id = options.notify_service_id

#notify_service_kind
if options.notify_service_kind==None:
    cl_notify_service_kind = ""
else:
    cl_notify_service_kind = options.notify_service_kind

#channel_id
if options.channel_id==None:
    print "channel_id is a required paramter!"
    sys.exit(1)
else:
    cl_channel_id = options.channel_id

#channel_kind
if options.channel_kind==None:
    cl_channel_kind=""
else:
    cl_channel_kind = options.channel_kind


#-----------------------------
#channel admin props

#max queue length
if options.max_queue_length!=None:
    temp_any = any.to_any(long(options.max_queue_length))
    CHANNEL_ADMIN_PROPS.append(CosNotification.Property(CosNotification.MaxQueueLength,
                                                        temp_any))
#max consumers
if options.max_consumers!=None:
    temp_any = any.to_any(long(options.max_consumers))
    CHANNEL_ADMIN_PROPS.append(CosNotification.Property(CosNotification.MaxConsumers,
                                                        temp_any))
#max suppliers
if options.max_suppliers!=None:
    temp_any = any.to_any(long(options.max_suppliers))
    CHANNEL_ADMIN_PROPS.append(CosNotification.Property(CosNotification.MaxSuppliers,
                                                        temp_any))
#reject new events
if options.reject_new_events!=None:
    if options.reject_new_events=="false":
        t_bool = 0
    else:
        t_bool = 1
    temp_any = any.to_any(t_bool)
    
    CHANNEL_ADMIN_PROPS.append(CosNotification.Property(CosNotification.RejectNewEvents,
                                                        temp_any))

#-----------------------------
#Channel quality of service props

#discard policy
if options.discard_policy!=None:
    temp_val = eval("CosNotification." +
                    options.discard_policy)
    temp_any = any.to_any(temp_val)
    CHANNEL_QOS_PROPS.append(CosNotification.Property(CosNotification.DiscardPolicy,
                                                      temp_any))

#event reliability
if options.event_reliability!=None:
    temp_val = eval("CosNotification." +
                    options.event_reliability)
    temp_any = any.to_any(temp_val)
    CHANNEL_QOS_PROPS.append(CosNotification.Property(CosNotification.EventReliability,
                                                      temp_any))
    
#connection reliability
if options.connection_reliability!=None:
    temp_val = eval("CosNotification." +
                    options.connection_reliability)
    temp_any = any.to_any(temp_val)
    CHANNEL_QOS_PROPS.append(CosNotification.Property(CosNotification.ConnectionReliability,
                                                      temp_any))
    
#priority
if options.priority!=None:
    temp_any = any.to_any(int(options.priority))
    CHANNEL_QOS_PROPS.append(CosNotification.Property(CosNotification.Priority,
                                                      temp_any))
#timeout
if options.timeout!=None:
    temp_any = any.to_any(long(options.timeout))
    CHANNEL_QOS_PROPS.append(CosNotification.Property(CosNotification.Timeout,
                                                      temp_any))
#order policy
if options.order_policy!=None:
    temp_val = eval("CosNotification." +
                    options.order_policy)
    temp_any = any.to_any(temp_val)
    CHANNEL_QOS_PROPS.append(CosNotification.Property(CosNotification.OrderPolicy,
                                                      temp_any))
#start time supported
if options.start_time_supported!=None:
    if options.start_time_supported=="false":
        t_bool = 0
    else:
        t_bool = 1
    temp_any = any.to_any(t_bool)
    CHANNEL_QOS_PROPS.append(CosNotification.Property(CosNotification.StartTimeSupported,
                                                      temp_any))    
#stop time supported
if options.stop_time_supported!=None:    
    if options.stop_time_supported=="false":
        t_bool = 0
    else:
        t_bool = 1
    temp_any = any.to_any(t_bool)
    CHANNEL_QOS_PROPS.append(CosNotification.Property(CosNotification.StopTimeSupported,
                                                      temp_any))
#max events per consumer
if options.max_events_per_consumer!=None:
    temp_any = any.to_any(long(options.max_events_per_consumer))
    CHANNEL_QOS_PROPS.append(CosNotification.Property(CosNotification.MaxEventsPerConsumer,
                                                      temp_any))

#------------------------------------------------------------------------------
#--Get the Naming and Notify Services
import CosNaming
import CosNotifyChannelAdmin

#name service corbaloc comes from the command-line
NAME_SERVICE = ORB.string_to_object(cl_name_service)
NAME_SERVICE = NAME_SERVICE._narrow(CosNaming.NamingContext)

#must get notify service from naming service using names provided
#from the command-line
notify_name = CosNaming.NameComponent(cl_notify_service_id,
                                      cl_notify_service_kind)
NOTIFY_SERVICE = NAME_SERVICE.resolve([notify_name])
NOTIFY_SERVICE = NOTIFY_SERVICE._narrow(CosNotifyChannelAdmin.EventChannelFactory)


#------------------------------------------------------------------------------
#--Create the channel
CHANNEL_REF, CHANNEL_ID = NOTIFY_SERVICE.create_channel(CHANNEL_QOS_PROPS,
                                                        CHANNEL_ADMIN_PROPS)

#------------------------------------------------------------------------------
#--Register the channel with the naming service
#from the command-line
channel_name = CosNaming.NameComponent(cl_channel_id,
                                       cl_channel_kind)
NAME_SERVICE.rebind([channel_name], CHANNEL_REF)

#------------------------------------------------------------------------------
#--Exit
#print "Channel created and has the ID:", CHANNEL_ID
sys.exit(0)

#
# ___oOo___
