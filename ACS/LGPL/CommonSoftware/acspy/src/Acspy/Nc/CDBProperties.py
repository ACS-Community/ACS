# @(#) $Id: CDBProperties.py,v 1.15 2010/05/14 23:47:57 agrimstrup Exp $
#
# Copyright (C) 2001
# Associated Universities, Inc. Washington DC, USA.
#
# Produced for the ALMA project
#
# This library is free software; you can redistribute it and/or modify it under
# the terms of the GNU Library General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your option) any
# later version.
#
# This library is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
# details.
#
# You should have received a copy of the GNU Library General Public License
# along with this library; if not, write to the Free Software Foundation, Inc.,
# 675 Massachusetts Ave, Cambridge, MA 02139, USA.  Correspondence concerning
# ALMA should be addressed as follows:
#
# Internet email: alma-sw-admin@nrao.edu
'''
This module will provide helper functions to access the characteristics of
notification channels found in the ACS CDB.

TODO:
- lots
'''

__revision__ = "$Id: CDBProperties.py,v 1.15 2010/05/14 23:47:57 agrimstrup Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
from operator import isSequenceType
#--CORBA STUBS-----------------------------------------------------------------
import CosNotification
from omniORB import any

#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log          import getLogger
from Acspy.Common.CDBAccess    import CDBaccess
from Acspy.Util.XmlObjectifier import XmlObject
#--GLOBALS---------------------------------------------------------------------
_cdb_access = None

_nsmappings = { 'Channel': 'Channels/NotificationServiceMapping/Channels_/_',
                'Domain':  'Channels/NotificationServiceMapping/Domains/_',
                'Default' : 'Channels/NotificationServiceMapping'
                }

def get_cdb_access(): # pragma: NO COVER
    '''
    This function is only around to fix some problems with pydoc documentation
    issues. That is, CDBaccess() cannot be called globally or everything NC-related
    breaks.
    '''
    global _cdb_access
    
    if _cdb_access==None:
        _cdb_access = CDBaccess()
    
    return _cdb_access

#------------------------------------------------------------------------------
def get_notification_service_mapping(maptype):
    '''
    Retrieves the Notification Service mapping information for the given type
    from the CDB.

    Params:
    - maptype is a string indicating which kind of mapping information should
    be returned.  One of: Channel, Domain or Default

    Returns: list containing all the data for the requested mapping.

    Raises:  Nothing.
    '''
    global _nsmappings
    
    try:
        return get_cdb_access().getElement("MACI/Channels/", _nsmappings[maptype])
    except:
        return []
#------------------------------------------------------------------------------
def cdb_channel_config_exists(channel_name):
    '''
    Simple function which returns 1 if the given channel has an entry in
    $ACS_CDB/CDB/MACI/EventChannels/ section of the ACS configuration database.

    Params:
    - channel_name is a string referring to the name of a Notification Channel
    registered in the CORBA Naming Service.

    Returns: 1 if $ACS_CDB/CDB/MACI/EventChannels/channel_name/channel_name.xml
    exists and is a valid XML. 0 otherwise.

    Raises: Nothing.
    '''
    try:
        get_cdb_access().getField("MACI/Channels/" + channel_name)
        return 1
    except:
        return 0
    
#------------------------------------------------------------------------------
def get_channel_dict(channel_name): # pragma : NO COVER
    '''
    Function which retrieves event channel config data from the CDB.

    Params:
    - channel_name is a string referring to the name of a Notification Channel
    registered in the CORBA Naming Service.
    
    Returns: a Python dictionary consisting of all the data associated with an
    event channel from the ACS CDB. Assumes the CDB entry for the channel
    exists ($ACS_CDB/CDB/MACI/EventChannels/channel_name/channel_name.xml).
    '''
    #there should only be one element in the list returned by the getElement
    #method.
    return get_cdb_access().getElement("MACI/Channels/" + channel_name,
                                       "EventChannel")[0]

#------------------------------------------------------------------------------
INTEGRATION_LOGS = {}

def get_integration_logs(channel_name):

    global INTEGRATION_LOGS

    #check to see if this function has been called before to save system
    #resources
    if INTEGRATION_LOGS.has_key(channel_name):
        return INTEGRATION_LOGS[channel_name]


    #well if the channel does not exist of course we do not
    #want to log everything!!!
    try:
        if cdb_channel_config_exists(channel_name)==0:
            INTEGRATION_LOGS[channel_name] = 0

    #OK, the channel exists. time to do the dirty work.
        elif get_channel_dict(channel_name)["IntegrationLogs"] == "false":
            INTEGRATION_LOGS[channel_name] = 0
        elif get_channel_dict(channel_name)["IntegrationLogs"] == "true":
            INTEGRATION_LOGS[channel_name] = 1

    #should never be the case...
        else:
            INTEGRATION_LOGS[channel_name] = 0

    except:
        INTEGRATION_LOGS[channel_name] = 0
        

    return INTEGRATION_LOGS[channel_name]

#------------------------------------------------------------------------------
def get_channel_qofs_props(channel_name):
    '''
    Given a channel name that exists in the ACS CDB
    ($ACS_CDB/CDB/MACI/EventChannels/channel_name/channel_name.xml), this
    function returns the channels quality of service properties in their CORBA
    format.
    
    DWF - check q of s and admin properties implemented with each
    new version of TAO and uncomment properties accordingly.
    '''
    prop_seq = []
    
    #dictionary holding data about the administrative properties
    temp_dict = get_channel_dict(channel_name)
    
    #discard policy
    temp_val = eval("CosNotification." +
                    temp_dict[CosNotification.DiscardPolicy])
    temp_any = any.to_any(temp_val)
    prop_seq.append(CosNotification.Property(CosNotification.DiscardPolicy,
                                             temp_any))
    
    #event reliability
    temp_val = eval("CosNotification." +
                    temp_dict[CosNotification.EventReliability])
    temp_any = any.to_any(temp_val)
    #prop_seq.append(CosNotification.Property(CosNotification.EventReliability,
    #                                         temp_any))
    
    #connection reliability
    temp_val = eval("CosNotification." +
                    temp_dict[CosNotification.ConnectionReliability])
    temp_any = any.to_any(temp_val)
    #prop_seq.append(CosNotification.Property(CosNotification.ConnectionReliability,
    #                                         temp_any))
    
    #priority
    temp_any = any.to_any(int(temp_dict[CosNotification.Priority]))
    prop_seq.append(CosNotification.Property(CosNotification.Priority,
                                             temp_any))
    
    #timeout
    temp_any = any.to_any(long(temp_dict[CosNotification.Timeout]))
    prop_seq.append(CosNotification.Property(CosNotification.Timeout,
                                             temp_any))
    
    #order policy
    temp_val = eval("CosNotification." +
                    temp_dict[CosNotification.OrderPolicy])
    temp_any = any.to_any(temp_val)
    prop_seq.append(CosNotification.Property(CosNotification.OrderPolicy,
                                             temp_any))

    #start time supported
    if temp_dict[CosNotification.StartTimeSupported]=="false":
        t_bool = 0
    else:
        t_bool = 1
    temp_any = any.to_any(t_bool)
    #prop_seq.append(CosNotification.Property(CosNotification.StartTimeSupported,
    #                                         temp_any))
    
    #stop time supported
    if temp_dict[CosNotification.StopTimeSupported]=="false":
        t_bool = 0
    else:
        t_bool = 1
    temp_any = any.to_any(t_bool)
    #prop_seq.append(CosNotification.Property(CosNotification.StopTimeSupported,
    #                                         temp_any))

    #max events per consumer
    temp_any = any.to_any(long(temp_dict[CosNotification.MaxEventsPerConsumer]))
    prop_seq.append(CosNotification.Property(CosNotification.MaxEventsPerConsumer,
                                             temp_any))
    
    return prop_seq
#-----------------------------------------------------------------------------
def get_channel_admin_props(channel_name):
    '''
    Given a channel name that exists in the ACS CDB
    ($ACS_CDB/CDB/MACI/EventChannels/channel_name/channel_name.xml), this
    function returns the channels administrative properties in their CORBA
    format.
    '''
    prop_seq = []
    
    #dictionary holding data about the administrative properties
    temp_dict = get_channel_dict(channel_name)
    
    #max queue length
    temp_any = any.to_any(long(temp_dict[CosNotification.MaxQueueLength]))
    prop_seq.append(CosNotification.Property(CosNotification.MaxQueueLength,
                                             temp_any))
    
    #max consumers
    temp_any = any.to_any(long(temp_dict[CosNotification.MaxConsumers]))
    prop_seq.append(CosNotification.Property(CosNotification.MaxConsumers,
                                             temp_any))

    #max suppliers
    temp_any = any.to_any(long(temp_dict[CosNotification.MaxSuppliers]))
    prop_seq.append(CosNotification.Property(CosNotification.MaxSuppliers,
                                             temp_any))
    
    #reject new events
    if temp_dict[CosNotification.RejectNewEvents]=="false":
        t_bool = 0
    else:
        t_bool = 1
    temp_any = any.to_any(t_bool)
    
    prop_seq.append(CosNotification.Property(CosNotification.RejectNewEvents,
                                             temp_any))
    
    return prop_seq
#------------------------------------------------------------------------------
def getEventHandlerTimeoutDict(channel_name):
    '''
    The following returns a dict where each key is the
    name of an event and the value is the maximum amount of time
    an event handler has to process the event before a warning
    message is logged.
    
    Params: channelName - name of the channel

    Return: a dictionary mapping event types to the maximum amount of time a
    handler has to process the event.

    Raises: ???
    '''
    ret_val = {}

    #sanity check to see if the CDB entry is present
    if cdb_channel_config_exists(channel_name)==0:
        return ret_val

    #get the raw XML
    t_xml = get_cdb_access().getField("MACI/Channels/" + channel_name)

    #create an xml helper object
    xml_obj = XmlObject(xmlString=t_xml)

    #get the events section
    try:
        events = xml_obj.EventChannel.Events._
    except:
        #if there's an except, obviously the optional
        #events section was not added to this particular
        #event channel XML. OK to bail
        return ret_val

    #XmlObject will not convert to a sequence if only one
    #element is present. this we must do ourselves.
    if isSequenceType(events)==0:
        events = [ events ]

    #for each event in the list populate our dict
    for dom in events:
        #get the event type
        event_name = dom.getAttribute('Name')
        #set the max timeout 
        ret_val[event_name] = float(dom.getAttribute('MaxProcessTime'))

    return ret_val
