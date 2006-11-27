# @(#) $Id: ArchiveSupplier.py,v 1.1 2006/11/27 09:55:56 cparedes Exp $
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
#------------------------------------------------------------------------------
'''
TODO:
- everything
'''

__revision__ = "$Id: ArchiveSupplier.py,v 1.1 2006/11/27 09:55:56 cparedes Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
import acscommon
from omniORB       import CORBA
from omniORB       import any
import CosNotification
#--ACS Imports-----------------------------------------------------------------
from ACSErrTypeCommonImpl         import TypeNotSupportedExImpl
from ACSErrTypeCommonImpl         import CORBAProblemExImpl
from ACSErrTypeCommonImpl         import CouldntPerformActionExImpl
from ACSErr                       import NameValue
import acsnc
from Acspy.Nc.Supplier   import Supplier
from Acspy.Nc.CDBProperties   import get_integration_logs
from Acspy.Common.TimeHelper  import getTimeStamp
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class ArchiveSupplier (Supplier):
    #--------------------------------------------------------------------------
    '''
    ArchiveSupplier is a a Supplier-derived class designed solely for the purpose of
    sending notification channel structured events automatically by BACI
    '''
    #--------------------------------------------------------------------------
    def __init__ (self):
        '''
        Constructor.
        
        Params:
        - handler
        
        Returns: Nothing
        
        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''

        Supplier.__init__(self, acscommon.ARCHIVING_CHANNEL_NAME)
    #--------------------------------------------------------------------------
    def getChannelKind(self):
        '''
        Overridden.
        
        Parameters: None
        
        Returns: pointer to a constant string.
        
        Raises: Nothing
        '''
        return acscommon.ARCHIVING_CHANNEL_KIND
    #--------------------------------------------------------------------------
    def getChannelDomain(self):
        '''
        Overridden.
        
        Parameters: None
        
        Returns: pointer to a constant string.
        
        Raises: Nothing
        '''
        return "*"
        
    #------------------------------------------------------------------------------
    def getNotificationFactoryName(self):
        '''
        Overridden.
        
        Parameters: None
        
        Returns: pointer to a constant string. Normally
        acscommon::ARCHIVE_NOTIFICATION_FACTORY_NAME
        
        Raises: Nothing
        '''
        return acscommon.ARCHIVE_NOTIFICATION_FACTORY_NAME
    #--------------------------------------------------------------------------
    def publishEvent (self,
                      simple_data=None,
                      type_name=None,
                      event_name="",
                      se=None,
                      supplier_name=None):
        '''
        publishEvent is the one method developers have to use.


        Params:
        - simple_data is a user-defined IDL struct. If this parameter is not
        specified by the developer, se MUST be used.  99% of the time developers
        should specify the simple_data parameter and NOTHING ELSE!
        - type_name is literally the type_name field of a structured event. This
        is an optional parameter and should not be specified under normal
        circumstances. If unspecified, the name of the simple_data object is used.
        - event_name is the event_name field of the structured event. Not really
        useful.
        - se is a fully-defined structured event. If this parameter is specified,
        all other parameters will be completely ignored. A check is made to ensure
        that this object is really what it claims to be. This parameter is
        reserved for ACS internal usage.
        - suppier_name is the name of the supplier publishing the event. This
        parameter is reserved for ACS internal usage.

        Returns: Nothing

        Raises:
        - ACSErrTypeCommonImpl.CORBAProblemExImpl
        - ACSErrTypeCommonImpl.CouldntPerformActionExImpl
        - ACSErrTypeCommonImpl.TypeNotSupportedExImpl
        '''
        #Whoah, user passed in the entire structured event...I'm impressed.
        if se != None:
            Supplier.publishEvent(simple_data,type_name,event_name,se, supplier_name)
	
	#User didn't specify type_name.  Assume it's the name of the
        #repository ID. If that doesn't work either, must be a simple
        #CORBA type.
        if (type_name == None) and (simple_data != None):
            try:
                type_name = str(simple_data.__class__.__name__)
            except Exception, e:
                self.logger.logWarning(str(e))
                print_exc()
                type_name = str(CORBA.id(simple_data))
        elif (simple_data == None):
            raise CouldntPerformActionExImpl(nvSeq=[NameValue("channelname",
                                                              self.channelName),
                                                    NameValue("reason",
                                                              "Empty data")])
        container_name = "container_name"
        device_name = "no_device"
        parameter_name = "no_param"
        if event_name == "":
	    event_name = container_name+":"+ device_name +":"+parameter_name 
        #get the time the event is being sent
        time_stamp = getTimeStamp()
        #create the CORBA Any in the "normal" manner first.  If this
        #fails, try omniORB's any helper module designed for simple types.
        try:
            corba_any = CORBA.Any(CORBA.TypeCode(CORBA.id(simple_data)),
                                  simple_data)
            corba_time = CORBA.Any(CORBA.TC_long, time_stamp.value)
        except Exception, e:
            self.logger.logTrace(str(e))
            try:
                corba_any = any.to_any(simple_data)
                corba_time = any.to_any(time_stamp.value)
            except Exception, e:
                print_exc()
                raise TypeNotSupportedExImpl(nvSeq=[NameValue("channelname",
                                                              self.channelName),
                                                    NameValue("exception",
                                                              str(e)),
                                                    NameValue("reason",
                                                              "Data not a CORBA type")])
    
        #Create the real structured event.  Look at CosNotification.idl to see these definitions
        fixed_header = CosNotification.FixedEventHeader(CosNotification.EventType(str(self.getChannelDomain()),
                                                                                  str(type_name)),
                                                        str(event_name))
        header = CosNotification.EventHeader (fixed_header,
                                              [])
        
        # create list of Property to pass in the event
        new_list = [CosNotification.Property("time_stamp",corba_time),
		    CosNotification.Property("value",corba_any)]

        #Create the event dscription
        #get the component/client's name
        if supplier_name==None:
            try:
                component_name = self.component._get_name()
            except Exception, e:
                component_name = "Unknown"
        else:
            component_name = supplier_name
                
        
        event_descrip = acsnc.EventDescription(component_name,
                                               long(time_stamp.value),
                                               long(self.count))
        event_descrip = CORBA.Any(CORBA.TypeCode(CORBA.id(event_descrip)),
                                  event_descrip)

        # create the structured event
        se = CosNotification.StructuredEvent(header,
                                             new_list,
                                             event_descrip)
        
        # publish the event
        try:
            self.sppc.push_structured_event(se)

            #For HLA/ITS - to be removed later!
            if get_integration_logs(self.channelName)==1:
                try:
                    comp_name = self.component.getName()
                except:
                    comp_name = "Unknown"
                    
                self.logger.logNotice("Publisher:" + comp_name +
                                      ", Event Type:" + type_name)
            
            self.count = self.count + 1
        except Exception, e:
            print_exc()
            raise CORBAProblemExImpl(nvSeq=[NameValue("channelname",
                                                      self.channelName),
                                            NameValue("exception",
                                                      str(e))])
        return
#------------------------------------------------------------------------------
