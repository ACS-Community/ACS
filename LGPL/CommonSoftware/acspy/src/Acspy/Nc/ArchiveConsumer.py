# @(#) $Id: ArchiveConsumer.py,v 1.6 2006/11/24 07:55:57 cparedes Exp $
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

__revision__ = "$Id: ArchiveConsumer.py,v 1.6 2006/11/24 07:55:57 cparedes Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
import acscommon
#--ACS Imports-----------------------------------------------------------------
from Acspy.Nc.Consumer   import Consumer
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class ArchiveConsumer (Consumer):
    #--------------------------------------------------------------------------
    '''
    ArchiveConsumer is a a Consumer-derived class designed solely for the purpose of
    processing notification channel structured events sent automatically by BACI
    properties under certain conditions. Basically all one has to do to use this 
    class is create an ArchiveConsumer object providing an object with implements
    "receive(String timeStamp, String device, String parameter, String value)"
    and then invoke the consumerReady() method. Since archive events do not contain
    complex IDL structs, filtering using the extended trader constraint language
    should work as well.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, handler):
        '''
        Constructor.
        
        Params:
        - handler
        
        Returns: Nothing
        
        Raises: ACSErrTypeCommonImpl.CORBAProblemExImpl on critical failures
        '''
        self.handler = handler

        Consumer.__init__(self, acscommon.ARCHIVING_CHANNEL_NAME)
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
    def push_structured_event (self, event):
        '''
        Overridden.
        
        Parameters: event is a CosNotification.StructuredEvent
        
        Returns: Nothing

        Raises: Nothing
        '''
        #extract useful info
        timeStamp = event.filterable_data[0].value.value()
        value =     event.filterable_data[1].value.value()
        t_name_list = event.header.fixed_header.event_name.split(':')
        container = t_name_list[0]
        device =    t_name_list[1]
        parameter = t_name_list[2]
	    
        try:
            #invoke the user-defined function on it
            self.handler(timeStamp, device, parameter, value)
            #ignore everything else
            return
        
        except Exception, e:
            self.logger.logCritical('Unable to use handler function...' +
                                    str(e))
            print_exc()
#------------------------------------------------------------------------------
