# @(#) $Id: LoggingConsumer.py,v 1.3 2008/11/05 19:34:21 rtobar Exp $
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

__revision__ = "$Id: LoggingConsumer.py,v 1.3 2008/11/05 19:34:21 rtobar Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
import acscommon
#--ACS Imports-----------------------------------------------------------------
from Acspy.Nc.Consumer   import Consumer
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class LoggingConsumer (Consumer):
    #--------------------------------------------------------------------------
    '''
    LoggingConsumer is a a Consumer-derived class designed solely for the purpose of
    processing notification channel structured events sent automatically by the logging
    system. Basically all one has to do to use this 
    class is create a LoggingConsumer object providing an object with implements
    "receive(String xml)"
    and then invoke the consumerReady() method. Since logging events do not contain
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

        Consumer.__init__(self, acscommon.LOGGING_CHANNEL_XML_NAME)
    #--------------------------------------------------------------------------
    def getChannelKind(self):
        '''
        Overridden.
        
        Parameters: None
        
        Returns: pointer to a constant string.
        
        Raises: Nothing
        '''
        return acscommon.LOGGING_CHANNEL_KIND
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
        acscommon::LOGGING_NOTIFICATION_FACTORY_NAME

        Raises: Nothing
        '''
        return acscommon.LOGGING_NOTIFICATION_FACTORY_NAME
    #--------------------------------------------------------------------------
    def push_structured_event (self, event):
        '''
        Overridden.
        
        Parameters: event is a CosNotification.StructuredEvent
        
        Returns: Nothing

        Raises: Nothing
        '''
        #extract useful info
        xml = event.remainder_of_body.value()
        
        try:
            #invoke the user-defined function on it
            self.handler(xml)
            #ignore everything else
            return
        
        except Exception, e:
            self.logger.logCritical('Unable to use handler function...' +
                                    str(e))
            print_exc()
#------------------------------------------------------------------------------
