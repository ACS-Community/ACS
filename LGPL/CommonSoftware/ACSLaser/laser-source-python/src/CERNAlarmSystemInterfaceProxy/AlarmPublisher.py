#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2008 
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
# "@(#) $Id$"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-10-15  created
#

import ACSJMSMessageEntity_idl
from Acspy.Nc.Supplier import Supplier

class AlarmPublisher(object):
    """
    This class provides the interface to the CORBA Notification Service used
    to send alarms to the LASER alarm server via a CORBA notification channel.
    """
    def __init__(self, topicName=None, component=None, domain=None):
        """
        Constructor

        Params:
        - topicName is the name of the notification channel to use
        - component is object generating the alarms (optional)
        - domain is the name of the domain of notification channels
          the channel belongs to. (optional)

        Returns: Nothing

        Raises:  Nothing
        """
        self.supplier = Supplier(topicName, component, domain)


    def __del__(self):
        """
        Destructor

        Params: None

        Returns: Nothing

        Raises:  Nothing
        """
        self.supplier.disconnect()
        self.supplier = None


    def publishAlarm(self, msg):
        """
        Send an alarm to the notification channel.

        Params:
        - msg is the alarm information to be sent

        Returns: Nothing

        Raises: Nothing
        """

        # The alarm notification channel expects ACSJMSMessageEntity structures to be
        # sent.  The XML form of the message is passed in the text field.  The
        # remaining fields are not used but must have default values set.
        emsg = ACSJMSMessageEntity_idl._0_com.cosylab.acs.jms.ACSJMSMessageEntity(msg.toXML(),0,False,"",0,0,[])
        self.supplier.publishEvent(simple_data=emsg)
        

#
# ___oOo___
