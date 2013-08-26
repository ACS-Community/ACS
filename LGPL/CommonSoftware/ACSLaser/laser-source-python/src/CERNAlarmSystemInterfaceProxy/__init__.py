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
# agrimstrup  2008-09-29  created
#

import sys
from socket import gethostname
import Acsalarmpy.AlarmSystemInterface as AlarmSystemInterface
import AlarmPublisher

class CERNAlarmSystemInterfaceProxy(AlarmSystemInterface.AlarmSystemInterface):
    """
    Implementation of an alarm source that sends messages to the LASER alarm server.
    """
    def __init__(self, sourceName = None):
        """
        Constructor.

        Params:
        - sourceName is the name of the alarm source

        Returns: Nothing

        Raises:  Nothing
        """
        self.publisher = None
        self.topic = None
        super(CERNAlarmSystemInterfaceProxy,self).__init__(sourceName, gethostname())

    def close(self):
        """
        Shutdown this alarm source

        Params:  None

        Returns:  Nothing

        Raises:  Nothing
        """
        self.logger.logTrace("%s: entering %s" % (self.__class__,sys._getframe().f_code.co_name))
        self.sourceName = None
        self.hostName = None
        self.publisher = None
        self.topic = None
        self.logger.logTrace("%s: exiting %s" % (self.__class__,sys._getframe().f_code.co_name))


    def publishMessage(self, msg):
        """
        Send an alarm to the system.

        Params:
        - mag is the ASIMessage to be sent to the LASER system.

        Returns:  Nothing

        Raises:  Nothing
        """
        self.logger.logTrace("%s: entering %s" % (self.__class__,sys._getframe().f_code.co_name))

        # Lazy evaluation is performed to create the connection to the LASER system.
        # This practice taken from the C++ implementation, presumably to reduce the
        # number of idle channels in the system.
        if self.topic is None:
            if msg.sourceName is not None:
                self.topic = self.configuration.alarmsTopic + '.' + msg.sourceName
            else:
                self.topic = self.configuration.alarmsTopic
        if self.publisher is None:
            self.publisher = AlarmPublisher.AlarmPublisher(self.topic)

        self.publisher.publishAlarm(msg)
        
        self.logger.logTrace("%s: exiting %s" % (self.__class__,sys._getframe().f_code.co_name))
#
# ___oOo___
