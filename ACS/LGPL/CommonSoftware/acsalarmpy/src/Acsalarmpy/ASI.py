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
# "@(#) $Id: ASI.py,v 1.2 2008/10/29 16:20:18 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-10-02  created
#

ASI_VERSION = "0.9"
ALARMS_TOPIC = "CMW.ALARM_SYSTEM.ALARMS.SOURCES"
ALARM_SOURCE_NAME = "ALARM_SYSTEM_SOURCES"
BACKUP_DELIVERY_MODE = 0
BACKUP_PRIORITY = 9
BACKUP_TIME_TO_LIVE = 60000
CHANGES_DELIVERY_MODE = 0
CHANGES_PRIORITY = 9
CHANGES_TIME_TO_LIVE = 60000

class ASIConfiguration(object):
    """
    Python class that encapsulates various configuration data for the LASER
    alarm system for use by Python alarm source clients.   
    """
    def __init__(self):
        self.asiVersion = ASI_VERSION
        self.alarmsTopic = ALARMS_TOPIC
        self.backupDeliveryMode = BACKUP_DELIVERY_MODE
        self.backupPriority = BACKUP_PRIORITY
        self.backupTimeToLive = BACKUP_TIME_TO_LIVE
        self.changesDeliveryMode = CHANGES_DELIVERY_MODE
        self.changesPriority = CHANGES_PRIORITY
        self.changesTimeToLive = CHANGES_TIME_TO_LIVE


class ASIMessage(object):
    """
    Class that encapsulates one or more fault states to be sent to the LASER
    alarm server.
    """
    def __init__(self, states = None):
        self.faultStates = states
        self.backup = False
        self.version = ASI_VERSION
        self.sourceName = ALARM_SOURCE_NAME
        self.sourceHostname = None
        self.sourceTimestamp = None

    def toXML(self):
        """
        Build the XML representation of the message which will be sent to the alarm server.
        """
        pad = '\t'.expandtabs(3)
        taglist = []
        header = '<?xml version="1.0" encoding="ISO-8859-1"?>\n'
        taglist.append('<ASI-message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" backup="%s" version="%s" xsi:type="ASI-message">\n' % (str(self.backup).lower(), self.version))
        taglist.append('<source-name>%s</source-name>\n' % self.sourceName)
        taglist.append('<source-hostname>%s</source-hostname>\n' % self.sourceHostname)
        taglist.append(self.sourceTimestamp.toXML('source-timestamp', 0))
        taglist.append('<fault-states>\n')
        try:
            for fs in self.faultStates:
                taglist.append(fs.toXML(amountToIndent=6))
        except TypeError:
            pass
        taglist.append('</fault-states>\n')
        return header + pad.join(taglist) + '</ASI-message>\n'
        
    
#
# ___oOo___
