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
# "@(#) $Id: AlarmSystemInterface.py,v 1.1 2008/10/09 16:11:10 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-09-29  created
#

import Acspy.Common.Log as Log
import Acsalarmpy.ASI as ASI
import Acsalarmpy.Timestamp as Timestamp

class AlarmSystemInterface(object):
    """
    Alarm System Interface for Python alarm generators.  It is based
    on CERN's Java implementation.
    """
    def __init__(self, sourceName = None, hostName = None):
        """
        Create an instance of the AlarmSystemInterface.

        Parameters:  sourceName is the name of this source
                     hostName is the name of the computer where the source is running.
        """
        self.logger = Log.getLogger()
        self.sourceName = sourceName
        self.hostName = hostName
        self.configuration = ASI.ASIConfiguration()

    def push(self, state):
        """
        Push fault states to the LASER server.

        Parameters: state is a single fault state or a list of fault states.
        """
        self.logger.logTrace("AlarmSystemInterface: entering push.")
        if isinstance(state, list):
            self._commonPush(state, False)
        else:
            self._commonPush([state], False)
        self.logger.logTrace("AlarmSystemInterface: exiting push.")

    def pushActiveList(self, activeFaults):
        """
        Pushes a list of active fault states to the LASER server.

        Parameters: activeFaults is the list of fault states.
        """
        self.logger.logTrace("AlarmSystemInterface: entering pushActiveList.")
        self._commonPush(activeFaults, True)
        self.logger.logTrace("AlarmSystemInterface: exiting pushActiveList.")

    def close(self):
        """
        Clean up the resources used by this interface.  Must be implemented by
        the subclasses.
        """
        raise Exception("Implement in derived classes")

    def publishMessage(self, msg):
        """
        Send a message to the LASER alarm server.  Must be implemented in the
        subclasses using the desired communication mechanism.

        Parameter:  msg is the ASIMessage to be sent.
        """
        raise Exception("Implement in derived classes")

    def _commonPush(self, states, backup):
        """
        Common method to push a collection of fault states to the LASER server.

        Parameters: states is a list of the fault states to be sent
                    backup is a flag indicating if we are sending backup alarms.
                    Backup alarms are alarms in the active list that are sent on
                    startup, when the source starts and periodically according to
                    the expected backup frequency.
        """
        self.logger.logTrace("AlarmSystemInterface: entering _commonPush.")
        msg = ASI.ASIMessage(states)
        msg.sourceTimestamp = Timestamp.Timestamp()
        msg.sourceName = self.sourceName
        msg.sourceHostname = self.hostName
        msg.backup = backup
        msg.version = self.configuration.asiVersion
        self.publishMessage(msg)
        for s in states:
            self.logger.logAlert('Alarm sent: <%s, %s, %d> %s' % (s.family, s.member, s.code, s.descriptor))
        self.logger.logTrace("AlarmSystemInterface: exiting _commonPush.")
            
#
# ___oOo___
