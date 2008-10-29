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
# "@(#) $Id: __init__.py,v 1.1 2008/10/29 08:38:05 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-09-29  created
#

import Acsalarmpy.AlarmSystemInterface as ASInterface

class ACSAlarmSystemInterfaceProxy(ASInterface.AlarmSystemInterface):
    """
    Implementation of an alarm source that sends messages to the log
    system rather than the LASER alarm server.
    """
    def __init__(self, sourceName = None):
        super(ACSAlarmSystemInterfaceProxy,self).__init__(sourceName)

    def _commonPush(self, states, backup):
        """
        Overridden implementation of base class method.
        """
        for s in states:
            self.logger.logAlert('Alarm sent: <%s, %s, %d> %s' % (s.family, s.member, s.code, s.descriptor))

#
# ___oOo___
