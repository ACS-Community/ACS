#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2007 
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
# "@(#) $Id: acspyTestLogWithAudience.py,v 1.4 2007/07/20 09:07:52 nbarriga Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# nbarriga  2007-07-11  created
#


from Acspy.Common.Log import getLogger
import ACSLog
from Acspy.Clients.SimpleClient import PySimpleClient
import logging 
from log_audience import OPERATOR
from log_audience import NO_AUDIENCE

simpleClient = PySimpleClient()

logger = getLogger("TestAudience")
logger.log(logging.WARNING, "Normal log")
logger.logNotSoTypeSafe(ACSLog.ACS_LOG_WARNING, "Log with audience, array and antenna", OPERATOR, "Array01", "Antenna01")
logger.logNotSoTypeSafe(ACSLog.ACS_LOG_WARNING, "Log with audience", OPERATOR)
logger.logNotSoTypeSafe(ACSLog.ACS_LOG_WARNING, "Log with array and antenna", NO_AUDIENCE, "Array01", "Antenna01")

simpleClient.disconnect()
#
# ___oOo___
