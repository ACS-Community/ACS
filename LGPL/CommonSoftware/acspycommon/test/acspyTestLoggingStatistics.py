#!/usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2002 
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration)
# and Cosylab 2002, All rights reserved
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307  USA
#
# @(#) $Id: acspyTestLogging.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $


__version__ = "$Id: acspyTestLogging.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

from Acspy.Common.Log import getLogger
import sys
import os
import time

print '============= Statistics tests ============'

logger = getLogger("StatisticsTest")

#Statistics parameters
if os.environ.get("LOCATION"):
	assert(not logger.stats.getDisableStatistics())
	assert(logger.stats.getStatisticsCalculationPeriod() == 10 * 60)
	assert(logger.stats.getStatisticsGranularity() == 3)
else:
	assert(logger.stats.getDisableStatistics())


#Statistics calculation logging
logger.logNotice('Message not included in statistics 1')
logger.logNotice('Message not included in statistics 2')
logger.logNotice('Message not included in statistics 3')
logger.stats.configureStatistics("AcspyTestLogging", False, 2, 1)
logger.logNotice('Message included in statistics. First batch: 1')
logger.logNotice('Message included in statistics. First batch: 2')
logger.logNotice('Message included in statistics. First batch: 3')

# Force statistics
time.sleep(2)
logger.logNotice('Message included in statistics. Second batch: 1')
logger.logNotice('Message included in statistics. Second batch: 2')
logger.logNotice('Message included in statistics. Second batch: 3')
logger.logNotice('Message included in statistics. Second batch: 4')
logger.logNotice('Message included in statistics. Second batch: 5')

# Force statistics
time.sleep(2)

logger.logNotice('Message included in statistics. Third batch: 1')
logger.logNotice('Message included in statistics. Third batch: 2')


logger.closeLogger()

print "== The end __oOo__"
