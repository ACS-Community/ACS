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
# "@(#) $Id: ACSLogTS.py,v 1.2 2007/01/30 12:06:53 nbarriga Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# nbarriga  2007-01-29  created
#
######################################################################
#from Acspy.Servants.ContainerServices import ContainerServices
from Acspy.Common.Log import getLogger
import ACSLog
from socket import gethostname
from Acspy.Common.TimeHelper import TimeUtil
import time
from os import getpid
from traceback import extract_stack
######################################################################
class ACSLogTS:
	"""
	This class is charged with sending the logs to the old log system
	"""
	#_members={}
	def __init__(self):
		self._members={}
		#ContainerServices.__init__(self)
		self._logger=getLogger("ACSLogTS --") #check if the name is needed

	def log(self):
		#TODO: check if fields actually exist, some are optional
		msg=self.shortDescription
		data=[ACSLog.NVPair("logName",self.name)]
		for key, value in self._members.items():
			data.append(ACSLog.NVPair(str(key),str(value)))
		
		cur_stack=extract_stack()
		
		
		rtCont=ACSLog.RTContext("",str(getpid()),str(gethostname()).replace("<", "").replace(">", ""),"","")

		
		srcInfo=ACSLog.SourceInfo(str(cur_stack[0][0]),str(cur_stack[0][2]),long(cur_stack[0][1]))

		timestamp=TimeUtil().py2epoch(time.time()).value
		self._logger.logTypeSafe(ACSLog.ACS_LOG_INFO, timestamp, msg, rtCont, srcInfo, data)

