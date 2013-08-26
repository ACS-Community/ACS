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
# @(#) $Id: acspyTestTimehelper.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
###############################################################################
'''
Tests the Python Time System.
'''
###############################################################################
import acstime
from Acspy.Common import TimeHelper

print "getTimeStamp is:", TimeHelper.getTimeStamp()

obj = TimeHelper.TimeUtil()

print "January 1, 1970 is:", obj.py2epoch(0).value, " in ACS time"

print "January 1, 1970 is:", obj.epoch2py(acstime.Epoch(obj.py2epoch(0).value)),  " in Python time"

print "October 15, 1582 is:", obj.py2epoch(obj.epoch2py(acstime.Epoch(0))).value, " in ACS time"

print "October 15, 1582 is:", obj.epoch2py(acstime.Epoch(0)),  " in Python time"

print "1000 seconds is:", obj.py2duration(1000).value, " in ACS time"

print "1000 seconds is:", obj.duration2py(acstime.Duration(obj.py2duration(1000).value)), " in Python time"
