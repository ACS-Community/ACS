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
# "@(#) $Id: simpleClient.py,v 1.2 2010/10/06 16:56:48 rtobar Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# arne  2008-06-16  created
#

from Acspy.Clients.SimpleClient import PySimpleClient

cl = PySimpleClient.getInstance()
c1 = cl.getComponent("COMP_JAVA")
c2 = cl.getComponent("COMP_CPP")
c3 = cl.getComponent("COMP_PY")

try:
	c1.getOtherComponent()
	c2.getOtherComponent()
	c3.getOtherComponent()
except Exception, e:
	print "Something went wrong on the server side :("
	print e
