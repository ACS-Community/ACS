#! /bin/sh
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
# "@(#) $Id: DynamicContainerConfigurationTest.sh,v 1.1 2007/03/07 09:09:08 nbarriga Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# nbarriga  2007-03-07  created
#

maciContainerLogLevel dynamicTestContainer get default
maciContainerLogLevel dynamicTestContainer set default 2 4
maciContainerLogLevel dynamicTestContainer get default
maciContainerLogLevel dynamicTestContainer refresh
maciContainerLogLevel dynamicTestContainer get default

#
# ___oOo___
