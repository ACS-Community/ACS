#! /bin/bash

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
# "@(#) $Id: DynamicContainerConfigurationTest.sh,v 1.3 2012/12/12 17:13:03 hsommer Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# nbarriga  2007-03-07  created
#

echo "Will test dynamic log level configuration using the 'maciContainerLogLevel' tool"

echo "Will read default logger config of 'dynamicTestContainer'. Expecting local level 4 set by ACS_LOG_STDOUT:"
maciContainerLogLevel dynamicTestContainer get default

echo "Will set default logger config of 'dynamicTestContainer' to (3, 5):"
maciContainerLogLevel dynamicTestContainer set default 3 5

echo "Will read default logger config of 'dynamicTestContainer':"
maciContainerLogLevel dynamicTestContainer get default

echo "Will refresh logger config of 'dynamicTestContainer' from CDB and static default values:"
maciContainerLogLevel dynamicTestContainer refresh

echo "Will read again the default logger config of 'dynamicTestContainer'. Expecting local level 2 set by the CDB which now takes precedence."
maciContainerLogLevel dynamicTestContainer get default

#
# ___oOo___
