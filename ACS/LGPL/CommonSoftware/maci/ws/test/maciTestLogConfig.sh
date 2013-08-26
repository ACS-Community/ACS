#! /bin/sh
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2007
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
# "@(#) $Id: maciTestLogConfig.sh,v 1.7 2010/09/21 18:12:03 tstaig Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
#
#echo "Test w/o ACS_LOG_STDOUT & ACS_LOG_REMOTE"
#unset ACS_LOG_STDOUT
#unset ACS_LOG_REMOTE
#maciTestLogConfigClient

#echo "Test with ACS_LOG_STDOUT=5 & ACS_LOG_REMOTE=5"
#export ACS_LOG_STDOUT=5
#export ACS_LOG_CENTRAL=5
#maciTestLogConfigClient

echo "Test ACS_LOG_STDOUT=1, ACS_LOG_CENTRAL unset and minLogLevel=9"
maciTestLogConfigClient

echo "Test with dynamic change of log"
maciContainerLogLevel ContainerTestLog set MACI_LOG_CONFIG 1 5 
maciTestLogConfigClient

echo "Test with dynamic change of log (2)"
maciContainerLogLevel ContainerTestLog set MACI_LOG_CONFIG 6 4 
maciTestLogConfigClient

sleep 5
echo "***************Remote***************"
grep "LogConfigTestClass\:\:test" tmp/ACS_INSTANCE.$ACS_INSTANCE/all_logs.xml 
echo "*************************************"

#
# ___oOo___

