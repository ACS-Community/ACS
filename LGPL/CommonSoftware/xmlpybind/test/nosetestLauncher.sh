#! /bin/bash
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# Copyright (c) European Southern Observatory, 2013 
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
#
# who       when      what
# --------  --------  ----------------------------------------------
# acaproni  2013-06-25  created
#

# This scripts avoids a dependency with ACS version in TestList.lite
# where  nosetests was called with full path 
# like "/alma/ACS-12.0/Python/bin/nosetests"
#
# This script instead gets the path of nosetests from $PYTHON_ROOT.
# In other modules like acspy this is not needed because there is a ACS session 
# already running and nosetests starts with a command like 
# "acsutilTATTestRunner nosetests"

$PYTHON_ROOT/bin/nosetests

#
# ___oOo___
