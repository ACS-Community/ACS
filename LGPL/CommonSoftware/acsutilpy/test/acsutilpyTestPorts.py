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
# @(#) $Id: acsutilpyTestPorts.py,v 1.3 2007/10/12 16:04:22 hsommer Exp $
###############################################################################
'''
Tests ACS Ports.
'''
from AcsutilPy.ACSPorts import getBasePort
from AcsutilPy.ACSPorts import getManagerPort
from AcsutilPy.ACSPorts import getNamingServicePort
from AcsutilPy.ACSPorts import getNotifyServicePort
from AcsutilPy.ACSPorts import getLoggingServicePort
from AcsutilPy.ACSPorts import getIRPort
from AcsutilPy.ACSPorts import getLogPort
from AcsutilPy.ACSPorts import getCDBPort
from AcsutilPy.ACSPorts import getContainerDaemonPort
from AcsutilPy.ACSPorts import getServicesDaemonPort
from AcsutilPy.ACSPorts import getIP
###############################################################################
# Test code

if __name__ == "__main__":
    #basically just make sure none of these throw exceptions because multiple values
    #are actually correct here!
    print type(getBasePort())
    print type(getManagerPort())
    print type(getNamingServicePort())
    print type(getNotifyServicePort())
    print type(getLoggingServicePort())
    print type(getIRPort())
    print type(getLogPort())
    print type(getCDBPort())
    print type(getContainerDaemonPort())
    print type(getServicesDaemonPort())
    print type(getIP())
    
    
