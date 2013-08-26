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
# @(#) $Id: acspyTestContainerServices.py,v 1.7 2006/04/06 21:44:58 dfugate Exp $
###############################################################################
'''
Tests the Container Services.
'''
from Acspy.Servants.ContainerServices import ContainerServices
###############################################################################
def dummie():
    pass

if __name__=="__main__":
    g = ContainerServices()
    print g.__dict__
    print
    
    g.setAll("blar",  #string-name of component
             None,  #handle...
             None,  #handle
             dummie  #Container's method
             )
    print g.__dict__
    print
    print "Thread: ", g.getThread(dummie, "dummie thread")
    print "Done..."
