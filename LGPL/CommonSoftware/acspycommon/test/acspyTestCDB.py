#!/usr/bin/env python
# @(#) $Id: acspyTestCDB.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
#
# Copyright (C) 2001
# Associated Universities, Inc. Washington DC, USA.
#
# Produced for the ALMA project
#
# This library is free software; you can redistribute it and/or modify it under
# the terms of the GNU Library General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your option) any
# later version.
#
# This library is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
# details.
#
# You should have received a copy of the GNU Library General Public License
# along with this library; if not, write to the Free Software Foundation, Inc.,
# 675 Massachusetts Ave, Cambridge, MA 02139, USA.  Correspondence concerning
# ALMA should be addressed as follows:
#
# Internet email: alma-sw-admin@nrao.edu
#------------------------------------------------------------------------
from Acspy.Common.CDBAccess import CDBaccess

print "------------------------------------------------------------------"
print "Creating CDBaccess..."
g = CDBaccess()
print "------------------------------------------------------------------"
print "Invoking getField on CDB/alma/MOUNT1"
h = g.getField("alma/MOUNT1")
if h == None:
    print "There was some sort of problem getting the XML record"
print "------------------------------------------------------------------"
print "Invoking getElement on actAz property of CDB/alma/MOUNT1"
h = g.getElement("alma/MOUNT1", "MOUNT/actAz")
print "Alarm high off is: ", h[0]['alarm_high_off']
print "------------------------------------------------------------------"
print "Done!"
