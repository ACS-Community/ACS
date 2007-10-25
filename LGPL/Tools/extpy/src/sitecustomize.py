#!/usr/bin/env python

# APEX - Atacama Pathfinder EXperiment Project
#
# Copyright (C) 2003-2005
# Max-Planck-Institut fuer Radioastronomie, Bonn, Germany
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
# 675 Massachusetts Ave, Cambridge, MA 02139, USA. Correspondence concerning
# APEX should be addressed as follows:
#
# Internet email: dmuders@mpifr-bonn.mpg.de
#
# "@(#) $Id: sitecustomize.py,v 1.1 2007/10/25 19:30:59 agrimstrup Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# agrimstrup2007-10-25  Added file to ACS source tree and modified
#                       to make searching import behaviour an ACS
#                       default
# dmuders   2005-08-19  Ported to Python 2.4 for heuristics
# dmuders   2005-08-16  Walk through site directories in reverse order and
#			add new packages at the beginning of the search path to
#			make sure that the APEX packages override the others.
# dmuders   2003-08-08  Exchanged SOFTWARE_ROOTS / INTROOT setup in favor of
#			simply going through sys.path
# dmuders   2003-08-07  Created


'''Add locally installed packages that are described by pth files.'''

import AcsutilPy.ACSImport
import sys, os
from site import addsitedir
from copy import deepcopy

try:
    sitedirs = deepcopy(sys.path)
    # Reverse order to walk through the paths from the back
    sitedirs.reverse()

    for sitedir in sitedirs:
	oldpath = deepcopy(sys.path)
        addsitedir(sitedir)
	newpath = deepcopy(sys.path)
	# Add new packages at the beginning of the search path
	sys.path = newpath[len(oldpath):] + oldpath

except Exception,e:
    print e
