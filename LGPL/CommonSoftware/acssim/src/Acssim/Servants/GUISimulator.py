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
# "@(#) $Id: GUISimulator.py,v 1.1 2008/10/28 09:00:49 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstup  2008-10-28  created
#

from Acssim.Servants.Simulator import Simulator
import Acssim.Servants.Widgets

class GUISimulator(Simulator):

    def __init__(self):
        Simulator.__init__(self)

#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Creating an object"
    g = GUISimulator()
    print "Done..."

#
# ___oOo___
