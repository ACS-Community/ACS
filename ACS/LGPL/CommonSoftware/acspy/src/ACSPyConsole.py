#!/usr/bin/env python

# @(#) $Id: ACSPyConsole.py,v 1.4 2005/06/13 18:04:24 dfugate Exp $
#
#    ALMA - Atacama Large Millimiter Array
#    (c) Associated Universities, Inc. Washington DC, USA, 2001
#    (c) European Southern Observatory, 2002
#    Copyright by ESO (in the framework of the ALMA collaboration)
#    and Cosylab 2002, All rights reserved
#
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

'''
acspyConsole.py:
Starts an ACS interactive python console.

TODO:
- reevaluate the use of REVISION/VERSION. That is, are they used and if not
how are they better than __version__
- Ctrl-D does not seem to exit out of the session.
'''

__version__ = "$Id: ACSPyConsole.py,v 1.4 2005/06/13 18:04:24 dfugate Exp $"

from Tkinter import *
import sys, string, traceback, types, __builtin__

REVISION = "$Revision: 1.4 $"
VERSION = string.split(REVISION)[1]

from Acspy.Util.Console import Console

# Main program.
if __name__ == "__main__":
    c = Console(local_dict={})
    c.dict["console"] = c
    c.pack(fill=BOTH, expand=1)
    c.master.title("ACS Python Console v%s" % VERSION)
    mainloop()
