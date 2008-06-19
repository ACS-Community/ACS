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
# "@(#) $Id: pyClient.py,v 1.1 2008/06/19 19:15:45 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# arne  2008-06-16  created
#

import sys
import signal
import Acspy.Clients.SimpleClient as SC

def handler(signum, frame):
    print 'Signal handler called with signal', signum

done = False
signal.signal(signal.SIGINT, handler)
signal.signal(signal.SIGTERM, handler)
cl = SC.PySimpleClient.getInstance()
comps = {}
print "Ready"
while not done:
    l = sys.stdin.readline().split()
    if l[0] == 'Done':
        done = True
    elif l[0] == 'Load':
        good = ""
        bad = ""
        for c in l[1:]:
            try:
                comps[c] = cl.getComponent(c)
                good += c
                good += ' '
            except Exception, e:
                bad += c
                bad += ' '
        print "Load Complete: %s  Failed with Exception: %s" % (good, bad)
    elif l[0] == 'Call':
        good = ""
        bad = ""
        for c in comps:
            try:
                comps[c].displayMessage()
                good += c
                good += ' '
            except Exception, e:
                bad += c
                bad += ' '
        print "Call Success: %s  Failed with Exception: %s" % (good,bad)

print "Bye"

#
# ___oOo___
