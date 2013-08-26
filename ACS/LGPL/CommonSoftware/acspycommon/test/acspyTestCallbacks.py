#!/usr/bin/env python
# @(#) $Id: acspyTestCallbacks.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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
#------------------------------------------------------------------------------
from Acspy.Common import Callbacks

def testCallback(cb):
    '''
    This simple function is designed to test the functionality
    of callback helper classes (without using any CORBA).
    '''
    print
    cb.working(1, None, None)
    cb.working(2, None, None)
    cb.working(3, None, None)
    cb.done(4, None, None)
    cb.negotiate(None, None)
    print cb.values

if __name__ == "__main__":
    print "Testing CBvoid"
    g = Callbacks.CBvoid()
    g.working(None, None)
    g.done(None, None)
    g.negotiate(None, None)
    print

    print "Testing all callbacks with archiving"
    testCallback(Callbacks.CBbool(archive=1))
    testCallback(Callbacks.CBonOffSwitch(archive=1))
    testCallback(Callbacks.CBlong(archive=1))
    testCallback(Callbacks.CBlongSeq(archive=1))
    testCallback(Callbacks.CBlongLong(archive=1))
    testCallback(Callbacks.CBuLongLong(archive=1))
    testCallback(Callbacks.CBdouble(archive=1))
    testCallback(Callbacks.CBdoubleSeq(archive=1))
    testCallback(Callbacks.CBfloat(archive=1))
    testCallback(Callbacks.CBfloatSeq(archive=1))
    testCallback(Callbacks.CBstring(archive=1))
    testCallback(Callbacks.CBstringSeq(archive=1))
    testCallback(Callbacks.CBpattern(archive=1))
    print

    print "Testing all callbacks with NO archiving"
    testCallback(Callbacks.CBbool())
    testCallback(Callbacks.CBonOffSwitch())
    testCallback(Callbacks.CBlong())
    testCallback(Callbacks.CBlongSeq())
    testCallback(Callbacks.CBlongLong())
    testCallback(Callbacks.CBuLongLong())
    testCallback(Callbacks.CBdouble())
    testCallback(Callbacks.CBdoubleSeq())
    testCallback(Callbacks.CBfloat())
    testCallback(Callbacks.CBfloatSeq())
    testCallback(Callbacks.CBstring())
    testCallback(Callbacks.CBstringSeq())
    testCallback(Callbacks.CBpattern())
    print
    print "Done..."
