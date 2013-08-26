#!/usr/bin/env python
# @(#) $Id: acspyTestDurationHelper.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
#
# Copyright (C) 2001
# Associated Universities, Inc. Washington DC, USA.
#
# Produced for the ALMA project
#
# This library is free software; you can redistribute it and/or modify it
# under
# the terms of the GNU Library General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your option)
# any
# later version.
#
# This library is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for
# more
# details.
#
# You should have received a copy of the GNU Library General Public License
# along with this library; if not, write to the Free Software Foundation,
# Inc.,
# 675 Massachusetts Ave, Cambridge, MA 02139, USA.  Correspondence concerning
# ALMA should be addressed as follows:
#
# Internet email: alma-sw-admin@nrao.edu
'''
'''
#--REGULAR IMPORTS-------------------------------------------------------------

import acstime
from Acspy.Common.DurationHelper import DurationHelper
#------------------------------------------------------------------------------
print "----------------------"
d1 = DurationHelper(0x7FFFFFFFFFFFFFFA)
print d1.toString("")
print "----------------------"
d1.value(long(0))
print d1.toString("")
print "----------------------"
#For some reason or another, this test taken from the C++ modular test of acstime
#causes overflow errors in Python! Commented out for now
#d1.value(0x8000000000000008 + 8L)
#print d1.toString("")
print "----------------------"
d1.fromString("10675199 02:48:05.47758")
print d1.value().value
print "----------------------"
d1.fromString("0:0:0")
print d1.value().value
print "----------------------"
d1.fromString("-10675199 2:48:5.47758")
print d1.value().value
print "----------------------"
d1.fromString("98 23:32:10.05")
print d1.toString("")
print "----------------------"
d1.fromString("10:23:54.12305")
print d1.toString("")
print "----------------------"
d1.positive(1)
d1.day(28)
d1.hour(17)
d1.minute(23)
d1.second(15)
d1.microSecond(654321L)
print d1.value().value
print d1.toString("")

print d1.positive(), ' ', d1.day(), ' ', d1.hour(), ' ', d1.minute(), ' ', d1.second(), ' ', d1.microSecond()
print "----------------------"
d2 = DurationHelper()
d2.fromString("-1 02:03:04.56789")
print d1.compare(d2.value())
print d2.compare(d1.value())
print "----------------------"
#test Duration.add() and Duration.subtract()
#d1 == "+1 02:03:04.567890" from above
d2.fromString("0:0:0")
d1.add(d2.value())
pStr = d1.toString("")
print pStr
tcom = d1.compare(d2.value())
print tcom
d1.subtract(d2.value())
pStr = d1.toString("")
print pStr
tcom = d1.compare(d2.value())
print tcom
print "----------------------"
d2.fromString("1:2:3")
d1.add(d2.value())
pStr = d1.toString("")
print pStr 
tcom = d1.compare(d2.value())
print tcom
d1.subtract(d2.value())
pStr = d1.toString("")
print pStr 
tcom = d1.compare(d2.value())
print tcom
print "----------------------"
d2.fromString("-4 3:2:1.078")
d1.add(d2.value())
pStr = d1.toString("")
print pStr 
tcom = d1.compare(d2.value())
print tcom
d1.subtract(d2.value())
pStr = d1.toString("")
print pStr 
tcom = d1.compare(d2.value())
print tcom
print "----------------------"
# test Duration.multiply() and Duration.divide()
# d1 == "+1 02:03:04.567890" from above
d1.divide(3L)
pStr = d1.toString("")
print pStr 
d1.multiply(3L)
pStr = d1.toString("")
print pStr 
print "----------------------"
# test Duration.normalize() switch
d1.normalize(1)
d1.fromString("-4 47:61:71.078")
pStr = d1.toString("")
print pStr 
print "----------------------"
#------------------------------------------------------------------------------
