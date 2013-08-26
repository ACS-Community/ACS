#!/usr/bin/env python
# @(#) $Id: acspyTestEpochHelper.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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
from Acspy.Common.TimeHelper     import getTimeStamp
from Acspy.Common.EpochHelper    import EpochHelper
from Acspy.Common.DurationHelper import DurationHelper
#------------------------------------------------------------------------------
print "DWF...need a real test here!!!"

# format string using all possible
allOut = "%G %g %x\n%Y %y %m %h %j %d %e %w %a %H:%M:%S.%q %1q %2q %3q %4q %5q %6q"

e1 = EpochHelper()	
e2 = EpochHelper()
	
d1 = DurationHelper()
	
# create an Epoch structure
eout = getTimeStamp()
	
e1.value(eout)
	
pStr = e1.toString(acstime.TSArray,
                   "%x",
                   0L, 
                   0L)
	
#print "Current time is " , pStr 

# test Epoch range & toUTUdate(), toJulianYear()
eout.value = 0xFFFFFFFFFFFFFFFA
e1.value(eout)
	
pStr = e1.toString(acstime.TSArray,"", 0L, 0L)
	
print pStr 
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
utc = e1.toUTCdate(0L, 0L)

julian = e1.toJulianYear(0L, 0L)

#mjdSeconds = e1.toMJDseconds()
	
print utc, julian  #, mjdSeconds 

eout.value = 0L
e1.value(eout)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
utc = e1.toUTCdate(0L, 0L)
	
julian = e1.toJulianYear(0L, 0L)
	
#mjdSeconds = e1.toMJDseconds()
print utc, julian  #, mjdSeconds 
	
e1.fromString(acstime.TSArray,
              "60038-3-11T5:36:10.955161")
	
eout = e1.value()
	
print eout.value
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 

e1.fromString(acstime.TSArray,
              "1582-10-15T00:00:00.000000")
	
eout = e1.value()
	
print eout.value
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
	
e1.fromString(acstime.TSArray,
              "1995-4-28T17:23:15.654321")
	
eout = e1.value()
	
print eout.value
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
utc = e1.toUTCdate(0L, 0L)

julian = e1.toJulianYear(0L, 0L)

#mjdSeconds = e1.toMJDseconds()

print utc, julian #, mjdSeconds 

# test Epoch usec implicit trailing zeroes
e1.fromString(acstime.TSArray,
              "1776-7-4T17:23:15.5")
	
pStr = e1.toString(acstime.TSArray,"%x",0L, 0L)
	
print pStr 
e1.fromString(acstime.TSArray,
              "2345-6-7T08:09:00.103")
	
pStr = e1.toString(acstime.TSArray,"%x",0L, 0L)

print pStr 
e1.fromString(acstime.TSArray,
              "2001-9-11T06:54:32.0506")
	
pStr = e1.toString(acstime.TSArray,"%x",0L, 0L)
	
print pStr 

# test Epoch.add()
e1.fromString(acstime.TSArray,
              "1943-04-05 05:36:10.955161")
	
eout = e1.value()
	
e2.value(eout)

d1.fromString("+1 02:03:04.567890")

e1.add(d1.value())
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 

# test Epoch.difference()
d2_val = e1.difference(e2.value())
	
d2 = DurationHelper(d2_val)
pStr = d2.toString("")
	
print pStr 
	
# test Epoch.subtract()
e1.subtract(d1.value())

pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr 

# test Epoch.compare() using Epoch.now()
eout = getTimeStamp()
	
e1.value(eout)

import time
time.sleep(8e-6)

eout = getTimeStamp()
	
e2.value(eout)
	
tcom = e1.compare(e2.value())
print tcom
tcom = e2.compare(e1.value())

print tcom
tcom = e1.compare(e1.value())

print tcom

# test Epoch setting by parts
e1.reset()

e1.year(1995)

e1.month(4)

e1.day(28)

e1.hour(17)

e1.minute(23)

e1.second(15)

e1.microSecond(654321)

eout = e1.value()

print eout.value
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 

e1.reset()

e1.year(1995)

e1.dayOfYear(118)

eout = e1.value()

print eout.value
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 

# test Epoch getting by parts
print e1.year(), e1.month(), e1.day(), e1.dayOfYear(), e1.dayOfWeek(), e1.hour(), e1.minute(), e1.second(), e1.microSecond()

# test Epoch.normalize() switch
e1.normalize(1)

e1.fromString(acstime.TSArray,
              "1900-13-32T25:67:71.955161")

pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr 

# test get/set microSecond value w/ normalize true
lngtmp = e1.microSecond()
	
e1.microSecond(lngtmp - 1111111)

pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr 
e1.microSecond(lngtmp + 1111111)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 

# test get/set second value w/ normalize true
lngtmp = e1.second()

e1.second(lngtmp - 61)

pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr 
e1.second(lngtmp + 61)


pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 

# test get/set minute value w/ normalize true
lngtmp = e1.minute()

e1.minute(lngtmp - 61)

pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
e1.minute(lngtmp + 61)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr 

# test get/set hour value w/ normalize true
lngtmp = e1.hour()
	
e1.hour(lngtmp - 25)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
e1.hour(lngtmp + 25)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 

# test get/set day value w/ normalize true (non-leap year)
e1.fromString(acstime.TSArray,
              "1901-02-26T21:18:37.955161")
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr

lngtmp = e1.day()
	
e1.day(lngtmp - 12)

pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr 
e1.day(lngtmp + 12)

pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr 

e1.fromString(acstime.TSArray, "1901-03-02T12:16:43.955161")

pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr 
lngtmp = e1.day()

e1.day(lngtmp - 12)

pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr 
e1.day(lngtmp + 12)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr 

# test get/set day value w/ normalize true (leap year)
e1.fromString(acstime.TSArray,"1904-02-26T08:53:12.955161")
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr 
lngtmp = e1.day()

e1.day(lngtmp - 12)

pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr 
e1.day(lngtmp + 12)

pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr 

e1.fromString(acstime.TSArray, "1904-03-02T18:37:21.955161")
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
lngtmp = e1.day()
	
e1.day(lngtmp - 12)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
e1.day(lngtmp + 12)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 

# test get/set day-of-year value w/ normalize true (non-leap year)
e1.fromString(acstime.TSArray, "1901-02-26T21:18:37.955161")
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
lngtmp = e1.dayOfYear()
	
e1.dayOfYear(lngtmp - 58)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
e1.dayOfYear(lngtmp + 12)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 

e1.fromString(acstime.TSArray,"1901-03-02T12:16:43.955161")
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
lngtmp = e1.dayOfYear()
	
e1.dayOfYear(lngtmp - 12)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
e1.dayOfYear(lngtmp + 12)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 

# test get/set day-of-year value w/ normalize true (leap year)
e1.fromString(acstime.TSArray,"1904-02-26T08:53:12.955161")
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
lngtmp = e1.dayOfYear()
	
e1.dayOfYear(lngtmp - 12)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
e1.dayOfYear(lngtmp + 12)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 

e1.fromString(acstime.TSArray,"1904-03-02T18:37:21.955161")
                          
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)

print pStr 
lngtmp = e1.dayOfYear()
	
e1.dayOfYear(lngtmp - 12)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
e1.dayOfYear(lngtmp + 12)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 

# test get/set month value w/ normalize true
e1.fromString(acstime.TSArray, "1904-02-14T18:37:21.955161")
	
lngtmp = e1.month()
	
e1.month(lngtmp - 12)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
e1.month(lngtmp + 12)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
        
# test get/set year value w/ normalize true
lngtmp = e1.year()
	
e1.year(lngtmp - 12)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 
e1.year(lngtmp + 12)
	
pStr = e1.toString(acstime.TSArray,allOut,0L, 0L)
	
print pStr 



#------------------------------------------------------------------------------
