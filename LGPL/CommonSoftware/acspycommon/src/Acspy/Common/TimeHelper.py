# @(#) $Id: TimeHelper.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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
This module contains the implementation of Python helper classes for the ACS
time system.  Specifically, these classes mimic what has been done by the
Control subsystem in C++.

TODO:
- getTimeStamp should get the Time from a Clock component!
'''

__revision__ = "$Id: TimeHelper.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------
import acstime
import time

#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
def getTimeStamp():
    '''
    Returns the current timestamp in acstime.Epoch format. To convert this value
    to a long, just use getTimeStamp().value

    Parameters: None

    Returns: the current time in an ACS Epoch structure

    Raises: Nothing
    '''
    helper = TimeUtil()
    return helper.py2epoch(time.time())
#------------------------------------------------------------------------------
class TimeUtil(object):
    '''
    TimeUtil is a utility class providing static methods to convert
    between Python and ACS time systems. It is very similar to the C++ class
    of the same name except that it deals with native Python time format instead
    of "ACE" time.
    '''
    #--------------------------------------------------------------------------
    def __init__(self):
        '''
        Constructor does not really initialize anything as this class has no
        members and consists solely of static methods.
        '''
        return
    #--------------------------------------------------------------------------
    def py2epoch(self, seconds):
        '''
        Convert Python Epoch (i.e., seconds since 1/1/1970 to ACS
        Epoch (i.e., 100 nanoseconds since 15/10/1582).

        Parameters:
        - seconds is the number of seconds that have passed
        since January 1, 1970.

        Return: seconds converted to units of 100 nanoseconds since
        October 15, 1582 and packed into an acstime.Epoch struct.

        Raises: Nothing
        '''
        return acstime.Epoch(acstime.ACE_BEGIN + long(seconds*10000000L))
    #--------------------------------------------------------------------------
    def epoch2py(self, epoch):
        '''
        Convert an ACS Epoch to a Python Epoch.

        Parameters: epoch is the number of 100 nanoseconds that have passed since
        October 15, 1582.

        Return: epoch converted a Python Epoch (i.e., seconds that have passed
        since January 1, 1970.)

        Raises: Nothing
        '''
        #let them specify a regular long instead of the nasty acstime.Epoch
        #struct
        if isinstance(epoch, long):
            epoch = acstime.Epoch(epoch)
        
        acs_time = epoch.value - acstime.ACE_BEGIN  #100ns units
        sec = acs_time / 10000000L
        return sec
    #--------------------------------------------------------------------------
    def py2duration(self, seconds):
        '''
        Convert seconds to 100 nanoseconds.

        Parameters: seconds is the number of seconds to convert

        Return: seconds converted to units of 100 nanoseconds packed into a
        Duration struct.
        
        Raises: Nothing
        '''
        return acstime.Duration(seconds * 10000000L)
    #--------------------------------------------------------------------------
    def duration2py(self, duration):
        '''
        Convert an ACS duration to a Python duration.
        
        Parameters: duration is in units of 100 nanoseconds

        Return: duration converted to seconds

        Raises: Nothing
        '''
        if isinstance(duration, long):
            duration = acstime.Duration(duration)
            
        sec = duration.value / 10000000L
        return sec

