# @(#) $Id: DurationHelper.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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
- EpochHelper and DurationHelper have not been fully tested yet!
'''

__revision__ = "$Id: DurationHelper.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
import sys
import acstimeSWIG
#--CORBA STUBS-----------------------------------------------------------------
import acstime

from ACSErrTypeCommonImpl import TypeNotFoundExImpl
#--GLOBALS---------------------------------------------------------------------
# This machine is using 64 bit integers
PY64BIT = sys.maxint > 2**32
#------------------------------------------------------------------------------
class DurationHelper(acstimeSWIG.DurationHelper):
    '''
    Designed to be a wrapper class for the acstime.Duration struct. This class
    is derived directly from the C++ DurationHelper class via SWIG and all inline
    doc there is applicable here as well. Only variations from the C++ documentation
    is covered here.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, duration=None):
        '''
        Constructor.
        
        Parameters: if duration is specified it is used. Otherwise we assume
        member values are decided by reset method.
        
        Raises: TypeNotFoundExImpl
        '''
        
        #if provided nothing just use the superclass directly
        if duration == None:
            acstimeSWIG.DurationHelper.__init__(self)
        #duration is SWIG generated
        elif isinstance(duration, acstimeSWIG.Duration):
            acstimeSWIG.DurationHelper.__init__(self, duration)
        #duration is CORBA generated
        elif isinstance(duration, acstime.Duration):
            #convert it into the SWIG generated class
            value = duration.value
            duration = acstimeSWIG.Duration()
            duration.value = value
            acstimeSWIG.DurationHelper.__init__(self, duration)
        #let them also use simple types...
        elif isinstance(duration, long) or (PY64BIT and isinstance(duration, int)):
            value = duration
            duration = acstimeSWIG.Duration()
            duration.value = value
            acstimeSWIG.DurationHelper.__init__(self, duration)
        else:
            raise TypeNotFoundExImpl()

    #--------------------------------------------------------------------------
    def value(self, duration=None):
        '''
        Overriden.

        Raises: TypeNotFoundExImpl
        '''
        #if provided nothing just use the superclass directly
        if duration == None:
            return_value = acstimeSWIG.DurationHelper.value(self)
            return acstime.Duration(return_value.value)
        #duration is SWIG generated
        elif isinstance(duration, acstimeSWIG.Duration):
            acstimeSWIG.DurationHelper.value(self, duration)
        #duration is CORBA generated
        elif isinstance(duration, acstime.Duration):
            #convert it into the SWIG generated class
            value = duration.value
            duration = acstimeSWIG.Duration()
            duration.value = value
            acstimeSWIG.DurationHelper.value(self, duration)
        #let them also use simple types...
        elif isinstance(duration, long):
            value = duration
            duration = acstimeSWIG.Duration()
            duration.value = value
            acstimeSWIG.DurationHelper.value(self, duration)
        else:
            raise TypeNotFoundExImpl()
    #--------------------------------------------------------------------------
    def compare(self, duration):
        '''
        Overriden.

        Raises: TypeNotFoundExImpl
        '''   
        #duration is SWIG generated
        if isinstance(duration, acstimeSWIG.Duration):
            return_value = acstimeSWIG.DurationHelper.compare(self, duration)
        #duration is CORBA generated
        elif isinstance(duration, acstime.Duration):
            #convert it into the SWIG generated class
            value = duration.value
            duration = acstimeSWIG.Duration()
            duration.value = value
            return_value = acstimeSWIG.DurationHelper.compare(self, duration)
        #let them also use simple types...
        elif isinstance(duration, long):
            value = duration
            duration = acstimeSWIG.Duration()
            duration.value = value
            return_value = acstimeSWIG.DurationHelper.compare(self, duration)
        else:
            raise TypeNotFoundExImpl()

        if return_value == acstimeSWIG.TCEqualTo:
            return acstime.TCEqualTo
        elif return_value == acstimeSWIG.TCLessThan:
            return acstime.TCLessThan
        elif return_value == acstimeSWIG.TCGreaterThan:
            return acstime.TCGreaterThan
        elif return_value == acstimeSWIG.TCIndeterminate:
            return acstime.TCIndeterminate
        else:
            raise TypeNotFoundExImpl()
    #--------------------------------------------------------------------------
    def add(self, duration):
        '''
        Overriden.

        Raises: TypeNotFoundExImpl
        '''
        #duration is SWIG generated
        if isinstance(duration, acstimeSWIG.Duration):
            acstimeSWIG.DurationHelper.add(self, duration)
        #duration is CORBA generated
        elif isinstance(duration, acstime.Duration):
            #convert it into the SWIG generated class
            value = duration.value
            duration = acstimeSWIG.Duration()
            duration.value = value
            acstimeSWIG.DurationHelper.add(self, duration)
        #let them also use simple types...
        elif isinstance(duration, long):
            value = duration
            duration = acstimeSWIG.Duration()
            duration.value = value
            acstimeSWIG.DurationHelper.add(self, duration)
        else:
            raise TypeNotFoundExImpl()
    #--------------------------------------------------------------------------
    def subtract(self, duration):
        '''
        Overriden.

        Raises: TypeNotFoundExImpl
        '''
        #duration is SWIG generated
        if isinstance(duration, acstimeSWIG.Duration):
            acstimeSWIG.DurationHelper.subtract(self, duration)
        #duration is CORBA generated
        elif isinstance(duration, acstime.Duration):
            #convert it into the SWIG generated class
            value = duration.value
            duration = acstimeSWIG.Duration()
            duration.value = value
            acstimeSWIG.DurationHelper.subtract(self, duration)
        #let them also use simple types...
        elif isinstance(duration, long):
            value = duration
            duration = acstimeSWIG.Duration()
            duration.value = value
            acstimeSWIG.DurationHelper.subtract(self, duration)
        else:
            raise TypeNotFoundExImpl()
    #--------------------------------------------------------------------------
    def modulo(self, duration):
        '''
        Overriden.

        Raises: TypeNotFoundExImpl
        '''
        #duration is SWIG generated
        if isinstance(duration, acstimeSWIG.Duration):
            acstimeSWIG.DurationHelper.modulo(self, duration)
        #duration is CORBA generated
        elif isinstance(duration, acstime.Duration):
            #convert it into the SWIG generated class
            value = duration.value
            duration = acstimeSWIG.Duration()
            duration.value = value
            acstimeSWIG.DurationHelper.modulo(self, duration)
        #let them also use simple types...
        elif isinstance(duration, long):
            value = duration
            duration = acstimeSWIG.Duration()
            duration.value = value
            acstimeSWIG.DurationHelper.modulo(self, duration)
        else:
            raise TypeNotFoundExImpl()

# ---------------------------------------------------------
