# @(#) $Id: EpochHelper.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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

__revision__ = "$Id: EpochHelper.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
import acstimeSWIG
#--CORBA STUBS-----------------------------------------------------------------
import acstime

from ACSErrTypeCommonImpl import TypeNotFoundExImpl
#--GLOBALS---------------------------------------------------------------------
    
#------------------------------------------------------------------------------
class EpochHelper(acstimeSWIG.EpochHelper):
    '''
    Designed to be a wrapper class for the acstime.Epoch struct. This class
    is derived directly from the C++ EpochHelper class via SWIG and all inline
    doc there is applicable here as well. Only variations from the C++ documentation
    is covered here.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, epoch=None):
        '''
        Constructor.
        
        Parameters: if epoch is specified it is used.  Otherwise we assume
        member values are decided by reset method.
        
        Raises: TypeNotFoundExImpl
        '''
        
        #if provided nothing just use the superclass directly
        if epoch == None:
            acstimeSWIG.EpochHelper.__init__(self)
        #epoch is SWIG generated
        elif isinstance(epoch, acstimeSWIG.Epoch):
            acstimeSWIG.EpochHelper.__init__(self, epoch)
        #epoch is CORBA generated
        elif isinstance(epoch, acstime.Epoch):
            #convert it into the SWIG generated class
            value = epoch.value
            epoch = acstimeSWIG.Epoch()
            epoch.value = value
            acstimeSWIG.EpochHelper.__init__(self, epoch)
        #let them also use simple types...
        elif isinstance(epoch, long):
            value = epoch
            epoch = acstimeSWIG.Epoch()
            epoch.value = value
            acstimeSWIG.EpochHelper.__init__(self, epoch)
        else:
            raise TypeNotFoundExImpl()

    #--------------------------------------------------------------------------
    def value(self, epoch=None):
        '''
        Overriden.

        Raises: TypeNotFoundExImpl
        '''
        #if provided nothing just use the superclass directly
        if epoch == None:
            return_value = acstimeSWIG.EpochHelper.value(self)
            return acstime.Epoch(return_value.value)
        #epoch is SWIG generated
        elif isinstance(epoch, acstimeSWIG.Epoch):
            acstimeSWIG.EpochHelper.value(self, epoch)
        #epoch is CORBA generated
        elif isinstance(epoch, acstime.Epoch):
            #convert it into the SWIG generated class
            value = epoch.value
            epoch = acstimeSWIG.Epoch()
            epoch.value = value
            acstimeSWIG.EpochHelper.value(self, epoch)
        #let them also use simple types...
        elif isinstance(epoch, long):
            value = epoch
            epoch = acstimeSWIG.Epoch()
            epoch.value = value
            acstimeSWIG.EpochHelper.value(self, epoch)
        else:
            raise TypeNotFoundExImpl()
    #--------------------------------------------------------------------------
    def compare(self, epoch):
        '''
        Overriden.

        Raises: TypeNotFoundExImpl
        '''   
        #epoch is SWIG generated
        if isinstance(epoch, acstimeSWIG.Epoch):
            return_value = acstimeSWIG.EpochHelper.compare(self, epoch)
        #epoch is CORBA generated
        elif isinstance(epoch, acstime.Epoch):
            #convert it into the SWIG generated class
            value = epoch.value
            epoch = acstimeSWIG.Epoch()
            epoch.value = value
            return_value = acstimeSWIG.EpochHelper.compare(self, epoch)
        #let them also use simple types...
        elif isinstance(epoch, long):
            value = epoch
            epoch = acstimeSWIG.Epoch()
            epoch.value = value
            return_value = acstimeSWIG.EpochHelper.compare(self, epoch)
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
    def toString(self, timesys, format, array2tai, tai2utc):
        '''
        Overriden.
        '''
        if timesys == acstime.TSArray:
            timesys=acstimeSWIG.TSArray
        elif timesys == acstime.TSTAI:
            timesys=acstimeSWIG.TSTAI
        elif timesys == acstime.TSUTC:
            timesys=acstimeSWIG.TSUTC

        return acstimeSWIG.EpochHelper.toString(self,
                                                timesys,
                                                format,
                                                array2tai,
                                                tai2utc)
    #--------------------------------------------------------------------------
    def fromString(self, timesys, stringtime):
        '''
        Overriden.
        '''
        if timesys == acstime.TSArray:
            timesys=acstimeSWIG.TSArray
        elif timesys == acstime.TSTAI:
            timesys=acstimeSWIG.TSTAI
        elif timesys == acstime.TSUTC:
            timesys=acstimeSWIG.TSUTC

        acstimeSWIG.EpochHelper.fromString(self, timesys, stringtime)
    #--------------------------------------------------------------------------
    def add(self, duration):
        '''
        Overriden.

        Raises: TypeNotFoundExImpl
        '''
        #duration is SWIG generated
        if isinstance(duration, acstimeSWIG.Duration):
            acstimeSWIG.EpochHelper.add(self, duration)
        #duration is CORBA generated
        elif isinstance(duration, acstime.Duration):
            #convert it into the SWIG generated class
            value = duration.value
            duration = acstimeSWIG.Duration()
            duration.value = value
            acstimeSWIG.EpochHelper.add(self, duration)
        #let them also use simple types...
        elif isinstance(duration, long):
            value = duration
            duration = acstimeSWIG.Duration()
            duration.value = value
            acstimeSWIG.EpochHelper.add(self, duration)
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
            acstimeSWIG.EpochHelper.subtract(self, duration)
        #duration is CORBA generated
        elif isinstance(duration, acstime.Duration):
            #convert it into the SWIG generated class
            value = duration.value
            duration = acstimeSWIG.Duration()
            duration.value = value
            acstimeSWIG.EpochHelper.subtract(self, duration)
        #let them also use simple types...
        elif isinstance(duration, long):
            value = duration
            duration = acstimeSWIG.Duration()
            duration.value = value
            acstimeSWIG.EpochHelper.subtract(self, duration)
        else:
            raise TypeNotFoundExImpl()
    #--------------------------------------------------------------------------
    def difference(self, epoch):
        '''
        Overriden.

        Raises: TypeNotFoundExImpl
        '''
        #epoch is SWIG generated
        if isinstance(epoch, acstimeSWIG.Epoch):
            return_value = acstimeSWIG.EpochHelper.difference(self, epoch)
            #convert it into a CORBA type
            return acstime.Duration(return_value.value)
        #epoch is CORBA generated
        elif isinstance(epoch, acstime.Epoch):
            #convert it into the SWIG generated class
            value = epoch.value
            epoch = acstimeSWIG.Epoch()
            epoch.value = value
            return_value = acstimeSWIG.EpochHelper.difference(self, epoch)
            #convert it into a CORBA type
            return acstime.Duration(return_value.value)
        #let them also use simple types...
        elif isinstance(epoch, long):
            value = epoch
            epoch = acstimeSWIG.Epoch()
            epoch.value = value
            return_value = acstimeSWIG.EpochHelper.difference(self, epoch)
            #convert it into a CORBA type
            return acstime.Duration(return_value.value)
        else:
            raise TypeNotFoundExImpl()
    #--------------------------------------------------------------------------
    def modulo(self, epoch):
        '''
        Overriden.

        Raises: TypeNotFoundExImpl
        '''
        #epoch is SWIG generated
        if isinstance(epoch, acstimeSWIG.Epoch):
            acstimeSWIG.EpochHelper.modulo(self, epoch)
        #epoch is CORBA generated
        elif isinstance(epoch, acstime.Epoch):
            #convert it into the SWIG generated class
            value = epoch.value
            epoch = acstimeSWIG.Epoch()
            epoch.value = value
            acstimeSWIG.EpochHelper.modulo(self, epoch)
        #let them also use simple types...
        elif isinstance(epoch, long):
            value = epoch
            epoch = acstimeSWIG.Epoch()
            epoch.value = value
            acstimeSWIG.EpochHelper.modulo(self, epoch)
        else:
            raise TypeNotFoundExImpl()

# ---------------------------------------------------------

