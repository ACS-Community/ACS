#!/usr/bin/env python

# APEX - Atacama Pathfinder EXperiment Project
#
# Copyright (C) 2003
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
# "@(#) $Id: Callbacks.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2003/11/20  Fixed bugs in all CB classes involving values.
# dmuders   2003/06/05  Replace "working" and "done" status variables with
#			"status" to avoid overwriting the "working" and "done"
#			methods.
# dmuders   2003/05/22  Created. Derived from Dave Fugate's example
#			acspyTestClientCallback.py script.

'''
This module includes the implementations of Python void and valued ACS callback
classes for use in Python applications.  It was contributed by developers from
the Atacama Pathfinder Experiment Project

Todo:
- 
'''

__revision__ = "$Id: Callbacks.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#------------------------------------------------------------------------------
import ACS__POA                   # Import the Python CORBA stubs for BACI
from   omniORB.CORBA import TRUE
#------------------------------------------------------------------------------

class CBvoid(ACS__POA.CBvoid):
    #--------------------------------------------------------------------------
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    device for any monitors we may create for void callbacks
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name = None): 
        '''
        Constructor.
        
        Parameters: name of this callback instance
        
        Raises: Nothing
        '''
        if name != None:
            self.name = name
        else:
            self.name = "NoName"
        # Flag for the application to check if the action is still going on and
        # if the callback has arrived.
        self.status = 'INIT'

        self.completion = None
    #--------------------------------------------------------------------------
    def working (self, completion, desc):
        '''
        Really this is the method that does all the work and is what the
        developer should be concerned with.
        
        Parameters:
        - completion is a CORBA completion structure
        - desc is callback struct description
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        #to make pychecker happy
        completion = None

        #to make pychecker happy
        desc = None
        
        self.status = 'WORKING'
    #--------------------------------------------------------------------------
    def done (self, completion, desc):
        '''
        Invoked asynchronously when the component has finished. Normally this
        is invoked just before a monitor is destroyed or when an asynchronous
        method has finished.
        
        Parameters:
        - completion is a CORBA completion structure
        - desc is callback struct description
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        #to make pychecker happy
        desc = None
        
        # Save completion to be able to fetch the error code.
        self.completion = completion
        self.status = 'DONE'
    #--------------------------------------------------------------------------
    def negotiate (self, time_to_transmit, desc):
        '''
        For simplicitys sake, we always return true. If you want more detailed
        information on this method, please see the BACI specs.
        
        Parameters: See the BACI specs.
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        #to make pychecker happy
        time_to_transmit = None
        desc = None
        
        return TRUE
#------------------------------------------------------------------------------
class BaseValueCB(object):
    '''
    This is the baseclass for all CB classes which use actual values (i.e.,
    not void).
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name, archive):
        '''
        Constructor.
        
        Parameters:
        - name of this callback instance
        - archive set to false implies values retrieved from the working method
        are not saved
        
        Raises: Nothing
        '''
        if name != None:
            self.name = name
        else:
            self.name = "No name specified"
        # Flag for the application to check if the action is still going on and
        # if the callback has arrived.
        self.status = 'INIT'

        #says whether values should be saved or not.
        self.archive = archive

        #saved values
        self.values = []
    #--------------------------------------------------------------------------
    def working (self, value, completion, desc):
        '''
        Really this is the method that does all the work and is what the
        developer should be concerned with.

        Parameters:
        - value is the value we are interested in
        - completion is a CORBA completion structure
        - desc is callback struct description

        Returns: Nothing

        Raises: Nothing
        '''
        #to make pychecker happy
        completion = None

        #to make pychecker happy
        desc = None
        
        # Save the value for later use
        if self.archive:
            self.values.append(value)
        else:
            self.values = value

        # Set flag.
        self.status = 'WORKING'
    #--------------------------------------------------------------------------
    def done (self, value, completion, desc):
        '''
        Invoked asynchronously when the DO has finished. Normally this is
        invoked just before a monitor is destroyed or when an asynchronous
        method has finished.

        Parameters:
        - value is the value we are interested in
        - completion is a CORBA completion structure
        - desc is callback struct description

        Returns: Nothing

        Raises: Nothing
        '''
        #to make pychecker happy
        desc = None
        
        # Save the value for later use
        if self.archive:
            self.values.append(value)
        else:
            self.values = value
        
        # Save completion to be able to fetch the error code.
        self.completion = completion
        
        # Set flags.
        self.status = 'DONE'
    #--------------------------------------------------------------------------
    def negotiate (self, time_to_transmit, desc):
        '''
        For simplicitys sake, we always return true. If you want more detailed
        information on this method, please see the BACI specs.

        Parameters: See the BACI specs.

        Returns: Nothing

        Raises: Nothing
        '''
        #to make pychecker happy
        time_to_transmit = None
        desc = None
        
        return TRUE
    #--------------------------------------------------------------------------
    def last (self):
        '''
        Return the last value received by the DO.

        Parameters: None.

        Returns: last archived value

        Raises: Nothing
        '''
        if self.archive:
            return self.values[-1]
        else:
            return self.values
#------------------------------------------------------------------------------
class CBlong(BaseValueCB, ACS__POA.CBlong):
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    device for any monitors we may create.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name=None, archive=0): 
        '''
        Constructor.
        
        Parameters: name of this callback instance

        Raises: Nothing
        '''
        BaseValueCB.__init__(self, name, archive)        
#------------------------------------------------------------------------------
class CBlongSeq(BaseValueCB, ACS__POA.CBlongSeq):
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    device for any monitors we may create.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name=None, archive=0): 
        '''
        Constructor.

        Parameters: name of this callback instance

        Raises: Nothing
        '''
        BaseValueCB.__init__(self, name, archive)
#------------------------------------------------------------------------------
class CBlongLong(BaseValueCB, ACS__POA.CBlongLong):
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    device for any monitors we may create.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name=None, archive=0): 
        '''
        Constructor.
        
        Parameters: name of this callback instance

        Raises: Nothing
        '''
        BaseValueCB.__init__(self, name, archive)        
#------------------------------------------------------------------------------
class CBuLongLong(BaseValueCB, ACS__POA.CBuLongLong):
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    device for any monitors we may create.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name=None, archive=0): 
        '''
        Constructor.
        
        Parameters: name of this callback instance

        Raises: Nothing
        '''
        BaseValueCB.__init__(self, name, archive)        
#------------------------------------------------------------------------------
class CBdouble(BaseValueCB, ACS__POA.CBdouble):
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    device for any monitors we may create.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name=None, archive=0): 
        '''
        Constructor.

        Parameters: name of this callback instance

        Raises: Nothing
        '''
        BaseValueCB.__init__(self, name, archive)
#------------------------------------------------------------------------------
class CBdoubleSeq(BaseValueCB, ACS__POA.CBdoubleSeq):
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    device for any monitors we may create.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name=None, archive=0): 
        '''
        Constructor.

        Parameters: name of this callback instance

        Raises: Nothing
        '''
        BaseValueCB.__init__(self, name, archive)
#------------------------------------------------------------------------------
class CBstring(BaseValueCB, ACS__POA.CBstring):
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    device for any monitors we may create.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name=None, archive=0): 
        '''
        Constructor.

        Parameters: name of this callback instance

        Raises: Nothing
        '''
        BaseValueCB.__init__(self, name, archive)
#------------------------------------------------------------------------------
class CBstringSeq(BaseValueCB, ACS__POA.CBstringSeq):
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    device for any monitors we may create.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name=None, archive=0): 
        '''
        Constructor.

        Parameters: name of this callback instance

        Raises: Nothing
        '''
        BaseValueCB.__init__(self, name, archive)
#------------------------------------------------------------------------------
class CBpattern(BaseValueCB, ACS__POA.CBpattern):
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    device for any monitors we may create.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name=None, archive=0): 
        '''
        Constructor.

        Parameters: name of this callback instance

        Raises: Nothing
        '''
        BaseValueCB.__init__(self, name, archive)
#------------------------------------------------------------------------------
class CBfloat(BaseValueCB, ACS__POA.CBfloat):
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    device for any monitors we may create.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name=None, archive=0): 
        '''
        Constructor.

        Parameters: name of this callback instance

        Raises: Nothing
        '''
        BaseValueCB.__init__(self, name, archive)
#------------------------------------------------------------------------------
class CBfloatSeq(BaseValueCB, ACS__POA.CBfloatSeq):
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    device for any monitors we may create.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name=None, archive=0): 
        '''
        Constructor.

        Parameters: name of this callback instance

        Raises: Nothing
        '''
        BaseValueCB.__init__(self, name, archive)
#------------------------------------------------------------------------------
class CBbool(BaseValueCB, ACS__POA.CBBool):
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    device for any monitors we may create.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name=None, archive=0): 
        '''
        Constructor.

        Parameters: name of this callback instance

        Raises: Nothing
        '''
        BaseValueCB.__init__(self, name, archive)
#------------------------------------------------------------------------------
class CBonOffSwitch(BaseValueCB, ACS__POA.CBOnOffSwitch):
    '''
    This class defines the method(s) that will be invoked asynchronously by the
    device for any monitors we may create.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name=None, archive=0): 
        '''
        Constructor.

        Parameters: name of this callback instance

        Raises: Nothing
        '''
        BaseValueCB.__init__(self, name, archive)
#------------------------------------------------------------------------------

