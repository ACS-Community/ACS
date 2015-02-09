# @(#) $Id: DevIO.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
#
# Copyright (C) 2001
# Associated Universities, Inc. Washington DC, USA.
#
# Produced for the ALMA project
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
# 675 Massachusetts Ave, Cambridge, MA 02139, USA.  Correspondence concerning
# ALMA should be addressed as follows:
#
# Internet email: alma-sw-admin@nrao.edu
# "@(#) $Id: DevIO.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004/07/21  Created.
#------------------------------------------------------------------------------

'''
This module includes the implementation of a simple DevIO class to be used
in conjunction with BACI properties.
'''

__version__ = "$Id: DevIO.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from copy import deepcopy
#--CORBA STUBS-----------------------------------------------------------------
#--ACS Imports-----------------------------------------------------------------
#--GLOBALS---------------------------------------------------------------------
#------------------------------------------------------------------------------
class DevIO(object):
    '''
    Device I/O implementation class.
    This class is a base class for providing input/output implementations to 
    the Property class.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, initVal):
        '''
        Constructor.

        Parameters: initVal is the initial value to be returned by the read
        method. If initVal is a list or tuple, a deep copy is made.

        Returns: Nothing

        Raises: Nothing
        '''
        if type(initVal)==list or type(initVal)==tuple:
            self.value = deepcopy(initVal)
        else:
            self.value = initVal
    #--------------------------------------------------------------------------
    def read(self):
        '''
        Method to read a value. Can/should be overriden by subclasses.

        Parameters: None

        Returns: device value

        Raises: Nothing
        '''
        return self.value
    #--------------------------------------------------------------------------
    def write(self, value):
        '''
        Method to write a value. It can/should be overriden by subclasses.

        Parameters: value to write to the device

        Returns: Nothing

        Raises: Nothing
        '''
        if type(value)==list or type(value)==tuple:
            self.value = deepcopy(value)
        else:
            self.value = value
    #--------------------------------------------------------------------------
