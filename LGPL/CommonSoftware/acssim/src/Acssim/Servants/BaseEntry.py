# @(#) $Id$
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
# "@(#) $Id$"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/12/09  Created.
#------------------------------------------------------------------------------
'''
Contains base class definition for simulated entries.
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from copy    import copy

#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log                  import getLogger
#--GLOBALS---------------------------------------------------------------------
 
#------------------------------------------------------------------------------
class BaseEntry:
    '''
    Class SimulatedEntry is a baseclass which describes simulated components.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, compname):
        '''
        Constructor.
        
        Parameters:
        - compname is the name of the component to be simulated

        Returns: Nothing

        Raises: Nothing
        '''
        #save the name of the component we're looking for
        self.compname = str(compname)

        #our logger
        self.logger = getLogger(compname)

        #this dictionary contains descriptions of all simulated component methods
        #and attributes
        self.methods = {}
    #--------------------------------------------------------------------------
    def getMethod(self, methName):
        '''
        Returns a Python dictionary describing the given method or None if it
        does not exist.
        '''
        if self.methods.has_key(methName):
            return self.methods[methName]
        else:
            return None
    #--------------------------------------------------------------------------
    def setMethod(self, methName, dict):
        '''
        Associates a method with a Python dictionary describing it.
        '''
        self.methods[methName] = copy(dict)
    #--------------------------------------------------------------------------
    