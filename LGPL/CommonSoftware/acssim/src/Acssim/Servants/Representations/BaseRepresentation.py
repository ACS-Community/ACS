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
from inspect import isfunction
#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log                  import getLogger
from Acssim.Goodies import getComponent
#--GLOBALS---------------------------------------------------------------------
__revision__ = "@(#) $Id$"
#------------------------------------------------------------------------------
class BaseRepresentation:
    '''
    Class BaseRepresentation is a baseclass which describes simulated components.
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
        
        #reference to the component
        self.comp_ref = None
        
    #--------------------------------------------------------------------------
    def getMethod(self, method_name):
        '''
        Returns a Python dictionary describing the given method or None if it
        does not exist.
        
        Parameters:
            method_name - name of the method
            comp_ref - reference to the component
        '''
        if self.methods.has_key(method_name):
            return self.methods[method_name]
        else:
            return None
    #--------------------------------------------------------------------------
    def setMethod(self, method_name, in_dict):
        '''
        Associates a method with a Python dictionary describing it.
        '''
        if not isfunction(in_dict):
            code = copy(in_dict)
        else:
            code = in_dict
        
        self.methods[method_name] = code
    #--------------------------------------------------------------------------
    def __checkCompRef(self):
        '''
        Helper method does a sanity check on the component reference member.
        This method is just around because an enduser might try to to define
        simulated component behavior long before the simulated component has
        ever been instantiated.
        '''
        # print "*******", "__checkCompRef: Trying to get component reference for", self.compname
        if self.comp_ref == None:
            self.comp_ref = getComponent(self.compname)
