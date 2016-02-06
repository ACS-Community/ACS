# @(#) $Id: ACSComponent.py,v 1.5 2010/02/12 22:15:19 agrimstrup Exp $
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
# "@(#) $Id: ACSComponent.py,v 1.5 2010/02/12 22:15:19 agrimstrup Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
#
#------------------------------------------------------------------------------

'''
This module provides an implementation of the ACSComponent IDL interface:

TODO:
- real test.
'''

__revision__ = "$Id: ACSComponent.py,v 1.5 2010/02/12 22:15:19 agrimstrup Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
#--CORBA STUBS-----------------------------------------------------------------
import ACS
#--ACS Imports-----------------------------------------------------------------
#--GLOBALS---------------------------------------------------------------------
#------------------------------------------------------------------------------
class ACSComponent:
    '''
    Components can be derived from ACSComponent only if their IDL derives from
    ACS::ACSComponent. Almost always the case.
    '''
    #--------------------------------------------------------------------------
    def __init__(self): # pragma: NO COVER
        '''
        Developer must invoke this from their components constructor. The alternative
        is for them to not provide a constructor at all (implying the Container
        ends up invoking this).

        Params: None.

        Returns: Nothing

        Raises: Nothing.
        '''
        self.name = None
        self.componentState = ACS.COMPSTATE_NEW
    #--CORBA ATTRIBUTE---------------------------------------------------------
    def _get_name(self): # pragma: NO COVER
        '''
        Implementation of the name attribute defined in the ACSComponent
        interface.

        Params: None.

        Returns: The name of this component.

        Raises: Nothing.

        readonly attribute string name;
        '''
        return str(self.name)
    #--------------------------------------------------------------------------
    def setName(self, name): # pragma: NO COVER
        '''
        Only the container should invoke this method.
        
        Params: name is the name of this component

        Returns: Nothing.

        Raises: Nothing.
        '''
        self.name = str(name)
        return
    #--CORBA ATTRIBUTE---------------------------------------------------------
    def _get_componentState(self): # pragma: NO COVER
        '''
        Implementation of the componentState attribute defined in the ACSComponent
        interface.

        Params: None.

        Returns: State of this component

        Raises: Nothing
        
        readonly attribute ComponentStates componentState;
        '''
        return self.componentState
    #--------------------------------------------------------------------------
    def setComponentState(self, state): # pragma: NO COVER
        '''
        The developer should feel free to invoke this method.
        
        Params: state is quite literally the sate of this component.  It can be
        any of the enumerations for ACS::ComponentStates

        Returns: Nothing.

        Raises: Nothing.
        '''
        self.componentState = state
        return
     #--------------------------------------------------------------------------

