# @(#) $Id: CharacteristicModel.py,v 1.6 2010/02/12 22:15:19 agrimstrup Exp $
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
# "@(#) $Id: CharacteristicModel.py,v 1.6 2010/02/12 22:15:19 agrimstrup Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004/07/21  Created.
#------------------------------------------------------------------------------

'''
This module provides an implementation of the CharacteristicModel IDL interface:

TODO:
- replace dummie implementations of all methods.
'''

__revision__ = "$Id: CharacteristicModel.py,v 1.6 2010/02/12 22:15:19 agrimstrup Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
#--CORBA STUBS-----------------------------------------------------------------
from omniORB                      import CORBA
from omniORB                      import any
#--ACS Imports-----------------------------------------------------------------

#--GLOBALS---------------------------------------------------------------------
#------------------------------------------------------------------------------
class CharacteristicModel:
    '''
    Models can be derived from CharacteristicModel only if their IDL derives from
    ACS::CharacteristicModel.
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
        self._corbaRef = None
    #--------------------------------------------------------------------------
    def get_characteristic_by_name(self, name): # pragma: NO COVER
        '''
        Implementation of the descriptor method found in the CharacteristicModel
        IDL interface.

        Params: None.

        Returns: The name of this component.

        Raises: Nothing.

        any get_characteristic_by_name (in string name)			
        raises (NoSuchCharacteristic);
        
        DWF-FIX ME!!!
        '''
        #To make NRI happy
        name = None
        return any.to_any(0L)
    #--------------------------------------------------------------------------
    def find_characteristic(self, reg_exp): # pragma: NO COVER
        '''
        Implementation of the descriptor method found in the CharacteristicModel
        IDL interface.

        Params: None.

        Returns: The name of this component.

        Raises: Nothing.

        stringSeq find_characteristic (in string reg_exp);

        DWF-FIX ME!!!
        '''
        #To make NRI happy
        reg_exp = None
        return ()
    #--------------------------------------------------------------------------
    def get_all_characteristics(self): # pragma: NO COVER
        '''
        Implementation of the descriptor method found in the CharacteristicModel
        IDL interface.

        Params: None.

        Returns: The name of this component.

        Raises: Nothing.

        CosPropertyService::PropertySet get_all_characteristics ();

        DWF-fix me!!!
        '''
        return CORBA.Object._nil
#--------------------------------------------------------------------------


