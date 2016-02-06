# @(#) $Id: CharacteristicModel.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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
# "@(#) $Id: CharacteristicModel.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004/07/21  Created.
#------------------------------------------------------------------------------

'''
This module provides an implementation of the CharacteristicModel IDL interface:

TODO:
- everything
'''

__version__ = "$Id: CharacteristicModel.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

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
    def __init__(self, name):
        '''
        Constructor

        Params: name is the quite literally the name of the characteristic model

        Returns: Nothing

        Raises: Nothing.
        '''
        self.name=name
        return
    #--------------------------------------------------------------------------
    def get_characteristic_by_name(self, name):
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
        del name   #to make pychecker happy
        return any.to_any(0L)
    #--------------------------------------------------------------------------
    def find_characteristic(self, reg_exp):
        '''
        Implementation of the descriptor method found in the CharacteristicModel
        IDL interface.

        Params: None.

        Returns: The name of this component.

        Raises: Nothing.

        stringSeq find_characteristic (in string reg_exp);

        DWF-FIX ME!!!
        '''
        del reg_exp   #to make pychecker happy
        return ()
    #--------------------------------------------------------------------------
    def get_all_characteristics(self):
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

