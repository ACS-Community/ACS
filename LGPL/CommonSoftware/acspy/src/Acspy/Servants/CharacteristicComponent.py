# @(#) $Id: CharacteristicComponent.py,v 1.6 2010/02/12 22:15:19 agrimstrup Exp $
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
# "@(#) $Id: CharacteristicComponent.py,v 1.6 2010/02/12 22:15:19 agrimstrup Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004/07/21  Created.
#------------------------------------------------------------------------------

'''
This module provides an implementation of the CharacteristicComponent IDL interface:

TODO:
- descriptor implementation is bad.
'''

__revision__ = "$Id: CharacteristicComponent.py,v 1.6 2010/02/12 22:15:19 agrimstrup Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
#--CORBA STUBS-----------------------------------------------------------------
from omniORB                      import CORBA
import ACS
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.ACSComponent        import ACSComponent
from Acspy.Servants.CharacteristicModel import CharacteristicModel
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class CharacteristicComponent(ACSComponent, CharacteristicModel):
    '''
    Components can be derived from CharacteristicComponent only if their IDL
    derives from ACS::CharacteristicComponent.
    '''
    #--------------------------------------------------------------------------
    def __init__(self): # pragma: NO COVER
        '''
        Developer must invoke this from their components constructor. The
        alternative is for them to not provide a constructor at all (implying
        the Container ends up invoking this).

        Params: None.

        Returns: Nothing

        Raises: Nothing.
        '''
        ACSComponent.__init__(self)
        CharacteristicModel.__init__(self)
    #--------------------------------------------------------------------------
    def descriptor(self): # pragma: NO COVER
        '''
        Implementation of the descriptor method found in the
        CharacteristicComponent IDL interface.

        Params: None.

        Returns: The name of this component.

        Raises: Nothing.

        CharacteristicComponentDesc descriptor ();
        '''

        '''
        struct CharacteristicComponentDesc {
        /** The IOR to the CharacteristicComponent */
        ACS::CharacteristicComponent characteristic_component_ref;
        
        /** The fully qualified name of the CharacteristicComponent */
        string name;
        /** A sequence of all property descriptors that belong to the
        CharacteristicComponent */
        PropertyDescSeq properties;
        /** The PropertySet object that allows access to all characteritics */
        CosPropertyService::PropertySet characteristics;
        /** The PropertySet object that allows access to all characteritics */
		
	    };

        '''
        return ACS.CharacteristicComponentDesc(self._corbaRef,
                                               self._get_name(),
                                               (),
                                               CORBA.Object._nil)
#--------------------------------------------------------------------------
