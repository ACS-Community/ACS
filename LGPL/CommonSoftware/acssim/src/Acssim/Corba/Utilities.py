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
Contains various utility methods of general use to the simulator.
Eventually some of these should be moved to Acspy.Util.*
These are NOT to be considered simulator API functions - for this see
Acssim.Servants.Goodies
'''
#--REGULAR IMPORTS-------------------------------------------------------------
import omniORB
import CORBA
#--CORBA STUBS-----------------------------------------------------------------
from Acspy.Util.ACSCorba        import interfaceRepository
from Acspy.Util.ACSCorba import getClient
from Acspy.Util.ACSCorba import getManager
#--GLOBALS---------------------------------------------------------------------
__revision__ = "@(#) $Id$"
omniORB.importIRStubs()
IFR = interfaceRepository()
#------------------------------------------------------------------------------
def getCompIfrID(comp_name):
    '''
    Given a component's name, returns said component's interface repository ID
    or throws an exception if the ID cannot be determined.
    
    Raises: CORBA.NO_RESOURCES
    '''
    #get a list of component info's
    comp_list = getManager().get_component_info(getClient().token.h, 
                                                [], 
                                                "*", 
                                                "*", 
                                                0)
    
    for comp in comp_list:
        if comp.name == comp_name:
            return comp.type
        
    #sanity check
    raise CORBA.NO_RESOURCES()                                          
                                    
#------------------------------------------------------------------------------
def getSuperIDs(ir_id):
    '''
    Given an interface repository ID, ir_id, this function returns a list
    consisting of the interface repository IDs ir_id inherits from.
    This function leaves out ACS defined interfaces such as 
    IDL:alma/ACS/ACSComponent:1.0
    
    Parameters: ir_id is the interface repository ID of an IDL interface
    
    Returns: an unordered list of IFR IDs ir_id inherits from
    
    Raises:???
    '''
    ret_val = []
    safe_list = ['IDL:alma/ACS/CharacteristicComponent:1.0', 
                  'IDL:alma/ACS/ACSComponent:1.0', 
                  'IDL:alma/ACS/CharacteristicModel:1.0']
    
    #get the interface definition
    interface = IFR.lookup_id(ir_id)
    interface = interface._narrow(CORBA.InterfaceDef)
    
    #use the definition to get a list of other interface definitions
    #this ID derives from
    base_interfaces = interface._get_base_interfaces()
    
    #Determine ALL bases
    for base_interface in base_interfaces:
        
        #i.e., "IDL:.../..../...:1.0"
        base_interface_id = base_interface._get_id()
        
        #skip ACS interfaces
        if safe_list.count(base_interface_id)!=0:
            continue
        
        #add it to the end of the list
        ret_val.append(base_interface_id)
        
        #use recursion to figure out the bases' base interfaces
        ret_val = ret_val + getSuperIDs(base_interface_id)
        
    return ret_val

#------------------------------------------------------------------------------
def getDefinition(irLabel):
    '''
    Helper function returns the interface definition of some IDL type given
    its complete interface repository location.

    Parameters: irLabal is the location of the interface defintion within the
    IFR. Typically this is something like 
    "IDL:alma/someModule/someInterface:1.0"

    Returns: the IFR definition of irLabel

    Raises: ???
    '''
    interf = IFR.lookup_id(irLabel)
    return interf
        
        
    