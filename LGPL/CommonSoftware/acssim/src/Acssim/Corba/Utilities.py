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
'''
#--REGULAR IMPORTS-------------------------------------------------------------
import omniORB
import CORBA
#--CORBA STUBS-----------------------------------------------------------------
from Acspy.Util.ACSCorba        import interfaceRepository
#--GLOBALS---------------------------------------------------------------------
omniORB.importIRStubs()
#------------------------------------------------------------------------------
def getSuperIDs(ir_id):
    '''
    Given an interface repository ID, ir_id, this function returns a list of
    consisting of the interface repository IDs ir_id inherits from.
    
    Parameters: ir_id is the interface repository ID of an IDL interface
    
    Returns: an unordered list of IFR IDs ir_id inherits from
    
    Raises:???
    '''
    ret_val = []
    safe_list = ['IDL:alma/ACS/CharacteristicComponent:1.0', 
                  'IDL:alma/ACS/ACSComponent:1.0', 
                  'IDL:alma/ACS/CharacteristicModel:1.0']
    
    #get the IFR
    ifr = interfaceRepository()
    ifr = ifr._narrow(CORBA.Repository)
    
    #get the interface definition
    interface = ifr.lookup_id(ir_id)
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
        
        
    