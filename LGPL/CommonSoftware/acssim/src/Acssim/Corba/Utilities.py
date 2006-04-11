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
Acssim.Goodies
'''
#--REGULAR IMPORTS-------------------------------------------------------------
import omniORB
import CORBA
#from compiler import compile
from code import compile_command

#--CORBA STUBS-----------------------------------------------------------------
from Acspy.Util.ACSCorba        import interfaceRepository
from Acspy.Util.ACSCorba import getClient
from Acspy.Util.ACSCorba import getManager
#--GLOBALS---------------------------------------------------------------------
__revision__ = "@(#) $Id$"
omniORB.importIRStubs()
IFR = interfaceRepository()
STD_WHITESPACE="    "
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
def getDefinition(ir_label):
    '''
    Helper function returns the interface definition of some IDL type given
    its complete interface repository location.

    Parameters: irLabal is the location of the interface defintion within the
    IFR. Typically this is something like 
    "IDL:alma/someModule/someInterface:1.0"

    Returns: the IFR definition of irLabel

    Raises: ???
    '''
    interf = IFR.lookup_id(ir_label)
    return interf
#-----------------------------------------------------------------------------
def listToCodeObj(code_list, locals_dict):
    '''
    Converts code_list, a list of Python strings, into a function object.
    code_list should only rely on one parameter, "parameters", which consists
    of a list.
    '''
    function_name = "stringFunction"
    function_declare = "def " + function_name + "(parameters):"
    
    #prepend some whitespace to each line
    for i in range(0, len(code_list)):
        code_list[i] = STD_WHITESPACE + code_list[i]
        
    #add the define to the beginning of the list
    code_list.insert(0, function_declare)
    
    #convert the list into a string delimited by '\n's
    code_string = ""
    for line in code_list:
        code_string = code_string + line + '\n'
        
    #finally ok to compile it
    code_obj = compile_command(code_string, "sim_file") #, "exec")
    #exec code_obj in globals(), locals_dict
    return code_obj
#--------------------------------------------------------------------
def getTypeCode(ifr_id):
    '''
    Returns a typecode object using the ifr_id.
    '''
    #use the ifr to lookup the ID
    lid = getDefinition(ifr_id)
    
    #return the typecode from that
    if (lid != None) and (isinstance(lid, CORBA._objref_Contained) == True):
        try:
            #dealing with an IDL interface most likely
            ifr_lid = lid._narrow(CORBA.InterfaceDef)
            
            #get the full description of the IDL interface
            full_descript = ifr_lid.describe_interface()
            
            #next take the type code
            type_code = full_descript.type

        except:
            type_code = lid._get_type()
        
    #ok...handle the simple types
    elif ifr_id == "IDL:omg.org/CORBA/Boolean:1.0": 
        type_code = CORBA.TC_boolean
    elif ifr_id == "IDL:omg.org/CORBA/Char:1.0": 
        type_code = CORBA.TC_char
    elif ifr_id == "IDL:omg.org/CORBA/Octet:1.0": 
        type_code = CORBA.TC_octet
    elif ifr_id == "IDL:omg.org/CORBA/Short:1.0": 
        type_code = CORBA.TC_short
    elif ifr_id == "IDL:omg.org/CORBA/UShort:1.0": 
        type_code = CORBA.TC_ushort
    elif ifr_id == "IDL:omg.org/CORBA/Long:1.0": 
        type_code = CORBA.TC_long
    elif ifr_id == "IDL:omg.org/CORBA/ULong:1.0": 
        type_code = CORBA.TC_ulong
    elif ifr_id == "IDL:omg.org/CORBA/LongLong:1.0": 
        type_code = CORBA.TC_longlong
    elif ifr_id == "IDL:omg.org/CORBA/ULongLong:1.0": 
        type_code = CORBA.TC_ulonglong
    elif ifr_id == "IDL:omg.org/CORBA/Float:1.0": 
        type_code = CORBA.TC_float
    elif ifr_id == "IDL:omg.org/CORBA/Double:1.0": 
        type_code = CORBA.TC_double
    elif ifr_id == "IDL:omg.org/CORBA/LongDouble:1.0": 
        type_code = CORBA.TC_longdouble
    elif ifr_id == "IDL:omg.org/CORBA/String:1.0": 
        type_code = CORBA.TC_string
    elif ifr_id == "IDL:omg.org/CORBA/Void:1.0": 
        type_code = CORBA.TC_void
    elif ifr_id == "IDL:omg.org/CORBA/Null:1.0": 
        type_code = CORBA.TC_null
    else:
        type_code = None
    
    return type_code
    
