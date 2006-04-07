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
Module consists of user-friendly functions to be used for configuring
simulated component behavior from interactive Python container sessions.
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from inspect import isfunction
from copy    import copy
from atexit    import register
#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acspy.Util.ACSCorba        import interfaceRepository
from Acspy.Common.CDBAccess    import CDBaccess
from Acspy.Util.XmlObjectifier import XmlObject
from Acssim.Corba.Utilities import getCompIfrID
from Acssim.Corba.Utilities import getSuperIDs
from Acspy.Nc.Supplier import Supplier
from Acssim.Corba.Utilities import getTypeCode

#--GLOBALS---------------------------------------------------------------------
__revision__="@(#) $Id$"
API = 'API'
CDB = 'CDB'
GEN = 'GEN'
LNS = 'LOCAL NAMESPACE'

#Standard timeout for simulated methods to wait before returning control
STD_TIMEOUT = 0.2

#Probability of an exception being thrown for any given simulated method.
EXCEPTION_CHANCE = 0.01

#Maximum size of a simulated sequence
MAX_SEQUENCE_SIZE = 15

#use the IFR to get a description of this class
IR = interfaceRepository()

#CHARS is a list full of random charcters that will be used
CHARS = []
for character in "abcdefghijklmnopqrstuvwxyz _":
    CHARS.append(character)

#storage for "static data" that can be shared between components
_GLOBALS = {}

#namespace of individual components
COMPONENTS_NS = {}
COMPONENTS_NS_LIST = {}

#described simulated behavior of the components
_COMP_PROXIES = {}

#maps component names to component instances
_COMP_REFS = {}

#first get access to the CDB
CDB_ACCESS = CDBaccess()

#each key in this dictionary consists of the stingified channel and the value
#is a supplier object for said channel.
_SUPPLIERS_DICT = {}
#------------------------------------------------------------------------------
def addComponent(comp_name, comp_ref):
    '''
    Adds a component to the singled dictionary contained within this module.

    Parameters:
    compName - name of the component in string format
    compRef - reference to the component.

    Raises: Nothing

    Returns: Nothing
    '''
    _COMP_REFS[comp_name] = comp_ref
    return
#------------------------------------------------------------------------------
def removeComponent(comp_name):
    '''
    Removes a component from the singled dictionary contained within this module.

    Parameters:
    compName - name of the component in string format

    Raises: ???

    Returns: Nothing
    '''
    _COMP_REFS.pop(comp_name)
    return
#------------------------------------------------------------------------------
def getComponent(comp_name):
    '''
    Returns a reference to a simulated component which has been activated.

    Parameters:
    compName - name of the component in string format

    Raises: ???

    Returns: reference to a simulated component.
    '''
    #sanity check
    if _COMP_REFS.has_key(comp_name) == False:
        return None
    
    return _COMP_REFS[comp_name]
#---------------------------------------------------------------------
def getSimProxy(comp_name):
    '''
    Returns a proxy object for this particular component.
    '''
    #sanity check
    if not _COMP_PROXIES.has_key(comp_name):
        from Acssim.Servants.Representations.BehaviorProxy import BehaviorProxy
        _COMP_PROXIES[comp_name] = BehaviorProxy(comp_name)
    
    return _COMP_PROXIES[comp_name]
#---------------------------------------------------------------------
def getComponentXMLObj(comp_name):
    '''
    Returns an XMLObjectifier for the given component name provided
    it exists within the ACS CDB. If this is not the case, simply 
    returns None
    '''
    try:
        #make sure the entry exists first of all...
        t_xml = CDB_ACCESS.getField("alma/simulated/" + comp_name)
    
        #create an xml helper object
        xml_obj = XmlObject(xmlString = t_xml)

    except:
        xml_obj = None
    
    return xml_obj
#---------------------------------------------------------------------
def addGlobalData(name, value):
    '''
    This function is used to add global data that will be accessible
    between simulated components. If for example you wanted
    to create a variable named "x" with the value 3.14, you would
    call:
      addGlobalData("x", 3.14)
    Please be careful as this function does NOT perform a deep copy
    on value!

    Parameters:
    name - the name of your variable in string format
    value - an object of your choice

    Returns: Nothing

    Raises: ???
    '''
    if name != "addGlobalData" and name != "removeGlobalData":
        _GLOBALS[str(name)] = value
    else:
        raise "Cannot add 'addGlobalData' or 'removeGlobalData'"
    return

def removeGlobalData(name):
    '''
    This function is used to remove global data that is accessible
    between simulated components. If for example you wanted
    to remove a variable named "x", you would call:
      removeGlobalData("x")

    Parameters:
    name - the name of your variable in string format

    Returns: Nothing

    Raises: ???
    '''
    if name != "addGlobalData" and name != "removeGlobalData":
        del _GLOBALS[str(name)]
    else:
        raise "Cannot remove 'addGlobalData' or 'removeGlobalData'"  
    return

def getGlobalData():
    '''
    Returns the global data dictionary shared between all simulated
    components.

    Parameters: None

    Returns: the global data dictionary shared between all simulated
    components
    
    Raises: Nothing
    '''
    return _GLOBALS

#add the functions to _GLOBALS
_GLOBALS[addGlobalData.__name__] = addGlobalData
_GLOBALS[removeGlobalData.__name__] = removeGlobalData
#------------------------------------------------------------------------------
def getCompLocalNSList(comp_name):
    '''
    '''
    getCompLocalNS(comp_name)
    return COMPONENTS_NS_LIST[comp_name]

def getCompLocalNS(comp_name):
    '''
    Gets the local namespace dictionary for a component.
    
    Parameters:
    comp_name - name of the component
    
    Returns: A dictionary conforming to the return value of the native 
    "locals()" function.
    
    Raises: Nothing
    '''
    #sanity check
    if COMPONENTS_NS.has_key(comp_name)==0:
        
        #initialize the temporary local namespace to be empty (in case
        #of some failure with the CDB)
        t_dict = {}
        comp_cdb_list = [comp_name]

        COMPONENTS_NS_LIST[comp_name] = []
        
        #because we must check the inherited IDL interfaces as well
        cdb_location = "interfaces/"
        try:
            comp_ifr_id = getCompIfrID(comp_name)
        except:
            comp_ifr_id = "IDL:alma/ACS/ACSComponent:1.0"
        
        comp_cdb_list.append(cdb_location + 
                             comp_ifr_id.split('IDL:')[1].replace(":", "/"))
        
        comp_ifr_ids = getSuperIDs(comp_ifr_id)
        for temp_id in comp_ifr_ids:
            
            temp_id = temp_id.split('IDL:')[1].replace(":", "/")
            temp_id = cdb_location + temp_id
            comp_cdb_list.append(temp_id)
        
        for name in comp_cdb_list:
            #-------------------------------
            #get the CDB helper object
            xml_obj = getComponentXMLObj(name)
            
            if xml_obj!=None:
                try: #use a try here because pythonImports section is not required
                    #get the imports
                    dom = xml_obj.SimulatedComponent.pythonImports
                    py_imports = dom.getValue().rstrip().lstrip().split('\n')

                    COMPONENTS_NS_LIST[comp_name] = COMPONENTS_NS_LIST[comp_name] + py_imports

                    #populate the locals dictionary
                    for py_import in py_imports:
                        exec py_import in globals(), t_dict
                except:
                    pass
            #-------------------------------
        COMPONENTS_NS[comp_name] = t_dict
        
        
    return COMPONENTS_NS[comp_name]
#------------------------------------------------------------------------------
def setCHARS(new_char_list):
    '''
    This API function allows developers to make methods and attributes returning a
    single character "less random" on a global level. In other words, by providing a new list of
    characters to setCHARS the developer can exclude certain character values
    from being returned by simulated CORBA objects. Sample usage could be something
    similar to:
    
       setCHARS(["a", "b", "d"])
    
    Please note that the probability of a specific character being used can also be
    increased by including it multiple times in this list.

    Parameters: newCharList is a list of characters. Each time a call to a
    simulated component which returns a character is made, a random character
    from newCharList will be returned.

    Returns: Nothing

    Raises: Nothing
    '''
    global CHARS
    CHARS = new_char_list
    return
#------------------------------------------------------------------------------
def getCHARS():
    '''
    Returns the list set by setCHARS method. See description there for more
    info.

    Parameters: None

    Returns: a list of characters. e.g., ['a', 'b', ...]

    Raises: Nothing
    '''
    return CHARS
#------------------------------------------------------------------------------
def setStandardTimeout(new_timeout):
    '''
    This API function allows developers to set the time simulated method/attribute
    invocations wait before returning control on a global level. Sample usage could
    be:
    
       setStandardTimeout(3.2)
    
    Parameters: an integer or decimal number of seconds 

    Returns: Nothing

    Raises: Nothing
    '''
    global STD_TIMEOUT
    STD_TIMEOUT=float(new_timeout)
    return
#------------------------------------------------------------------------------
def getStandardTimeout():
    '''
    Returns the timeout set by getStandardTimeout. See description there for 
    more info.
    
    Parameters: None
    
    Returns: the standard timeout in floating point seconds
    
    Raises: Nothing
    '''
    return STD_TIMEOUT
#------------------------------------------------------------------------------
def setExceptProb(new_prob):
    '''
    NOT CURRENTLY USED!

    Parameters: the new probability (a float) of an exception occuring on any given
    simulated attribute/method invocation.

    Returns: Nothing

    Raises: Nothing
    '''
    global EXCEPTION_CHANCE
    EXCEPTION_CHANCE=new_prob
    return

#------------------------------------------------------------------------------
def getExceptProb():
    '''
    NOT CURRENTLY USED!

    Parameters: None

    Returns: the new probability (a float) of an exception occuring on any given
    simulated attribute/method invocation.

    Raises: Nothing
    '''
    return EXCEPTION_CHANCE
#------------------------------------------------------------------------------
def setMaxSeqSize(new_seq_size):
    '''
    This API function allows developers to set the maximum sequence size for simulated
    methods/attributes on a global level. Sample usage could be:

       setMaxSeqSize(200)

    Parameters: the new maximum length of sequences that will be returned on any given
    simulated attribute/method invocation.

    Returns: Nothing

    Raises: Nothing
    '''
    global MAX_SEQUENCE_SIZE
    MAX_SEQUENCE_SIZE=new_seq_size
    return
#------------------------------------------------------------------
def getMaxSeqSize():
    '''
    Returns the maximum sequence size for simulated
    methods/attributes on a global level.

    Parameters: None

    Returns: the new maximum length of sequences that will be returned on any given
    simulated attribute/method invocation.    

    Raises: Nothing
    '''
    return MAX_SEQUENCE_SIZE
#-----------------------------------------------------------------
def setComponentMethod(comp_name,
                       meth_name,
                       code_list,
                       timeout=getStandardTimeout()):
    '''
    This is an ACS Component Simulator API method and developers are encouraged
    to invoke it from their own code or from the Python interpreter at any time.
    setComponentMethod is used to do exactly what its name implies - setup a
    simulated component method to behave in a certain manner. Using this method,
    one can:
    - change the return value of a simulated method
    - make a simulated method throw an exception
    - make a simulated method sleep for a desired amount of time before returning
    control

    Paramters:
    - compName is the components name in string format. "FRIDGESIM1" for example.
    - methName is the name of the IDL method in string format that is being simulated.
    "open" for example.
    - code is a list of Python code in string format to be executed each time the
    simulated method is invoked. The last string of this list should either
    contain a "raise ..." statement where the exception being thrown is derived
    from an IDL exception OR the last string should represent some return value
    to be evaluated by the Python "eval" statement. A sample value for code
    could be [ "import CORBA", "CORBA.TRUE" ] or [ "import ACSExceptionCommon",
    "raise ACSExceptionCommon.CommonExImpl()" ]. It is important to note that
    this list must not be empty!
    - timeout is the amount of time that should pass before the method/attribute
    returns control

    Returns: Nothing

    Raises: Nothing
    '''
    if not isfunction(code_list):
        code = copy(code_list)
    else:
        code = code_list
    
    #create the temporary dictionary
    temp_dict = { 'Value':code,
             'Timeout': float(timeout)}
    
    #store it globally
    getSimProxy(comp_name).api_handler.setMethod(meth_name, temp_dict)
#----------------------------------------------------------------------------
def supplyEventByType(component_name, channel_name, ifr_id):
    '''
    Supplies an event to a notification channel by the event's IFR ID type.
    
    Parameters:
        - component_name - name of the component publisihing the event
        - channel_name - name of the channel the event should be published
        on
        - ifr_id - the interface repository ID of the IDL structure that 
        should be dynamically created and sent as an event
        
    Returns: Nothing
    
    Raises: ???
    '''
    from Acssim.Corba.Generator import getRandomValue
    
    #create an instance of the IDL struct
    #get the type code first
    type_code = getTypeCode(ifr_id)
    event_instance = getRandomValue(type_code, 
                                    getComponent(component_name))
                                    
    #now just delegate to another function
    supplyEventByInstance(component_name, channel_name, event_instance)

def supplyEventByInstance(component_name, channel_name, event_instance):
    '''
    Supplies an event to a notification channel.
    
    Parameters:
        - component_name - name of the component publisihing the event
        - channel_name - name of the channel the event should be published
        on
        - event_instance - an instance of the IDL struct to publish
        
    Returns: Nothing
    
    Raises: ???
    '''
    #sanity check
    if _SUPPLIERS_DICT.has_key(channel_name)==False:
        _SUPPLIERS_DICT[channel_name] = Supplier(channel_name,
                                                 getComponent(component_name))
    
    _SUPPLIERS_DICT[channel_name].publishEvent(simple_data = event_instance,
                                               supplier_name = component_name)    
#----------------------------------------------------------------------------
def __cleanUp():
    '''
    This function cleans up static objects from this module just before the 
    interpreter exits.
    '''
    #disconnect all suppliers
    for sup in _SUPPLIERS_DICT.values():
        sup.disconnect()

#ensure all static module objects are cleaned up properly before exiting
register(__cleanUp)