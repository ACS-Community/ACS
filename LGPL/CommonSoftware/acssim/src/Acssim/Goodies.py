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
from atexit  import register
from sys import exc_info
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.CDBAccess     import CDBaccess
from Acspy.Util.ACSCorba        import interfaceRepository
from Acspy.Util.XmlObjectifier  import  XmlObject
from Acssim.Corba.Utilities     import getCompIfrID
from Acssim.Corba.Utilities     import getSuperIDs
from Acssim.Corba.Utilities     import getTypeCode
from Acspy.Nc.Supplier          import Supplier

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
    This is in place to be called by the simulator framework and probably
    isn't of much use to end-users.

    Parameters:
        comp_name - name of the component in string format
        comp_ref - reference to the component.

    Raises: Nothing

    Returns: Nothing
    '''
    _COMP_REFS[comp_name] = comp_ref
    return
#------------------------------------------------------------------------------
def removeComponent(comp_name):
    '''
    Removes a component from the singled dictionary contained within this module.
    This is in place to be called by the simulator framework and probably
    isn't of much use to end-users.
    
    Parameters:
        comp_name - name of the component in string format

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
        comp_name - name of the component in string format

    Raises: Nothing

    Returns: Reference to a simulated component.
    '''
    #sanity check
    if _COMP_REFS.has_key(comp_name) == False:
        return None
    
    return _COMP_REFS[comp_name]
#---------------------------------------------------------------------
def getSimProxy(comp_name, comp_type=None):
    '''
    Returns a proxy object for this particular component. The proxy object
    is what actually determines how a simulated component behaves. That is,
    it chooses whether to use info found in the ACS CDB, the Python
    console, GUI, etc for any given method.
    
    Parameters:
        comp_name is the name of the component in string format
        
    Returns:
        An instance of Acssim.Servants.Representations.BehaviorProxy
        which is the principal object that determines what kind of 
        behavior simulated components have. This is primarily useful
        if you have implemented 
        Acssim.Servants.Representations.BaseRepresenation on your own
        and wish to "register" this representation ahead of the 
        representations that ACS provides. See the attachNewHandler
        method of BehaviorProxy for more info.
        
    Raises: ???
    '''
    #sanity check
    if not _COMP_PROXIES.has_key(comp_name):
        from Acssim.Servants.Representations.BehaviorProxy import BehaviorProxy
        _COMP_PROXIES[comp_name] = BehaviorProxy(comp_name, comp_type)
    
    return _COMP_PROXIES[comp_name]
#---------------------------------------------------------------------
def getComponentXMLObj(comp_name):
    '''
    Returns an XMLObjectifier for the given component name provided
    it exists within the ACS CDB. If this is not the case, simply 
    returns None. Probably not of much use outside the simulator
    framework.
    
    Params:
        comp_name - name of the component
        
    Returns:
        An Acspy.Util.XMLObjectifier.XmlObject if there's a description
        of the component found in $ACS_CDB/CDB/alma/simulated/*. If
        this is not the case, None is returned.
	
	Raises: Nothing
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
def setGlobalData(name, value):
    '''
    This function is used to add global data that will be accessible
    between simulated components. If for example you wanted
    to create a variable named "x" with the value 3.14, you would
    call:
      setGlobalData("x", 3.14)
    Please be careful as this function does NOT perform a deep copy
    on value!

    Parameters:
    	name - the name of your variable in string format
    	value - an object of your choice

    Returns: Nothing

    Raises: ???
    '''
    if name != "setGlobalData" and name != "removeGlobalData":
        _GLOBALS[str(name)] = value
    else:
        raise "Cannot add 'setGlobalData' or 'removeGlobalData'"
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
    if name != "setGlobalData" and name != "removeGlobalData":
        del _GLOBALS[str(name)]
    else:
        raise "Cannot remove 'setGlobalData' or 'removeGlobalData'"  
    return

def getGlobalData(name):
    '''
    Returns the global data object defined by name shared between all simulated
    components.

    Parameters:
    	name - the name of your variable in string format

    Returns: the global data defined by name or None if it does not
    exist
    
    Raises: Nothing
    '''
    if _GLOBALS.has_key(name)==0:
        return None
    else:
        return _GLOBALS[name]

#add the functions to _GLOBALS
_GLOBALS[setGlobalData.__name__] = setGlobalData
_GLOBALS[removeGlobalData.__name__] = removeGlobalData
#------------------------------------------------------------------------------
def getCompLocalNSList(comp_name):
    '''
    Returns a list of all imports added to CDB method/attribute 
    implementations. This is needed as the Python compile module 
    does not seem to recognize imports from locals() when compiling
    custom code.
    
    Parameters:
        comp_name is the name of the component
    
    Return: a list of strings where each string represents a single
    Python import statement
    
    Raises: ???
    '''
	#make sure we have namespace for the component
    getCompLocalNS(comp_name)

	#everything deemed to be of general use to end-users
	#is defined here
    extra_funcs = [ "from Acssim.Goodies import getComponent", 
                    "from Acssim.Goodies import supplyEventByType",
                    "from Acssim.Goodies import supplyEventByInstance",
                    "from Acssim.Goodies import setComponentMethod",
                    "from Acssim.Goodies import getGlobalData",
                    "from Acssim.Goodies import setGlobalData",
                    "from Acssim.Goodies import removeGlobalData"
                  ]
	                  
    return extra_funcs + COMPONENTS_NS_LIST[comp_name]
#------------------------------------------------------------------------------
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
    This API function allows developers to make methods and attributes 
    returning a single character "less random" on a global level. In other 
    words, by providing a new list of characters to setCHARS the developer 
    can exclude certain character values from being returned by simulated 
    CORBA objects. Sample usage could be something similar to:
    
       setCHARS(["a", "b", "d"])
    
    Please note that the probability of a specific character being used 
    can also be increased by including it multiple times in this list.
    
    Parameters: 
        newCharList is a list of characters. Each time a call to a
        simulated component which returns a character is made, a random 
        character from newCharList will be returned.
    
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
    info. Most likely of little use to end-users (i.e., just in place to give
    the framework access)
    
    Parameters: None
    
    Returns: a list of characters. e.g., ['a', 'b', ...]
    
    Raises: Nothing
    '''
    return CHARS
#------------------------------------------------------------------------------
def setStandardTimeout(new_timeout):
    '''
    This API function allows developers to set the time simulated 
    method/attribute invocations wait before returning control on a global 
    level. Sample usage could be:
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
    Returns the timeout set by setStandardTimeout. See description there for 
    more info. Most likely of little use to end-users (i.e., just in place to 
    give the framework access)
    
    Parameters: None
    
    Returns: the standard timeout in floating point seconds
    
    Raises: Nothing
    '''
    return STD_TIMEOUT
#------------------------------------------------------------------------------
def setExceptProb(new_prob):
    '''
    NOT CURRENTLY USED!

    Parameters: the new probability (a float) of an exception occuring on any 
	given simulated attribute/method invocation.

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

    Returns: the new probability (a float) of an exception occuring on any 
	given simulated attribute/method invocation.

    Raises: Nothing
    '''
    return EXCEPTION_CHANCE
#------------------------------------------------------------------------------
def setMaxSeqSize(new_seq_size):
    '''
    This API function allows developers to set the maximum sequence size for 
    simulated methods/attributes on a global level. Sample usage could be:
    
       setMaxSeqSize(200)
    
    Parameters: the new maximum length of sequences that will be returned 
    on any given simulated attribute/method invocation.
    
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
    methods/attributes on a global level. Most likely of little use 
    to end-users (i.e., just in place to give the framework access)
    
    Parameters: None
    
    Returns: the maximum length of sequences that will be returned 
    on any given simulated attribute/method invocation.    
    
    Raises: Nothing
    '''
    return MAX_SEQUENCE_SIZE
#-----------------------------------------------------------------
def setComponentMethod(comp_name,
                       meth_name,
                       code,
                       timeout=getStandardTimeout()):
    '''
    setComponentMethod is used to do exactly what its name implies - setup a
    simulated component method to behave in a certain manner. Using this method,
    one can:
    - change the return value of a simulated method
    - make a simulated method throw an exception
    - make a simulated method sleep for a desired amount of time before returning
    control
    
    Paramters:
        comp_name is the components name in string format. "FRIDGESIM1" 
        for example.
        
        meth_name is the name of the IDL method in string format that is 
        being simulated. "open" for example.
        
        code is a Python function or method to be executed 
        each time the simulated method is invoked. Whatever value this
        function returns is the same value the simulated method will 
        return. Likewise, if this function throws an exception; it will
        be as if the component method threw the exception
        
        timeout is the amount of time that should pass before the 
        method/attribute returns control
        
    Returns: Nothing
    
    Raises: Nothing
    '''
    #create the temporary dictionary
    temp_dict = { 'Value':   code,
                  'Timeout': float(timeout)}
    
    #store it globally
    getSimProxy(comp_name).api_handler.setMethod(meth_name, temp_dict)
#----------------------------------------------------------------------------
def supplyEventByType(component_name, channel_name, ifr_id):
    '''
    Supplies an event to a notification channel by the event's IFR ID type.
    
    Parameters:
        component_name - name of the component publisihing the event
        channel_name - name of the channel the event should be published
        on
        ifr_id - the interface repository ID of the IDL structure that 
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
#----------------------------------------------------------------------------
def supplyEventByInstance(component_name, channel_name, event_instance):
    '''
    Supplies an event to a notification channel.
    
    Parameters:
        component_name - name of the component publisihing the event
        channel_name - name of the channel the event should be published
        on
        event_instance - an instance of the IDL struct to publish
        
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
    interpreter exits. Should never be called from developer code!
    '''
    #disconnect all suppliers
    for sup in _SUPPLIERS_DICT.values():
        sup.disconnect()

#ensure all static module objects are cleaned up properly before exiting
register(__cleanUp)
