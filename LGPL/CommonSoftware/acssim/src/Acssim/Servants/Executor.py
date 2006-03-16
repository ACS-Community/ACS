# @(#) $Id: Executor.py,v 1.6 2006/03/16 00:00:59 dfugate Exp $
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
# "@(#) $Id: Executor.py,v 1.6 2006/03/16 00:00:59 dfugate Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/12/09  Created.
#------------------------------------------------------------------------------
'''

TODO LIST:

'''
#--REGULAR IMPORTS-------------------------------------------------------------
from time    import sleep
from inspect import isfunction
from copy    import copy
from sys     import stdout
#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log       import getLogger

from Acssim.Servants.Goodies           import *
from Acssim.Servants.Generator         import *
from Acssim.Servants.SimulatedEntry    import SimulatedEntry
from Acssim.Servants.SimulatedCDBEntry import SimulatedCDBEntry
from Acssim.Servants.DynamicEntry      import DynamicEntry

from Acssim.Corba.Utilities            import getSuperIDs
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
def _execute(compName, compType, methName, compRef, args, local_ns):
    '''
    Given a component name/type as well as a method or CORBA attribute name,
    this function will simulate the execution of it. It first searches to see
    if the user has specified some code to be executed using the API; followed
    by a search of the CDB; and finally tries to generate a return value on the
    fly.

    Parameters:
    - compName is the name of the component being simulated
    - compType is the IDL type of the component being simulated
    - methName is the name of the method being simulated
    - compRef just needs to be a real component we use to gain access to the
    ContainerServices object. It must provide an "activateOffShoot" method.
    - local_ns is the local namespace
    
    Returns: Anything

    Raises: Anything
    '''
    global API, CDB, GEN

    #first check to see if this component has an entry
    if not getComponentsDict().has_key(compName):
        #if not, create it
        if_list = getSuperIDs(compType)
        if_list.append(compType)
        getComponentsDict()[compName] = { API:SimulatedEntry(compName),
                                  CDB:SimulatedCDBEntry(compName,
                                                        if_list),
                                  GEN:DynamicEntry(compName, 
                                                   compType)}
        
    #if the end-user has setup some specific code to be executed using the
    #API...
    if getComponentsDict()[compName][API].getMethod(methName) != None:
        #...execute it directly
        getLogger("Acssim.Servants.Executor").logDebug("Executing the '" + methName + "' method of the '" +
                             compName + "' simulated component using the API.")
        return _executeDict(getComponentsDict()[compName][API].getMethod(methName), 
                            args,
                            local_ns)
    #if the end-user has setup a CDB entry for the method...
    elif getComponentsDict()[compName][CDB].getMethod(methName) != None:
        #...execute it directly
        getLogger("Acssim.Servants.Executor").logDebug("Executing the '" + methName + "' method of the '" +
                             compName + "' simulated component using the CDB.")
        return _executeDict(getComponentsDict()[compName][CDB].getMethod(methName), 
                            args,
                            local_ns)
    #damn...everything needs to be generated on the fly
    else:
        getLogger("Acssim.Servants.Executor").logDebug("Executing the '" + methName + "' method of the '" +
                             compName + "' simulated component on the fly.")
        #...execute it directly
        retVal = _executeDict(getComponentsDict()[compName][GEN].getMethod(methName, compRef), 
                              args,
                              local_ns)
        
        getLogger("Acssim.Servants.Executor").logDebug(methName + " return value looks like:" + str(retVal) + " of type:" + str(type(retVal)))
        
        return retVal
#------------------------------------------------------------------------------
def _executeDict(dict, args, local_ns):
    '''
    Given a Python dictionary describing a simulated method, this function
    essentially just simulates execution of it. Really all this function does
    is sleeps for a given amount of time and ivokes another function to actually
    execute the code contained in the dictionary.

    Parameters:
    - dict is a dictionary defining the IDL method/attribute. Typically this
    defines at least a couple of keys including "Timeout" and "Value"
    - local_ns is the local namespace
    
    Returns: Anything

    Raises: Anything
    '''
    sleep(dict['Timeout'])
    return _executeList(dict['Value'], args, local_ns)
#------------------------------------------------------------------------------
def _executeList(codeList, args, local_ns):
    '''
    Given a list where each element consists of a stringified Python statement,
    each line will be executed except for the very last line. If the final line
    contains the string "raise", this line will also be executed directly.
    Otherwise it is assumed the line should be executed by the Python eval function
    and the result of this will be returned.

    Parameters:
    - codeList is a Python List of strings which are executed one after another
    without doing any sanity checks. The last string of this list should either
    contain a "raise ..." statement where the exception being thrown is derived
    from an IDL exception OR the last string should represent some return value
    to be evaluated by the Python "eval" function. Additionally, the last "string"
    can actually be a Python object which will simply be returned instead of
    being evaluated by the "eval" function. A sample value for code
    could be [ "import CORBA", "CORBA.TRUE" ] or [ "import ACSExceptionCommon",
    "raise ACSExceptionCommon.CommonExImpl()" ]
    - local_ns is the local namespace

    Returns: the last string in code evaluated by the Python eval statement if
    it does not contain the substring "raise " within it. If the last "string"
    is not really a string but instead some other Python object, this is returned
    instead.

    Raises: the last string in code if it does contain the substring "raise "
    within it.
    '''
    #force a copy of it
    _locals = copy(local_ns)
    #extend it
    _locals['parameters'] = args
    
    #well...through the API the developer could have just specified a function...
    if isfunction(codeList):
        #great, this makes things much easier for us
        try:
            #assume they want to see the arguments
            return codeList(getComponentsDict(), _locals)
        except:
            #fine...it's a parameterless function
            return codeList()
    else:
        #to make sure the last value isn't really popped off
        code = copy(codeList)
    
    #pop off the final return value
    finalVal = code.pop()
    
    #execute each line still left in the list
    for line in code:
        try:
            exec line in getComponentsDict(), _locals
        except:
            getLogger("Acssim.Servants.Executor").logWarning("Failed to execute the '" + line + "' statement!")

    #if there's a raise in the last line, obviously the developer is trying
    #to simulate an exception. in this case the line should be exec'ed instead
    #of eval'ed
    if type(finalVal)==str and finalVal.count("raise ") == 1:
        #it's OK to do this without a try/except because the end-user is
        #almost certainly TRYING to throw an exception.
        exec finalVal in getComponentsDict(), _locals

    #there was a complaint about not seeing output from commandcenter so we do this:(
    stdout.flush()
    
    #the final line is the actual return value which must be evaluated
    try:
        return eval(finalVal, getComponentsDict(), _locals)
    except:
        #this is only a debug message because the finalVal may already be a Python object
        getLogger("Acssim.Servants.Executor").logDebug("Failed to evaluate the '" + str(finalVal) + "' statement!")
        return finalVal
#---------------------------------
#the final thing we do is startup the GUI
import Acssim.Servants.SimGUI
