# @(#) $Id: Executor.py,v 1.19 2010/10/01 17:20:48 javarias Exp $
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
# "@(#) $Id: Executor.py,v 1.19 2010/10/01 17:20:48 javarias Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/12/09  Created.
#------------------------------------------------------------------------------
'''
Module consists of code used to dynamically execute methods.
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from time    import sleep
from inspect import isfunction
from copy    import copy
from sys     import stdout
import types, code
from traceback import print_exc, print_tb
#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log       import getLogger

from Acssim.Goodies           import getSimProxy
from Acssim.Corba.Generator            import *

#--GLOBALS---------------------------------------------------------------------
LOGGER = getLogger("Acssim.Servants.Executor")
__revision__ = "@(#) $Id: Executor.py,v 1.19 2010/10/01 17:20:48 javarias Exp $"
#------------------------------------------------------------------------------
def _execute(comp_name, meth_name, args, local_ns):
    '''
    Given a component name/type as well as a method or CORBA attribute name,
    this function will simulate the execution of it. It first searches to see
    if the user has specified some code to be executed using the API; followed
    by a search of the CDB; and finally tries to generate a return value on the
    fly.

    Parameters:
    - comp_name is the name of the component being simulated
    - meth_name is the name of the method being simulated
    - local_ns is the local namespace
    
    Returns: Anything

    Raises: Anything
    '''     
    ret_val = _executeDict(getSimProxy(comp_name).getMethod(meth_name), 
                           args,
                           local_ns)
        
    LOGGER.logDebug(meth_name + " return value looks like:" + str(ret_val) + " of type:" + str(type(ret_val)))
    return ret_val
#------------------------------------------------------------------------------
def _executeDict(in_dict, args, local_ns):
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
    sleep(in_dict['Timeout'])
    return _executeList(in_dict['Value'], args, local_ns)
#------------------------------------------------------------------------------
def _executeList(code_list, args, local_ns):
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
    #_locals = local_ns
    #extend it
    _locals['parameters'] = args
    
    #well...through the API the developer could have just specified a function...
    if isfunction(code_list):
        #great, this makes things much easier for us
        try:
            #assume they want to see the arguments
            return code_list(args)
        except:
            print_exc()
            #fine...it's a parameterless function
            return code_list()
    elif type(code_list)==types.CodeType:
        
        exec code_list in globals(), _locals
        exec "joe = stringFunction(parameters)" in globals(), _locals
        return _locals['joe']

        
    else:
        #to make sure the last value isn't really popped off
        code = copy(code_list)
    
    #pop off the final return value
    final_val = code.pop()
    # execute all lines left in the list
    code_str = ""
    for line in code: code_str += line + '\n'
    try:
        exec code_str in globals(), _locals
    except:
        LOGGER.logWarning("Failed to execute the simulation code!:")
        print_exc()

#    (RH) I don't know why code was executed line by line here. This prevent to
#    execute multiline statements.
#    #execute each line still left in the list
#     for line in code:
#         try:
#             exec line in globals(), _locals
#         except:
#             LOGGER.logWarning("Failed to execute the '" + line + "' statement!")

    #if there's a raise in the last line, obviously the developer is trying
    #to simulate an exception. in this case the line should be exec'ed instead
    #of eval'ed
    if type(final_val)==str and final_val.count("raise ") == 1:
        #it's OK to do this without a try/except because the end-user is
        #almost certainly TRYING to throw an exception.
        exec final_val in globals(), _locals

    #there was a complaint about not seeing output from commandcenter so we do this:(
    stdout.flush()

    if type(final_val)==str and final_val.count("return ") == 1:
        return eval(final_val.replace("return ", ""), globals(), _locals)

    if type(final_val)==str and final_val == "return":
        return None
    
    #the final line is the actual return value which must be evaluated
    try:
        return eval(final_val, globals(), _locals)
    except:
        #this is only a debug message because the finalVal may already be a Python object
        LOGGER.logDebug("Failed to evaluate the '" + str(final_val) + "' statement!")
        return final_val
#---------------------------------
#the final thing we do is startup the GUI
#import Acssim.Servants.Widgets
