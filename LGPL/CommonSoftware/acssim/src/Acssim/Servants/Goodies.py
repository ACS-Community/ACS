# @(#) $Id: Goodies.py,v 1.1 2005/11/23 05:58:03 dfugate Exp $
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
# "@(#) $Id: Goodies.py,v 1.1 2005/11/23 05:58:03 dfugate Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/12/09  Created.
#------------------------------------------------------------------------------
'''

TODO LIST

'''
#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------
import omniORB
import CORBA
#--ACS Imports-----------------------------------------------------------------
from Acspy.Util.ACSCorba        import interfaceRepository
from Acssim.Servants.Components import getComponent as getComponent
#--GLOBALS---------------------------------------------------------------------
_COMPONENTS = {}

API = 'API'
CDB = 'CDB'
GEN = 'GEN'

#Standard timeout for simulated methods to wait before returning control
STD_TIMEOUT = 0.2

#Probability of an exception being thrown for any given simulated method.
EXCEPTION_CHANCE = 0.01

#Maximum size of a simulated sequence
MAX_SEQUENCE_SIZE = 15

#use the IFR to get a description of this class
#DWF-really this should be done from ACSCorba!
omniORB.importIRStubs()
IR = interfaceRepository()
IR = IR._narrow(CORBA.Repository)

#CHARS is a list full of random charcters that will be used
CHARS = []
for character in "abcdefghijklmnopqrstuvwxyz _":
    CHARS.append(character)

#storage for "static data" that can be shared between components
_GLOBALS = {}

#---------------------------------------------------------------------
def getComponentsDict():
    global _COMPONENTS
    return _COMPONENTS
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
    if name!="addGlobalData" and name!="removeGlobalData":
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
    if name!="addGlobalData" and name!="removeGlobalData":
        del _GLOBALS[str(name)]
    else:
        raise "Cannot remove 'addGlobalData' or 'removeGlobalData'"  
    return

#add the functions to _GLOBALS
_GLOBALS[addGlobalData.__name__] = addGlobalData
_GLOBALS[removeGlobalData.__name__] = removeGlobalData

#------------------------------------------------------------------------------
def setCHARS(newCharList):
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
    CHARS=newCharList
    return
#------------------------------------------------------------------------------
def setStandardTimeout(newTimeout):
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
    STD_TIMEOUT=float(newTimeout)
    return
#------------------------------------------------------------------------------
def setExceptProb(newProb):
    '''
    NOT CURRENTLY USED!

    Parameters: the new probability (a float) of an exception occuring on any given
    simulated attribute/method invocation.

    Returns: Nothing

    Raises: Nothing
    '''
    global EXCEPTION_CHANCE
    EXCEPTION_CHANCE=newProb
    return
#------------------------------------------------------------------------------
def setMaxSeqSize(newSequenceSize):
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
    MAX_SEQUENCE_SIZE=newSequenceSize
    return
