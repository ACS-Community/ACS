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
TODO LIST:
o Use better exceptions than the CORBA.NO_IMPLEMENT()
'''
#--REGULAR IMPORTS-------------------------------------------------------------


from random  import randrange
from random  import choice
from new     import instance
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
import CORBA
import ACSErr
import ACS
#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log       import getLogger
from Acspy.Common.TimeHelper import getTimeStamp
from Acssim.Goodies           import *
from Acssim.Corba.KnownAcsTypes import getKnownBaciType
from Acssim.Corba.KnownAcsTypes import tryCallbackParams
from Acssim.Corba.Utilities import getDefinition
from ACSSim import DataErrorEx
#--GLOBALS---------------------------------------------------------------------
LOGGER = getLogger("Acssim.Corba.Generator")
#------------------------------------------------------------------------------
def getRandomValue(typeCode, compRef):
    '''
    TODO:
    - complete me!
    '''
    #Determine the value type first. This is really just an enumeration for the
    #CORBA typecode
    valType = typeCode.kind()
    
    #--------------------------------------------------------------------------
    #First check to see if valType is a simple CORBA type we can immediately return.
    #If this is the case it's just returned...otherwise an exception is thrown and...
    try:
        return getRandomSimpleValue(valType)
    except:
        #we move on
        LOGGER.logTrace("Not a simple CORBA Type:" + str(valType))
    #--------------------------------------------------------------------------
    #if it's a sequence, array, etc...this is a very special case:
    try:
        return getRandomTuple(typeCode, compRef, valType)
    except CORBA.NO_IMPLEMENT, e:
        raise e
    except:
        LOGGER.logTrace("Wasn't an alias-related type:" + str(valType))
    #--------------------------------------------------------------------------
    if valType == CORBA.tk_objref:
        LOGGER.logTrace("CORBA object:" + str(valType))
        return getRandomCORBAObject(typeCode, compRef)
    
    elif valType == CORBA.tk_enum:
        return getRandomEnum(typeCode)
    
    elif valType == CORBA.tk_alias:
        return getRandomValue(typeCode.content_type(), compRef)
    
    elif valType == CORBA.tk_null:
        LOGGER.logTrace("Null return value")
        return None
    #--------------------------------------------------------------------------
    elif valType == CORBA.tk_struct:
        return getRandomStruct(typeCode, compRef)
    #--------------------------------------------------------------------------
    else:
        return getUnsupported(typeCode, compRef)
#------------------------------------------------------------------------------
def getUnsupported(typeCode, compRef):
    '''
    Helper function
    '''
    valType = typeCode.kind()
    LOGGER.logCritical(str(valType) + " not yet supported")
    raise CORBA.NO_IMPLEMENT()
    
    #if valType == CORBA.tk_Principal:
    #    raise CORBA.NO_IMPLEMENT()
    #
    #elif valType == CORBA.tk_TypeCode:
    #    raise CORBA.NO_IMPLEMENT()
    #
    #elif valType == CORBA.tk_abstract_interface:
    #    raise CORBA.NO_IMPLEMENT()
    #
    #elif valType == CORBA.tk_any:
    #    raise CORBA.NO_IMPLEMENT()
    #
    #elif valType == CORBA.tk_except:
    #    raise CORBA.NO_IMPLEMENT()
    #
    #elif valType == CORBA.tk_fixed:
    #    raise CORBA.NO_IMPLEMENT()
    #
    #elif valType == CORBA.tk_local_interface:
    #    raise CORBA.NO_IMPLEMENT()
    #
    #elif valType == CORBA.tk_native:
    #    raise CORBA.NO_IMPLEMENT()
    #
    #elif valType == CORBA.tk_union:
    #    raise CORBA.NO_IMPLEMENT()
    #
    #elif valType == CORBA.tk_value:
    #    raise CORBA.NO_IMPLEMENT()
    #
    #elif valType == CORBA.tk_wchar:
    #    raise CORBA.NO_IMPLEMENT()
    #
    #elif valType == CORBA.tk_wstring:
    #    raise CORBA.NO_IMPLEMENT()
    #
    #else:
    #    raise CORBA.NO_IMPLEMENT()
    
#------------------------------------------------------------------------------
def getRandomStruct(typeCode, compRef):
    '''
    Helper function
    '''
    structDef = getDefinition(typeCode.id())
        
    try:
        return getKnownBaciType(structDef._get_id())
    except:
        pass
    
    #determine which namespace the struct is in...
    #changes 'IDL:alma/someMod/.../struct:1.0" to 
    # [ 'someMod', ...,'struct' ]
    nameHierarchy = structDef._get_id().split(':')[1].split('/')[1:]
    #Just the 'struct' part...
    structName = nameHierarchy.pop()
    moduleName = nameHierarchy.pop(0)
    LOGGER.logTrace("module=" + moduleName
                        + "; hierarchy=" + str(nameHierarchy)
                        + "; struct="    + structName)        
    #import the top module
    tGlobals = {}
    tLocals = {}
    #module object where the struct is contained
    container = __import__(moduleName, tGlobals, tLocals, [])
    if container == None:
        msg =  "import of module \'" + moduleName + "\' failed"
        LOGGER.logCritical(msg)
        raise CORBA.NO_IMPLEMENT(msg) 
                 
    # Now navigate down the nested hierarchy of objects
    for h in nameHierarchy:
        previousContainer = container
        container = container.__dict__.get(h) 
        if container == None:
            msg =  "Name \'" + h + "\' not found in " + str( previousContainer)
            LOGGER.logCritical(msg)
            raise CORBA.NO_IMPLEMENT(msg)                   
   
    #class object for the struct
    tClass = container.__dict__.get(structName)
    if tClass == None:
        msg =  "Could not get structure \'" + structName + "\' from " \
                    + str(container)
        LOGGER.logCritical(msg)
        raise CORBA.NO_IMPLEMENT(msg) 

    #create an instance of the struct using a kooky Python mechanism.
    retVal = instance(tClass)

    #populate the fields of the struct using the IFR
    for member in structDef._get_members():
        LOGGER.logTrace("Adding a member variable for: " + 
                         str(member.name))
        retVal.__dict__[member.name] = getRandomValue(member.type_def._get_type(), 
                                                      compRef)
    
    return retVal
#------------------------------------------------------------------------------
def getRandomSimpleValue(valType):
    '''
    Helper function returns a random value of (typecode) valType or throws an
    exception if valType is not a simple type. Sample usage could be:

       getRandomSimpleValue(CORBA.tk_boolean)

    Parameters: valType is a typeCode

    Returns: a random value of type typeCode

    Raises: an exception if valType is not really a simple CORBA type
    '''
    if valType == CORBA.tk_boolean:
        #randomly returns 0 or 1
        retVal = randrange(0,100) % 2
    
    elif valType == CORBA.tk_char:
        #randomly returns some predetermined character
        retVal = choice(getCHARS())
    
    elif valType == CORBA.tk_octet:
        #returns 0-255
        retVal = int(randrange(0,256))
    
    elif valType == CORBA.tk_short:
        #returns a random short
        retVal = int(randrange(-(2**15), (2**15) - 1))
        
    elif valType == CORBA.tk_ushort:
        #returns a random unsigned short
        retVal = int(randrange(0, (2**16) - 1))
    
    elif valType == CORBA.tk_long:
        #returns a random long
        retVal = int(randrange(-(2**31), (2**31) - 1))
        
    elif valType == CORBA.tk_ulong:
        #returns a random unsigned long
        retVal = long(randrange(0, (2**32) - 1))

    elif valType == CORBA.tk_longlong:
        #returns a random long long
        retVal = long(randrange(-(2**63), (2**63) - 1))
        
    elif valType == CORBA.tk_ulonglong:
        #returns a random unsigned long long
        retVal = long(randrange(0, (2**64) - 1))
    
    elif valType == CORBA.tk_float:
        #DWF-make this really go through a float's entire range of values
        retVal = eval(str(getRandomSimpleValue(CORBA.tk_short)) + '.' +
                    str(getRandomSimpleValue(CORBA.tk_octet)))
    
    elif valType == CORBA.tk_double:
        #returns a random float plus more digits
        retVal =  getRandomSimpleValue(CORBA.tk_float) * 1000.0

    elif valType == CORBA.tk_longdouble:
        #DWF-CORBA IDL->Py mapping specifies a CORBA.long_double() object
        #to be used in cases like these. Unfortunately omniORB does not
        #currently support it.
        LOGGER.logDebug("long doubles not supported by omniORBPy")
        return 3.1415926535897931

    elif valType == CORBA.tk_string:
        retVal = "..." + str(getRandomSimpleValue(CORBA.tk_double)) + "..."

    elif valType == CORBA.tk_void:
        retVal = None
        
    elif valType == CORBA.tk_null:
        retVal = None
        
    else:
        raise CORBA.NO_IMPLEMENT()

    return retVal
#------------------------------------------------------------------------------
def getRandomCORBAObject(typeCode, compRef):
    '''
    Helper function returns a random CORBA object.

    Parameters:
    - typeCode is the typecode of the object to be created
    - compRef is an ACSComponent which will activate the newly created
    CORBA object

    Returns: the newly created CORBA object

    Raises: ???
    '''
    #has to be in this method unfortunately to avoid cyclic dependencies.
    from Acssim.Servants.Simulator import BaseSimulator
    LOGGER.logTrace("Creating a random CORBA object")
    
    try:
        #for methods...
        irLabel = typeCode.id()
    except:
        #...and attributes
        irLabel = typeCode.type.id()
    
    objName = irLabel.replace(":1.0","").replace("IDL:", "").replace("/", ".")
    
    LOGGER.logTrace("CORBA object type is:" + str(irLabel))
    LOGGER.logTrace("CORBA object name is:" + str(objName))
    LOGGER.logTrace("Component Reference: " + str(compRef))

    #create a new simulated object
    offshootName = compRef._get_name()
    offshootName = offshootName + "__OFFSHOOT__" + irLabel
    if 'activateOffShoot' in dir(compRef):
        # In this case compRef is a simulated component (Simulator class)
        parentComp = compRef
    else:
        # otherwise compRef is itself an Offshoot
        parentComp = compRef.parent
    retVal = BaseSimulator(irLabel, offshootName, parentComp)
    addComponent(offshootName, retVal)

    # activate it as a CORBA object...        
    retVal = parentComp.activateOffShoot(retVal)

    return retVal
#------------------------------------------------------------------------------
def getRandomEnum(typeCode):
    '''
    Helper function returns a random enumueration based on the typecode 
    provided.

    Parameters: type code is quite literally a CORBA typecode

    Returns: a random enumeration of the type specified by typeCode

    Raises: ???
    '''
    LOGGER.logTrace("Dealing with an enum")
    
    #Determine the value type first. This is really just an enumeration 
    #for the CORBA typecode
    enumDef =getDefinition(typeCode.id())
    
    #determine which Python package the enum is in...
    #changes 'IDL:alma/someMod/.../enumeration:1.0" to [ 'someMod', 
    #..., 'enumeration' ]
    enumName = enumDef._get_id().split(':')[1].split('/')[1:]
    modName = enumName[0]

    #convert the list to a stringified Python package structure which can
    #be used with eval
    enumName = reduce((lambda x, y : str(x) + '.' + str(y)), enumName)

    LOGGER.logTrace("enum name is:" + str(enumName))
    LOGGER.logTrace("enum module is:" + str(modName))
    
    #now comes the complicated part...importing the correct CORBA stub
    #without polluting the local namespace...
    tGlobals = {}
    tLocals  = {}
    exec "from random import choice" in tGlobals, tLocals
    exec "import " + modName in tGlobals, tLocals
    
    #with any luck, we should now be able to return the enumeration value
    #without any problems
    retVal = eval("choice(" + enumName + "._items)", tGlobals, tLocals)
    LOGGER.logTrace("enum return value is:" + str(retVal))
    return retVal
#------------------------------------------------------------------------------
def getRandomTuple(typeCode, compRef, valType):
    '''
    Helper function returns a random Python tuple for CORBA sequences and array
    types.

    Parameters:
    - type code is quite literally a CORBA typecode
    - compRef is a reference to a component used to activated IDL OffShoot 
    interfaces
    - valType is the value type of the random value we are trying to get

    Returns: a random enumeration of the type specified by typeCode

    Raises: an (unknown) exception if the typecode does not really specify a
    list type to be returned or a CORBA.NO_IMPLEMENT if we have not gotten
    around to supporting the specific typecode yet (e.g., value boxes).
    '''
    #if this next block does not throw an exception...
    realValType  = typeCode.content_type().kind()
    realTypeCode = getDefinition(typeCode.id())._get_original_type_def()._get_element_type()
    
    #we're really dealing with a sequence, array, value_box, or an alias
    LOGGER.logTrace("Dealing with a sequence, array, value_box, or alias:" +
                     str(realValType) + " " + str(realTypeCode))

    #Sequence
    if realValType == CORBA.tk_sequence:
        LOGGER.logTrace("Dealing with a sequence.")
        retVal = []
        for i in range(0,randrange(0, getMaxSeqSize())):
            retVal.append(getRandomValue(realTypeCode, compRef))
        #Sequences of octects and characters must be handled specially in 
        #Python
        if realTypeCode.kind()==CORBA.tk_octet or realTypeCode.kind()==CORBA.tk_char:
            LOGGER.logTrace("Dealing with a sequence of characters/octets.")
            return reduce((lambda x, y : str(x) + str(y)), retVal)
        else:
            return tuple(retVal)

    #Array
    #DWF-take into consideration multi-dimensional arrays
    elif realValType == CORBA.tk_array:
        size = getDefinition(typeCode.id())._get_original_type_def()._get_type().length()
        
        LOGGER.logTrace("Dealing with an array of size:" + str(size))
        retVal = []
        for i in range(0,size):
            retVal.append(getRandomValue(realTypeCode, compRef))
            
        #Sequences of octects and characters must be handled specially in
        #Python
        if realTypeCode.kind()==CORBA.tk_octet or realTypeCode.kind()==CORBA.tk_char:
            LOGGER.logTrace("Dealing with an array of characters/octets.")
            return reduce((lambda x, y : str(x) + str(y)), retVal)
        else:
            return tuple(retVal)
            
    #Value Box
    elif realValType == CORBA.tk_value_box:
        LOGGER.logCritical("value_box not yet supported")
        raise CORBA.NO_IMPLEMENT()

    #If this block of code can ever really be executed in practice...I'll 
    #be amazed!
    elif valType == CORBA.tk_alias:
        return getRandomValue(realTypeCode, compRef)
#-----------------------------------------------------------------------
