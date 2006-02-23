# @(#) $Id: Simulator.py,v 1.21 2005/11/23 05:58:03 dfugate Exp $
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
# "@(#) $Id: Simulator.py,v 1.21 2005/11/23 05:58:03 dfugate Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/12/09  Created.
#------------------------------------------------------------------------------
'''
This module contains the implementation of a generic IDL interface simulator
component.  Currently broken.

TODO LIST:
- all
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
import CORBA
import omniORB
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.ContainerServices        import ContainerServices
from Acspy.Servants.ComponentLifecycle       import ComponentLifecycle
from Acspy.Servants.CharacteristicComponent  import CharacteristicComponent

from Acspy.Util.BaciHelper   import addProperty
from ACSImpl.DevIO           import DevIO
from Acspy.Util.ACSCorba     import interfaceRepository

from Acssim.Corba.DynamicImplementation    import DynamicImplementation
from Acssim.Servants.Executor              import _execute
from Acssim.Servants.SimulatedCDBEntry     import SimulatedCDBEntry
from Acssim.Servants.Executor              import _executeDict
from Acssim.Servants.Generator             import tryCallbackParams
from Acssim.Servants.Generator             import getRandomEnum
from Acssim.Servants.Components            import addComponent
from Acssim.Servants.Components            import removeComponent
#--GLOBALS---------------------------------------------------------------------
_DEBUG = 0
#------------------------------------------------------------------------------
class BaseSimulator(DynamicImplementation):
    '''
    '''
    def __init__(self, ir, name=None):
        '''
        '''
        self.__ir = ir
        
        DynamicImplementation.__init__(self, ir)

        if name != None:
            self.__name = str(name)

        else:
            self.__name = ir
    #------------------------------------------------------------------------------
    def invoke(self, args, moreargs):
        '''
        This method has all calls to CORBA operations forwarded to it.  Definitly
        needs to be overriden in subclasses.

        Parameters:
        - args is a tuple of arguments...args[0] is the calling methods name and
        everything that follows were parameters to the calling method.
        - moreargs is a dictionary of yet more arguments

        Returns: Anything

        Raises: Anything
        '''
        if _DEBUG == 1:
            print self.__name, " - DynamicImplementation.invoke(", args, moreargs, ")"

        #simulate doing something with callbacks...
        tryCallbackParams(args[1:], self)
            
        return _execute(self.__name,
                        self.__ir,
                        args[0],
                        self,
                        args[1:])

#------------------------------------------------------------------------------
class Simulator(CharacteristicComponent,  #Base IDL interface
                BaseSimulator, #CORBA stubs for IDL interface
                ContainerServices,  #Developer niceties
                ComponentLifecycle):  #HLA stuff
    '''
    '''
    #------------------------------------------------------------------------------
    def __init__(self):
        '''
        Just call superclass constructors here.
        '''
        CharacteristicComponent.__init__(self)
        ContainerServices.__init__(self)
        return
    #------------------------------------------------------------------------------
    #--Override ComponentLifecycle methods-----------------------------------------
    #------------------------------------------------------------------------------
    def initialize (self):
        '''
        Overriden from baseclass.
        '''
        #we can finally access the IR location and call BaseSimulator's
        #constructor...dynamically changing this object's inheritance
        BaseSimulator.__init__(self, self.ir, self._get_name())

        #handle attributes that should NOT be generically simulated
        self.__setupSpecialCases()

        #add myself to the global lis
        addComponent(self._get_name(), self)

        if _DEBUG == 1:
            print "****************"
            print dir(self)
            print self.__class__.__bases__
            print "****************"
    #------------------------------------------------------------------------------
    def cleanUp(self):
        '''
        Overriden from baseclass.
        '''
        ComponentLifecycle.cleanUp(self)
        removeComponent(self._get_name())
    #------------------------------------------------------------------------------
    def __setupSpecialCases(self):
        '''
        Helper method designed to handle special cases that we do not necessarily
        want being 100% simulated like BACI properties, callbacks, etc.
        '''
        #look in the CDB for instructions on how to setup the special cases. This
        #is mainly used to see if the end-user has specified some devIO class to
        #be used with a BACI property.
        simCDB = SimulatedCDBEntry(self._get_name())

        #IFR
        ir = interfaceRepository()

        #_executeXyz methods need an argument list
        args = []

        #get an interface description for this component
        interf = ir.lookup_id(self._NP_RepositoryId)
        interf = interf._narrow(CORBA.InterfaceDef)
        interf = interf.describe_interface()

        #use the IFR descrip to go searching for BACI properties
        for attribute in interf.attributes:
            
            #if the typecode is NOT an object reference it cannot be a BACI property.
            #that implies it's OK to skip
            if not isinstance(attribute.type, omniORB.tcInternal.TypeCode_objref):
                continue

            #save the short version of the attribute's ID (i.e., ROdouble)
            tempType = attribute.type.id().split(":")[1].split("/").pop()

            #sequence BACI property
            if (tempType=="ROstringSeq")or(tempType=="ROdoubleSeq")or(tempType=="RWdoubleSeq")or(tempType=="ROlongSeq")or(tempType=="RWlongSeq"):
                cdbAttrDict = simCDB.getMethod(attribute.name)
                if  cdbAttrDict!= None:
                    devio = _executeDict(cdbAttrDict, args)
                else:
                    devio = DevIO([])                    
                    
                addProperty(self, attribute.name, devio_ref=devio)
                continue

            #double BACI property
            elif (tempType=="ROdouble")or(tempType=="RWdouble"):
                cdbAttrDict = simCDB.getMethod(attribute.name)
                if  cdbAttrDict!= None:
                    devio = _executeDict(cdbAttrDict, args)
                else:
                    devio = DevIO(float(0))                    
                    
                addProperty(self, attribute.name, devio_ref=devio)
                continue

            #long BACI property
            elif (tempType=="ROlong")or(tempType=="RWlong"):
                cdbAttrDict = simCDB.getMethod(attribute.name)
                if  cdbAttrDict!= None:
                    devio = _executeDict(cdbAttrDict, args)
                else:
                    devio = DevIO(0)                    
                    
                addProperty(self, attribute.name, devio_ref=devio)
                continue

            #long (Python long also) BACI property
            elif (tempType=="ROpattern")or(tempType=="RWpattern")or(tempType=="ROlongLong")or(tempType=="RWlongLong")or(tempType=="ROuLongLong")or(tempType=="ROuLongLong"):
                cdbAttrDict = simCDB.getMethod(attribute.name)
                if  cdbAttrDict!= None:
                    devio = _executeDict(cdbAttrDict, args)
                else:
                    devio = DevIO(0L)                    
                    
                addProperty(self, attribute.name, devio_ref=devio)
                continue

            #string BACI property
            elif (tempType=="ROstring")or(tempType=="RWstring"):
                cdbAttrDict = simCDB.getMethod(attribute.name)
                if  cdbAttrDict!= None:
                    devio = _executeDict(cdbAttrDict, args)
                else:
                    devio = DevIO("")                    
                    
                addProperty(self, attribute.name, devio_ref=devio)
                continue

            else:
                
                ifrName = attribute.type.id()
                
                try:
                    #get an interface description for this property
                    tIfr = ir.lookup_id(ifrName)
                    tIfr = tIfr._narrow(CORBA.InterfaceDef)
                    tIfr = tIfr.describe_interface()

                    for tAttr in tIfr.attributes:
                        #check if it's a default_value AND an enum!
                        if (tAttr.name=="default_value") and (tAttr.type.kind()==CORBA.tk_enum):
                            #GREAT! It's completely safe to add!
                            cdbAttrDict = simCDB.getMethod(attribute.name)
                            if  cdbAttrDict!= None:
                                devio = _executeDict(cdbAttrDict, args)
                            else:
                                devio = DevIO(getRandomEnum(tAttr.type))
                            addProperty(self, attribute.name, devio_ref=devio)
                                              
                            break

                except:
                    print_exc()
                    continue
    #------------------------------------------------------------------------------
    #--Implementation of IDL methods-----------------------------------------------
    #------------------------------------------------------------------------------



#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Creating an object"
    g = Simulator()
    print "Done..."

