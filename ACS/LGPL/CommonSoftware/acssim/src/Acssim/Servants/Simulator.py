1# @(#) $Id: Simulator.py,v 1.37 2010/10/01 17:20:48 javarias Exp $
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
# "@(#) $Id: Simulator.py,v 1.37 2010/10/01 17:20:48 javarias Exp $"
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

# from Acspy.Util.BaciHelper   import addProperty
from Acssim.BaciHelper import addProperty
from ACSImpl.DevIO           import DevIO
from Acspy.Util.ACSCorba     import interfaceRepository

from Acssim.Corba.DynamicImplementation    import DynamicImplementation
from Acssim.Servants.Executor              import _execute
from Acssim.Goodies               import getSimProxy
from Acssim.Servants.Executor              import _executeDict
from Acssim.Corba.Generator             import tryCallbackParams
from Acssim.Corba.Generator             import getRandomEnum
from Acssim.Goodies               import addComponent
from Acssim.Goodies               import removeComponent
from Acssim.Goodies               import getCompLocalNS
from Acssim.Corba.Utilities                import getSuperIDs
from Acssim.Servants.Representations.BehaviorProxy import BehaviorProxy
from Acssim.Corba.EventDispatcher import EventDispatcher
from Acssim.Recorder import Recorder
#--GLOBALS---------------------------------------------------------------------
_DEBUG = 0
#------------------------------------------------------------------------------
class BaseSimulator(DynamicImplementation):
    '''
    '''
    def __init__(self, ir, name=None, parent=None):
        '''
        '''
        self.__ir = ir
        
        DynamicImplementation.__init__(self, ir)

        if name != None:
            self.__name = str(name)

        else:
            self.__name = ir

        self.parent = parent

        self.recorder = Recorder(self.__name)
        self.recorder.begin()

    #------------------------------------------------------------------------------
    def _get_name(self):
        return self.__name

    #------------------------------------------------------------------------------
    def cleanUp(self):
        self.recorder.end()
        
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
        
        meth_name = args[0]

        if len(args)>1:
            #create the list of args
            args = list(args[1:])
            #append the component reference
            args.append(self)
            new_args = args
            
        else:
            new_args = [self]
        self.recorder.record(meth_name, new_args[:-1])
        
        local_namespace = getCompLocalNS(self.__name)
        local_namespace['SELF'] = self    
        return _execute(self.__name,
                        meth_name,
                        new_args,
                        local_namespace)

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

#         # hook to call the initialize method from the Simulation Server
#         simServer = getSimProxy(self._get_name(), self.ir).server_handler
#         print 'Getting initialize method from server'
#         initMethodDict = simServer.getMethod('initialize2')
#         if initMethodDict != None:
#              print 'executing initialize method: ' + str(initMethodDict['Value'])
#              _executeDict(initMethodDict, [self], getCompLocalNS(self._get_name()))

        #add myself to the global lis
        addComponent(self._get_name(), self)

        # Create a BehaviorProxy, passing the interface repository id to the
        # constructor.
        proxy = getSimProxy(self._get_name(), self.ir)
                
        #possible for developers to configure an initialize method
        #for the simulated component.
        ns = getCompLocalNS(self._get_name())
        ns['SELF'] = self
        _execute(self._get_name(),
                 "initialize",
                 [self],
                 ns)
                
        #create the object used to dispatch events automatically         
        self.event_dispatcher = EventDispatcher(self)

        #handle attributes that should NOT be generically simulated
        self.__setupSpecialCases()

        
    #------------------------------------------------------------------------------
    def cleanUp(self):
        '''
        Overriden from baseclass.
        '''
        BaseSimulator.cleanUp(self)
        self.event_dispatcher.destroy()
        
        #possible for developers to configure cleanUp method
        #for the simulated component.
        _execute(self._get_name(),
                 "cleanUp",
                 [self],
                 getCompLocalNS(self._get_name()))
        
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
        if_list = getSuperIDs(self.ir)
        if_list.append(self.ir)
        simCDB = getSimProxy(self._get_name(), self.ir).cdb_handler
        simServer = getSimProxy(self._get_name(), self.ir).server_handler

        #IFR
        ir = interfaceRepository()

        #_executeXyz methods need an argument list
        args = [self]

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
            if (tempType=="ROstringSeq")or(tempType=="ROdoubleSeq")or(tempType=="RWdoubleSeq")or(tempType=="ROlongSeq")or(tempType=="RWlongSeq")or(tempType=="ROfloatSeq")or(tempType=="RWfloatSeq"):
                attrDict = simServer.getMethod(attribute.name)
                if attrDict == None: attrDict = simCDB.getMethod(attribute.name)
                if  attrDict!= None:
                    devio = _executeDict(attrDict, args, getCompLocalNS(self._get_name()))
                else:
                    devio = DevIO([])                    
                    
                addProperty(self, attribute.name, devio_ref=devio)
                continue

            #double BACI property
            elif (tempType=="ROdouble")or(tempType=="RWdouble"):
                attrDict = simServer.getMethod(attribute.name)
                if attrDict == None: attrDict = simCDB.getMethod(attribute.name)

                if  attrDict!= None:
                    devio = _executeDict(attrDict, args, getCompLocalNS(self._get_name()))
                else:
                    devio = DevIO(float(0))                    
                    
                addProperty(self, attribute.name, devio_ref=devio)
                continue

            #float BACI property
            elif (tempType=="ROfloat")or(tempType=="RWfloat"):
                attrDict = simServer.getMethod(attribute.name)
                if attrDict == None: attrDict = simCDB.getMethod(attribute.name)

                if  attrDict!= None:
                    devio = _executeDict(attrDict, args, getCompLocalNS(self._get_name()))
                else:
                    devio = DevIO(float(0))                    
                    
                addProperty(self, attribute.name, devio_ref=devio)
                continue


            #long BACI property
            elif (tempType=="ROlong")or(tempType=="RWlong"):
                attrDict = simServer.getMethod(attribute.name)
                if attrDict == None: attrDict = simCDB.getMethod(attribute.name)
                if  attrDict!= None:
                    devio = _executeDict(attrDict, args, getCompLocalNS(self._get_name()))
                else:
                    devio = DevIO(0)                    
                    
                addProperty(self, attribute.name, devio_ref=devio)
                continue

            #long (Python long also) BACI property
            elif (tempType=="ROpattern")or(tempType=="RWpattern")or(tempType=="ROlongLong")or(tempType=="RWlongLong")or(tempType=="ROuLongLong")or(tempType=="ROuLongLong"):
                attrDict = simServer.getMethod(attribute.name)
                if attrDict == None: attrDict = simCDB.getMethod(attribute.name)
                if  attrDict!= None:
                    devio = _executeDict(attrDict, args, getCompLocalNS(self._get_name()))
                else:
                    devio = DevIO(0L)                    
                    
                addProperty(self, attribute.name, devio_ref=devio)
                continue

            #string BACI property
            elif (tempType=="ROstring")or(tempType=="RWstring"):
                attrDict = simServer.getMethod(attribute.name)
                if attrDict == None: attrDict = simCDB.getMethod(attribute.name)
                if  attrDict!= None:
                    devio = _executeDict(attrDict, args, getCompLocalNS(self._get_name()))
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
                            attrDict = simServer.getMethod(attribute.name)
                            if attrDict == None: attrDict = simCDB.getMethod(attribute.name)
                            if  attrDict!= None:
                                devio = _executeDict(attrDict, args, getCompLocalNS(self._get_name()))
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

