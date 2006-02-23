# @(#) $Id: DynamicImplementation.py,v 1.4 2004/08/23 22:01:42 dfugate Exp $
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
# "@(#) $Id: DynamicImplementation.py,v 1.4 2004/08/23 22:01:42 dfugate Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/12/09  Created.
#------------------------------------------------------------------------------
'''
This module contains the implementation of a generic IDL implementation class
specified in the Python Language Mapping, but not implemented by omniORB.

TODO LIST:
- parse the irLoc (IR ID) in a more generic, non-ALMA way.
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from new    import instancemethod
import omniORB

#--CORBA STUBS-----------------------------------------------------------------
import CORBA

#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
def _mergeClasses(completeDict, newClass):
    '''
    A helper function designed to merge the method and attributes of classes.
    '''
    #first just copy the new class's attributes and methods if applicable
    tKeys = newClass.__dict__.keys()
    for key in tKeys:
        if not completeDict.has_key(key):
            completeDict[key] = newClass.__dict__[key]

    #unfortunately we must now look at base classes
    for base in newClass.__bases__:
        completeDict = _mergeClasses(completeDict, base)

    return completeDict
        
#------------------------------------------------------------------------------
def _createMethodImplementation(objRef, methodName):
    '''
    A helper function designed to dynamically add methods to a Python object.
    Really all this does is pass the methodName and parameters to another method
    the object should have implemented: invoke.

    Parameters:
    objRef - a reference to an instance of DynamicImplementation
    methodName - the name of the method we wish to create

    Returns: Nothing

    Raises: ???
    '''
    def genericFunction(*args, **moreargs):
        '''
        Generic implementation of an IDL method.  Catches the invocation
        and passes it to the invoke method.

        Parameters:
        args - a tuple of arguments
        moreargs - a dictionary of arguments

        Returns: whatever invoke returns

        Raises: whatever invoke raises
        '''
        #corba specifies the operation's name should be the first parameter
        #of the invoke method
        args = list(args)
        args.insert(1, methodName)

        #CORBA specs say context object should be None
        if not moreargs.has_key('context'):
            moreargs['context'] = None
        #return whatever invoke returns
        return objRef.invoke(tuple(args[1:]), moreargs)
    
    #register the newly created method
    try:
        #if an exception on this occurs...the method does not already exist
        temp = callable(getattr(objRef, methodName))
        del temp
    except:
        #add the method.
        objRef.__dict__[methodName] = instancemethod(genericFunction, objRef)
    return

#------------------------------------------------------------------------------
class DynamicImplementation:
    '''
    A complete implementation of the OMG-defined DynamicImplementation class.
    '''
    def __init__(self, irLoc=None):
        '''
        Standard constructor dynamically changes this objects inheritance.
        Also implements all IDL methods/attributes on the fly which just pass
        the call to the invoke method (which should be overriden in subclasses!!!).

        Parameters:
        - irLoc is the interface repository ID of the IDL interface we want to
        inherit from.  If this is None, it is assumed _get_interface() has
        been defined in a subclass (as-per the CORBA specs).

        Returns: Nothing

        Raises: ???
        '''
        #set member variables
        if irLoc == None:
            #assume the _get_interface method is implemented in a subclass...
            self.__irLoc = self._get_interface()
            self.__irLoc = self.__irLoc._narrow(CORBA.InterfaceDef)
            self.__irLoc = self.__irLoc._get_id()
        else:
            self.__irLoc = irLoc
            
        self.__realModule = None
        self.__realClass = None
        
        #create the POA class first...
        self.__realClass =  self.__irLoc.split(':')[1].split('/').pop()  #get interface name
        self.__realModule = self.__irLoc.split(':')[1].split('/')[1] + "__POA"  #"IDL:alma/acspytest/PyTest:1.0" becomes "acspytest__POA"
        self.__realModule = __import__(self.__realModule, globals(), locals(), [self.__realClass]) #get module
        self.__realClass = self.__realModule.__dict__.get(self.__realClass) #get class

        #copy it's class locally thereby converting DynamicImplementation
        #into the POA object!
        self.__dict__ = _mergeClasses(self.__dict__, self.__realClass)
        
        tList = list(self.__class__.__bases__)
        try:
            #OK for subclasses...
            tList.insert(tList.index(DynamicImplementation), self.__realClass)
        except:
            #should really never be the case...
            tList.insert(0, self.__realClass)
        self.__class__.__bases__ = tuple(tList)
        
            
        #except:
        #    #should really never be the case...
        #    print "Something went terribly awry in DynamicImplementation..."
        #    tList.insert(0, self.__realClass)

        #now that all the underlying infrastructure is in place, we can finally use
        #CORBA introspection to start adding methods!
        #get the interface repository.
        omniORB.importIRStubs()  # Make sure IR stubs are loaded
        #have to do this because omniORB does not provide an IR at present
        if 0:
            ir = omniORB.orb.resolve_initial_references("InterfaceRepository")
        else:
            from Acspy.Util.ACSCorba import interfaceRepository
            ir = interfaceRepository()

        ir = ir._narrow(CORBA.Repository)
        if ir is None:
            raise CORBA.INTF_REPOS(omniORB.INTF_REPOS_NotAvailable,
                                   CORBA.COMPLETED_NO)
        #assume this can be found in a subclass as well.
        self.__interf = ir.lookup_id(self._NP_RepositoryId)
        self.__interf = self.__interf._narrow(CORBA.InterfaceDef)
        self.__interf = self.__interf.describe_interface()

        #add methods 
        for method in self.__interf.operations:
            _createMethodImplementation(self, method.name)

        #add attributes
        for attribute in self.__interf.attributes:
            #read-only and read-write attributes need the _get_ method...
            _createMethodImplementation(self, "_get_" + attribute.name)
            #but only normal attributes have the _set_ method defined
            if attribute.mode == CORBA.ATTR_NORMAL:
                _createMethodImplementation(self, "_set_" + attribute.name)
        
        return
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
        print "DynamicImplementation.invoke(", args, moreargs, ")"
        return

#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Creating an object"
    g = DynamicImplementation("IDL:alma/acspytest/PyTest:1.0")
    g = DynamicImplementation("IDL:alma/PS/PowerSupply:1.0")
    
    g.on(None)
    print dir(g)
    print "Done..."

