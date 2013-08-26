# @(#) $Id: DynamicImplementation.py,v 1.11 2010/10/01 17:20:48 javarias Exp $
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
# "@(#) $Id: DynamicImplementation.py,v 1.11 2010/10/01 17:20:48 javarias Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/12/09  Created.
#------------------------------------------------------------------------------
'''
This module contains the implementation of a generic IDL implementation class
specified in the Python Language Mapping, but not implemented by omniORB.
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from new    import instancemethod
from Acspy.Util.ACSCorba import interfaceRepository
import CORBA

#--GLOBALS---------------------------------------------------------------------
__revision__ = "@(#) $Id: DynamicImplementation.py,v 1.11 2010/10/01 17:20:48 javarias Exp $"
#------------------------------------------------------------------------------
def _mergeClasses(complete_dict, new_class):
    '''
    A helper function designed to merge the method and attributes of classes.
    '''
    #first just copy the new class's attributes and methods if applicable
    temp_keys = new_class.__dict__.keys()
    for key in temp_keys:
        if not complete_dict.has_key(key):
            complete_dict[key] = new_class.__dict__[key]

    #unfortunately we must now look at base classes
    #just use recursion to do this
    for base in new_class.__bases__:
        complete_dict = _mergeClasses(complete_dict, base)

    return complete_dict
        
#------------------------------------------------------------------------------
def _createMethodImplementation(obj_ref, meth_name):
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
        args.insert(1, meth_name)

        #CORBA specs say context object should be None
        if not moreargs.has_key('context'):
            moreargs['context'] = None
        #return whatever invoke returns
        return obj_ref.invoke(tuple(args[1:]), moreargs)
    
    #register the newly created method
    try:
        #if an exception on this occurs...the method does not already exist
        callable(getattr(obj_ref, meth_name))
    except Exception, ex:
        #add the method.
        obj_ref.__dict__[meth_name] = instancemethod(genericFunction, obj_ref)
    return

#------------------------------------------------------------------------------
class DynamicImplementation:
    '''
    A complete implementation of the OMG-defined DynamicImplementation class.
    '''
    def __init__(self, ir_id=None):
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
        if ir_id == None:
            #assume the _get_interface method is implemented in a subclass...
            self.__ir_id = self._get_interface()
            self.__ir_id = self.__ir_id._narrow(CORBA.InterfaceDef)
            self.__ir_id = self.__ir_id._get_id()
        else:
            self.__ir_id = ir_id
            
        self.__real_mod = None
        self.__real_class = None
        
        #create the POA class first...
        classname =  self.__ir_id.split(':')[1].split('/').pop()  #get interface name
        self.__real_mod = self.__ir_id.split(':')[1].split('/')[1] + "__POA"  #"IDL:alma/acspytest/PyTest:1.0" becomes "acspytest__POA"
        self.__real_mod = __import__(self.__real_mod, globals(), locals(), [classname]) #get module
        self.__real_class = self.__real_mod.__dict__.get(classname) #get class
        if self.__real_class is None:
            reload(self.__real_mod)
            self.__real_class = self.__real_mod.__dict__.get(classname) #get class

        #copy it's class locally thereby converting DynamicImplementation
        #into the POA object!
        self.__dict__ = _mergeClasses(self.__dict__, self.__real_class)
        
        temp_list = list(self.__class__.__bases__)
        try:
            #OK for subclasses...
            temp_list.insert(temp_list.index(DynamicImplementation), self.__real_class)
        except:
            #should really never be the case...
            temp_list.insert(0, self.__real_class)
        self.__class__.__bases__ = tuple(temp_list)
        
        #now that all the underlying infrastructure is in place, we can finally use
        #CORBA introspection to start adding methods!
        #get the interface repository.       
        ifr = interfaceRepository()
                                   
        #assume this can be found in a subclass as well.
        self.__interf = ifr.lookup_id(self._NP_RepositoryId)
        if self.__interf is None:
            print "Failed to find interface", self._NP_RepositoryId
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
    

