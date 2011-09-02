# @(#) $Id: ContainerServices.py,v 1.31 2011/09/02 16:09:07 javarias Exp $
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
# "@(#) $Id: ContainerServices.py,v 1.31 2011/09/02 16:09:07 javarias Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2003/08/05  Created.
#------------------------------------------------------------------------------
'''
This module provides access to any ACS/CORBA services a component/client wants
to use. Essentially all it contains are references to the managers methods,
components, etc. It does not provide direct access to the container in any way,
shape, or form.

TODO:
- getComponent does NOT return the nasty IDL struct for dynamic/default
components but instead returns the narrowed component reference. The side
effect here is that nameless components cannot be released properly by the
developer. For now, we can depend on Manager to keep track of whats going on
but this solution is less than ideal.
'''

__revision__ = "$Id: ContainerServices.py,v 1.31 2011/09/02 16:09:07 javarias Exp $"

#--GLOBALS---------------------------------------------------------------------

#--REGULAR IMPORTS-------------------------------------------------------------
import threading
#--CORBA STUBS-----------------------------------------------------------------
import CORBA
import maci
from maciErrTypeImpl          import CannotGetComponentExImpl, ComponentDeactivationFailedExImpl, ComponentDeactivationUncleanExImpl
import maciErrType
import ACSErr
#--ACS Imports-----------------------------------------------------------------
from Acspy.Util.ACSCorba      import getORB, getManager
from Acspy.Common.Log         import getLogger, acsPrintExcDebug
from Acspy.Common.CDBAccess   import CDBaccess
from ACS__POA                 import CBlong
from ACS                      import CBDescIn 
#------------------------------------------------------------------------------
class ContainerServices:
    '''
    Class ContainerServices provides components and PySimpleClients
    with ACS and CORBA services. Developers should never instantiate this class
    directly. That is, ContainerServices is a baseclass and is functionally
    useless until its setAll method has been invoked properly.
    '''
    #--------------------------------------------------------------------------
    def __init__(self): # pragma: NO COVER
        '''
        Components derived from ContainerServices must invoke this constructor!
        Automatically taken care of in PySimpleClient objects.

        Parameters: None

        Raises: Nothing
        '''
        #Name of this component if applicable
        self.__name = None
        #Name of this component container if applicable
        self.__contname = None
        #Token given to us by manager
        self.__token = None
        #Handle give to us by manager
        self.__handle = None
        #Logger for this client
        self.__logger = None
        #Provides access to the ACS CDB
        self.__cdb_access = CDBaccess()

        #The real container/client method(s) invoked by this classes methods.
        #In doing this, we can hide the container/client completely as long
        #as it invokes the setAll(...) method.
        self.activateOffShootMethod = None
    #--------------------------------------------------------------------------
    def getName(self): # pragma: NO COVER
        '''
        Returns our name.

        Parameters: None

        Return: name of the component or client derived from ContainerServices.

        Raises: Nothing
        '''
        return self.__name
    #--------------------------------------------------------------------------
    def getContName(self): # pragma: NO COVER
        '''
        Returns our container name.

        Parameters: None

        Return: name of the container the component was started by.

        Raises: Nothing
        '''
        return self.__contname
    #--------------------------------------------------------------------------
    def getCDBRecord(self, record_name): # pragma: NO COVER
        '''
        Returns the stringified version of an XML record in the ACS
        configuration database defined by record_name.

        Parameters:
        - record_name is the full name of an XML record in the CDB.  An
        example would be "alma/MOUNT1".

        Return: the stringified XML record 
        
        Raises: ???
        '''
        return self.__cdb_access.getField(record_name)
    
    #--------------------------------------------------------------------------
    def getCDBElement(self, record_name, ele_name): # pragma: NO COVER
        '''
        This method returns all of the attributes in the form of a dictionary
        for a given XML element within an XML file.
        
        Parameters:
        - record_name is the name of the XML file (e.g., "alma/MOUNT1")
        - ele_name is the name of the element (e.g, "MOUNT/actAz")
        - element_name is the name of the element (e.g, "MOUNT/actAz",
          where acsAz is an XML element contianing all attributes
          representing the characteristics of a BACI property).

        Return: a dictionary

        Raises: ???
        '''
        return self.__cdb_access.getElement(record_name, ele_name)
        
    #--------------------------------------------------------------------------
    def getLogger(self):
        '''
        Returns the component/client logger.

        Parameters: None

        Return: a logger

        Raises: Nothing
        '''
        if not self.__logger:
            self.__logger = getLogger(self.__name)
        return self.__logger
    #--------------------------------------------------------------------------
    def getComponent(self,
                     comp_name=None,
                     activate=CORBA.TRUE,
                     comp_idl_type=None,
                     comp_code=None,
                     container_name=None,
                     is_dynamic=0):
        '''
        NOTE: all keyword parameters with the exception of comp_name are
        deprecated!
        
        Get a component reference from the Manager.

        This seemingly simple method is actually quite complicated. First, its
        important to note that this method narrows the reference for the
        developer and even imports the proper IDL Python stub. Under certain
        circumstances though, the IDL Python stub may NOT exist in the correct
        place which has led some developers to believe this method was broken
        in the past. So if you see an error message beginning with
        "Unable to import...", please check that you really CAN import the
        CORBA stubs for the component you are trying to access using the
        infamous "python -i" command.

        The next important thing to realize is this method does not just
        retrieve named components as its own name implies - based on the
        parameters passed it could return default and dynamic components
        also.

        Parameters:
        - name is the components name. If not None and the rest of the
        default parameters are left as-is, this is assumed to be a static
        component.
        - activate tells manager whether the component should be activated
        if it has not already been instantiated. Not too useful.
        - comp_idl_type is the interface repository IDL location of the
        component. If not None and the rest of the default parameters are
        left as-is, it is assumed the developer wants a default component.
        - comp_code is a shared library implementing component. If not
        None, it is assumed the developer wants a dynamic component.
        - container_name is the name of the container to activate component.
        If not None, it is assumed the developer wants a dynamic component.
        - is_dynamic states whether a component should be retrieved as
        default or dynamic. In simple terms, when this parameter and
        comp_idl_type are the only params that have been changed from their
        original values, this is the difference between retrieving a reference
        to a dynamic component or a default component.
        
        Returns: a narrowed reference to the component or None if
        that reference cannot be obtained.

        Raises: CannotGetComponentExImpl if the component stubs cannot be loaded
                or found
        '''
        #if the user is trying to get a "normal" static component
        if (comp_name is not None) and (comp_idl_type is None) and \
           (comp_code is None) and (container_name is None) and \
           (is_dynamic == 0):
            
            #Import the correct Python CORBA stub for the developer.
            comp_class = self.__importComponentStubs(comp_name, comp_idl_type)
            
            #get the component from manager
            corba_obj = getManager().get_component(self.__handle,
                                                   comp_name,
                                                   activate)
            
            #return the narrowed reference
            return self.__narrowComponentReference(corba_obj, comp_class)
            
        #if the user is trying to get a static default component
        elif (comp_idl_type is not None) and (comp_name is None) and \
             (comp_code is None) and (container_name is None) and \
             (is_dynamic == 0):
            return self.getDefaultComponent(str(comp_idl_type))
            
        #user must be trying to get a dynamic component
        else:
            return self.getDynamicComponent(comp_name,
                                            comp_idl_type,
                                            comp_code,
                                            container_name)
                                            
    #--------------------------------------------------------------------------
    def __importComponentStubs(self,
                               comp_name=None,
                               comp_type=None):
        '''
        Helper method tries to automatically import the CORBA stubs for a 
        developer. In the event that this fails, a critical message is logged.
        
        Parameters:
            comp_name - name of the component
            comp_type - IFR type of the component
        
        Notes: at least one of the parameters above has be a string
        
        Returns: the component class CORBA stub
        
        Raises: CannotGetComponentExImpl if type is unknown or import fails
        '''
        t_idl_type = comp_type

        if (comp_name is not None) and (comp_type is None):
            #Get a list of all components
            components = self.availableComponents()

            #search each Component
            for component in components:
                #for the one that has the given name
                if component.name == comp_name:
                    #get that component's IR location
                    #e.g., "IDL:alma/PS/PowerSupply:1.0"
                    t_idl_type = component.type  
                    break

        if t_idl_type is None:
            #getting this far means the component was not
            #found
            ex = CannotGetComponentExImpl()
            ex.setReason("Component type unavailable!")
            self.__logger.logWarning("Unable to import '" + str(comp_name) + 
                                   "' component's module: " + ex.getReason())
            raise ex

        try:
            #extract the proper Python module from the type string.
            #("alma", "PS", "PowerSupply")
            temp = t_idl_type.split(':')[1].split('/')  
            #component's class name
            comp_class = temp.pop()  #"PowerSupply"
            #components module name
            comp_module = temp.pop()  #"PS"
            
            #Now import the real module
            comp_module = __import__(comp_module,
                                     globals(),
                                     locals(),
                                     [comp_class]) #import it
            #get class reference
            comp_class = comp_module.__dict__.get(comp_class) 
        
        except Exception, e:
            #for some reason or another, the module could not be imported.
            #this is not a total failure so it's just logged.
            self.__logger.logWarning("Unable to import '" + str(comp_name) + 
                                   "' component's module: " + str(e))
            acsPrintExcDebug()
            raise CannotGetComponentExImpl(e)
            
        return comp_class
    #--------------------------------------------------------------------------
    def __narrowComponentReference(self, corba_obj, comp_class):
        '''
        Helper method which narrows the component reference to the correct type
        for the developer.
        
        Parameters:
            corba_obj - reference to the component
            comp_class - CORBA stub class for the component
            
        Returns: 
            the narrowed component reference or None
            
        Raises: Nothing
        '''
        #try to narrow the object...if this fails for any reason, just return
        #the unnarrowed object
        try:
            narrowed_ref = corba_obj._narrow(comp_class)
            return narrowed_ref
        
        except Exception, e:
            
            if corba_obj is None:
                self.__logger.logCritical("Unable to obtain reference to component: " + str(e))
            else:
                self.__logger.logWarning("Unable to narrow component: " + str(e))
                acsPrintExcDebug()
                
            return corba_obj
    #--------------------------------------------------------------------------
    def getDefaultComponent(self, comp_type): # pragma: NO COVER
        '''
        Gets the default component specified by the component type.
        The type is the IDL type, such as "IDL:alma/PS/PowerSupply:1.0"

        Parameters:
        - comp_type is the interface repository IDL location of the
        component.
        
        Returns: a narrowed reference to the component or None if
        that reference cannot be obtained.

        Raises: CannotGetComponentExImpl if the component stubs cannot be loaded
                or found
        '''
        #Import the correct Python CORBA stub for the developer.
        comp_class = self.__importComponentStubs(None, comp_type)
        
        comp_info = getManager().get_default_component(self.__handle,
                                                       str(comp_type))
        corba_obj = comp_info.reference
        
        return self.__narrowComponentReference(corba_obj, comp_class)
    #--------------------------------------------------------------------------
    def getComponentNonSticky(self, comp_name): # pragma: NO COVER
        '''
        Gets the component in a non stick way.
        The comp_name is a name of the component.

        Parameters:
        - comp_name is the name of the component.
        
        Returns: a narrowed reference to the component or None if
        that reference cannot be obtained.

        Raises: CannotGetComponentExImpl if the component stubs cannot be loaded
                or found
        '''
        #Import the correct Python CORBA stub for the developer.
        comp_class = self.__importComponentStubs(comp_name)
        
        corba_obj = getManager().get_component_non_sticky(self.__handle,
                                                          str(comp_name))
        
        return self.__narrowComponentReference(corba_obj, comp_class)
    #--------------------------------------------------------------------------
    def getDynamicComponent(self,
                            name, #name of the component    
                            comp_type, #IR IDL location
                            code, #shared library implementing comp
                            container, #container to activate component
                            ): # pragma: NO COVER
        '''
        Gets a component whose instance is not registered in the CDB 
        at deployment time.
        
        Parameters:
        - name is the components name in string format
        - comp_type is the interface repository IDL location of the component
        - code is a shared library implementing component
        - container is the name of the container to activate component.
        
        Returns: a narrowed reference to the component or None if
        that reference cannot be obtained.
        
        Raises: CannotGetComponentExImpl if the component stubs cannot be loaded
                or found
        '''
        #Import the correct Python CORBA stub for the developer.
        comp_class = self.__importComponentStubs(None, comp_type)
        
        #convert the default values into something manager can handle
        if name is None:
            name = maci.COMPONENT_SPEC_ANY
                
        if comp_type is None:
            comp_type = maci.COMPONENT_SPEC_ANY
                
        if code is None:
            code = maci.COMPONENT_SPEC_ANY
                
        if container is None:
            container = maci.COMPONENT_SPEC_ANY
            
        comp_spec = maci.ComponentSpec(str(name),
                                       str(comp_type),
                                       str(code),
                                       str(container))
                                           
        comp_info = getManager().get_dynamic_component(self.__handle,
                                                       comp_spec,
                                                       0)
        corba_obj = comp_info.reference
            
        return self.__narrowComponentReference(corba_obj, comp_class)
    #--------------------------------------------------------------------------
    def releaseComponent(self, comp_name): # pragma: NO COVER
        '''
        Release the component defined by comp_name.

        Parameter: comp_name is the name of the component to be released.

        Returns: The number of objects still attached to the released component

        Raises: Nothing
        '''
        return getManager().release_component(self.__handle, comp_name)
    
    def releaseComponentAsync(self, comp_name, callback=None):
        '''
        Release the component defined by comp_name in asynchronous way.

        Parameters: 
         - comp_name is the name of the component to be released.
         - callback by default is None or it must be a subclass of relaseCallback

        Returns: The number of objects still attached to the released component

        Raises: TypeError if callback is not instance of ComponentReleaseCallback
        '''
        myCBDescIn = CBDescIn(0, 0, "")
        
        if callback is None:
            myCBlong = MyCBlongImpl(None)
            try:
                getManager().release_component_async(self.__handle, comp_name, myCBlong._this(), myCBDescIn)
            except:
                self.getLogger().warning('Problem trying to release component ' + str(comp_name) + '. Check manager logs for more details')
        
        else:
            try:
                getManager().release_component_async(self.__handle, comp_name, callback.myCBlong, myCBDescIn)
            except maciErrType.NoPermissionEx, ex:
                callback.errorNoPermission(ex.message)
                callback.callOver()
            except ACSErr.ACSbaseEx, ex:
                e = ComponentDeactivationUncleanExImpl(exception = ex)
                callback.errorComponentReleaseFailed(e)
                callback.callOver()
            except CORBA.SystemException, ex:
                e = ComponentDeactivationUncleanExImpl(exception = ex)
                callback.errorComponentReleaseFailed(e)
                callback.callOver()
            except:
                callback.callOver()
                
            
                
                
    
    #--------------------------------------------------------------------------
    def forceReleaseComponent(self, comp_name): # pragma: NO COVER
        '''
        Forcefully releases the component defined by comp_name.

        Parameter: comp_name is the name of the component to be released.

        Returns: The number of objects still attached to the released component

        Raises: Nothing
        '''
        return getManager().force_release_component(self.__handle,
                                                    comp_name)
    
    #--------------------------------------------------------------------------
    def findComponents(self,
                       curl_wildcard="*",
                       type_wildcard="*",
                       activated=CORBA.FALSE): # pragma: NO COVER
        '''
        Finds components by their instance name and/or by their type.

	    Wildcards can be used for the curl and type.
	    This method returns a possibly empty array of component curls; 
	    for each curl, you may use getComponent to obtain the reference.

        Parameters:
        - name_wildcard (None is understood as "*")
        - type_wildcard (None is understood as "*")
        - activated is a boolean value which specifies whether the search 
        should be limited to components which have already been activated.

        Return: the curls of the component(s) that match the search.

        Raises: ???
        '''
        #Set them to everything if undefined
        if curl_wildcard is None:
            curl_wildcard = "*"
        else:
            curl_wildcard = str(curl_wildcard)
        
        if type_wildcard is None:
            type_wildcard = "*"
        else:
            type_wildcard = str(type_wildcard)

        #Get all Component info
        comp_info_list = getManager().get_component_info(self.__handle,
                                                         [],
                                                         curl_wildcard,
                                                         type_wildcard,
                                                         activated)
        ret_string_list = []

        #Take only the component names and return that
        for comp_info in comp_info_list:
            ret_string_list.append(comp_info.name)
            
        return ret_string_list
    #--------------------------------------------------------------------------
    def availableComponents(self,
                            name_wildcard="*",
                            type_wildcard="*",
                            activated=0): # pragma: NO COVER
        '''
        Returns a list of ComponentInfos consisting of all Components known 
        to manager.

        Parameters:
        - name_wildcard is a wildcard that the components name must match in 
        order for its information to be returned
        - type_wildcard is a wildcard that the components type must match in
        order for its information to be returned.
        - activated is a boolean value which specifies whether the search 
        should be limited to components which have already been activated.

        Returns: a list consisting of ComponentInfo structures for every
        component manager knows of

        Raises: ???
        '''
        #using these cryptic parameters we find out exactly which
        #components are available
        components = getManager().get_component_info(self.__handle,
                                                     [],
                                                     name_wildcard,
                                                     type_wildcard,
                                                     activated)
        return components

    #--------------------------------------------------------------------------
    def makeCompImmortal(self,
                         comp_name,
                         state): # pragma: NO COVER
        '''
        Change mortality state of a component.
        Compnent must be already active.
        The caller must be owner of the component or have administator rights.
        
        Parameters:
        - comp_name name of the component
        - state new mortality state
        
        Returns: None
        
        Raises: ???
        '''
        #just delegate call directly to manager
        getManager().make_component_immortal(self.__handle,
                                             comp_name,
                                             state)
        
        return

    #--------------------------------------------------------------------------
    def restartComp(self,
                    comp_name): # pragma: NO COVER
        '''
        Restarts a component.
        
        Parameters:
        - comp_name name of the component
        
        Returns: Reference to the restarted component.
        
        Raises: ???
        '''
        #just delegate call directly to manager
        return getManager().restart_component(self.__handle,
                                              comp_name)

    #--------------------------------------------------------------------------
    def getCollocatedComp(self,
                          comp_spec,
                          mark_as_default,
                          target_comp): # pragma: NO COVER
        '''
        Activation of a component so that it runs in the same process as
        another given component.
        
        Parameters:
        - comp_spec (maci.ComponentSpec) Component to be obtained.
        - mark_as_default (bool) Mark component as default component of its type
        - target_comp Name of the target component (where to activate component)
        
        Returns: a narrowed reference to the component or None if
        that reference cannot be obtained.
        
        Raises:
        - IncompleteComponentSpec
        - InvalidComponentSpec
        - ComponentSpecIncompatibleWithActiveComponent
        '''

        comp_info = getManager().get_collocated_component(self.__handle,
                                                     comp_spec,
                                                     mark_as_default,
                                                     target_comp)

        if comp_info is not None:
            corba_obj = comp_info.reference
            comp_class = self.__importComponentStubs(None, comp_info.type)
            return self.__narrowComponentReference(corba_obj, comp_class)

        else:
            return None
    
    #--------------------------------------------------------------------------
    def activateOffShoot(self, py_obj): # pragma: NO COVER
        '''
        Activates an OffShoot derived object as a CORBA object.

        Parameters:
        - py_obj is (non-CORBA) Python object.

        Return: a reference to the CORBA object that almost definitely needs to
        be narrowed to the correct type.  If anything goes wrong though, returns
        None.

        Raises: ???
        '''
        return self.activateOffShootMethod(self.__name, py_obj)
    #--------------------------------------------------------------------------
    def corbaObjectToString(self, comp_ref): # pragma: NO COVER
        '''
        Converts a CORBA object its string representation.

        Parameters: comp_ref is a reference to the CORBA object.

        Return: the string representation (an IOR)

        Raises: ???
        '''
        return getORB().object_to_string(comp_ref)
    #--------------------------------------------------------------------------
    def corbaObjectFromString(self, obj_uri): # pragma: NO COVER
        '''
        Converts a string to a CORBA object reference.
        
        Parameters:
        - obj_uri is the address to the CORBA object

        Return: a CORBA reference to obj_uri

        Raises: ???
        '''
        return getORB().string_to_object(obj_uri)

    #--------------------------------------------------------------------------
    def getThread(self,
                  target,
                  name,
                  args=None,
                  kwargs=None): # pragma: NO COVER
        '''
        This method returns a Python threading.Thread object.
        
        Parameters:
        - target callable object to be invoked by the run() method
        - name the thread name. Must be unique
        - args is the argument tuple for the target invocation. Defaults to ()
        - kwargs is a dictionary of keyword arguments for the target
        invocation. Defaults to {}
        
        Return: a threading.Thread object
        
        Raises: ???
        '''
        #setup keyword parameters to their correct mutable types
        if args is None:
            args = ()
        if kwargs is None:
            kwargs = {}
        
        #create the thread
        new_thread = threading.Thread(None,
                                      target,
                                      str(name),
                                      args,
                                      kwargs)
        new_thread.setDaemon(1)
        
        return new_thread
    #--------------------------------------------------------------------------
    def setAll(self,
               name, #string-name of component
               token, #Security token from manager
               handle, #Security handle from manager
               activate_offshoot_method, #Container's method
               contname=None # Container's name
               ): # pragma: NO COVER
        '''
        This method should only be invoked by the container and provides the
        component with all container services. DO NOT INVOKE FROM YOUR CODE!!!
        '''
        #Set member variables first
        self.__name = name
        self.__token = token
        self.__handle = handle
        self.__contname = contname
        
        #Set method variables next
        self.activateOffShootMethod = activate_offshoot_method
        
        #initialize logging
        self.getLogger()
        
        return
    #--------------------------------------------------------------------------

class ComponentReleaseCallback:
    def __init__(self):
        self.__condition = threading.Condition()
        self.__condition.acquire()
        self.myCBlong = MyCBlongImpl(self)
    
    def errorNoPermission(self, message):
        pass
    
    def componentReleased(self, deactivationUncleanEx=None):
        pass
    
    def errorComponentReleaseFailed(self, deactivationFailureEx):
        pass
    
    def awaitComponentRelease (self, timeout):
        '''
        This method must be called by the same thread who created the instance of this object
        Parameters:
        - timeout in msecs
        '''
        self.__condition.wait(timeout / 1000.0)
        self.__condition.release()
        
    def callOver(self):
        self.__condition.acquire()
        self.__condition.notify_all()
        self.__condition.release()
    
class MyCBlongImpl(CBlong): 
    def __init__(self, callback):
        self.__callback = callback
        if (self.__callback is None):
            return
        if (not isinstance(self.__callback, ComponentReleaseCallback)):
            raise TypeError, 'callback must be instance of ComponentReleaseCallback'
    
    def working(self, value, c, desc):
        pass
    
    def done(self, value, c, desc):
        if (self.__callback is None):
            return
        if (len (c.previousError) > 0):
            ex = ComponentDeactivationFailedExImpl()
            self.__callback.componentReleased(ex)
        else:
            self.__callback.componentReleased()
        self.__callback.callOver()
    
    def negotiate(self, time_to_transmit, desc):
        return True
    
