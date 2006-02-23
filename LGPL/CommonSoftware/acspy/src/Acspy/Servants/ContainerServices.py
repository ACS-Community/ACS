# @(#) $Id: ContainerServices.py,v 1.14 2005/06/20 17:49:40 dfugate Exp $
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
# "@(#) $Id: ContainerServices.py,v 1.14 2005/06/20 17:49:40 dfugate Exp $"
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
- Use the Python way of hiding member variables and setAll method.
- Provide XML helper methods.
- Double-check that no methods can raise exceptions!!!
- getComponent does NOT return the nasty IDL struct for dynamic/default
components but instead returns the narrowed component reference. The side
effect here is that nameless components cannot be released properly by the
developer. For now, we can depend on Manager to keep track of whats going on
but this solution is less than ideal.
'''

__revision__ = "$Id: ContainerServices.py,v 1.14 2005/06/20 17:49:40 dfugate Exp $"

#--GLOBALS---------------------------------------------------------------------

#--REGULAR IMPORTS-------------------------------------------------------------
import threading
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
import CORBA
import maci
#--ACS Imports-----------------------------------------------------------------
from Acspy.Util.ACSCorba      import getORB, getManager
from Acspy.Common.Log         import getLogger
from Acspy.Common.CDBAccess   import CDBaccess
#------------------------------------------------------------------------------
class ContainerServices:
    '''
    Class ContainerServices provides components and PySimpleClients
    with ACS and CORBA services. Its based primarily on the Java class of the
    same name although XML support is quite limited here.
    '''
    #--------------------------------------------------------------------------
    def __init__(self):
        '''
        Components derived from ContainerServices must invoke this constructor!
        Automatically taken care of in PySimpleClient objects.

        Parameters: None

        Raises: Nothing
        '''
        #Name of this component if applicable
        self.name = None
        #Token given to us by manager
        self.token = None
        #Handle give to us by manager
        self.handle = None
        #Provides access to the ACS CDB
        self.cdbAccess = CDBaccess()
        #Standard ACS Python logger
        self.logger = None

        #The real container/client method(s) invoked by this classes methods.
        #In doing this, we can hide the container/client completely as long
        #as it invokes the setAll(...) method.
        self.activateOffShootMethod = None
    #--------------------------------------------------------------------------
    def getName(self):
        '''
        Returns our name.

        Parameters: None

        Return: name of the component derived from ContainerServices.

        Raises: Nothing
        '''
        return self.name
    #--------------------------------------------------------------------------
    def getCDBRecord(self, record_name):
        '''
        Returns the stringified version of an XML record in the ACS
        configuration database defined by record_name.

        Parameters:
        - record_name is the full name of an XML record in the CDB.  An
        example would be "alma/MOUNT1".

        Return: the stringified XML record or None on failure.

        Raises: Nothing
        '''
        try:
            return self.cdbAccess.getField(record_name)
        except Exception, e:
            return None
    #--------------------------------------------------------------------------
    def getCDBElement(self, record_name, ele_name):
        '''
        This method returns all of the attributes in the form of a dictionary
        for a given XML element within an XML file.
        
        Parameters:
        - record_name is the name of the XML file (e.g., "alma:MOUNT1")
        - ele_name is the name of the element (e.g, "MOUNT:actAz")

        Return: a dictionary or None on failure.

        Raises: Nothing
        '''
        try:
            return self.cdbAccess.getElement(record_name, ele_name)
        except Exception, e:
            return None
    #--------------------------------------------------------------------------
    def getLogger(self):
        '''
        Returns the component/client logger.

        Parameters: None

        Return: a logger

        Raises: Nothing
        '''
        if self.logger==None:
            self.logger = getLogger(self.getName())
        return self.logger
    #--------------------------------------------------------------------------
    def getComponent(self,
                     comp_name = None,  #name of a component
                     activate = CORBA.TRUE, #whether comp should be activated 
                     comp_idl_type = None,    #IR IDL location
                     comp_code = None,       #shared library implementing comp
                     container_name = None,  #container to activate component
                     is_dynamic = 0  #0 & comp_name==None really implies default comp 
                     ):
        '''
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

        Raises: ??? if the dynamic component parameters are messed-up
        '''

        #Import the correct Python CORBA stub for the developer.
        try:
            if (comp_name != None) and (comp_idl_type == None):
                #Get a list of all components
                components = getManager().get_component_info(self.handle,
                                                             [],
                                                             "*",
                                                             "*",
                                                             0)
                
                #search each Component
                for component in components:
                    #for the one that has the given name
                    if component.name == comp_name:
                        #get that component's IR location
                        #e.g., "IDL:alma/PS/PowerSupply:1.0"
                        t_idl_type = component.type  
                        break
            else:
                t_idl_type = comp_idl_type
                
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
            self.logger.logWarning("Unable to import '" + str(comp_name) +
                                   "' component's module: " + str(e))
            print_exc()

        #if the user is trying to get a "normal" static component
        if (comp_name != None)and(comp_idl_type == None)and(comp_code == None)and(container_name == None)and(is_dynamic == 0):
            corba_obj, status = getManager().get_component(self.handle,
                                                           comp_name,
                                                           activate)
            #to make the NRI happy
            status = 0
        #if the user is trying to get a static default component
        elif (comp_idl_type != None)and(comp_name == None)and(comp_code == None)and(container_name == None)and(is_dynamic == 0):
            comp_info = getManager().get_default_component(self.handle,
                                                           str(comp_idl_type))
            corba_obj = comp_info.reference
        #user must be trying to get a dynamic component
        else:
            #convert the default values into something manager can handle
            if comp_name == None:
                comp_name = maci.COMPONENT_SPEC_ANY
            if comp_idl_type == None:
                comp_idl_type=maci.COMPONENT_SPEC_ANY
            if comp_code == None:
                comp_code = maci.COMPONENT_SPEC_ANY
            if container_name == None:
                container_name = maci.COMPONENT_SPEC_ANY
            comp_spec = maci.ComponentSpec(str(comp_name),
                                           str(comp_idl_type),
                                           str(comp_code),
                                           str(container_name))
            comp_info = getManager().get_dynamic_component(self.handle,
                                                           comp_spec,
                                                           0)
            corba_obj = comp_info.reference

        #try to narrow the object...if this fails for any reason, just return
        #the unnarrowed object
        try:
            narrowed_ref = corba_obj._narrow(comp_class)
            return narrowed_ref
        except Exception, e:
            
            if corba_obj==None:
                self.logger.logCritical("Unable to obtain reference to '" +
                                        str(comp_name) + "' component: " + str(e))
            else:
                self.logger.logWarning("Unable to narrow '" + str(comp_name) +
                                       "' component: " + str(e))
                print_exc()
                
            return corba_obj
    #--------------------------------------------------------------------------
    def getDefaultComponent(self, type):
        '''
        Gets the default component specified by the component type.
        The type is the IDL type, such as "IDL:alma/PS/PowerSupply:1.0</code>"

        Parameters:
        - type is the interface repository IDL location of the
        component.
        
        Returns: a narrowed reference to the component or None if
        that reference cannot be obtained.

        Raises: ??? 
        '''
        return self.getComponent(comp_idl_type=type)
    #--------------------------------------------------------------------------
    def getDynamicComponent(self,
                            name,       #name of the component    
                            type,       #IR IDL location
                            code,       #shared library implementing comp
                            container,  #container to activate component
                            ):
        '''
        Gets a component whose instance is not registered in the CDB 
        at deployment time.
        
        Parameters:
        - name is the components name in string format
        - type is the interface repository IDL location of the component
        - code is a shared library implementing component
        - container is the name of the container to activate component.
        
        Returns: a narrowed reference to the component or None if
        that reference cannot be obtained.
        
        Raises: ??? 
        '''
        return self.getComponent(comp_name = name,  #name of a component
                                 comp_idl_type = type,    #IR IDL location
                                 comp_code = code,       #shared library implementing comp
                                 container_name = container,  #container to activate component
                                 is_dynamic = 1)
    #--------------------------------------------------------------------------
    def releaseComponent(self, comp_name):
        '''
        Release the component defined by comp_name.

        Parameter: comp_name is the name of the component to be released.

        Returns: The number of objects still attached to the released component

        Raises: Nothing
        '''
        try:
            return getManager().release_component(self.handle, comp_name)
        except Exception, e:
            self.logger.logWarning("Unable to release component..." + str(e))
            print_exc()
            return None
    #--------------------------------------------------------------------------
    def validate(self, xml_string, schema_name):
        '''
        NOT IMPLEMENTED!!!
        Validates an XML file against its schema.

        Parameters:
        - xml_string is an entire XML file in the form of a string.
        - schema_name is the name of the schema xml_string will be validated
        against. It should either be the complete location of the schema (i.e.,
        "/tmp/mySchema.xsd") or just the name of the schema
        (i.e., "mySchema.xsd") where this schema is located in one of the
        directories the $IDL_PATH environment variable defines.

        Return: 1 if the schema can be validated and 0 if not.
        '''
        #to make pychecker happy
        xml_string = None

        #to make pychecker happy
        schema_name = None
        
        print "ContainerServices.validate(...) not implemented"
        return 0
    #--------------------------------------------------------------------------
    def findComponents(self, curl_wildcard = None, type_wildcard = None):
        '''
        Finds components by their instance name (curl) and/or by their type.

	Wildcards can be used for the curl and type.
	This method returns a possibly empty array of component curls; 
	for each curl, you may use getComponent to obtain the reference.

        Parameters:
        - nameWildcard (None is understood as "*")
        - type_wildcard (None is understood as "*")

        Return: the curls of the component(s) that match the search.

        Raises: ???
        '''
        #Set them to everything if undefined
        if curl_wildcard == None:
            curl_wildcard = "*"
        else:
            curl_wildcard = str(curl_wildcard)
        if type_wildcard == None:
            type_wildcard = "*"
        else:
            type_wildcard = str(type_wildcard)

        #Get all Component info
        comp_info_list = getManager().get_component_info(self.handle,
                                                         [],
                                                         curl_wildcard,
                                                         type_wildcard,
                                                         CORBA.FALSE)
        ret_string_list = []

        #Take only the component names and return that
        for comp_info in comp_info_list:
            ret_string_list.append(comp_info.name)
        return ret_string_list
    #--------------------------------------------------------------------------
    def availableComponents(self,
                            name_wildcard="*",
                            type_wildcard="*",
                            activated=0):
        '''
        Returns a list of ComponentInfos about all Components known to manager.

        Parameters:
        - name_wildcard is a wildcard that the components name must match in order
        for its information to be returned
        - type_wildcard is a wildcard that the components type must match in
        order for its information to be returned.
        - activated is a boolean value which specifies whether the search should
        be limited to components which have already been activated.

        Returns: a list consisting of ComponentInfo structures for every
        component manager knows of or [] if this method fails.

        Raises: Nothing
        '''
        try:
            #using these cryptic parameters we find out exactly which
            #components are available
            components = getManager().get_component_info(self.handle,
                                                         [],
                                                         name_wildcard,
                                                         type_wildcard,
                                                         activated)
            return components

        except Exception, e:
            self.logger.logWarning("Unable to find components..." + str(e))
            print_exc()
            return []
    #--------------------------------------------------------------------------
    def activateOffShoot(self, py_obj):
        '''
        Activates an OffShoot derived object as a CORBA object.

        Parameters:
        - py_obj is (non-CORBA) Python object.

        Return: a reference to the CORBA object that almost definitely needs to
        be narrowed to the correct type.  If anything goes wrong though, returns
        None.

        Raises: ???
        '''
        return self.activateOffShootMethod(self.name, py_obj)
    #--------------------------------------------------------------------------
    def corbaObjectToString(self, comp_ref):
        '''
        Converts a CORBA object its string representation.

        Parameters: comp_ref is a reference to the CORBA object.

        Return: the string representation (an IOR)

        Raises: ???
        '''
        return getORB().object_to_string(comp_ref)
    #--------------------------------------------------------------------------
    def corbaObjectFromString(self, comp_name):
        '''
        Converts a string to a CORBA object reference.
        
        Parameters:
        - comp_name is the name of the CORBA object.

        Return: a CORBA reference to comp_name.

        Raises: ???
        '''
        return getORB().string_to_object(comp_name)

    #--------------------------------------------------------------------------
    def getThread(self,
                  target=None,
                  name=None,
                  args=(),
                  kwargs={}):
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
        return threading.Thread(None,
                                target,
                                name,
                                args,
                                kwargs)
    #--------------------------------------------------------------------------
    def setAll(self,
               name,  #string-name of component
               token,  #Security token from manager
               handle, #Security handle from manager
               activate_offshoot_method  #Container's method
               ):
        '''
        This method should only be invoked by the container and provides the
        component with all container services. DO NOT INVOKE FROM YOUR CODE!!!
        '''
        #Set member variables first
        self.name = name
        self.token = token
        self.handle = handle
        
        #Set method variables next
        self.activateOffShootMethod = activate_offshoot_method
        
        return
    #--------------------------------------------------------------------------

