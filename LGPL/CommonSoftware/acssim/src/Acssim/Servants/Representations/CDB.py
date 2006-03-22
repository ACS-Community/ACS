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
Contains the concrete implementation of Acssim.Servants.Representations.
BaseRepresentation.

This particular implementation stores method/attribute definitions stored by
the ACS CDB.
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from operator import isSequenceType
#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log       import getLogger
from Acssim.Servants.Goodies import getComponentXMLObj
from Acssim.Servants.Representations.BaseRepresentation import BaseRepresentation
#--GLOBALS---------------------------------------------------------------------
__revision__ = "@(#) $Id$"
#------------------------------------------------------------------------------
class CDB(BaseRepresentation):
    '''
    Class derived from BaseRepresentation to be used only with the CDB. In other words,
    this class searches the CDB for entries describing method/attribute return
    values.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, compname, supported_interfaces):
        '''
        Constructor.

        Paramters:
        - compname is the name of the component being simulated
        - supported_interfaces is an optional list of IDL interfaces which 
        this particular component supports.
        
        Returns: Nothing

        Raises: ???
        '''
        #superclass constructor
        BaseRepresentation.__init__(self, compname)
        
        #setup the logger
        self.__logger = getLogger(str(CDB) + "(" + compname + ")")
        
        #bool value showing whether the CDB entry exists or not
        self.exists=0
        
        #determine if this simulated component allows inheritence
        allows_inheritence = self.handleCDB(compname)
        
        if allows_inheritence:
            #look at all supported IDL interfaces first
            self.handleInterfaces(supported_interfaces)
            
        #add this individual component one more time to override
        #anything defined in the subinterfaces. this is necessary if
        #the component is of type IDL:alma/x/y:1.0 and this entry
        #exists in the CDB
        self.handleCDB(compname)
            
    #--------------------------------------------------------------------------
    def handleCDB(self, name):
        '''
        Handles an individual CDB entry. This means that if parameter, "name",
        exists within the ACS CDB; we take all info found within the CDB XML
        and add it to this object instance overriding previous 
        method/attribute defininitions where applicable.
        
        Parameters: name is the name of the CDB XML within the /alma/simulated
        section we're searching for.
        
        Returns: True if the current XML allows us to look at superinterfaces.
        False otherwise.
        
        Raises: Nothing
        '''
        ret_val = True
        
        #create an xml helper object
        xml_obj = getComponentXMLObj(name)
        
        if xml_obj!=None:
            #at least one entry exists. good!
            self.exists = 1
            
            #get the corba methods
            self.getCorbaMethods(xml_obj)

            #get the corba attributes
            self.getCorbaAttributes(xml_obj)
            
            #allow inheritance?
            ret_val = xml_obj.SimulatedComponent.getAttribute('AllowInheritance')
            
        return ret_val
    #--------------------------------------------------------------------------
    def handleInterfaces(self, supported_interfaces):
        '''
        Add behavior from derived interfaces for the concrete IDL interface. 
        
        Parameters: supported_interfaces is a list of IDL interface IDs. 
        Arrangement should matter - IDL does not support overriding 
        method declarations in subinterfaces. A simple list could be:
            [ 'IDL:/alma/FRIDGE/FridgeControl:1.0', 
              'IDL:/alma/ACS/CharacteristicComponent:1.0']
        
        Returns: Nothing
        
        Raises: ???
        '''
        #convert the names in supported_interfaces to actual CDB locations
        for supported_interface in supported_interfaces:
            cdb_location = "interfaces/"
            #Turn "IDL:alma/someModule/someInterface:1.0" into:
            #"alma/someModule/someInterface/1.0/1.0"
            try:
                supported_interface = supported_interface.split('IDL:')[1].replace(":", "/")
            except Exception, ex:
                self.__logger.logWarning("Cannot parse '" + supported_interface +
                                          "' to a CDB location!")
            cdb_location = cdb_location + supported_interface
            
            #now try to extract some useful info
            self.handleCDB(cdb_location)
            
    #--------------------------------------------------------------------------
    def getCorbaMethods(self, xml_obj):
        '''
        Sets the CORBA methods of this object.
        TODO: rename
        '''
        #methods is the somewhat formatted data taken from the XML. not really
        #nice enough to work with yet.
        try:
            methods = xml_obj.SimulatedComponent._corbaMethod
            if isSequenceType(methods)==0:
                methods = [ methods ]
        except:
            return

        #for each method in the list
        for dom in methods:
            #dictionary defining the method
            temp_dict = {}

            #extract the method name
            methname = dom.getAttribute('Name')

            #set the timeout
            temp_dict['Timeout'] = float(dom.getAttribute('Timeout'))

            #get the code to be executed yielding a return value
            temp_dict['Value'] = dom.getValue().rstrip().lstrip().split('\n')
        
            #save the dictionary
            self.setMethod(methname, temp_dict)
    #--------------------------------------------------------------------------
    def getCorbaAttributes(self, xml_obj):
        '''
        Sets the CORBA attributes of this object.
        TODO: rename
        '''
        #attributes is the somewhat formatted data taken from the XML. not really
        #nice enough to work with yet.
        try:
            attributes = xml_obj.SimulatedComponent._corbaAttribute
            if isSequenceType(attributes)==0:
                attributes = [ attributes ]
        except:
            return

        #for each method in the list
        for dom in attributes:
            #dictionary defining the attribute
            temp_dict = {}

            #extract the attribute name
            attrname = dom.getAttribute('Name')

            #set the timeout
            temp_dict['Timeout'] = float(dom.getAttribute('Timeout'))

            #get the code to be executed yielding a return value
            temp_dict['Value'] = dom.getValue().rstrip().lstrip().split('\n')
        
            #save the dictionary
            self.setMethod(attrname, temp_dict)
    #--------------------------------------------------------------------------
