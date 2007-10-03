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

This particular implementation generates method/attribute definitions 
on the fly using the CORBA IFR.
'''
#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------
import CORBA
#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log       import getLogger

from Acssim.Servants.Representations.BaseRepresentation         import BaseRepresentation
from Acssim.Corba.Generator         import getRandomValue
#--GLOBALS---------------------------------------------------------------------
__revision__ = "@(#) $Id$"
from Acssim.Goodies import IR
from Acssim.Goodies import getStandardTimeout
#------------------------------------------------------------------------------
class Dynamic(BaseRepresentation):
    '''
    Class derived from BaseRepresentation which dynamically generates method
    implementations on the fly.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, compname, comptype):
        '''
        Constructor
        '''
        #superclass constructor
        BaseRepresentation.__init__(self, compname)
        
        self.__logger = getLogger(str(Dynamic) + "(" + compname + ")")

        #---------------------------------------------------------------------
        def __initialize(args):
            '''
            Fake lifecycle method.
            '''
            self.__logger.logDebug("Simulated lifecyle method")
            return
    
        def __cleanUp(args):
            '''
            Fake lifecycle method.
            '''
            self.__logger.logDebug("Simulated lifecyle method")
            return
        
        self.setMethod('initialize',
                        {'Timeout' : 0.0,
                          'Value' : __initialize}
                       )
        
        self.setMethod('cleanUp',
                        {'Timeout' : 0.0,
                          'Value' : __cleanUp}
                       )

        #save the IDL type
        self.comp_type = comptype

        self.__interf = IR.lookup_id(comptype)
        try:
            self.__interf = self.__interf._narrow(CORBA.InterfaceDef)
            self.__interf = self.__interf.describe_interface()
        except Exception, ex:
            self.__logger.logCritical("Cannot find a definition for '" +
                                       self.comp_type + "' components!")
            raise CORBA.NO_RESOURCES()
    #--------------------------------------------------------------------------
    def __handleReadAttribute(self, method_name, in_list):
        '''
        Helper function which tries to handle a readonly CORBA attribute.
        Modifies in_list in place.
        '''
        #strip this out to get the real CORBA attribute name
        method_name = method_name.replace("_get_","",1)

        #cycle through all attributes of the interface description
        #looking for this particular CORBA attribute
        for attr in self.__interf.attributes:
            if method_name == attr.name:
                #good...we found a match. just attach it to the list
                in_list.append(attr.type)
                self.__logger.logDebug("Readonly attribute:" + method_name)
                break
        
        return in_list
    #--------------------------------------------------------------------------
    def __handleNormalMethod(self, method_name, in_list):
        '''
        Helper function which tries to handle a normal IDL method.
        Modifies in_list in place.
        '''
        #cycle through the list of methods from the interface
        #description
        for method in self.__interf.operations:
            
            if method_name == method.name:                
                #well there has to be at least a single return value
                #(even if it is void)
                in_list.append(method.result)
                
                #next check for in and inout parameters
                for param in method.parameters:
                    #corba out parameters
                    if str(param.mode) == "PARAM_OUT":
                        in_list.append(param.type)
                    #corba inout parameters
                    elif str(param.mode) == "PARAM_INOUT":
                        in_list.append(param.type)
                
                #no reason to continue...we know what has to be
                #returned
                self.__logger.logDebug("Method:" + method_name)
                break
    #--------------------------------------------------------------------------
    def __typecodesToObjects(self, in_list):
        '''
        Converts a list full of typecodes to real implementations of those 
        types.
        '''
        #good - no in/inout parameters
        if len(in_list) == 1:
            ret_val = [getRandomValue(in_list[0], self.comp_ref)]
        
        else:
            #iterate through the return value and all 
            for i in range(0, len(in_list)):
                in_list[i] = getRandomValue(in_list[i], self.comp_ref)
            ret_val = [tuple(in_list)]
            
        return ret_val
    #--------------------------------------------------------------------------
    def getMethod(self, method_name):
        '''
        Overriden from baseclass.
        '''
        #sanity check
        self._BaseRepresentation__checkCompRef()
        if BaseRepresentation.getMethod(self,
                                         method_name)!=None:
            return BaseRepresentation.getMethod(self, method_name)
        
        #list of return values
        return_list = []
        
        #create the temporary dictionary that will be returned later
        ret_val = { 'Value':[ "None" ],
                   'Timeout': getStandardTimeout()}

        #Check the method's name to see if it's really a RW attribute
        #This will be true if the method name starts with "_set_". If this
        #happens to be the case, just return
        if method_name.rfind("_set_") == 0:
            self.__logger.logDebug("Write attribute:" + method_name)
            return ret_val

        #Check to see if the method's name begins with "_get_". If this is
        #true, this is a special case because we do not have to worry 
        #about inout and out parameters.
        elif method_name.rfind("_get_") == 0:
            self.__handleReadAttribute(method_name, return_list)

        #Since we've gotten this far, we must now examine the IFR to determine
        #which return values (if any) and exceptions this method can 
        #return/throw.
        else:
            self.__handleNormalMethod(method_name, return_list)
        
        #convert the list of typecodes to a list of real implementations of
        #those types
        #if the methodname was not found...
        if len(return_list) == 0:
            self.__logger.logWarning("Failed to dynamically generate '" +
                                      method_name + "' because the IFR" + 
                                      " was missing information on the method!")
            raise CORBA.NO_RESOURCES()
        
        ret_val['Value'] = self.__typecodesToObjects(return_list)
        self.__logger.logDebug("retVal looks like:" + method_name +
                               " " + str(ret_val))
        return ret_val

