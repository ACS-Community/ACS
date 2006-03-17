# @(#) $Id: DynamicEntry.py,v 1.2 2006/03/17 20:41:31 dfugate Exp $
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
# "@(#) $Id: DynamicEntry.py,v 1.2 2006/03/17 20:41:31 dfugate Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/12/09  Created.
#------------------------------------------------------------------------------
'''
TODO LIST:
'''
#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------
import CORBA
#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log       import getLogger

from Acssim.Servants.BaseEntry         import BaseEntry
from Acssim.Servants.Generator         import getRandomValue
#--GLOBALS---------------------------------------------------------------------

from Acssim.Servants.Goodies import IR
from Acssim.Servants.Goodies import getStandardTimeout
#------------------------------------------------------------------------------
class DynamicEntry(BaseEntry):
    '''
    Class derived from BaseEntry which dynamically generates method
    implementations on the fly.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, compname, comptype):
        '''
        '''
        global IR
        #superclass constructor
        BaseEntry.__init__(self, compname)

        #save the IDL type
        self.compType = comptype

        self.__interf = IR.lookup_id(comptype)
        self.__interf = self.__interf._narrow(CORBA.InterfaceDef)
        self.__interf = self.__interf.describe_interface()
        
        self.__logger = getLogger("Acssim.Servants.DynamicEntry")
        
    #--------------------------------------------------------------------------
    def getMethod(self, methName, compRef):
        '''
        Overriden from baseclass.
        '''
        returnList = []
        
        #create the temporary dictionary that will be returned later
        retVal = { 'Value':[ "None" ],
                   'Timeout': getStandardTimeout()}

        #Check the method's name to see if it's really a RW attribute
        #This will be true if the method name starts with "_set_". If this
        #happens to be the case, just return
        if methName.rfind("_set_") == 0:
            self.__logger.logTrace("Trying to simulate a write to an attribute...ignoring:" + str(methName))
            return retVal

        #Check to see if the method's name begins with "_get_". If this is true,
        #this is a special case because we do not have to worry about inout and
        #out parameters.
        if methName.rfind("_get_") == 0:

            self.__logger.logTrace("Found what appears to be a read for an attribute:" + str(methName))

            #strip this out to get the real attribuute name
            methName = methName.replace("_get_","",1)

            for attr in self.__interf.attributes:
                if methName == attr.name:
                    #good...we found a match
                    self.__logger.logTrace("Encountered an attribute:" + str(methName))
                    returnList.append(attr)
                    break
        else:
            #Since we've gotten this far, we must now examine the IFR to determine
            #which return values (if any) and exceptions this method can return/throw.
            self.__logger.logTrace("Must be an IDL method:" + str(methName))
            for method in self.__interf.operations:
                if methName == method.name:
                    self.__logger.logTrace("Found a match for the IDL method:" + str(methName))
                    #well there has to be at least a single return value
                    #(even if it is void)
                    returnList.append(method.result)
                
                    #next check for in and inout parameters
                    for param in method.parameters:
                        if str(param.mode) == "PARAM_OUT":
                            self.__logger.logTrace("Adding an OUT parameter for the IDL method:" +
                                                 str(methName) + " " + str(param.type))
                            returnList.append(param.type)
                        elif str(param.mode) == "PARAM_INOUT":
                            self.__logger.logTrace("Adding an INOUT parameter for the IDL method:" +
                                                 str(methName) + " " + str(param.type))
                            returnList.append(param.type)

                    #no reason to continue...we know what has to be
                    #returned
                    self.__logger.logTrace("Was an IDL method:" + str(methName))
                    break
        
        #if the methodname was not found...
        if len(returnList) == 0:
            self.__logger.logWarning("Failed to dynamically generate the  '" + methName +
                                   "' method for the '" + self.compname + "' component " +
                                   "because the IFR was missing information on the method!")
            return retVal
        #good - no in/inout parameters
        elif len(returnList) == 1:
            self.__logger.logTrace("There were no out/inout params:" + str(methName))
            retVal['Value'] = [getRandomValue(returnList[0], compRef)]
        else:
            #iterate through the return value and all 
            for i in range(0, len(returnList)):
                self.__logger.logTrace("Adding a return value:" + str(methName) +
                                     " " + str(returnList[i]))
                returnList[i] = getRandomValue(returnList[i], compRef)
            retVal['Value'] = [tuple(returnList)]

            
        self.__logger.logDebug("retVal looks like:" + str(methName) +
                             " " + str(retVal))
        return retVal

