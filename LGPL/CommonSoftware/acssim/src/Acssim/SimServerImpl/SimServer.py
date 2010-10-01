#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2005 
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id: SimServer.py,v 1.3 2010/10/01 17:20:48 javarias Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# rhiriart  2006-12-06  created
#

import sys

import ACSSim__POA
from Acspy.Servants.ACSComponent       import ACSComponent
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Servants.ContainerServices  import ContainerServices
from Acssim.Goodies import setGlobalData
from Acssim.Goodies import removeGlobalData
from Acssim.Goodies import getGlobalData
from ACSSim import MethodInfo
from ACSSim import NoSuchMethodEx
from ACSSim import NoSuchDataEx
from ACSSim import DataErrorEx

class SimServer(ACSSim__POA.Simulator,
                ACSComponent,
                ContainerServices,
                ComponentLifecycle):
    """Simulator component implementation.
    """
    
    #------------------------------------------------------------------------------
    #--Constructor-----------------------------------------------------------------
    #------------------------------------------------------------------------------
    def __init__(self):
        '''Constructor.
        '''
        ACSComponent.__init__(self)
        ContainerServices.__init__(self)

        self.logger = self.getLogger()

        # Stores method information for all components
        # self.methods is indexed by component name
        # self.methods[comp_name] is a map indexed by method name
        # self.methods[comp_name][method_name] is an instance of MethodInfo
        self.methods = {}

        # Stores method information for all interfaces
        # self.methods is indexed by interface ('IDL:bla/bla/bla:1.0') name
        # self.methods[if_name] is a map indexed by method name
        # self.methods[if_name][method_name] is an instance of MethodInfo
        self.if_methods = {}

    #------------------------------------------------------------------------------
    #--Override ComponentLifecycle methods-----------------------------------------
    #------------------------------------------------------------------------------
    def initialize(self):
        '''
        Override this method inherited from ComponentLifecycle
        '''
        self.getLogger().logInfo("called...")
        
    #------------------------------------------------------------------------------
    def cleanUp(self):
        '''
        Override this method inherited from ComponentLifecycle
        '''
        self.getLogger().logInfo("called...")
            
    #------------------------------------------------------------------------------
    def getName(self):
        return "SimServer"
    
    #------------------------------------------------------------------------------
    #--Implementation of IDL methods-----------------------------------------------
    #------------------------------------------------------------------------------

    #------------------------------------------------------------------------------
    def setMethod(self, comp_name, method_name, method_code, timeout):
        if not comp_name in self.methods.keys():
            self.methods[comp_name] = {}
        self.methods[comp_name][method_name] = MethodInfo(method_code.split('\n'),
                                                          timeout)

    #------------------------------------------------------------------------------
    def setMethodIF(self, if_name, method_name, method_code, timeout):
        if not if_name in self.if_methods.keys():
            self.if_methods[if_name] = {}
        self.if_methods[if_name][method_name] = MethodInfo(method_code.split('\n'),
                                                           timeout)

    #------------------------------------------------------------------------------
    def removeMethod(self, comp_name, method_name):
        if not comp_name in self.methods.keys(): pass
        try:
            self.methods[comp_name].pop(method_name)
        except KeyError: pass

    #------------------------------------------------------------------------------
    def removeMethodIF(self, if_name, method_name):
        if not if_name in self.if_methods.keys(): pass
        try:
            self.if_methods[if_name].pop(method_name)
        except KeyError: pass

    #------------------------------------------------------------------------------
    def removeAllMethods(self):
        self.methods.clear()

    #------------------------------------------------------------------------------
    def removeAllMethodsIF(self):
        self.if_methods.clear()

    #------------------------------------------------------------------------------
    def getMethod(self, comp_name, method_name):
        try:
            return self.methods[comp_name][method_name]
        except KeyError:
            raise NoSuchMethodEx()

    #------------------------------------------------------------------------------
    def getMethodIF(self, if_name, method_name):
        try:
            return self.if_methods[if_name][method_name]
        except KeyError:
            raise NoSuchMethodEx()

    #------------------------------------------------------------------------------
    def setGlobalData(self, name, value):
        try:
            setGlobalData(name, value)
        except:
            # setGlobalData() raises a string exception.
            ex_info = sys.exc_info()
            raise DataErrorEx(ex_info[0])
        
    #------------------------------------------------------------------------------
    def removeGlobalData(self, name):
        try:
            removeGlobalData(name)
        except KeyError, ex:
            raise NoSuchDataEx()
        except:
            ex_info = sys.exc_info()
            raise DataErrorEx(ex_info[0])            

    #------------------------------------------------------------------------------
    def getGlobalData(self, name):
        value = getGlobalData(name)
        if value == None:
            raise NoSuchDataEx()
        else:
            return str(value)

#----------------------------------------------------------------------------------
#--Main defined only for generic testing-------------------------------------------
#----------------------------------------------------------------------------------
if __name__ == "__main__":
    print "Creating an object"
    s = SimServer()
    print "Done..."

#
# ___oOo___

