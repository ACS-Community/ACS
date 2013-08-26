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
#
# "@(#) $Id: Server.py,v 1.4 2011/03/24 16:09:44 javarias Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# rhiriart  2007/02/20  Created.
#------------------------------------------------------------------------------
'''
Contains the concrete implementation of Acssim.Servants.Representations.
BaseRepresentation.

This particular implementation interacts with the Simulator Server component
to get methods.
'''
#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acssim.Servants.Representations.BaseRepresentation import BaseRepresentation
from Acspy.Clients.SimpleClient import PySimpleClient
from ACSSim import NoSuchMethodEx
from maciErrType import NoDefaultComponentEx

#--GLOBALS---------------------------------------------------------------------
__revision__ = "@(#) $Id: Server.py,v 1.4 2011/03/24 16:09:44 javarias Exp $"
#------------------------------------------------------------------------------
#def initialize(args): pass

#------------------------------------------------------------------------------
#def cleanUp(args): pass

#------------------------------------------------------------------------------
class Server(BaseRepresentation):

    #--------------------------------------------------------------------------
    def __init__(self, compname, comp_type):
        BaseRepresentation.__init__(self, compname)
        self.comp_type = comp_type
        self.simulator = None
        self.client = PySimpleClient()
        self.simulator = None
        try: 
           self.simulator = self.client.getDefaultComponent("IDL:alma/ACSSim/Simulator:1.0")
        except NoDefaultComponentEx, ex:
           # fine, no Simulator Server component defined in the CDB
           pass

#        self.setMethod('initialize', {'Timeout': 0.0, 'Value': initialize})
#        self.setMethod('cleanUp', {'Timeout': 0.0, 'Value': cleanUp})
        
    #--------------------------------------------------------------------------
    # Note: this class is managed in a kind-of singleton way, so it's not
    # really necessary to release the component here.
    # Even more, when the container goes down, the component
    # is released *before* the class is destructed so a CORBA Object does not
    # exist exception is thrown.
    # def __del__(self):
    #     if self.simulator != None:
    #         simulator_name = self.simulator._get_name()
    #         self.client.releaseComponent(simulator_name)
        
    #--------------------------------------------------------------------------
    def getMethod(self, method_name):

        mdict = BaseRepresentation.getMethod(self, method_name)
        if mdict != None:
            return mdict

        if self.simulator == None:
            return None

        try:
            mdict = {}
            method_info = self.simulator.getMethod(self.compname, method_name)
            mdict['Timeout'] = method_info.timeout
            mdict['Value'] = method_info.code
            return mdict
        except NoSuchMethodEx: pass

        try:
            mdict = {}
            method_info = self.simulator.getMethodIF(self.comp_type, method_name)
            mdict['Timeout'] = method_info.timeout
            mdict['Value'] = method_info.code
            return mdict
        except NoSuchMethodEx:
            return None

#
# __oOo__
