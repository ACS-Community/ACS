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
Contains base class definition for simulated entries.
'''
#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log                  import getLogger
from Acssim.Servants.Representations.CDB import CDB
from Acssim.Servants.Representations.GUI import GUI
from Acssim.Servants.Representations.Dynamic      import Dynamic
from Acssim.Servants.Representations.API          import API
from Acssim.Servants.Representations.Server       import Server
from Acssim.Corba.Utilities            import getCompIfrID
from Acssim.Corba.Utilities            import getSuperIDs
import CORBA

#--GLOBALS---------------------------------------------------------------------
__revision__ = "@(#) $Id$"
#------------------------------------------------------------------------------
class BehaviorProxy:
    '''
    Class BehaviorProxy is a baseclass which describes simulated components.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, compname, comptype=None):
        '''
        Constructor.
        
        Parameters:
        - compname is the name of the component to be simulated

        Returns: Nothing

        Raises: Nothing
        '''
        #save the name of the component we're looking for
        self.compname = str(compname)

        #save the component type
        self.comptype = comptype

        # print "****** BehaviorProxy constructor " + compname + " " + str(comptype)

        #our logger
        self.logger = getLogger(compname)
        
        #objects which actually do something when a 
        #method is invoked
        self.dynamic_handler = None
        self.cdb_handler = None
        self.api_handler = None
        self.gui_handler = None
        self.server_handler = None
        self.handlers = []
        
        self.__setupCases()
    #--------------------------------------------------------------------------
    def __setupCases(self):
        '''
        Helper method which creates entries objects
        '''
        #get the IFR ID of the component
        if self.comptype is None:
            # print "***** Getting component type from name " + self.compname
            try:
                comp_type = getCompIfrID(self.compname)
            except CORBA.NO_RESOURCES, ex:
                # In this case is an Offshoot
                if self.compname.find("__OFFSHOOT__") == -1:
                    raise ex
                comp_type = self.compname.split("__OFFSHOOT__").pop()
        else:
            comp_type = self.comptype

        #get all superclasses of the IDL interfaces
        if_list = getSuperIDs(comp_type)
        if_list.append(comp_type)
        
        #create a dynamic handler object
        self.dynamic_handler = Dynamic(self.compname, comp_type)
        self.attachNewHandler(self.dynamic_handler)
        
        #create a CDB handler object
        self.cdb_handler = CDB(self.compname, if_list)
        self.attachNewHandler(self.cdb_handler)
        
        #create a Server handler object
        self.server_handler = Server(self.compname, comp_type)
        self.attachNewHandler(self.server_handler)

        #create a GUI handler object
        self.gui_handler = GUI(self.compname)
        self.attachNewHandler(self.gui_handler)
        
        #create an API handler object
        self.api_handler = API(self.compname)
        self.attachNewHandler(self.api_handler)
    #--------------------------------------------------------------------------
    def getMethod(self, meth_name):
        '''
        Returns a Python dictionary describing the given method or None if it
        does not exist.
        '''
        #cycle through all the handlers looking for a method implementation
        for handler in self.handlers:
            
            meth_obj = handler.getMethod(meth_name)
            
            #found a match
            if meth_obj != None:
                return meth_obj
    #----------------------------------------------------------------------
    def attachNewHandler(self, handler_obj):
        '''
        Attach a user implementation of BaseRepresentation to an internal
        list which determines component behavior.
        
        In plain English, this means someone using the API could come
        up with a completely new way to simulate a component's behavior and 
        add it to this proxy to override ACS's simulator representations
        entirely.
        '''
        self.handlers.insert(0, handler_obj)
        
