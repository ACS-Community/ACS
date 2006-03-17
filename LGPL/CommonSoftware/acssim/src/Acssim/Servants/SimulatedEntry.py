# @(#) $Id: SimulatedEntry.py,v 1.3 2006/03/17 23:49:27 dfugate Exp $
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
# "@(#) $Id: SimulatedEntry.py,v 1.3 2006/03/17 23:49:27 dfugate Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/12/09  Created.
#------------------------------------------------------------------------------
'''
Contains base class definition for simulated entries.
'''
#--REGULAR IMPORTS-------------------------------------------------------------
from copy    import copy

#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log                  import getLogger
from Acssim.Servants.Entries.SimulatedCDBEntry import SimulatedCDBEntry
from Acssim.Servants.Entries.DynamicEntry      import DynamicEntry
from Acssim.Servants.Entries.APIEntry          import APIEntry
from Acssim.Corba.Utilities            import getCompIfrID
from Acssim.Corba.Utilities            import getSuperIDs
#--GLOBALS---------------------------------------------------------------------
 
#------------------------------------------------------------------------------
class SimulatedEntry:
    '''
    Class SimulatedEntry is a baseclass which describes simulated components.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, compname):
        '''
        Constructor.
        
        Parameters:
        - compname is the name of the component to be simulated

        Returns: Nothing

        Raises: Nothing
        '''
        #save the name of the component we're looking for
        self.compname = str(compname)

        #our logger
        self.logger = getLogger(compname)
        
        #objects which actually do something when a 
        #method is invoked
        self.dynamic_handler = None
        self.cdb_handler = None
        self.api_handler = None
        
        self.setupCases()
    #--------------------------------------------------------------------------
    def setupCases(self):
        '''
        Helper method which creates entries objects
        '''
        #get the IFR ID of the component
        comp_type = getCompIfrID(self.compname)

        #get all superclasses of the IDL interfaces
        if_list = getSuperIDs(comp_type)
        if_list.append(comp_type)
        
        #create a dynamic handler object
        self.dynamic_handler = DynamicEntry(self.compname, comp_type)
        
        #create a CDB handler object
        self.cdb_handler = SimulatedCDBEntry(self.compname, if_list)
        
        #create an API handler object
        self.api_handler = APIEntry(self.compname)
    #--------------------------------------------------------------------------
    def getMethod(self, meth_name, comp_ref=None):
        '''
        Returns a Python dictionary describing the given method or None if it
        does not exist.
        '''
        if self.api_handler.getMethod(meth_name) != None:
            self.logger.logDebug("Executing the '" + meth_name + "' method of the '" +
                                 self.compname + "' simulated component using the API.")
            return self.api_handler.getMethod(meth_name)
        
        elif self.cdb_handler.getMethod(meth_name) != None:
            self.logger.logDebug("Executing the '" + meth_name + "' method of the '" +
                             self.compname + "' simulated component using the CDB.")
            return self.cdb_handler.getMethod(meth_name)
        
        else:
            self.logger.logDebug("Executing the '" + meth_name + "' method of the '" +
                             self.compname + "' simulated component on the fly.")
            return self.dynamic_handler.getMethod(meth_name, comp_ref)
        