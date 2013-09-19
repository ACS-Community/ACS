# @(#) $Id: SimpleClient.py,v 1.14 2011/11/29 18:43:17 nsaez Exp $
#
#    ALMA - Atacama Large Millimiter Array
#    (c) Associated Universities, Inc. Washington DC, USA,  2001
#    (c) European Southern Observatory, 2002
#    Copyright by ESO (in the framework of the ALMA collaboration)
#    and Cosylab 2002, All rights reserved
#
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# Internet email: alma-sw-admin@nrao.edu
# "@(#) $Id: SimpleClient.py,v 1.14 2011/11/29 18:43:17 nsaez Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2003/08/04  Created.
#------------------------------------------------------------------------------

'''
PySimpleClient - An extension to the BaseClient class.

This module is designed to provide easy access to the majority of CORBA services
and ACS components. Essentially it is used to manipulate components.

TODO:
- in general more argument type checking is needed for all functions.
- integrate with the ACS Error System
'''

__revision__ = "$Id: SimpleClient.py,v 1.14 2011/11/29 18:43:17 nsaez Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
import os,pwd
import socket
from traceback import print_exc
from atexit import register
#--CORBA STUBS-----------------------------------------------------------------
import ACS
#--ACS Imports-----------------------------------------------------------------
from Acspy.Clients.BaseClient         import BaseClient
from Acspy.Servants.ContainerServices import ContainerServices
from ACSErrTypeCommonImpl             import CORBAProblemExImpl
#--GLOBALS---------------------------------------------------------------------
_instance = None    #singleton reference
_DEBUG = 0
#------------------------------------------------------------------------------
class PySimpleClient(BaseClient, ContainerServices): 
    '''
    PySimpleClient class provides access to ACS components, CORBA services, etc.
    Furthermore, it provides access to the same ContainerServices provided by
    the Container to components. This makes it almost trivial to test components as
    PySimpleClients and vice versa.
    '''
    #--------------------------------------------------------------------------
    
    def __init__(self, name="Python Client"):
        '''
        Initialize the client.

        Parameters:
        - name is what manager will refer to this client instance as
        - runAsSingleton True if the client is a singleton

        Returns: Nothing

        Raises: CORBAProblemExImpl
        '''
        loginName = "UNKNOWN_USER"
        try:
            loginName = pwd.getpwuid(os.getuid())[0]
        except:
            print "Error getting the user name!"
            
        hostName = "UNKNOWN_HOST"
        try:
            hostName = socket.gethostname()
        except:
            print "Error getting the host name!"
        #just to be sure
        name = name + ": initialized by " + loginName + "@" + hostName
        name = str(name)
        
        BaseClient.__init__(self, name)
        if _DEBUG: # pragma: NO COVER
            print "Got past BaseClient in SimpleClient"

        #call superclass constructors
        ContainerServices.__init__(self)
        if _DEBUG: # pragma: NO COVER
            print "Got past ContainerServices in SimpleClient"
            
        #set some things to make ContainerServices happy
        self.setAll(self.name,
                    self.token,
                    self.token.h,
                    self.__activateOffShoot)
        if _DEBUG: # pragma: NO COVER
            print "Got past Constructor in SimpleClient"
            
        # Ensure that disconnect is called when python exit
        register(self.disconnect)
    #--------------------------------------------------------------------------
    #The following objects and functions all deal with a singleton instance of
    #PySimpleClient    
    def getInstance(name="PySingletonClient"):
        '''
        Returns a singleton instance of the PySimpleClient class.
        
        Parameters:
        - name is what manager will refer to this client singleton instance as

        Returns: Nothing

        Raises: CORBAProblemExImpl
        '''
        global _instance
        
        #If there is no singleton yet; create one
        if _instance == None:
            _instance = PySimpleClient(name=name)
        return _instance
    #Create the real static method
    getInstance = staticmethod(getInstance)
    #--------------------------------------------------------------------------
    def __activateOffShoot(self,
                           comp_name,
                           py_object): # pragma: NO COVER
        '''
        A method designed to activate ACS::OffShoot IDL interfaces as CORBA
        objects.

        Parameters:
        - py_object is a Python object which implements an IDL interface derived from
        ACS::OffShoot
        
        Returns: a reference to the newly created CORBA object

        Raises: CORBAProblemExImpl
        '''
        try: 
            return py_object._this()
        except:
            print_exc()
            raise CORBAProblemExImpl()
    #--------------------------------------------------------------------------
