# @(#) $Id: BaseClient.py,v 1.17 2012/04/23 22:45:14 javarias Exp $
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
# "@(#) $Id: BaseClient.py,v 1.17 2012/04/23 22:45:14 javarias Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004/04/22  Created.
#------------------------------------------------------------------------------

'''
BaseClient - An implementation of the MACI simple client interface

This module includes a bare-bones implementation of the Client IDL interface
designed to be used in all Python servant implementations derived from Client.
BaseClient is more of a helper class than anything else.
'''

__revision__ = "$Id: BaseClient.py,v 1.17 2012/04/23 22:45:14 javarias Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
import maci
from maci__POA import Client
from CORBA import TRUE
#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log                 import getLogger, startPeriodicFlush,\
    stopPeriodicFlush
from Acspy.Util.ACSCorba              import getManager, getClient
from ACSErrTypeCommonImpl             import CORBAProblemExImpl
from Acspy.Common.TimeHelper          import getTimeStamp
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class BaseClient(Client): 
    '''
    BaseClient class is an implementation of the MACI Client IDL interface in
    Python.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, name="Python Client"):
        '''
        Initialize the client.

        Parameters:
        - name is what manager will refer to this client instance as

        Returns: Nothing

        Raises: CORBAProblemExImpl
        '''
        
        #security token given to us by the manager
        self.token = None
        #name we will call ourself when contacting the manager
        self.name = str(name)
        #Logger used by container/components
        try:
            self.logger
        except:
            self.logger = getLogger(name)
        startPeriodicFlush(3)
        #List of components manager says are active
        self.managerComponents = []
        #CORBA reference to ourself
        self.corbaRef = None
        
        try:
            #get our token from manager
            self.token = getManager().login(self.getMyCorbaRef())
        except Exception, e:
            #cannot go on if the login fails
            print_exc()
            raise CORBAProblemExImpl()
        
        #sanity check
        if self.token == None:
            # a nil token implies manager doesn't "like" this client
            raise CORBAProblemExImpl()
        
    #--CLIENT IDL--------------------------------------------------------------
    def _get_name(self): # pragma: NO COVER
        '''
        Returns the name of this client.
        
        Parameters: None

        Returns: stringified name of this client.
        
        Raises: Nothing

        readonly attribute string name;
        '''
        return self.name
    #--CLIENT IDL--------------------------------------------------------------
    def disconnect(self):
        '''
        Disconnect notification. The disconnect method is called by the Manager
        to notify the client that it will be unavailable and that the client
        should log off.

        Also, the client developer is required to disconnect all connected clients
        before exiting the Python interpreter.

        Parameters: None

        Returns: None
        
        Raises: Nothing
        
        oneway void disconnect ();
        '''
        self.logger.logInfo('Shutdown called for client')
        stopPeriodicFlush()
        try:
            #here we literally log out of manager
            getManager().logout(self.token.h)
        except Exception, e:
            self.logger.logWarning('Failed to log out of manager: ' +
                                   str(e))
            print_exc()

        return
    #--CLIENT IDL--------------------------------------------------------------
    def authenticate(self, execution_id, question):
        '''
        Authentication method. Method authenticate is the challenge issued to
        the client after it tries to login. The login will be successful if the
        clients authenticate() produces the expected result. Only in this case
        will the Managers login method return a valid handle, which the client
        will later use as the id parameter with all calls to the Manager.

        Parameters: question this string does not currently matter

        Return Answer - first character of the answer identifies the type of
        the client, and can be one of:
        - C = a regular client (implements just the Client interface).
        - A = a container (implements the Container interface).
        - AR = a container with recovery capability (implements the Container interface).
        - S = Supervisor (implements the Administrator interface).

        Raises: Nothing
    
        string authenticate (in string question);
        '''
        #to make pychecker happy
        question = None

        try:
            self.client_type
        except:
            self.client_type = maci.CLIENT_TYPE

        try:
            self.canRecover
        except:
            self.canRecover = False
        
        return maci.AuthenticationData(self.getCode() + self.name, self.client_type,
                                 maci.PYTHON, self.canRecover, getTimeStamp().value, execution_id)
    #--CLIENT IDL--------------------------------------------------------------
    def message (self, message_type, message):
        '''
        The Manager and administrators use this method for sending textual messages
        to the client.

        This implementation first attempts to use the ACS logging mechanism to
        display the message and if that fails for any reason, it is only sent
        to standard out.

        Parameters:
        - message_type is an integer defined as a constant in the Client interface
        - message is a string

        Returns: Nothing

        Raises: Nothing
        
        oneway void message (in short message_type, in string message) 
        '''

        if message_type == maci.Client.MSG_ERROR:
            self.logger.logWarning("Error message from the manager: " + message)

        elif message_type == maci.Client.MSG_INFORMATION:
            self.logger.logInfo("Info message from the manager: " + message)
            
        else:
            self.logger.logInfo("Message of unknown type from the manager: " + message)
    #--CLIENT IDL--------------------------------------------------------------
    def ping(self): # pragma: NO COVER
        '''
        Manager pings its clients to verify they still exist.

        Parameters: None

        Returns: CORBA.TRUE (i.e., 1)

        Raises: Nothing
        
        boolean ping ();
        '''
        return TRUE
    #--CLIENT IDL--------------------------------------------------------------
    def components_available(self, components): # pragma: NO COVER
        '''
        Notify client about the change (availability) of the components currently
        in use by this client. For administrative clients, notification is issued
        for the change of availability of any component in the domain.

        Parameters:
        - components is a sequence of ComponentInfo structures

        Returns: None

        Raises: Nothing
        
        oneway void components_available(in ComponentInfoSeq components)
        '''
        self.managerComponents = components
        return 
    #--CLIENT IDL--------------------------------------------------------------
    def components_unavailable(self, component_names):
        '''
        Notify client that some of the Components currently in use by client
        have become unavailable.
        
        Parameters:
        - component_names names of various unavailable components

        Returns: None

        Raises: Nothing
        
	oneway void Components_unavailable (in stringSeq component_names)
        '''
        #look at each individual name
        for name in component_names:
            #iterate through the entire list of component descriptions looking
            #for the name
            for comp_descrip in self.managerComponents:
                #if the names are identical
                if comp_descrip.name == name:
                    #make it unavailable
                    self.managerComponents.remove(comp_descrip)
        return
    #--------------------------------------------------------------------------
    def getMyCorbaRef(self): # pragma: NO COVER
        '''
        Helper method which returns this Python objects underlying CORBA servant
        reference.

        This implementation implicityly activates the client using the default
        POA if it has not been activated before.

        Parameters: None

        Returns: a CORBA reference to this object

        Raises: CORBAProblemExImpl
        '''
        
        #if this object has not already been activated as a CORBA object...
        if self.corbaRef == None:
            try:
                #...activate it using the default POA
                self.corbaRef = self._this()
            except Exception, e:
                self.logger.logWarning('Failed to activate self as a CORBA object: ' +
                                       str(e))
                print_exc()
                raise CORBAProblemExImpl()

            #sanity check
            if self.corbaRef == None:
                # without a client, we can't go on
                self.logger.logWarning('Failed to activate self as a CORBA object: ' +
                                       str(e))
                raise CORBAProblemExImpl()
        
            #OK to return at this point
            return self.corbaRef
        
        #otherwise return the saved reference
        else:
            return self.corbaRef
    #--------------------------------------------------------------------------
    def getCode(self): # pragma: NO COVER
        '''
        Returns the code to be used when manager tries to authenticate this
        object

        Parameters: None

        Returns: the code to be returned to manager.

        Raises: Nothing
        '''
        return "C"
    #--------------------------------------------------------------------------
