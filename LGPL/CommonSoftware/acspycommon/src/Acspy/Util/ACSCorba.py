# @(#) $Id: ACSCorba.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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

'''
Python CORBA module for ACS.

Takes care of initializing the ORB and setting initial reference to MACI
manager. Also provides functions to get service and device references
from the manager.
'''
__revision__ = "$Id: ACSCorba.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from sys       import argv
from os        import environ
from traceback import print_exc
from atexit    import register
from time      import sleep
import os
#--CORBA STUBS-----------------------------------------------------------------
from omniORB import CORBA
from omniORB import importIRStubs
from omniORB import installTransientExceptionHandler
import maci__POA
import maci
from maci import Manager
from maci import AuthenticationData
import CosNotifyChannelAdmin
import CDB
import ACSLog
import CosNaming

#This only needs to be done once for omniORB
importIRStubs()
#--ACS Imports-----------------------------------------------------------------
from AcsutilPy.ACSPorts          import getIP
from AcsutilPy.ACSPorts          import getManagerPort
from AcsutilPy.ACSPorts          import getLogPort
from AcsutilPy.ACSPorts          import getNamingServicePort
from AcsutilPy.ACSPorts          import getCDBPort
from AcsutilPy.ACSPorts          import getIRPort
from AcsutilPy.ACSPorts          import getNotifyServicePort
from Acspy.Common.TimeHelper     import getTimeStamp
#--GLOBALS---------------------------------------------------------------------
ORB = None
POA_ROOT = None 
POA_MANAGER = None
MGR_CORBALOC = None
MGR_REF = None
SINGLETON_CLIENT = None
#----------------------------------------------------------------------------
#--Most developers will not find the following functions very interesting as 
#--they just provide public access to general CORBA objects.
def getManagerCorbaloc(new_corbaloc=None):
    '''
    Get (or set) managers corbaloc.
    
    Params: If the developer specifies a value other than None for
    new_corbaloc, it is assumed this is managers corbaloc and sets it
    accordingly.
    
    Returns: the stringified corbaloc of manager.
    
    Raises: Nothing.
    '''
    global MGR_CORBALOC

    if new_corbaloc != None:
        #Developer is trying to manually set the singled reference.
        MGR_CORBALOC = str(new_corbaloc)
  
    elif MGR_CORBALOC == None:
        #If this function has never been called before...
        #Check command-line args for the corbaloc
        for i in range(0, len(argv) - 1):
            if argv[i] == '-managerReference' or argv[i] == '-m':
                #Found it!  OK to set now.
                MGR_CORBALOC = argv[i + 1]
                break
        
        if MGR_CORBALOC == None:
            #Not in the command-line so check an environment variable reference
            if environ.has_key('MANAGER_REFERENCE'):
                #Found it!  OK to set now.
                MGR_CORBALOC = environ['MANAGER_REFERENCE']
            else:
                #Assume manager is running under the local machine
                MGR_CORBALOC = 'corbaloc::' + str(getIP()) + ':' + getManagerPort() + '/Manager'

    return MGR_CORBALOC
#----------------------------------------------------------------------------
def getManagerHost(): # pragma: NO COVER
    '''
    Returns the hostname manager is running on.
    '''
    #use another helper function to get manager's corbaloc
    mgr_corbaloc = getManagerCorbaloc()
    
    #parse it to get the hostname
    return mgr_corbaloc.split("corbaloc::")[1].split(":")[0]
#----------------------------------------------------------------------------
def getORB():
    '''
    Returns a reference to the singled ORB.
    
    Params: None
    
    Returns: a reference to the ORB (or None if there are network problems).
    
    Raises: Nothing.
    '''
    global ORB

    if ORB == None:
        #If this function has never been called before...
        try:
            ORB = CORBA.ORB_init(argv)
            #so we can start receiving requests as well
            getPOAManager()
        
        except Exception, e:
            ORB = None
            print_exc()

    return ORB
#----------------------------------------------------------------------------
def getPOARoot():
    '''
    Returns a reference to the singled root POA.
    
    Params: None
    
    Returns: a reference to the root POA (or None if there are network problems).
    
    Raises: Nothing.
    '''
    global POA_ROOT
    
    if POA_ROOT == None:
        #If this function has never been called before...
        try:
            POA_ROOT = getORB().resolve_initial_references("RootPOA")
        except Exception, e:
            POA_ROOT = None
            print_exc()
  
    return POA_ROOT
#----------------------------------------------------------------------------
def getPOAManager():
    '''
    Returns a reference to the singled POA manager and activates it so 
    incoming requests can be processed.
    
    Params: None
    
    Returns: a reference to the POA manager (or None if there are network
    problems).
    
    Raises: Nothing.
    '''
    global POA_MANAGER
    
    if POA_MANAGER == None:
        #If this function has never been called before...
        try:
            #Cannot do much without the POA manager
            POA_MANAGER = getPOARoot()._get_the_POAManager()
            #In truth, it's pretty dangerous to start processing requests this
            #early but since this module is singled.
            POA_MANAGER.activate()
        except Exception, e:
            POA_MANAGER = None
            print_exc()
  
    return POA_MANAGER
#----------------------------------------------------------------------------
def getManager():
    '''
    Returns a reference to the Manager.
    
    Params: None
    
    Returns: a reference to the Manager (or None if there are network problems).
    
    Raises: Nothing.
    '''
    global MGR_REF

    if MGR_REF == None:
        #If this function has never been called before...
        try:
            MGR_REF = getORB().string_to_object(getManagerCorbaloc())
            if MGR_REF != None and (not CORBA.is_nil(MGR_REF)):
                try:
                    MGR_REF._non_existent()
                    MGR_REF = MGR_REF._narrow(Manager)
                except:
                    MGR_REF = None
            else:
                MGR_REF = None
        except Exception, e:
            MGR_REF = None
            print_exc()
  
    return MGR_REF
#----------------------------------------------------------------------------
class _Client (maci__POA.Client):
    '''
    General use client for getting references to components and services from
    the MACI manager. Developers should ignore this class entirely.
    '''
    #--------------------------------------------------------------------------
    def __init__(self): 
        '''
        Constructor
        '''
        
        #Even though we do nothing with the POA manager, the reference must be
        #retreived so Python clients can begin sending/receiving CORBA requests
        getPOAManager()
    
        #CORBA reference to ourself
        self.corbaRef = self._this()

        if getManager()==None:
            print "Acspy.Util.ACSCorba._Client.__init__ - manager is not available."
            print "      This method will block until manager comes online!"

        if argv[0].split('/').pop() != "pydoc":
            while getManager()==None:
                sleep(1)
        else:
            print "This script is being executed by pydoc - will not block waiting"
            print "for manager: ", argv[0], argv[0].split('/').pop()
        
        #Must have a valid reference to the manager
        self.mgr = getManager()
        try:
            #Need a security token given from the manager to do anything
            self.token = self.mgr.login(self.corbaRef)
             
            #make sure this eventually gets shutdown
            register(self.disconnect)
        except:
            self.token = None
    
    #--CLIENT IDL--------------------------------------------------------------
    def _get_name(self): # pragma: NO COVER
        '''
        Implementation of IDL method.
        '''
        return 'acsCORBA Client'
    #--CLIENT IDL--------------------------------------------------------------
    def disconnect(self): # pragma: NO COVER
        '''
        Implementation of IDL method.
        '''
        global SINGLETON_CLIENT
        global ORB
        global POA_ROOT
        global POA_MANAGER
        global MGR_REF

        installTransientExceptionHandler(self, maxRetry)

        try:
            self.mgr.logout(self.token.h)
        except:
            pass
        
        #According to Duncan Grisby, maintainer of omniORBPy,
        #one should not use the textbook shutdown calls to
        #stop omniORBPy:
        #  Having said that, you are using a particularly byzantine and awkward way
        #  of shutting everything down. You should just call orb->shutdown(0)
        #  inside your shutdown method. That destroys the POAs then shuts down the
        #  ORB in one easy and convenient call. It will make your code much
        #  simpler, and also avoid the race condition.
        #What he says should not be necessary at all but it
        #does seem to fix a bug in omniORBPy (some assertion error)
        #DWF - commented out the various shutdown calls for the
        #time being.
        self.corbaRef._release()
        #getPOAManager().deactivate(CORBA.TRUE, CORBA.TRUE)
        #getPOARoot().destroy(CORBA.TRUE, CORBA.TRUE)
        #getORB().shutdown(CORBA.TRUE)
        #getORB().shutdown(CORBA.FALSE)
        #getORB().destroy()
    
        ORB = None
        POA_ROOT = None
        POA_MANAGER = None
        MGR_REF = None
        SINGLETON_CLIENT = None

        return
    #--CLIENT IDL--------------------------------------------------------------
    def authenticate(self, execution_id, question): # pragma: NO COVER 
        '''
        Implementation of IDL method.
        '''
#        return "CacsCORBA Client"
        return AuthenticationData("CacsCORBA Client", maci.CLIENT_TYPE,
                                  maci.PYTHON, False, getTimeStamp().value, execution_id)
    #--CLIENT IDL--------------------------------------------------------------
    def message(self, message_type, message): # pragma: NO COVER
        '''
        Implementation of IDL method.
        '''
        return
    #--CLIENT IDL--------------------------------------------------------------
    def ping(self): # pragma: NO COVER
        '''
        Implementation of IDL method.
        '''
        return 1
    #--CLIENT IDL--------------------------------------------------------------
    def components_available(self, components): # pragma: NO COVER
        '''
        Implementation of IDL method.
        '''
        return
    #--CLIENT IDL--------------------------------------------------------------
    def components_unavailable(self, components): # pragma: NO COVER
        '''
        Implementation of IDL method.
        '''
        return
    #--------------------------------------------------------------------------
    def getService(self, curl, activate = 1): 
        '''
        Return object reference (properly narrowed if Python module for the
        device IDL has been previously imported.) Mainly used to get MACI/CORBA
        service references.
        
        Parameters:
        - curl is the object CURL
        - activate: 1, if object should be activated; 0, otherwise
        
        Return: object reference
        
        Raises: ???
        '''
        result = None
        component = None
        if self.mgr is not None:
            component = self.mgr.get_service(self.token.h, curl, activate)
      
        if component is not None:
            result = component
        return result
#----------------------------------------------------------------------------
def getClient():
    '''
    Returns a reference to the singled client.
    
    Params: None
    
    Returns: a reference to the client (or None if there are network problems).
    
    Raises: Nothing
    '''
    global SINGLETON_CLIENT
    
    if SINGLETON_CLIENT == None:
        #If this function has never been called before...
        try:
            SINGLETON_CLIENT = _Client()
        except Exception, e:
            SINGLETON_CLIENT = None
            print_exc()

    return SINGLETON_CLIENT
#----------------------------------------------------------------------------
def log(): # pragma: NO COVER
    '''
    Returns log service reference.
    
    Params: None
    
    Returns: Log service reference.
    
    Raises: ???
    '''
    return getClient().getService('Log')
#----------------------------------------------------------------------------
def logFactory(): # pragma: NO COVER
    '''
    Returns log factory reference
    
    Params: None
    
    Returns: Log factory reference
    
    Raises: ???
    '''
    return getClient().getService('LogFactory')
#----------------------------------------------------------------------------
def notifyEventChannelFactory(): # pragma: NO COVER
    '''
    Returns Notification Event Channel factory reference
    
    Params: None
    
    Returns: Notification Event Channel factory reference
    
    Raises: ???
    '''
    necf = getClient().getService('NotifyEventChannelFactory')
    return necf._narrow(CosNotifyChannelAdmin.EventChannelFactory)
#----------------------------------------------------------------------------
def archivingChannel(): # pragma: NO COVER
    '''
    Returns archiving channel reference
    
    Params: None
    
    Returns: Archiving channel reference
    
    Raises: ???
    '''
    return getClient().getService('ArchivingChannel@ARCHIVING.channels')
#----------------------------------------------------------------------------
def loggingChannel(): # pragma: NO COVER
    '''
    Returns logging channel reference
    
    Params: None
    
    Returns: Logging channel reference
    
    Raises: ???
    '''

    if os.environ.has_key('ACS_LOG_BIN') and os.environ['ACS_LOG_BIN'] == "true":
        loggingChannelName = "LoggingChannelBin@LOGGING.channels"
    else:
        loggingChannelName = "LoggingChannel@LOGGING.channels"

    return getClient().getService(loggingChannelName)
#----------------------------------------------------------------------------
def interfaceRepository(): # pragma: NO COVER
    '''
    Returns interface repository reference
    
    Params: None
    
    Returns: Interface repository reference
    
    Raises: ???
    '''
    ifr = getClient().getService('InterfaceRepository')
    return ifr._narrow(CORBA.Repository)
#----------------------------------------------------------------------------
def cdb(): # pragma: NO COVER
    '''
    Returns configuration database reference
    
    Params: None
    
    Returns: Configuration database reference
    
    Raises: ???
    '''
    cdb_obj = getClient().getService('CDB')
    return cdb_obj._narrow(CDB.DAL)
#----------------------------------------------------------------------------
def acsLogSvc(): # pragma: NO COVER
    '''
    Returns ACS Log Service reference
    
    Params: None
    
    Returns: ACS Log Service reference
    
    Raises: ???
    '''
    als = getClient().getService('ACSLogSvc')
    return als._narrow(ACSLog.LogSvc)
#----------------------------------------------------------------------------
def nameService(): # pragma: NO COVER
    '''
    Returns name service reference
    
    Params: None
    
    Returns: Name service reference
    
    Raises: ???
    '''
    ns = getClient().getService('NameService')
    return ns._narrow(CosNaming.NamingContext)
#----------------------------------------------------------------------------
def maxRetry(cookie, retries, exc): # pragma: NO COVER
    if retries < 5:
        return True
    else:
        return False
#----------------------------------------------------------------------------


