# @(#) $Id: ACSCorba.py,v 1.15 2006/01/25 15:26:11 dfugate Exp $
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

Takes care of initializing ORB and setting initial reference to MACI
manager. Also provides functions to get service and device references
from the manager.

TODO:
- this module is not very safe! The first time it is imported the CORBA services
and manager must be up and running. If not, nothing will ever work. This should
be changed so that manager can go offline and a later invocation of this modules
methods should return new references to manager!
'''
__revision__ = "$Id: ACSCorba.py,v 1.15 2006/01/25 15:26:11 dfugate Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from sys       import argv
from os        import environ
from traceback import print_exc
from atexit    import register
from thread    import start_new_thread
from time      import sleep
#--CORBA STUBS-----------------------------------------------------------------
from omniORB import CORBA
from omniORB import importIRStubs
from omniORB import ir_idl
import maci__POA
from maci import Manager
#--ACS Imports-----------------------------------------------------------------
from AcsutilPy.ACSPorts          import getManagerPort
from AcsutilPy.ACSPorts          import getIP
#--GLOBALS---------------------------------------------------------------------
_orb = None
_poaRoot = None
_poaManager = None
_mgrRef = None
_manager = None
_client = None
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
  global _mgrRef

  if new_corbaloc != None:
    #Developer is trying to manually set the singled reference.
    _mgrRef = str(new_corbaloc)
  
  elif _mgrRef == None:
    #If this function has never been called before...
    #Check command-line args for the corbaloc
    for i in range(0, len(argv) - 1):
      if argv[i] == '-managerReference' or argv[i] == '-m':
        #Found it!  OK to set now.
        _mgrRef = argv[i + 1]
        break

    if _mgrRef == None:
      #Not in the command-line so check an environment variable reference
      if environ.has_key('MANAGER_REFERENCE'):
        #Found it!  OK to set now.
        _mgrRef = environ['MANAGER_REFERENCE']
      else:
        #Assume manager is running under the local machine
        _mgrRef = 'corbaloc::' + str(getIP()) + ':' + getManagerPort() + '/Manager'

  return _mgrRef
#----------------------------------------------------------------------------
def getORB(new_orb=None):
  '''
  Returns a reference to the singled ORB or sets it.

  Params: If the developer specifies a value other than None for new_orb,
  the singled value is set to this.

  Returns: a reference to the ORB (or None if there are network problems).

  Raises: Nothing.
  '''
  global _orb

  if new_orb != None:
    #Developer is trying to manually set the singled reference.
    _orb = new_orb
  elif _orb == None:
    #If this function has never been called before...
    try:
      _orb = CORBA.ORB_init(argv)
      #start_new_thread(run_orb_thread, ())
    except Exception, e:
      _orb = None
      print_exc()

  return _orb
#----------------------------------------------------------------------------
def run_orb_thread():
  '''
  Function used to run the orb in a separate thread of execution.
  '''
  if _orb != None:
    print "Running ORB"
    _orb.run()
#----------------------------------------------------------------------------
def getPOARoot(new_poaroot=None):
  '''
  Returns a reference to the singled root POA (or sets it).

  Params: If the developer specifies a value other than None for new_poaroot, the
  singled value is set to this.

  Returns: a reference to the root POA (or None if there are network problems).

  Raises: Nothing.
  '''
  global _poaRoot

  if new_poaroot != None:
    #Developer is trying to manually set the singled reference.
    _poaRoot = new_poaroot
  elif _poaRoot == None:
    #If this function has never been called before...
    try:
      _poaRoot = getORB().resolve_initial_references("RootPOA")
    except Exception, e:
      _poaRoot = None
      print_exc()
  
  return _poaRoot
#----------------------------------------------------------------------------
def getPOAManager(new_poamanager=None):
  '''
  Returns a reference to the singled POA manager (or sets it).

  Params: If the developer specifies a value other than None for
  new_poamanager, the singled value is set to this.

  Returns: a reference to the POA manager (or None if there are network
  problems).

  Raises: Nothing.
  '''
  global _poaManager

  if new_poamanager != None:
    #Developer is trying to manually set the singled reference.
    _poaManager = new_poamanager
  elif _poaManager == None:
    #If this function has never been called before...
    try:
      #Cannot do much without the POA manager
      _poaManager = getPOARoot()._get_the_POAManager()
      #In truth, it's pretty dangerous to start processing requests this
      #early but since this module is singled.
      _poaManager.activate()
    except Exception, e:
      _poaManager = None
      print_exc()
  
  return _poaManager
#----------------------------------------------------------------------------
def getManager(new_manager=None):
  '''
  Returns a reference to the singled Manager (or sets it).

  Params: If the developer specifies a value other than None for new_manager,
  the singled value is set to this.

  Returns: a reference to the Manager (or None if there are network problems).

  Raises: Nothing.
  '''
  global _manager

  if new_manager != None:
    #Developer is trying to manually set the singled reference.
    _manager = new_manager
  elif _manager == None:
    #If this function has never been called before...
    try:
      _manager = getORB().string_to_object(getManagerCorbaloc())
      if _manager!=None and (not CORBA.is_nil(_manager)):
        try:
          _manager._non_existent()
          _manager = _manager._narrow(Manager)
        except:
          _manager = None
      else:
        _manager = None
    except Exception, e:
      _manager = None
      print_exc()
  
  return _manager
#----------------------------------------------------------------------------
class _Client (maci__POA.Client):
  '''
  General use client for getting references to components and services from
  the MACI manager. Developers should ignore this class entirely.
  '''
  #--------------------------------------------------------------------------
  def __init__(self):
    '''
    '''
    
    #This only needs to be done once for omniORB
    importIRStubs()
    
    #Even though we do nothing with the POA manager, the reference must be
    #retreived so Python clients can begin sending/receiving CORBA requests
    getPOAManager()
    
    #CORBA reference to ourself
    self.corbaRef = self._this()

    if getManager()==None:
      print "Acspy.Util.ACSCorba._Client.__init__ - manager is not available."
      print "      This method will block until manager comes online!"

    if argv[0].split('/').pop()!="pydoc":
      while getManager()==None:
        sleep(1)
    else:
      print "This script is being executed by pydoc - will not block waiting"
      print "for manager: ", argv[0], argv[0].split('/').pop()
      
    #Must have a valid reference to the manager
    self.mgr = getManager()

    #Need a security token given from the manager to do anything
    self.token = self.mgr.login(self.corbaRef)

    #make sure this eventually gets shutdown
    register(self.disconnect)
    
  #--CLIENT IDL--------------------------------------------------------------
  def _get_name(self):
    '''
    Implementation of IDL method.
    '''
    return 'acsCORBA Client'
  #--CLIENT IDL--------------------------------------------------------------
  def disconnect(self):
    '''
    Implementation of IDL method.
    '''
    global _client
    global _orb
    global _poaRoot
    global _poaManager 
    global _mgrRef 
    global _manager
        
    self.mgr.logout(self.token.h)

    #wrap all of these ORB calls with try/except blocks to try to remove
    #some random omniORBPy thread exception messages that occur. it looks
    #as if Python's atexit module sometimes executes the disconnect method
    #after some function internal to omniORB has already killed everything
    #off
    try:
      self.corbaRef._release()
    except:
      pass

    try:
      getPOAManager().deactivate(CORBA.TRUE, CORBA.TRUE)
    except:
      pass

    try:
      getPOARoot().destroy(CORBA.TRUE, CORBA.TRUE)
    except:
      pass

    try:
      getORB().shutdown(CORBA.TRUE)
    except:
      pass

    try:
      getORB().destroy()
    except:
      pass
    
    _orb = None
    _poaRoot = None
    _poaManager = None
    _mgrRef = None
    _manager = None
    _client = None

    return
  #--CLIENT IDL--------------------------------------------------------------
  def authenticate(self, question):
    '''
    Implementation of IDL method.
    '''
    #to make the NRI happy
    question = None
    return "CacsCORBA Client"
  #--CLIENT IDL--------------------------------------------------------------
  def message(self, message_type, message):
    '''
    Implementation of IDL method.
    '''
    #to make the NRI happy
    message_type = None
    message = None
    
    return
  #--CLIENT IDL--------------------------------------------------------------
  def ping(self):
    '''
    Implementation of IDL method.
    '''
    return 1
  #--CLIENT IDL--------------------------------------------------------------
  def components_available(self, components):
    '''
    Implementation of IDL method.
    '''
    #to make the NRI happy
    components = None
    return
  #--CLIENT IDL--------------------------------------------------------------
  def components_unavailable(self, components):
    '''
    Implementation of IDL method.
    '''
    #to make the NRI happy
    components = None
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
    if self.mgr is not None:
      (component, status) = self.mgr.get_service(self.token.h, curl, activate)
      #to make the NRI happy
      status = None
      
      if component is not None:
        result = component
    return result
#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
def getClient(new_client=None):
  '''
  Returns a reference to the singled client or sets it.

  Params: If the developer specifies a value other than None for new_client,
  the singled value is set to this.

  Returns: a reference to the client (or None if there are network problems).

  Raises: Nothing.
  '''
  global _client

  if new_client != None:
    #Developer is trying to manually set the singled reference.
    _client = new_client
  elif _client == None:
    #If this function has never been called before...
    try:
      _client = _Client()
    except Exception, e:
      _client = None
      print_exc()

  return _client
#----------------------------------------------------------------------------
def log():
  '''
  Log service reference.

  Params: None

  Returns: Log service reference.

  Raises: ???
  '''
  return getClient().getService('Log')
#----------------------------------------------------------------------------
def logFactory():
  '''
  Log factory reference

  Params: None

  Returns: Log factory reference

  Raises: ???
  '''
  return getClient().getService('LogFactory')
#----------------------------------------------------------------------------
def notifyEventChannelFactory():
  '''
  Notification Event Channel factory reference

  Params: None

  Returns: Notification Event Channel factory reference

  Raises: ???
  '''
  import CosNotifyChannelAdmin
  return getClient().getService('NotifyEventChannelFactory')
#----------------------------------------------------------------------------
def archivingChannel():
  '''
  Archiving channel reference

  Params: None

  Returns: Archiving channel reference

  Raises: ???
  '''
  import CosNotifyChannelAdmin
  return getClient().getService('ArchivingChannel')
#----------------------------------------------------------------------------
def loggingChannel():
  '''
  Logging channel reference

  Params: None

  Returns: Logging channel reference

  Raises: ???
  '''
  import CosNotifyChannelAdmin
  return getClient().getService('LoggingChannel')
#----------------------------------------------------------------------------
def interfaceRepository():
  '''
  Interface repository reference

  Params: None

  Returns: Interface repository reference

  Raises: ???
  '''
  ifr = getClient().getService('InterfaceRepository')
  return ifr._narrow(CORBA.Repository)
#----------------------------------------------------------------------------
def cdb():
  '''
  Configuration database reference

  Params: None

  Returns: Configuration database reference

  Raises: ???
  '''
  import CDB
  return getClient().getService('CDB')._narrow(CDB.DAL)
#----------------------------------------------------------------------------
def acsLogSvc():
  '''
  ACS Log Service reference

  Params: None

  Returns: ACS Log Service reference

  Raises: ???
  '''
  import ACSLog
  return getClient().getService('ACSLogSvc')
#----------------------------------------------------------------------------
def nameService():
  '''
  Naming service reference

  Params: None

  Returns: Naming service reference

  Raises: ???
  '''
  import CosNaming
  return getClient().getService('NameService')._narrow(CosNaming.NamingContext)
#----------------------------------------------------------------------------

