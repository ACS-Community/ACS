# @(#) $Id: Container.py,v 1.59 2012/06/21 15:24:08 acaproni Exp $
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
# "@(#) $Id: Container.py,v 1.59 2012/06/21 15:24:08 acaproni Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2003/08/04  Created.
#------------------------------------------------------------------------------
'''
This module includes the implementation of a Python Container for the
maci::Container IDL interface.

TODO LIST:
- integrate with the new ACS Error System
- mutex lock methods (if needed)
- fix the interfaces param to ComponentInfo in activate_component(...)
- a ComponentLifecycleException has been defined in IDL now...
'''

__revision__ = "$Id: Container.py,v 1.59 2012/06/21 15:24:08 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from time      import sleep
from time      import time
from signal    import signal, SIGINT
from new       import instance
from traceback import print_exc
import sys
from os        import environ
from threading import Event, Thread
#--CORBA STUBS-----------------------------------------------------------------
import PortableServer
import maci
import maci__POA
import Logging
import Logging__POA
from CORBA import FALSE
from maci  import ComponentInfo
from ACS   import OffShoot
import ACS
from ACSErrTypeCommonImpl              import CORBAProblemExImpl, CouldntCreateObjectExImpl
from ACSErrTypeOKImpl import ACSErrOKCompletionImpl
from maciErrTypeImpl                   import CannotActivateComponentExImpl,\
    CannotActivateComponentCompletionImpl
#--ACS Imports-----------------------------------------------------------------
import Acspy.Common.Log as Log
import Acsalarmpy
from Acspy.Common.CDBAccess                 import CDBaccess
from cdbErrType                             import CDBRecordDoesNotExistEx
from Acspy.Clients.BaseClient               import BaseClient, ManagerAsyncConnector
from Acspy.Servants.ContainerServices       import ContainerServices
from Acspy.Servants.ComponentLifecycle      import ComponentLifecycle
from Acspy.Servants.ComponentLifecycle      import ComponentLifecycleException
from Acspy.Servants.ACSComponent            import ACSComponent
from Acspy.Servants.CharacteristicComponent import CharacteristicComponent
from Acspy.Util                             import ACSCorba
from AcsutilPy.FindFile                     import findFile
from Acspy.Util                             import LoggingConfig_xsd
from AcsContainerLogLTS                     import LOG_CompAct_Loading_OK
from AcsContainerLogLTS                     import LOG_CompAct_Instance_OK
from AcsContainerLogLTS                     import LOG_CompAct_Corba_OK
from AcsContainerLogLTS                     import LOG_CompAct_Init_OK
from maciErrTypeImpl                        import ComponentDeactivationFailedExImpl, ComponentDeactivationUncleanExImpl
from ACS                  import CBDescOut
#--GLOBALS---------------------------------------------------------------------
#Manager commands to this container
ACTIVATOR_RELOAD = 0
ACTIVATOR_REBOOT = 1
ACTIVATOR_EXIT   = 2

#The fields of a component dictionary entry
HANDLE      = 'HANDLE'
NAME        = 'NAME'
EXEID       = 'EXEID'
EXE         = 'EXE'
TYPE        = 'TYPE'
POA         = 'POA'
PYCLASS     = 'PYCLASS'
PYREF       = 'PYREF'
CORBAREF    = 'CORBAREF'
COMPONENTINFO     = 'COMPONENTINFO'
POAOFFSHOOT = 'POAOFFSHOOT'
COMPMODULE  = 'COMPMODULE'
#------------------------------------------------------------------------------
class ContainerLogThrottleAlarmer(Log.LogThrottleAlarmerBase):
    def __init__(self, cont):
        '''
        cont Is the container to invoke sendAlarm to send alarms
        '''
        self.container=cont
        
    def sendThrottleAlarm(self, active):
        '''
        See Log.LogThrottleAlarmerBase
        '''
        self.container.sendAlarm("Logging", self.container.name, 10, active)
#------------------------------------------------------------------------------
class AsyncComponentActivator(Thread):
    '''
    This thread activates the component asynchronously by delegating to
    Container#activate_component(...) 
    '''
    def __init__(self,logger, container,h, execution_id, componentName, exe, type, callback, desc):
        '''
        Constructor
        
        Parameters:
        - logger The logger
        - container A reference to the container
        - h is the handle the component will be given
        - name is simply the components name
        - exe is the name of the Python module that has to be imported for the
              components implementation
        - idl_type is the the IR Location for the component
        - callback To callback to notify that the component has been activated 
                   or report an error
        '''
        Thread.__init__(self,name="ComponentAsyncActivator thread for component "+componentName)
        #The logger
        self.logger=logger
        
        # The reference to the container, to call activate_component
        self.container=container
        
        # Store the params to activate the component
        self.h=h
        self.execution_id=execution_id
        self.componentName=componentName
        self.exe=exe
        self.type=type
        self.callback=callback
        self.desc=desc
        
        # It is a daemon so that it does not prevent the application to terminate
        self.daemon=True
    
    def run(self):
        self.logger.logDebug("Activating "+self.componentName+" (type "+self.type+")")
        descOut=CBDescOut(0L,self.desc.id_tag)
        try:
            componentInfo=self.container.activate_component(self.h, self.execution_id, self.componentName, self.exe, self.type)
        except Exception, e:
            e2 = e2 = CannotActivateComponentCompletionImpl(exception=e)
            dummyCI=ComponentInfo(type,  #string type;
                                  self.exe,  #string code;
                                  None,  #Object reference;
                                  self.componentName,  #string name;
                                  [],  #HandleSeq clients;
                                  self.container.token.h,  #Handle activator;
                                  self.container.name,   #string container_name;
                                  self.h,  #Handle h;
                                  0,  #AccessDescriptor access;
                                  []  #stringSeq interfaces;
                                  )
            try:
                self.callback.done(componentInfo, e2 , descOut)
            finally:
                return
        if componentInfo is None:
            e2 = CannotActivateComponentCompletionImpl()
            # Error activating component
            dummyCI=ComponentInfo(type,  #string type;
                                  exe,  #string code;
                                  None,  #Object reference;
                                  self.componentName,  #string name;
                                  [],  #HandleSeq clients;
                                  self.container.token.h,  #Handle activator;
                                  self.container.name,   #string container_name;
                                  h,  #Handle h;
                                  0,  #AccessDescriptor access;
                                  []  #stringSeq interfaces;
                                  )
            try:
                self.callback.done(componentInfo, e2, descOut)
            finally:
                return
        
        retry=0
        while retry<3:
            okCompletion=ACSErrOKCompletionImpl() 
            try:
                self.logger.logDelouse("Calling maci::CBComponentInfo::done with descOut.id_tag = "+str(descOut.id_tag)+" for '"+self.componentName+"'");
                self.callback.done(componentInfo,okCompletion , descOut);
                self.logger.logDelouse("Call to maci::CBComponentInfo::done with descOut.id_tag = "+str(descOut.id_tag)+" for '"+self.componentName+"' completed");
                return
            except Exception, e:
                retry=retry+1
                self.logger.logDelouse("Call to maci::CBComponentInfo::done with descOut.id_tag = "+str(descOut.id_tag)+" for '"+self.componentName+"' failed, retrying...");
                sleep(1)
        self.logger.logError("Call to maci::CBComponentInfo::done with descOut.id_tag = "+str(descOut.id_tag)+" for '"+self.componentName+"' failed, deactivating the component.");
        try:
            self.container.deactivate_component(h)
        except:
            self.logger.logError("Deactivation of component failed descOut.id_tag = "+str(descOut.id_tag)+" for '"+self.componentName+"'");
#------------------------------------------------------------------------------
class Container(maci__POA.Container, Logging__POA.LoggingConfigurable, BaseClient):
    '''
    The Python implementation of a MACI Container.

    This implies it implements both the Container and Client interfaces. It is
    multi-threaded and currently supports components derived from
    ACSComponent, ContainerServices, and the LifeCycle classes.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name):
        '''
        Constructor.

        Initializes member variables and CORBA

        Parameters: name is the stringified name of this container.

        Raises: ???
        '''

        print maci.Container.ContainerStatusStartupBeginMsg
        #Member variables
        self.isReady = Event()
        self.running = 1  #As long as this is true, container is not shutdown
        self.name = name  #Container Name
        self.canRecover = True  #Whether this container is capable of recovery
        self.components = {}  #A dict where components are referenced by name
        self.compHandles = {}  #A dict where comp names are referenced by handles
        self.shutdownHandles = []
        self.containerPOA = None  #POA to activate this container
        self.componentPOA = None  #POA to create POAs for components
        self.compPolicies = []  #Policy[] for components
        self.offShootPolicies = []  #Policy[] for offshoots
        self.corbaRef = None  #reference to this object's CORBA reference
        self.logger = Log.getLogger(name) # Container's logger
        self.client_type = maci.CONTAINER_TYPE
        self.cdbContainerInfo = {}
        self.autoLoadPackages = []
        #dictionary which maps package names to the number of active components
        #using said package
        self.compModuleCount = {}
        
        # A reference to the thread to autoreconnect
        # @see slef.disconnect()
        self.reconnectionThread = None
        
        # Initialize the alarm factory
        Acsalarmpy.AlarmSystemInterfaceFactory.init(ACSCorba.getManager())
        # The alarm source
        self.alarmSource = None
        
        # The interface to receive notification from the LogThrottle 
        # for sending alarms
        self.clta=ContainerLogThrottleAlarmer(self)

        #Configure CORBA
        print maci.Container.ContainerStatusORBInitBeginMsg
        self.configCORBA()
        print maci.Container.ContainerStatusORBInitEndMsg

        #call superclass constructor
        print maci.Container.ContainerStatusMgrInitBeginMsg
        BaseClient.__init__(self, self.name)
        print maci.Container.ContainerStatusMgrInitEndMsg

        self.logger.logTrace('CORBA configured for Container: ' + self.name)

        self.cdbAccess = CDBaccess()

        self.logger.logTrace('Starting Container: ' + self.name)

        #get info from the CDB
        self.getCDBInfo()
        self.refresh_logging_config()
        self.configureComponentLogger(name)

        #Run everything
        self.logger.logInfo('Container ' + self.name + ' waiting for requests')
        self.isReady.set()
        print maci.Container.ContainerStatusStartupEndMsg
        sys.stdout.flush()
        
    #--CLIENT IDL--------------------------------------------------------------
    def disconnect(self): # pragma: NO COVER
        '''
        Disconnect from manager.

        oneway void disconnect ();
        
        The container overrides BaseClient.disconnect() because it tries 
        to reconnect to the manager.
        '''
        self.logger.logTrace('Disconnect called for Container: ' + self.name)
        BaseClient.disconnect(self)
        self.logger.logInfo("Logged out from manager")
        
        # Give time to the manger to shutdown or whatever else
        sleep(10)
        
        # Start the thread to autoreconenct to the manager
        try:
            retries=int(self.cdbContainerInfo[0]['ManagerRetry'])
        except:
            # no entry in CDB: default to 10
            retries=10
        if self.reconnectionThread is not None:
            if self.reconnectionThread.isAlive():
                # This method has been before the previous attempt terminated
                # so we ignore the call
                self.logger.logWarning("The thread to aysnchronously reconnect to the Manager is already running")
                return
            
        self.reconnectionThread=ManagerAsyncConnector(client=self,attempts=retries,logger=self.logger)
        self.reconnectionThread.daemon=True
        self.reconnectionThread.start()
        
    #--CLIENT IDL--------------------------------------------------------------
    def taggedmessage (self, message_type, message_id, message): # pragma: NO COVER
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
        if message_id == maci.Client.MSGID_AUTOLOAD_START:
            print maci.Container.ContainerStatusCompAutoloadBeginMsg
        if message_type == maci.Client.MSG_ERROR:
            self.logger.logWarning("Error message from the manager: " + message)
            sys.stdout.flush()

        elif message_type == maci.Client.MSG_INFORMATION:
            self.logger.logInfo("Info message from the manager: " + message)
            sys.stdout.flush()

        else:
            self.logger.logInfo("Message of unknown type from the manager: " + message)
            sys.stdout.flush()
        if message_id == maci.Client.MSGID_AUTOLOAD_END:
            print maci.Container.ContainerStatusCompAutoloadEndMsg
            print maci.Container.ContainerStatusReadyMsg
            sys.stdout.flush()

    #--ACTIVATOR IDL-----------------------------------------------------------
    def activate_component(self, h, exeid, name, exe, idl_type):
        '''
        Activates a component (or returns a reference to it if already exists).

        Parameters:
        - h is the handle the component will be given
        - name is simply the components name
        - exe is the name of the Python module that has to be imported for the
        components implementation
        - idl_type is the the IR Location for the component

        Raises: CannotActivateComponentExImpl exception when invalid

        Returns: a ComponentInfo structure for manager to use.

        activate_component(in Handle h,in string name,in string exe,in string idl_type)
        '''
        #Block requests while container is initializing
        self.isReady.wait()

        #Check to see if this Component already exists
        comp = self.getExistingComponent(name)
        if comp != None:
            return comp[COMPONENTINFO]

        #Create a dictionary record for this component
        self.components[name] = None
        temp = {}
        try:
            temp[HANDLE] = h  #Handle of the component that is being activated
            temp[NAME] = name  #Name-redundant but possibly useful
            temp[EXEID] = exeid  #Execution ID number for the component being activated.
            temp[EXE] = exe  #Python module containing servant implementation
            temp[TYPE] = idl_type  #The type of the component to instantiate
            temp[POA] = self.createPOAForComponent(name)  #POA for this component
            temp[POAOFFSHOOT] = temp[POA].create_POA("OffShootPOA", ACSCorba.getPOAManager(), self.offShootPolicies)
            temp[PYCLASS] = None  #Class object used for this component
            temp[PYREF] = None  #Reference to the python object
            temp[CORBAREF] = None  #Reference to the CORBA object
            temp[COMPONENTINFO] = None  #An IDL struct given to manager
            temp[PYCLASS] = temp[TYPE].split(':')[1].split('/').pop() #get class name

            start = time()
            temp[COMPMODULE] = __import__(temp[EXE], globals(), locals(), [temp[PYCLASS]]) #get module
            interval = time() - start

            log = LOG_CompAct_Loading_OK()
            log.setTimeMillis(int(interval*1000))
            log.setCompName(name)
            log.log()

            try:
                temp[PYCLASS] = temp[EXE].split('.').pop() #get class name
                temp[PYCLASS] = temp[COMPMODULE].__dict__.get(temp[PYCLASS]) #get class
                temp[PYREF] = instance(temp[PYCLASS]) #create Python object
            except Exception, e:
                temp[PYCLASS] = temp[COMPMODULE].__dict__.get(temp[PYCLASS]) #get class
                temp[PYREF] = instance(temp[PYCLASS]) #create Python object

        except (TypeError, ImportError), e:
            e2 = CannotActivateComponentExImpl(exception=e)
            if isinstance(e,TypeError):
                e2.setDetailedReason("Verify that the name of implementation class matches the module name *%s*" % temp[EXE].split('.').pop())
            else:
                e2.setDetailedReason("Verify that CDB Code entry and Python install path match for module *%s*" % temp[EXE])

            self.failedActivation(temp)
            e2.log(self.logger)
            raise e2
        except Exception, e:
            self.logger.logWarning("Failed to create Python object for: " + name)
            print_exc()
            self.failedActivation(temp)
            return None

        #these are some non-standard members needed by the component simulator
        temp[PYREF].ir = temp[TYPE]
        temp[PYREF].library = temp[EXE]

        #instance(...) does not call the constructor!
        try:
            start = time()
            temp[PYREF].__init__()
            interval = time() - start

            log = LOG_CompAct_Instance_OK()
            log.setTimeMillis(int(interval*1000))
            log.setCompName(name)
            log.log()
        except:
            print_exc()
            self.logger.logWarning("Standard constructor does not exist for: " + name)

        #Check to see if its derived from ContainerServices
        if isinstance(temp[PYREF], ContainerServices):
            temp[PYREF].setAll(temp[NAME],  #string-name of component
                               self.token,   #Security handle from manager
                               temp[HANDLE],  #Security handle from manager
                               self.activateOffShoot,  #Container's method
                               self.name # Container's Name
                               )

        #Check to see if it's an ACSComponent next
        if isinstance(temp[PYREF], ACSComponent) or isinstance(temp[PYREF], CharacteristicComponent):
            temp[PYREF].setName(temp[NAME])

        #Check to see if it's derived from ComponentLifeCycle next!!!
        #If it is, we have to mess with the state model and invoke the lifecycle
        #methods accordingly.
        if isinstance(temp[PYREF], ComponentLifecycle):

            start = time()
            try:
                #Have to mess with the state model
                if isinstance(temp[PYREF], ACSComponent):
                    #this assumes the component's constructor will NOT change
                    #the state!
                    temp[PYREF].setComponentState(ACS.COMPSTATE_INITIALIZING)
                #actually initialize the sucker
                temp[PYREF].initialize()
                #if it's an ACSComponent...
                temp_state = temp[PYREF]._get_componentState()
                if (isinstance(temp[PYREF], ACSComponent) or isinstance(temp[PYREF], CharacteristicComponent)) and (temp_state==ACS.COMPSTATE_INITIALIZED or temp_state==ACS.COMPSTATE_INITIALIZING):
                    #good...initialize did not fail.
                    temp[PYREF].setComponentState(ACS.COMPSTATE_INITIALIZED)
                elif isinstance(temp[PYREF], ACSComponent) or isinstance(temp[PYREF], CharacteristicComponent):
                    #bad...the developer has changed the state from the initialize method.  warn the user but continue
                    self.logger.logWarning("initialize method of " +
                                           "ComponentLifecycle failed for the '" +
                                           temp[NAME] + "' component changed the component's state to something unexpected!")

            except ComponentLifecycleException, e:
                print_exc()
                self.logger.logWarning("initializeComponent method of " +
                                       "ComponentLifecycle failed for the '" +
                                       temp[NAME] + "' component with this message: " +
                                       str(e.args))
                #Have to mess with the state model
                if isinstance(temp[PYREF], ACSComponent) or isinstance(temp[PYREF], CharacteristicComponent):
                    temp[PYREF].setComponentState(ACS.COMPSTATE_ERROR)

            except Exception, e:
                print_exc()
                self.logger.logCritical("initializeComponent method of " +
                                        "ComponentLifecycle failed for the '" +
                                        temp[NAME] + "'.\n" + str(e.args) + "\nDestroying!")
                self.failedActivation(temp)
                return None

            try:
                temp[PYREF].execute()

                #Have to mess with the state model
                temp_state = temp[PYREF]._get_componentState()
                if (isinstance(temp[PYREF], ACSComponent) or isinstance(temp[PYREF], CharacteristicComponent)) and (temp_state==ACS.COMPSTATE_INITIALIZED or temp_state==ACS.COMPSTATE_OPERATIONAL):
                    temp[PYREF].setComponentState(ACS.COMPSTATE_OPERATIONAL)
                elif isinstance(temp[PYREF], ACSComponent) or isinstance(temp[PYREF], CharacteristicComponent):
                    #bad...the developer has changed the state from the initialize method.  warn the user but continue
                    self.logger.logWarning("execute method of " +
                                           "ComponentLifecycle failed for the '" +
                                           temp[NAME] + "' component: changed the component's state to something unexpected!")
            except ComponentLifecycleException, e:
                print_exc()
                self.logger.logWarning("executeComponent method of " +
                                       "ComponentLifecycle failed for the '" +
                                       temp[NAME] + "' component with this message: " +
                                       str(e.args))
                #Have to mess with the state model
                if isinstance(temp[PYREF], ACSComponent) or isinstance(temp[PYREF], CharacteristicComponent):
                    temp[PYREF].setComponentState(ACS.COMPSTATE_ERROR)


            except Exception, e:
                print_exc()
                self.logger.logCritical("executeComponent method of " +
                                        "ComponentLifecycle failed for the '" +
                                        temp[NAME] + "'.\n" + str(e.args) + "\nDestroying!")
                self.failedActivation(temp)
                return None

            interval = time() - start

            log = LOG_CompAct_Init_OK()
            log.setTimeMillis(int(interval*1000))
            log.setCompName(name)
            log.log()


        #DWF-should check to see if it's derived from CharacteristicComponent next!!!

        #Next activate it as a CORBA object.
        try:
            start = time()
            temp[POA].activate_object_with_id(temp[NAME], temp[PYREF])
            temp[CORBAREF] = temp[POA].servant_to_reference(temp[PYREF])
            interval = time() - start

            log = LOG_CompAct_Corba_OK()
            log.setTimeMillis(int(interval*1000))
            log.setCompName(name)
            log.log()
            if(temp[CORBAREF]==None):
                self.logger.logWarning("CORBA object Nil for: " + name)
                self.failedActivation(temp)
                return None
            #hack to give the component access to it's own CORBA reference
            temp[PYREF]._corbaRef = temp[CORBAREF]
        except:
            print_exc()
            self.logger.logWarning("Failed to create CORBA object for: " + name)
            self.failedActivation(temp)
            return None

        #Create the structure and give it to manager
        #DWF-FIX ME!!! The next line screws everything up for some reason!
        #temp[PYREF]._get_interface()
        #DWF-warning...this assumes temp[TYPE] is the IR ID...
        interfaces = ["IDL:omg.org/CORBA/Object:1.0", temp[TYPE]]
        temp[COMPONENTINFO] = ComponentInfo(temp[TYPE],  #string type;
                                            temp[EXE],  #string code;
                                            temp[CORBAREF],  #Object reference;
                                            name,  #string name;
                                            [],  #HandleSeq clients;
                                            self.token.h,  #Handle activator;
                                            self.name,   #string container_name;
                                            temp[HANDLE],  #Handle h;
                                            0,  #AccessDescriptor access;
                                            interfaces  #stringSeq interfaces;
                                            )

        #Make a copy of everything for the container
        self.compHandles[temp[HANDLE]] = temp[NAME]
        self.components[name] = temp

        #keep track of how many components are using the package
        if not self.compModuleCount.has_key(temp[COMPMODULE]):
            self.compModuleCount[temp[COMPMODULE]] = 1
        else:
            self.compModuleCount[temp[COMPMODULE]] = self.compModuleCount[temp[COMPMODULE]] + 1
        #configure the components logger from the CDB
        self.configureComponentLogger(name)

        self.logger.logInfo("Activated component: " + name)

        return self.components[name][COMPONENTINFO]

    def activate_component_async(self, h, execution_id, name, exe, type, callback, desc):
        '''
        Activates a component asynchronously.
        
        Parameters:
        - h is the handle the component will be given
        - name is simply the components name
        - exe is the name of the Python module that has to be imported for the
              components implementation
        - idl_type is the the IR Location for the component
        - callback To callback to notify that the component has been activated 
                   or report an error
        '''
        self.logger.logDelouse("Starting async activation of "+name+"(type "+type+")")
        activatorThread=AsyncComponentActivator(
                                                self.logger,
                                                self,
                                                h, 
                                                execution_id, 
                                                name, 
                                                exe, 
                                                type, 
                                                callback, 
                                                desc)
        activatorThread.start()
        
        
    #--------------------------------------------------------------------------
    def failedActivation(self, comp_entry): # pragma: NO COVER
        '''
        Helper method used to destroy various things if the component cannot
        be created.

        Parameters:
        comp_entry - dictionary describing the component
        '''
        #release the corba reference
        try:
            comp_entry[CORBAREF]._release()
        except:
            pass

        #destroy the Offshoot POA
        try:
            comp_entry[POAOFFSHOOT].destroy(FALSE, FALSE)
        except:
            pass

        #deactivate the component's underlying CORBA object
        try:
            comp_entry[POA].deactivate_object(comp_entry[NAME])
        except:
            pass

        #destroy the component's "personal" POA
        try:
            comp_entry[POA].destroy(FALSE, FALSE)
        except:
            pass

        del self.components[comp_entry[NAME]]

    #--ACTIVATOR IDL-----------------------------------------------------------
    def deactivate_component(self, handle):
        '''
        Deactivate all components whose handles are given.

        Deactivation is the inverse process of activation: component is detached from
        the POA, and thus made unavailable through CORBA, and its resources are
        freed. If its code-base is no longer used, it is unloaded from memory.

        Parameters: handle, specifies a component handle

        void deactivate_component (in Handle h)
        '''

        deactivationUncleanEx = None
        deactivationFailedEx = None
        try:
            comp_entry = self.components[self.compHandles[handle]]
        except:
            self.logger.logWarning("No entry for handle: " + str(handle))
            print_exc()
            return
        self.logger.logInfo("Deactivating component: " + comp_entry[NAME])

        #release the corba reference
        comp_entry[CORBAREF]._release()

        #destroy the Offshoot POA
        comp_entry[POAOFFSHOOT].destroy(FALSE, FALSE)

        #deactivate the component's underlying CORBA object
        comp_entry[POA].deactivate_object(comp_entry[NAME])

        #Have to mess with the state model
        if isinstance(comp_entry[PYREF], ACSComponent) or isinstance(comp_entry[PYREF], CharacteristicComponent):
            comp_entry[PYREF].setComponentState(ACS.COMPSTATE_DESTROYING)

        try:  #Invoke the cleanUp method if implemented...
            comp_entry[PYREF].cleanUp()
        except Exception, e:
            self.logger.logAlert("Failed to invoke 'cleanUp' LifeCycle method of: " + comp_entry[NAME])
            print_exc()
            deactivationUncleanEx = ComponentDeactivationUncleanExImpl(exception = e)
            deactivationUncleanEx.setCURL(self.compHandles[handle]);
            deactivationUncleanEx.setReason("Failed to invoke 'cleanUp' LifeCycle method of: " + comp_entry[NAME]);

        #destroy the component's "personal" POA
        comp_entry[POA].destroy(FALSE, FALSE)

        #Have to mess with the state model
        if isinstance(comp_entry[PYREF], ACSComponent) or isinstance(comp_entry[PYREF], CharacteristicComponent):
            comp_entry[PYREF].setComponentState(ACS.COMPSTATE_DEFUNCT)

        #remove one from the container's list of modules
        self.compModuleCount[comp_entry[COMPMODULE]] = self.compModuleCount[comp_entry[COMPMODULE]] - 1

        #if the number of references to this module falls to zero, it should be reloaded
        if self.compModuleCount[comp_entry[COMPMODULE]] == 0:
            try:
                reload(comp_entry[COMPMODULE])
            except:
                self.logger.logWarning("Unable to reload:" + str(comp_entry[COMPMODULE]))
                print_exc()

            #remove it from the container's list
            del self.compModuleCount[comp_entry[COMPMODULE]]

        #Finally delete our references so the garbage collector can be used
        del self.components[self.compHandles[handle]]
        del self.compHandles[handle]
        if deactivationFailedEx is not None:
            raise deactivationFailedEx
        if deactivationUncleanEx is not None:
            raise deactivationUncleanEx
        return
    #--LOGGINGCONFIGURABLE IDL-----------------------------------------------------------
    def configureComponentLogger(self, name):
        '''
        Configure the logger for the given component name from the values in the CDB.

        Parameters:
        name is the name of the component
        '''
        # Each component has an associated logger instance
        clogger = Log.getLogger(name)

        # Default levels are used for missing values
        defaultlevels = Log.getDefaultLevels()
        
        try:
            #Get the global unnamed logging config to retrieve the maxLogsPerSecond attribute
            logconfigG = self.cdbAccess.getElement("MACI/Containers/"  + self.name + "/LoggingConfig", "LoggingConfig")
            maxLogsPerSec = int(logconfigG[0]['maxLogsPerSecond'])
        except:
            # No value was supplied so default is used
            maxLogsPerSec = -1
        
        try:
            # Process all the named logger configurations
            logconfig = self.cdbAccess.getElement("MACI/Containers/"  + self.name + "/LoggingConfig", "LoggingConfig/log:_")
            for cfg in logconfig:
                if cfg["Name"] == name:
                    try:
                        centrallevel = int(cfg['minLogLevel'])
                    except KeyError:
                        # No value was supplied so default is used
                        centrallevel = defaultlevels.minLogLevel
                    try:
                        locallevel = int(cfg['minLogLevelLocal'])
                    except KeyError:
                        # No value was supplied so default is used
                        locallevel = defaultlevels.minLogLevelLocal
                    
                    clogger.setLevels(Logging.LoggingConfigurable.LogLevels(False, centrallevel, locallevel))
                    clogger.configureLogging(maxLogsPerSec)
                    # There should only be one entry per logger so we are done
                    break
            else:
                # No matching named logger was found so the default values are used
                clogger.setLevels(Logging.LoggingConfigurable.LogLevels(True, 0, 0))
        except Exception:
            # No named loggers were defined so the default values are used
            clogger.setLevels(Logging.LoggingConfigurable.LogLevels(True, 0, 0))

    #--LOGGINGCONFIGURABLE IDL-----------------------------------------------------------
    def get_default_logLevels(self): # pragma: NO COVER
        '''
        Retrieve the default log levels used in this container.

        Returns: Logging.LoggingConfigurable.LogLevels instance containing default log level values

        Raises: Nothing
        '''
        return Log.getDefaultLevels()

    #--LOGGINGCONFIGURABLE IDL-----------------------------------------------------------
    def set_default_logLevels(self, levels): # pragma: NO COVER
        '''
        Set the default log level for this container.

        Parameter:
        levels - Logging.LoggingConfigurable.LogLevels instance containing default log level values

        Raises: Nothing
        '''
        Log.setDefaultLevels(levels)

    #--LOGGINGCONFIGURABLE IDL-----------------------------------------------------------
    def get_logger_names(self): # pragma: NO COVER
        '''
        Retrieve the names of the currently active loggers

        Returns: list of logger name strings
        '''
        return Log.getLoggerNames()

    #--LOGGINGCONFIGURABLE IDL-----------------------------------------------------------
    def get_logLevels(self, logger_name):
        """
        Retrieve the log levels for a given component.

        Parameter:
        logger_name - name of the component's logger

        Returns: Logging.LoggingConfigurable.LogLevels instance containing the logger's log level values

        Raises: Logging.LoggerDoesNotExistEx if the logger is not active.
        """
        if Log.doesLoggerExist(logger_name):
            return Log.getLogger(logger_name).getLevels()
        else:
            raise Logging.LoggerDoesNotExistEx(logger_name)

    #--LOGGINGCONFIGURABLE IDL-----------------------------------------------------------
    def set_logLevels(self, logger_name, levels):
        """
        Set the default log level for this component.

        Parameter:
        logger_name - name of the component's logger
        levels - Logging.LoggingConfigurable.LogLevels instance containing default log level values

        Raises: Logging.LoggerDoesNotExistEx if the logger is not active.
        """
        if Log.doesLoggerExist(logger_name):
            Log.getLogger(logger_name).setLevels(levels)
        else:
            raise Logging.LoggerDoesNotExistEx(logger_name)

    #--LOGGINGCONFIGURABLE IDL-----------------------------------------------------------
    def refresh_logging_config(self):
        '''
        Reset the logging configuration to the original CDB settings.

        Returns:  Nothing

        Raises:  Nothing
        '''

        # Default values from the XML Schema
        lcfg = LoggingConfig_xsd.LoggingConfig()
        centrallevel = lcfg.minLogLevel
        locallevel = lcfg.minLogLevelLocal
        cap = lcfg.maxLogQueueSize
        batch = lcfg.dispatchPacketSize
        displevel = lcfg.immediateDispatchLevel
        flush = lcfg.flushPeriodSeconds

        # Retrieve the CDB information
        try:
            logconfig = self.cdbAccess.getElement("MACI/Containers/"  + self.name + "/LoggingConfig", "LoggingConfig")
            try:
                centrallevel = int(logconfig[0]['minLogLevel'])
            except:
                # Default value used because CDB has no setting for this attribute
                pass
            try:
                locallevel = int(logconfig[0]['minLogLevelLocal'])
            except:
                # Default value used because CDB has no setting for this attribute
                pass
            try:
                cap = int(logconfig[0]['maxLogQueueSize'])
            except:
                # Default value used because CDB has no setting for this attribute
                pass
            try:
                batch = int(logconfig[0]['dispatchPacketSize'])
            except:
                # Default value used because CDB has no setting for this attribute
                pass
            try:
                displevel = int(logconfig[0]['immediateDispatchLevel'])
            except:
                # Default value used because CDB has no setting for this attribute
                pass
            try:
                flush = int(logconfig[0]['flushPeriodSeconds'])
            except:
                # Default value used because CDB has no setting for this attribute
                pass

            # Refresh all named loggers from the CDB as well.
            for log in self.get_logger_names():
                self.configureComponentLogger(log)
        except:
            # No logging configuration given in the CDB so defaults are used.
            pass

        # Environment variable takes precedence over the CDB value
        if 'ACS_LOG_CENTRAL' in environ:
            centrallevel = int(environ['ACS_LOG_CENTRAL'])

        if 'ACS_LOG_STDOUT' in environ:
            locallevel = int(environ['ACS_LOG_STDOUT'])

        Log.setDefaultLevels(Logging.LoggingConfigurable.LogLevels(False, centrallevel, locallevel))
        Log.setCapacity(cap)
        Log.setBatchSize(batch)
        Log.setImmediateDispatchLevel(displevel)

        # No need to create another flush thread when one already exists.
        if Log.isFlushRunning():
            Log.setFlushInterval(flush)
        else:
            Log.startPeriodicFlush(flush)

    #--CONTAINER IDL-----------------------------------------------------------
    def shutdown(self, action):
        '''
        Shutdown the Container.

        Normally invoked via CORBA but can also "self terminate" so to speak.

        Parameters:
        - action is an encrypted value that tells the container what action to take

        oneway void shutdown (in unsigned long action)
        '''
        
        if self.reconnectionThread is not None:
            # Signal the thread to terminate
            self.reconnectionThread.terminateThread()
            
        action = (action >> 8) & 0xFF

        if (action == ACTIVATOR_EXIT) or (action == ACTIVATOR_REBOOT) or (action == ACTIVATOR_RELOAD):

            self.logger.logTrace("Shutting down container: " + self.name)

            #Logout from manager
            BaseClient.managerLogout(self)

            if (action == ACTIVATOR_REBOOT) or (action == ACTIVATOR_RELOAD):
                print "Container.shutdown(): Action may not work correctly...-", str(action)
                self.__init__(self.name)
            else:
                # Close the alarm interface factory
                Acsalarmpy.AlarmSystemInterfaceFactory.done()
                #tell the main thread of execution to stop
                self.running = 0
                Log.stopPeriodicFlush()
        else:
            self.logger.logWarning("Unable to process 'shutdown' request at this time: " + str(action))
        

    #----------------------------------------------------------------------------
    def set_component_shutdown_order(self, handles): # pragma: NO COVER
        '''
        Set component shutdown order.

        void set_component_shutdown_order(in HandleSeq h);
        '''
        self.shutdownHandles = handles
    #----------------------------------------------------------------------------
    def get_component_info(self, handles):
        '''
        Returns information about a subset of components that are currently hosted by
        the Container.

        Note:  If the list of handles is empty, information about all components hosted
        by the activator is returned!

        Parmaters: handles is a sequence of integers specifiying component handles.
        Return: Information about the selected components.

        ComponentInfoSeq get_component_info (in HandleSeq h);
        '''
        return_seq=[]

        if (handles == None) or (handles == []):
            for record in self.components.keys():
                return_seq.append(self.components[record][COMPONENTINFO])
            return return_seq

        for handle in handles:
            if self.compHandles.has_key(handle):
                return_seq.append(self.components[self.compHandles[handle]][COMPONENTINFO])
            else:
                self.logger.logWarning("Container has no components with handle:" + str(handle))
        return return_seq
    #--------------------------------------------------------------------------
    def getCDBInfo(self):
        '''
        getCDBInfo is a helper method which is responsible for retrieving info
        from the CDB associated with this container.

        Parameters: None

        Return: None

        Raises: ???
        '''
        #obtain generic container information
        try:
            self.cdbContainerInfo = self.cdbAccess.getElement("MACI/Containers/"  + self.name, "Container")
        except:
            self.logger.logDebug("No container information found in the CDB")
            return

        #get a list of libraries to preload
        # [{'string': 'baci'}]
        temp_list = []
        try:
            temp_list = self.cdbAccess.getElement("MACI/Containers/" + self.name + "/Autoload", "Autoload/cdb:_")
        except:
            self.logger.logDebug("No autoload elemnt found in the CDB")
            return

        #get rid of libraries that can't be found!
        for temp_dict in temp_list:
            package = temp_dict['string']
            package = findFile("bin/" + str(package))[0]
            if package != "":
                #if it really exists add it
                self.autoLoadPackages.append(package)
            else:
                self.logger.logAlert("The '" + str(temp_dict['string']) + "' Python script specified by this container's CDB Autoload element cannot be found!")

        #now try loading the packages!
        for temp_package in self.autoLoadPackages:
            try:
                execfile(temp_package)
            except:
                self.logger.logCritical("There was a problem autoloading the '" + str(temp_package) + "' Python script!")
                print_exc()
                
        try:
            #Get the global unnamed logging config to retrieve the maxLogsPerSecond attribute
            logconfigG = self.cdbAccess.getElement("MACI/Containers/"  + self.name + "/LoggingConfig", "LoggingConfig")
            maxLogsPerSec = int(logconfigG[0]['maxLogsPerSecond'])
        except:
            # No value was supplied so default is used
            maxLogsPerSec = -1
        self.logger.configureLogging(maxLogsPerSec,self.clta)


    #--------------------------------------------------------------------------
    def configCORBA(self): # pragma: NO COVER
        '''
        configCORBA is a helper method responsible for initializing the ORB,
        POAs, etc.

        Parameters: None

        Return: None

        Raises: ???
        '''

        #Create the Container's POA
        try:
            cont_policies = []  #CORBA.PolicyList
            cont_policies.append(ACSCorba.getPOARoot().create_id_assignment_policy(PortableServer.USER_ID))
            cont_policies.append(ACSCorba.getPOARoot().create_lifespan_policy(PortableServer.PERSISTENT))
            cont_policies.append(ACSCorba.getPOARoot().create_request_processing_policy(PortableServer.USE_ACTIVE_OBJECT_MAP_ONLY))
            cont_policies.append(ACSCorba.getPOARoot().create_servant_retention_policy(PortableServer.RETAIN))
            self.containerPOA = ACSCorba.getPOARoot().create_POA("ContainerPOA", ACSCorba.getPOAManager(), cont_policies)
            for policy in cont_policies:
                policy.destroy()
        except Exception, e:
            self.logger.logWarning("Unable to create the container's POA - " + str(e))
            print_exc()
            raise CouldntCreateObjectExImpl()

        #Create the Components POA
        try:
            self.compPolicies.append(ACSCorba.getPOARoot().create_id_assignment_policy(PortableServer.USER_ID))
            self.compPolicies.append(ACSCorba.getPOARoot().create_lifespan_policy(PortableServer.PERSISTENT))
            self.compPolicies.append(ACSCorba.getPOARoot().create_request_processing_policy(PortableServer.USE_SERVANT_MANAGER))
            self.compPolicies.append(ACSCorba.getPOARoot().create_servant_retention_policy(PortableServer.RETAIN))
            self.componentPOA = ACSCorba.getPOARoot().create_POA("ComponentPOA", ACSCorba.getPOAManager(), self.compPolicies)
        except Exception, e:
            self.logger.logWarning("Unable to create the components' POA - " + str(e))
            print_exc()
            raise CouldntCreateObjectExImpl()

        #Create the Offshoot Policies
        try:
            self.offShootPolicies.append(ACSCorba.getPOARoot().create_id_assignment_policy(PortableServer.SYSTEM_ID))
            self.offShootPolicies.append(ACSCorba.getPOARoot().create_lifespan_policy(PortableServer.TRANSIENT))
            self.offShootPolicies.append(ACSCorba.getPOARoot().create_request_processing_policy(PortableServer.USE_ACTIVE_OBJECT_MAP_ONLY))
            self.offShootPolicies.append(ACSCorba.getPOARoot().create_servant_retention_policy(PortableServer.RETAIN))
        except Exception, e:
            self.logger.logWarning("Unable to create the OffShoots' POA - " + str(e))
            print_exc()
            raise CouldntCreateObjectExImpl()

        # register this object with the Container POA and have
        # it come alive
        try:
            self.containerPOA.activate_object_with_id(self.name, self)
        except Exception, e:
            self.logger.logWarning("Unable to activate this container as a CORBA servant - " +
                                   str(e))
            print_exc()
            raise CORBAProblemExImpl()
    #--------------------------------------------------------------------------
    def getManagerHost(self): # pragma: NO COVER
        '''
        Helper function returns a string consisting of managers host.

        Return: hostname where manager is running.

        Raises: ???
        '''
        temp = ACSCorba.getManagerCorbaloc()
        # words end up as ['corbaloc', '', 'condor', 'xxxx/Manager']
        words = temp.split(':')
        return words[2]
    #--------------------------------------------------------------------------
    def run(self): # pragma: NO COVER
        '''
        Runs the container until a sig-int is caught.

        This is a blocking call!

        Raises: ???
        '''
        signal(SIGINT, self.handler)
        while(self.running):
            sleep(1)
        self.destroyCORBA()
        print "Goodbye"
        sys.stdout.flush()
    #--------------------------------------------------------------------------
    def handler(self, signum, frame): # pragma: NO COVER
        '''
        Catches SIGINTs and shuts down the container.

        Used only by the run method.

        Parameters: signum is the signal being caught
        frame is the execution frame.

        Return: None

        Raises: ???
        '''
        #to make pychecker happy
        signum = None

        #to make pychecker happy
        frame = None

        print "-->Signal Interrupt caught...shutting everything down cleanly"
        
        if (self.reconnectionThread is not None) and self.reconnectionThread.isAlive():
            self.reconnectionThread.terminateThread()

        #Destroy what manager has told us about first
        for h in self.shutdownHandles:
            self.deactivate_component(h)
        self.shutdownHandles = []
        #Double-check to see if there's any extra components manager did not
        #let us know about!
        for h in self.compHandles.keys():
            self.deactivate_component(h)
        self.shutdown(ACTIVATOR_EXIT<<8)
    #--------------------------------------------------------------------------
    def createPOAForComponent(self, comp_name): # pragma: NO COVER
        '''
        Creates a new POA that is responsible for exactly one component and
        the new POA is created as a child of the ComponentPOA.

        Parameters: comp_name is the components stringified name.

        Return: a new POA.

        Raises: ???
        '''
        return self.componentPOA.create_POA("ComponentPOA" + comp_name, ACSCorba.getPOAManager(), self.compPolicies)
    #--------------------------------------------------------------------------
    def destroyCORBA(self): # pragma: NO COVER
        '''
        Helper function designed to shutdown/destroy all CORBA associated with
        this specific container.

        Raises: ???
        '''
        for policy in self.compPolicies:
            policy.destroy()

        for policy in self.offShootPolicies:
            policy.destroy()

        try:
            self.corbaRef._release()
            self.componentPOA.destroy(FALSE, FALSE)
            self.containerPOA.destroy(FALSE, FALSE)
        except Exception, e:
            self.logger.logWarning("Failed to destroy the container's CORBA object: " + str(e))
            print_exc()
    #--------------------------------------------------------------------------
    def getExistingComponent(self, name): # pragma: NO COVER
        '''
        Searches to see if the component "name" has already been activated by
        this container.

        Parameters: name of the component.

        Return: component record if found; else None.
        '''
        if self.components.has_key(name):
            return self.components[name]
        else:
            return None
    #--------------------------------------------------------------------------
    #--CONTAINER SERVICES METHODS----------------------------------------------
    #--------------------------------------------------------------------------
    def activateOffShoot(self, comp_name, os_corba_ref):
        '''
        Activates an OffShoot derived object.

        Actually this will work on any CORBA object because its Python.

        Parameters:
        - comp_name is the components name.
        - os_corba_ref is a reference to the Python object to become a CORBA object.

        Return: a reference to the CORBA object that almost definitely needs to
        be narrowed to the correct type.  If anything goes wrong though, returns
        None.

        Raises: ???
        '''
        comp = self.getExistingComponent(comp_name)
        if comp == None:
            self.logger.logWarning("Component '" + comp_name + "' does not exist")
            return None
        elif not isinstance(os_corba_ref, OffShoot):
            #Not an offshoot but try activating it anyways!
            self.logger.logWarning("Not an OffShoot '" + str(os_corba_ref) + "'")

        try:
            comp[POAOFFSHOOT].activate_object(os_corba_ref)
            return comp[POAOFFSHOOT].servant_to_reference(os_corba_ref)
        except Exception, e:
            self.logger.logWarning("Unable to activate '" + str(os_corba_ref) + "'")
            print_exc()
            return None
    #--------------------------------------------------------------------------
    def getMyCorbaRef(self):
        '''
        Overriden from BaseClient
        '''

        #if this object has not already been activated as a CORBA object...
        if self.corbaRef == None:
            try:
                #...activate it using the default POA
                self.corbaRef = self.containerPOA.servant_to_reference(self)
            except Exception, e:
                self.logger.logWarning("Cannot activate self as a CORBA servant")
                print_exc()
                raise CORBAProblemExImpl()

            #sanity check
            if self.corbaRef == None:
                # without a client, we can't go on
                self.logger.logWarning("Cannot activate self as a CORBA servant")
                raise CORBAProblemExImpl()

            #OK to return at this point
            return self.corbaRef

        #otherwise return the saved reference
        else: # pragma: NO COVER
            return self.corbaRef
    #--------------------------------------------------------------------------
    def getCode(self): # pragma: NO COVER
        '''
        Overriden from BaseClient
        '''
        if self.canRecover:
            return "AR"
        else:
            return "A"
    #--------------------------------------------------------------------------
    def sendAlarm(self, family, member, code, active):
        '''
        Raise or clear an alarm with the passed triplet

        family: FaultFamily
        member: FaultMember
        code: FaultCode
        active: if True raise the alarm otherwise clear the alarm
        '''
        if self.alarmSource==None:
            self.alarmSource = Acsalarmpy.AlarmSystemInterfaceFactory.createSource("ALARM_SYSTEM_SOURCES")

        # Create a test fault
        fltstate = Acsalarmpy.AlarmSystemInterfaceFactory.createFaultState(family,member, code)
        if active:
            fltstate.descriptor = Acsalarmpy.FaultState.ACTIVE_STRING
        else:
            fltstate.descriptor = Acsalarmpy.FaultState.TERMINATE_STRING
        fltstate.userTimestamp = Acsalarmpy.Timestamp.Timestamp()
        
        # The heart of the test
        self.alarmSource.push(fltstate)
    #--------------------------------------------------------------------------
    def setToken(self,newToken):
        '''
        Override BaseClient.setToken to handle the case of
        a failure connecting to the manager
        '''
        if newToken is not None:
            BaseClient.setToken(self, newToken)
            self.logger.logInfo("Successfully reconnected to the Manager")
        else:
            # Terminate the container
            self.logger.logError("Manager not available: bailing out!")
            self.shutdown(ACTIVATOR_EXIT<<8)