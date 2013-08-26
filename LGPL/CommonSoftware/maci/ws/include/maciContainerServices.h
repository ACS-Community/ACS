
#ifndef maciContainerServices_h
#define maciContainerServices_h
/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 *
 * "@(#) $Id: maciContainerServices.h,v 1.40 2012/05/18 06:54:45 bjeram Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * rcirami   27/11/03  created 
 */

#include <acsutil.h>
#ifndef MAKE_VXWORKS
#include <AlarmSourceImpl.h>
#include <acsContainerServices.h>
#endif
#include <maciComponentStateManager.h>
#include <acscomponentImpl.h>
#include <logging.h>
#include <maciContainerImpl.h>
#include <cdbDALS.h>
#include <acscomponentS.h>
#include <acscommonS.h>
#include <acsErrTypeContainerServices.h>
#include <acsErrTypeLifeCycle.h>
#include <vector>
#include <string>
#include <iterator>

#include <ace/Synch.h>
#include <ace/Hash_Map_Manager.h>

namespace maci {
	
class ContainerImpl;
class MACIContainerServices;
class ComponentReleaseCallback;

//Dummy CBlong Callback implementation
class MyCBlongImpl: public POA_ACS::CBlong {
public:
    MyCBlongImpl(ComponentReleaseCallback *cb);
    virtual ~MyCBlongImpl();
    void working(::CORBA::Long value, const ::ACSErr::Completion & c,
            const ::ACS::CBDescOut & desc);
    void done(::CORBA::Long value, const ::ACSErr::Completion & c,
            const ::ACS::CBDescOut & desc);
    ::CORBA::Boolean negotiate(::ACS::TimeInterval time_to_transmit,
            const ::ACS::CBDescOut & desc);
private:
    ComponentReleaseCallback *callback;
};

class ComponentReleaseCallback {
    friend class MACIContainerServices;
    friend class MyCBlongImpl;

public:
    ComponentReleaseCallback();
    virtual ~ComponentReleaseCallback();
    /**
     * Called when the client cannot legally release the component, e.g. because it no longer holds a reference to it.
     */
    virtual void errorNoPermission(std::string message);
    /**
     * Called when the component reference has been successfully released.
     * @param deactivationUncleanEx If the component was de-activated with problems, this exception will be forwarded; otherwise <code>null</code> for clean deactivation.
     */
    virtual void componentReleased(maciErrType::ComponentDeactivationUncleanEx deactivationUncleanEx);
    virtual void componentReleased();
    /**
     * Called when the target component deactivation failed.
     * @param deactivationFailureEx to provide details about the failure.
     */
    virtual void errorComponentReleaseFailed(maciErrType::ComponentDeactivationUncleanEx deactivationFailureEx);

    /**
     * This is not a callback method but a convenience method to "park" the calling thread
     * until
     * <ul>
     *   <li>The component has been released, or
     *   <li>the given timeout has struck, or
     *   <li>component release failed with an exception.
     * </ul>
     * A client that only wants to wait for component release without caring about the details
     * does not have to subclass <code>ComponentReleaseCallback</code>,
     * but can simply call <code>awaitComponentRelease</code>.
     * The client may in addition override the callback methods though.
     *
     * @param timeout The maximum time to wait for the component release to succeed in microseconds (us).
     * @return <code>true</code> if the component was released properly, <code>false</code> if the call returns because of a timeout.
     * */
    bool awaitComponentRelease(unsigned long timeout);

private:
    MyCBlongImpl myCBlong;
    ACE_Mutex mutex;

    void callOver();
};

/**
 * The default implementation of the ContainerServices abstract class.
 */
class MACIContainerServices: public ContainerServices

{
  public:
  
  /**
   * Constructor to be used in Container instances
   */
  MACIContainerServices(
    const maci::Handle componentHandle, 
    ACE_CString& name,
    ACE_CString& type,
    PortableServer::POA_ptr poa
#ifndef MAKE_VXWORKS
    ,const acsalarm::AlarmSourceThread* alarmSourceThread_p
#endif
    );

  /**
	 * Constructor to be used in client instances
	 */
  MACIContainerServices(
		  const maci::Handle componentHandle,
		  ACE_CString& name,
		  PortableServer::POA_ptr poa,
		  Manager_ptr manager);

  /**
   * Destructor
   */
  virtual ~MACIContainerServices();
 
  public:
 
    void test(const char* txt);
    /**
     * Implementation of acsContainerServices::getCORBAComponent(const char* name)
     * @throw maciErrType::CannotGetComponentExImpl
     */
    CORBA::Object* getCORBAComponent(const char* name);

    /**
     * Implementation of acsContainerServices::getCORBAComponentNonSticky(const char* name)
     * @throw maciErrType::CannotGetComponentExImpl
     */
    CORBA::Object* getCORBAComponentNonSticky(const char* name);
    
    /**
     * Implementation of acsContainerServices::getCORBADynamicComponent(const char* name)
     * @throw maciErrType::NoPermissionExImpl
     * @throw maciErrType::IncompleteComponentSpecExImpl
     * @throw maciErrType::InvalidComponentSpecExImpl
     * @throw maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl
     * @throw maciErrType::CannotGetComponentExImpl
     */
    CORBA::Object* getCORBADynamicComponent(maci::ComponentSpec compSpec, bool markAsDefault);
    
    /**
     * Implementation of acsContainerServices::getCORBACollocatedComponent(...)
     * @throw maciErrType::NoPermissionExImpl
     * @throw maciErrType::IncompleteComponentSpecExImpl
     * @throw maciErrType::InvalidComponentSpecExImpl
     * @throw maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl
     * @throw maciErrType::CannotGetComponentExImpl
     */
    CORBA::Object* getCORBACollocatedComponent(maci::ComponentSpec compSpec, bool markAsDefault, const char* targetComponent);
    
    /**
     * Implementation of acsContainerServices::getCORBADefaultComponent(const char* name)
     * @throw maciErrType::NoPermissionExImpl
     * @throw maciErrType::CannotGetComponentExImpl
     * @throw maciErrType::NoDefaultComponentExImpl
     */
    CORBA::Object* getCORBADefaultComponent(const char* idlType);
  
 public:

  /**
   * Gets the component info for the component
   * 
   * @param componentName The name of the component
   * @throw acsErrTypeContainerServices::GettingCompInfoExImpl
   * @return The ComponentInfo struct of the component
   */
  ComponentInfo getComponentDescriptor(const char* componentName);

  /**
   * Finds components by their instance name (curl) and/or by their type.
   * Wildcards can be used for the curl and type.
   * This method returns a possibly empty array of component curls; 
   * for each curl, you may use {@link #getComponent} to obtain the reference.
   * 
   * @param nameWildcard (<code>null</code> is understood as "*")
   * @param typeWildcard (<code>null</code> is understood as "*")
   * @return A vector of ACE_CString that contains the name of the component(s) that
   * match the search.
   * @htmlonly
   * <br><hr>
   * @endhtmlonly
   */
  ACE_CString_Vector findComponents(const char *nameWilcard, const char *typeWildcard);

  /**
   * Releases the specified component.
   *
   * @param The name of the component instance to be released
   * @throw maciErrType::CannotReleaseComponentExImpl
   * @return void  
   * @htmlonly
   * <br><hr>
   * @endhtmlonly
   */
    void releaseComponent(const char *name);
    
    /**
     * Releases the specified component.
     *
     * @param name The name of the component instance to be released
     * @param callback
     * @throw maciErrType::CannotReleaseComponentExImpl
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    void releaseComponent(std::string &name, ComponentReleaseCallback *callback);

  /**
   * Release all the components
   * 
   * @return void
   */
    void releaseAllComponents();

  /**
   * Get a reference to the DAL object
   *
   * @return A reference to the DAL 
   * @throw acsErrTypeContainerServices::CanNotGetCDBExImpl
   * @htmlonly
   * <br><hr>
   * @endhtmlonly
   */
  CDB::DAL_ptr getCDB();
  
  /// Get the OffShoot POA
  /// @return The offshoot POA
  PortableServer::POA_var getOffShootPOA() { return m_offShootPOA; }

  /**
   * Activates a CORBA servant that implements the OffShoot interface. 
   *
   * @param cbServant  the CORBA-generated servant, e.g. CBdoublePOA
   * @return  A reference to the OffShoot
   * @htmlonly
   * <br><hr>
   * @endhtmlonly
   */
  ACS::OffShoot_ptr activateOffShoot(PortableServer::Servant cbServant);
  
  /**
   * Deactivate the offshoot CORBA servant
   * @param cbServant the CORBA servant
   * @throw acsErrTypeContainerServices::OffShootDeactivationExImpl
   * @throw acsErrTypeContainerServices::OffShootPOAExImpl
   */
  void deactivateOffShoot(PortableServer::Servant cbServant);
  
  /**
   * Create the offshoot POA
   * @return The newly created POA
   */
  PortableServer::POA_var createOffShootPOA();
  
  /**
   * Returns a pointer to the <code>ComponentStateManager</code> 
   * through which the component and the container administrate the
   * state of the component.
   * <p>
   * The component needs to access the <code>ComponentStateManager</code>
   * if it wishes to change its state. 
   * If it doesn't, only the container will change the state based on 
   * the information it has available.
   * 
   * @return the state manager
   * @see alma.ACS.ComponentStates
   */
   maci::ComponentStateManager* getComponentStateManager();

   /**
    * Returns a pointer to the <code>AlarmSource</code>
    * object used to send alarms in a smart way.
    *
    * @return the alarm source object owned by this Container Services
    * @see alma.ACS.ComponentStates
    */
#ifndef MAKE_VXWORKS
   acsalarm::AlarmSource* getAlarmSource() {
	   if (m_alarmSource==NULL) {
		   m_alarmSource = new acsalarm::AlarmSourceImpl(m_alarmSourceThread_p);
		   m_alarmSource->start();
	   }
	   return m_alarmSource;
   }
#endif
    void fireComponentsUnavailable(ACE_CString_Vector& compNames);
    void fireComponentsAvailable(ACE_CString_Vector& compNames);

 private:

  /// Reference to the manager
  maci::Manager_var m_manager;

  /// Pointer to the container
  maci::ContainerImpl *m_containerImpl;
  
  /// The POA for the offshoot
  PortableServer::POA_var m_offShootPOA; 
  
  /// Component handle
  maci::Handle m_componentHandle;
  
  /// Component hash map (used as hash set, value is owner handle)
  typedef ACE_Hash_Map_Manager <ACE_CString, maci::Handle, ACE_Recursive_Thread_Mutex> COMPONENT_HASH_MAP;
  typedef ACE_Hash_Map_Iterator <ACE_CString, maci::Handle, ACE_Recursive_Thread_Mutex> COMPONENT_HASH_MAP_ITER;
  typedef ACE_Hash_Map_Entry <ACE_CString, maci::Handle> COMPONENT_HASH_MAP_ENTRY;

  /// Map (used as set) of activated components
  COMPONENT_HASH_MAP m_usedComponents;

  /// The component state manager
  maci::ComponentStateManager* componentStateManager_mp;

  // The thread to update the alarm sources
  const acsalarm::AlarmSourceThread* m_alarmSourceThread_p;

#ifndef MAKE_VXWORKS
  acsalarm::AlarmSource* m_alarmSource;
#endif

};

} // end namespace maci

#endif // maciContainerServices_h

