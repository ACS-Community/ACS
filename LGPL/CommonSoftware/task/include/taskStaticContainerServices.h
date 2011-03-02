#ifndef _TASK_STATIC_CONTAINER_SERVICES_H
#define _TASK_STATIC_CONTAINER_SERVICES_H
/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: taskStaticContainerServices.h,v 1.11 2011/03/02 17:23:42 rtobar Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram  yyyy-mm-dd  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif


#include <acsContainerServices.h>
#include <maciComponentStateManager.h>


class StaticContainerServices: public maci::ContainerServices

{
  public:
  
  /**
   * Constructor
   */
  StaticContainerServices(
    const maci::Handle componentHandle, 
    ACE_CString& name,
    ACE_CString& type,
    PortableServer::POA_ptr poa,
    CORBA::ORB_ptr orb );

  /**
   * Destructor
   */
    virtual ~StaticContainerServices(){}
 
  public:
 
    /**
    * @throw maciErrType::CannotGetComponentExImpl
    */
    CORBA::Object* getCORBAComponent(const char* name) 
	{ 
	    return CORBA::Object::_nil(); 
	}

    /**
    * @throw maciErrType::CannotGetComponentExImpl
    */
    CORBA::Object* getCORBAComponentNonSticky(const char* name) 
	{ 
	    return CORBA::Object::_nil(); 
	}
    
    /**
     * Implementation of acsContainerServices::getCORBADynamicComponent(const char* name)
     * @throw maciErrType::IncompleteComponentSpecExImpl
     * @throw maciErrType::InvalidComponentSpecExImpl
     * @throw maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl
     * @throw maciErrType::CannotGetComponentExImpl
     */
    CORBA::Object* getCORBADynamicComponent(maci::ComponentSpec compSpec, bool markAsDefault) 
	{ 
	    return CORBA::Object::_nil(); 
	}
    
    /**
     * Implementation of acsContainerServices::getCORBADefaultComponent(const char* name)
     * @throw maciErrType::NoDefaultComponentExImpl
     * @throw maciErrType::CannotGetComponentExImpl
     */
    CORBA::Object* getCORBADefaultComponent(const char* idlType) 
	{ 
	    return CORBA::Object::_nil(); 
	}
  
  /**
   * Gets the component info for the component
   * 
   * @param componentName The name of the component
   * @throw acsErrTypeContainerServices::GettingCompInfoExImpl
   * @return The ComponentInfo struct of the component
   */
  maci::ComponentInfo getComponentDescriptor(const char* componentName)
   { return maci::ComponentInfo(); }

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
    ACE_CString_Vector findComponents(const char *nameWilcard, const char *typeWildcard) { return ACE_CString_Vector(); }

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
    void releaseComponent(const char *name)
	{}
  
  /**
   * Release all the components
   * 
   * @return void
   */
    void releaseAllComponents(){}

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
  PortableServer::POA_var getOffShootPOA() { return  PortableServer::POA::_nil(); }

  /**
   * Activates a CORBA servant that implements the OffShoot interface. 
   *
   * @param cbServant  the CORBA-generated servant, e.g. CBdoublePOA
   * @return  A reference to the OffShoot
   * @htmlonly
   * <br><hr>
   * @endhtmlonly
   */
    ACS::OffShoot_ptr activateOffShoot(PortableServer::Servant cbServant) {  return ACS::OffShoot::_nil(); }
  
  /**
   * Deactivate the offshoot CORBA servant
   * @param cbServant the CORBA servant
   * @throw acsErrTypeContainerServices::OffShootDeactivationExImpl
   * @throw acsErrTypeContainerServices::OffShootPOAExImpl
   */
  void deactivateOffShoot(PortableServer::Servant cbServant)
	    {}
  
  /**
   * Create the offshoot POA
   * @return The newly created POA
   */
    PortableServer::POA_var createOffShootPOA(){ return PortableServer::POA::_nil(); }
  
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
   * @throw maciErrType::IncompleteComponentSpecEx
   * @throw maciErrType::InvalidComponentSpecEx
   * @throw maciErrType::ComponentSpecIncompatibleWithActiveComponentEx
   * @throw maciErrType::CannotGetComponentEx
   * @return the state manager
   * @see alma.ACS.ComponentStates
   */
    maci::ComponentStateManager* getComponentStateManager()
	{ 
	    return &componentStateManager_m; 
	}

    /*
   * @throw maciErrType::IncompleteComponentSpecEx
   * @throw maciErrType::InvalidComponentSpecEx
   * @throw maciErrType::ComponentSpecIncompatibleWithActiveComponentEx
   * @throw maciErrType::CannotGetComponentEx
    */
    virtual CORBA::Object* getCORBACollocatedComponent(maci::ComponentSpec, 
						       bool, const char*)
	{
	    return CORBA::Object::_nil();
	}
	
 private:
    CORBA::ORB_var orb_m;

  /// Pointer to the container
//  maci::ContainerImpl *m_containerImpl;
  
  /// Component handle
  maci::Handle m_componentHandle;
  
  /// The component state manager
  maci::MACIComponentStateManager componentStateManager_m;

};

#endif /*!_H*/
