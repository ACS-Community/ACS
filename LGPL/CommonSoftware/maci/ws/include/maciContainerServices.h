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
 * "@(#) $Id: maciContainerServices.h,v 1.23 2006/10/23 15:39:00 bjeram Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * rcirami   27/11/03  created 
 */

#include <acsutil.h>
#include <acsContainerServices.h>
#include <maciComponentStateManager.h>
#include <acscomponentImpl.h>
#include <logging.h>
#include <maciContainerImpl.h>
#include <cdbDALS.h>
#include <acscomponentS.h>
#include <acsErrTypeContainerServices.h>
#include <acsErrTypeLifeCycle.h>
#include <vector>
#include <string>
#include <iterator>

namespace maci {
	
using namespace acsErrTypeContainerServices;

class ContainerImpl;

/**
 * The default implementation of the ContainerServices abstract class.
 */
class MACIContainerServices: public ContainerServices

{
  public:
  
  /**
   * Constructor
   */
  MACIContainerServices(
    const maci::Handle componentHandle, 
    ACE_CString& name,
    PortableServer::POA_ptr poa);

  /**
   * Destructor
   */
  virtual ~MACIContainerServices();
 
  public:
 
    void test(const char* txt);
    /**
     * Implementation of acsContainerServices::getCORBAComponent(const char* name)
     */
    CORBA::Object* getCORBAComponent(const char* name)
	throw (maciErrType::CannotGetComponentExImpl);

    /**
     * Implementation of acsContainerServices::getCORBAComponentNonSticky(const char* name)
     */
    CORBA::Object* getCORBAComponentNonSticky(const char* name)
	throw (maciErrType::CannotGetComponentExImpl);
    
    /**
     * Implementation of acsContainerServices::getCORBADynamicComponent(const char* name)
     */
    CORBA::Object* getCORBADynamicComponent(maci::ComponentSpec compSpec, bool markAsDefault) 
	throw(maciErrType::IncompleteComponentSpecExImpl, 
	      maciErrType::InvalidComponentSpecExImpl, 
	      maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl, 
	      maciErrType::CannotGetComponentExImpl);
    
    /**
     * Implementation of acsContainerServices::getCORBACollocatedComponent(...)
     */
    CORBA::Object* getCORBACollocatedComponent(maci::ComponentSpec compSpec, bool markAsDefault, const char* targetComponent)
	throw(maciErrType::IncompleteComponentSpecExImpl, 
	      maciErrType::InvalidComponentSpecExImpl, 
	      maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl, 
	      maciErrType::CannotGetComponentExImpl);
    
    /**
     * Implementation of acsContainerServices::getCORBADefaultComponent(const char* name)
     */
    CORBA::Object* getCORBADefaultComponent(const char* idlType)
	throw (maciErrType::NoDefaultComponentExImpl, 
	       maciErrType::CannotGetComponentExImpl); 
  
 public:

  /**
   * Gets the component info for the component
   * 
   * @param componentName The name of the component
   * @return The ComponentInfo struct of the component
   */
  ComponentInfo getComponentDescriptor(const char* componentName)
  	throw (acsErrTypeContainerServices::GettingCompInfoExImpl);

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
   * @return void  
   * @htmlonly
   * <br><hr>
   * @endhtmlonly
   */
  void releaseComponent(const char *name);
  
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
   * @htmlonly
   * <br><hr>
   * @endhtmlonly
   */
  CDB::DAL_ptr getCDB() throw(acsErrTypeContainerServices::CanNotGetCDBExImpl);
  
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
   */
  void deactivateOffShoot(PortableServer::Servant cbServant)
  throw (
  	acsErrTypeContainerServices::OffShootDeactivationExImpl,
  	acsErrTypeContainerServices::OffShootPOAExImpl);
  
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

 private:

  /// Reference to the manager
  maci::Manager_var m_manager;

  /// Pointer to the container
  maci::ContainerImpl *m_containerImpl;
  
  /// The POA for the offshoot
  PortableServer::POA_var m_offShootPOA; 
  
  /// Component handle
  maci::Handle m_componentHandle;
  
  /// The vector of the activated components 
  std::vector<std::string> m_usedComponents;
  
  /// The component state manager
  maci::ComponentStateManager* componentStateManager_mp;

  /**
   * Find the component with the given name in the list of the used
   * components
   * 
   * @param name The name of the component to look for
   * @return The position of the component in the list
   *         If the name is not found returns m_usedComponents.end()
   */
  std::vector<std::string>::iterator findUsedComponent(std::string name); 
};

} // end namespace maci

#endif // maciContainerServices_h

