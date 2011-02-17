#ifndef acsContainerServices_h
#define acsContainerServices_h
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
 * "@(#) $Id: acsContainerServices.h,v 1.23 2011/02/17 18:25:40 rtobar Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * acaproni  2005-04-06  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acscomponentS.h>
#include <acscomponentC.h>
#include <maciC.h>
#include <acsErrTypeContainerServices.h>
#include <cdbDALS.h>
#include <acsComponentStateManager.h>
#include <acsThreadManager.h>
#include <logging.h>
#include <loggingLoggable.h>
#include <maciErrType.h>
#include <acsComponentListener.h>
#include <acsComponentSmartPtr.h>

namespace maci {

    /**
     * Specializing this class, the container offers services to its hosted components.
     * The component must call these methods explicitly; in this respect, 
     * <code>ContainerServices</code> is different from the other services that the container
     * provides without the component implementation knowing about it. 
     * It can be thought of as a callback handle or a library.
     * Currently, methods are added to this interface as the functionality becomes available. 
     */
   class ContainerServices : public Logging::Loggable {
    public:
        /**
         * Constructor
         * 
         * @param componentName The name of the component
         * @param poa The POA
         */
        ContainerServices(ACE_CString& compName, PortableServer::POA_ptr poa);
        
        
        /**
         * Destructor
         */
        virtual ~ContainerServices();
    
    protected:
        /**
         * Gets the specified component as a Corba object. 
         * 
         * @param The name of the deployed component instance
         * @throw maciErrType::CannotGetComponentExImpl
         * @return Reference to the component
         * @htmlonly
         * <br><hr>
         * @endhtmlonly
         */
        virtual CORBA::Object* getCORBAComponent(const char* name)=0;
        /**
         * Gets the specified component non sticky as a Corba object. 
	 * for details about getting a component non sticky see #get_component_non_sticky
         * 
         * @param The name of the deployed component instance
         * @throw maciErrType::CannotGetComponentExImpl
         * @return Reference to the component
         * @htmlonly
         * <br><hr>
         * @endhtmlonly
         */
       virtual CORBA::Object* getCORBAComponentNonSticky(const char* name)=0;
  
        /**
         * Gets a dynamic component as a Corba object. 
         * 
         * @param compSpec The description of the component to activate
         * @param markAsDefault If true, the component becomes the default component 
         *                      for that IDL type
         * @throw maciErrType::NoPermissionExImpl
         * @throw maciErrType::IncompleteComponentSpecExImpl
         * @throw maciErrType::InvalidComponentSpecExImpl
         * @throw maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl
         * @throw maciErrType::CannotGetComponentExImpl
         * @return The reference to the component
         */
        virtual CORBA::Object* getCORBADynamicComponent(maci::ComponentSpec compSpec, bool markAsDefault)=0;

        /**
         * Gets a collocated component as a Corba object. 
         * 
         * @param compSpec The description of the component to activate
         * @param markAsDefault If true, the component becomes the default component 
         *                      for that IDL type
	 * @param targetComponent name of the target component (where to activate component)
         * @throw maciErrType::NoPermissionExImpl
         * @throw maciErrType::IncompleteComponentSpecExImpl
         * @throw maciErrType::InvalidComponentSpecExImpl
         * @throw maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl
         * @throw maciErrType::CannotGetComponentExImpl
         * @return The reference to the component
         */
        virtual CORBA::Object* getCORBACollocatedComponent(maci::ComponentSpec compSpec, bool markAsDefault, const char* targetComponent)
	    =0;
        
        /**
         * Gets the default component specified by the IDL component type as a CORBA object.
         * 
         * @param idlType: the idl type of the component to activate
         *                 For example IDL:alma/PS/PowerSupply:1.0
         * @throw maciErrType::NoPermissionExImpl
         * @throw maciErrType::NoDefaultComponentExImpl
         * @throw maciErrType::CannotGetComponentExImpl
         * @return The reference to the component
         */
        virtual CORBA::Object* getCORBADefaultComponent(const char* idlType)=0; 
        
    public:
    
        /**
         * Return the name of the component
         * 
         * @return The ACE_CString string with the name of the component
         */
        ACE_CString getName() {
            return m_componentName;
        }

        /**
         * Return the component IDL type
         * 
         * @return The ACE_CString string with the component IDL type
         */
        ACE_CString getType() {
            return m_componentType;
        }
        
        /**
         * Get POA reference
         * This function is used to return m_poa because inherited classes would not 
         * have access to it otherwise.
         *
         * @return POA reference
         * @htmlonly
         * <br><hr>
         * @endhtmlonly
         */
        PortableServer::POA_var getPOA() { 
            return m_poa; 
        }

                
        void registerComponentListener(ComponentListener* listener);
        void fireComponentsUnavailable(ACE_CString_Vector& compNames);
        void fireComponentsAvailable(ACE_CString_Vector& compNames);
 
        /**
         * Gets the specified component
         * This method uses templates, so no cast to the request object is required
         * 
         * @param The name of the deployed component instance
         * @throw maciErrType::CannotGetComponentExImpl
         * @return Reference to the component
         * @htmlonly
         * <br><hr>
         * @endhtmlonly
         */
        template<class T> T* getComponent(const char *name);	    
        
       /**
         * Gets the specified component as non sticky.
	 * for the details of getting a componet non sticky see #get_component_non_sticky
         * This method uses templates, so no cast to the request object is required
         * 
         * @param The name of the deployed component instance
         * @throw maciErrType::CannotGetComponentExImpl
         * @return Reference to the component
         * @htmlonly
         * <br><hr>
         * @endhtmlonly
         */
       template<class T> T* getComponentNonSticky(const char *name);

        /**
         * Gets a dynamic component
         * This method uses templates, so no cast to the request object is required
         * 
         * @param compSpec The description of the component to activate
         * @param markAsDefault If true, the component becomes the default component 
         *                      for that IDL type
         * @throw maciErrType::NoPermissionExImpl
         * @throw maciErrType::IncompleteComponentSpecExImpl
         * @throw maciErrType::InvalidComponentSpecExImpl
         * @throw maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl
         * @throw maciErrType::CannotGetComponentExImpl
         * @return The reference to the component
         */
        template<class T> T* getDynamicComponent(maci::ComponentSpec compSpec, bool markAsDefault);
        /**
         * Gets a collocated component
         * This method uses templates, so no cast to the request object is required
         * 
         * @param compSpec The description of the component to activate
         * @param markAsDefault If true, the component becomes the default component 
         *                      for that IDL type
	     * @param targetComponent name of the target component (where to activate component)
         * @throw maciErrType::NoPermissionExImpl
         * @throw maciErrType::IncompleteComponentSpecExImpl
         * @throw maciErrType::InvalidComponentSpecExImpl
         * @throw maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl
         * @throw maciErrType::CannotGetComponentExImpl
         * @return The reference to the component
         */
        template<class T> T* getCollocatedComponent(maci::ComponentSpec compSpec, bool markAsDefault, const char* targetComponent);
    
        /**
         * Gets the default component specified by the IDL component type.
         * This method uses templates, so no cast to the request object is required
         * 
         * @param idlType: the idl type of the component to activate
         *                 For example IDL:alma/PS/PowerSupply:1.0
         * @throw maciErrType::NoPermissionExImpl
         * @throw maciErrType::NoDefaultComponentExImpl
         * @throw maciErrType::CannotGetComponentExImpl
         * @return The reference to the component
         */
        template<class T> T* getDefaultComponent(const char* idlType);
      
         /**
	  * Gets a smart pointer to the specified component
	  * This method uses templates, so no cast to the request object is required
	  * 
	  * @param The name of the deployed component instance
      * @throw maciErrType::CannotGetComponentExImpl
	  * @return A smart pointer containing the reference to the component
	  * @htmlonly
	  * <br><hr>
	  * @endhtmlonly
	  */
         template <typename T>
         SmartPtr<T> getComponentSmartPtr(const char *name);    

         /**
	  * Gets a smart pointer to the specified component as non sticky.
	  * for the details of getting a componet non sticky see #get_component_non_sticky
	  * This method uses templates, so no cast to the request object is required
	  * 
	  * @param The name of the deployed component instance
      * @throw maciErrType::CannotGetComponentExImpl
	  * @return A smart pointer containing the reference to the component
	  * @htmlonly
	  * <br><hr>
	  * @endhtmlonly
	  */
         template <typename T>
         SmartPtr<T> getComponentNonStickySmartPtr(const char *name);

         /**
	  * Gets a smart pointer to a dynamic component
	  * This method uses templates, so no cast to the request object is required
	  * 
	  * @param compSpec The description of the component to activate
	  * @param markAsDefault If true, the component becomes the default component 
	  *                      for that IDL type
      * @throw maciErrType::NoPermissionExImpl
      * @throw maciErrType::IncompleteComponentSpecExImpl
      * @throw maciErrType::InvalidComponentSpecExImpl
      * @throw maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl
      * @throw maciErrType::CannotGetComponentExImpl
	  * @return A smart pointer containing the reference to the component
	  */
         template <typename T>
         SmartPtr<T> getDynamicComponentSmartPtr(maci::ComponentSpec compSpec, bool markAsDefault);

         /**
	  * Gets a smart ponter to a collocated component
	  * This method uses templates, so no cast to the request object is required
	  * 
	  * @param compSpec The description of the component to activate
	  * @param markAsDefault If true, the component becomes the default component 
	  *                      for that IDL type
	  * @param targetComponent name of the target component (where to activate component)
      * @throw maciErrType::NoPermissionExImpl
      * @throw maciErrType::IncompleteComponentSpecExImpl
      * @throw maciErrType::InvalidComponentSpecExImpl
      * @throw maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl
      * @throw maciErrType::CannotGetComponentExImpl
	  * @return A smart pointer containing the reference to the component
	  */
         template <typename T>
         SmartPtr<T> getCollocatedComponentSmartPtr(maci::ComponentSpec compSpec, bool markAsDefault, const char* targetComponent);

         /**
	  * Gets a smart pointer to the default component specified by the IDL component type.
	  * This method uses templates, so no cast to the request object is required
	  * 
	  * @param idlType: the idl type of the component to activate
	  *                 For example IDL:alma/PS/PowerSupply:1.0
      * @throw maciErrType::NoPermissionExImpl
      * @throw maciErrType::NoDefaultComponentExImpl
      * @throw maciErrType::CannotGetComponentExImpl
	  * @return A smart pointer containing the reference to the component
	  */
         template <typename T>
         SmartPtr<T> getDefaultComponentSmartPtr(const char* idlType); 

        /**
         * Gets the component info for the component
         * 
         * @param componentName The name of the component
         * @throw acsErrTypeContainerServices::GettingCompInfoExImpl
         * @return The ComponentInfo struct of the component
         */
        virtual maci::ComponentInfo getComponentDescriptor(const char* componentName)=0;
        
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
        virtual ACE_CString_Vector findComponents(const char *nameWilcard, const char *typeWildcard)=0;
    
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
        virtual void releaseComponent(const char *name)=0;
      
        /**
         * Release all the components
         * 
         * @return void
         */
        virtual void releaseAllComponents()=0;
    
        /**
         * Get a reference to the DAL object
         *
         * @throw acsErrTypeContainerServices::CanNotGetCDBExImpl 
         * @return A reference to the DAL 
         * @htmlonly
         * <br><hr>
         * @endhtmlonly
         */
        virtual CDB::DAL_ptr getCDB()=0;
      
        /** Get the OffShoot POA
         * @return The offshoot POA
         */
        virtual PortableServer::POA_var getOffShootPOA()=0;
    
        /**
         * Activates a CORBA servant that implements the OffShoot interface. 
         *
         * @param cbServant  the CORBA-generated servant, e.g. CBdoublePOA
         * @return  A reference to the OffShoot
         * @htmlonly
         * <br><hr>
         * @endhtmlonly
         */
        virtual ACS::OffShoot_ptr activateOffShoot(PortableServer::Servant cbServant)=0;
      
        /**
         * Deactivate the offshoot CORBA servant
         * @param cbServant the CORBA servant
         * @throw acsErrTypeContainerServices::OffShootDeactivationExImpl
         * @throw acsErrTypeContainerServices::OffShootPOAExImpl
         */
        virtual void deactivateOffShoot(PortableServer::Servant cbServant)=0;
      
        /**
         * Create the offshoot POA
         * @return The newly created POA
         */
        virtual PortableServer::POA_var createOffShootPOA()=0;
        
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
        virtual ComponentStateManager* getComponentStateManager()=0;

       /**
	* Returns a pointer to the <code>ThreadManager</code>
        * which should be used for creating and destroying threads inside a component
	*
	* @todo This returns a pointer to the intermal threadManager object.
	*        This operation is potentially dangerous and we should may be
	*        better return a reference counting Loki smart pointer,
	*        but this requires changing the interfaces.
	*/
       virtual ACS::ThreadManager* getThreadManager(){ return &threadManager_m; }

   protected:
        ACE_CString m_componentName;
        ACE_CString m_componentType;
        
        ComponentListener* compListener; 
        bool withCompListener; 
        /** POA reference
         *  This is a reference to the POA activating the component
         *  that owns this container services
         */
        PortableServer::POA_var m_poa;

       /** ThreadManager
	* Thread Manager should be used for creating threads inside a component
	*/
       // It could be that the manager should be moved to MACIContainerServices
       ACS::ThreadManager threadManager_m;
      
   };
   
}; // namespace maci

// Include the implementation 
#include "acsContainerServices.i"
   


#endif // acsContainerServices_h
