#ifndef mockManager_h
#define mockManager_h

/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
#include <acsutil.h>
#include <maciS.h>
#include <cdb.h>
#include <logging.h>

namespace maci 
{
	class MockManager: public virtual POA_maci::Manager, public virtual maci::Manager
	{
		public:

		MockManager(void) { }

		/* ----------------------------------------------------------------*/
		/* --------------------- [ CORBA interface ] ----------------------*/
		/* ----------------------------------------------------------------*/

		//virtual void refresh_logging_config() throw (CORBA::SystemException);

		/** 
		   Login to MACI. 
		   Containers, Clients and Administrative clients call 
		   this function first to identify themselves 
		   with the Manager. 
		   The Manager authenticates them (through the authenticate 
		   function), and assigns them access rights and a handle, 
		   through which they will identify themselves 
		   at subsequent calls to the Manager.

		   @return A ClientInfo structure with handle (h) 
		   and access fields filled-in. 
		   If the client with this name did not logout prior to 
		   calling login, the components sequence in ClientInfo 
		   contains the handles of all components that the 
		   client was using. 
		   (For containers, the components sequence contains 
		   handles of all components previously hosted by the container.)
		*/
 		virtual maci::ClientInfo * login (maci::Client_ptr reference)  { return NULL; }

		/** 
	    Logout from MACI.
	 	*/
		virtual void logout (maci::Handle id) {}

		/** 
	    Register a CORBA object as a component, 
	    assigning it a CURL and making it accessible 
	    through the Manager. 
	    The component is treated as an immortal component.

        @throw maciErrType::CannotRegisterComponentEx
	    @return Returns the handle of the newly created component.
	 	*/
		virtual ::maci::Handle register_component (maci::Handle id, const char * component_url, const char * type, CORBA::Object_ptr c)
         { return 0; }

		/** 
	    Unregister a component from the Manager.
        @throw maciErrType::CannotUnregisterComponentEx
		 */
		virtual void unregister_component (maci::Handle id, maci::Handle h) {}

		/** 
	    Get a service, activating it if necessary (components). 
	    The client represented by id (the handle) 
	    must have adequate access rights to access 
	    the service. 
	    NOTE: a component is also a service, i.e. a service activated by a container.
	    
	    @return Reference to the service. 
	    If the service could not be activated, a nil 
	    reference is returned, and the status contains 
	    an error code detailing the cause of failure 
	    (one of the COMPONENT_* constants).
        @throw maciErrType::CannotGetComponentEx
        @throw maciErrType::ComponentNotAlreadyActivatedEx
        @throw maciErrType::ComponentConfigurationNotFoundEx
	 	*/
		virtual CORBA::Object_ptr get_service (maci::Handle id, const char * service_url, CORBA::Boolean activate);

		/** 
	    Get a component, activating it if necessary. 
	    The client represented by id (the handle) must 
	    have adequate access rights to access the component.
	    
	    @return Reference to the component. 
	    If the component could not be activated, 
	    a nil reference is returned, and the status 
	    contains an error code detailing the cause of 
	    failure (one of the COMPONENT_* constants).
        @throw maciErrType::CannotGetComponentEx
        @throw maciErrType::ComponentNotAlreadyActivatedEx
        @throw maciErrType::ComponentConfigurationNotFoundEx
		*/
		virtual ::CORBA::Object_ptr get_component (maci::Handle id, const char * service_url, CORBA::Boolean activate)
	    { return CORBA::Object::_nil(); }

		/** 
		    Get a non-sticky reference to a component.

		    A non-sticky reference does not bind the
		    Manager to keep alive the Component and 
		    the Client requesting for a non-sticky references
		    is not considered when checking for reference
		    counts. 
		    The Manager can deactivate Components
		    independently from any non-sticky reference.

		    This is typically used by "weak clients" like
		    graphical user interfaces.

                    Since a non-sticky reference is not considered in
                    reference counting, it will also not activate
		    the component if it is not already active.

		    As a consequence, asking for a non-sticky reference
                    to a not-active Component throws an exception.
  
		    The client represented by id (the handle) must 
		    have adequate access rights to access the component.
		    
            @throw maciErrType::CannotGetComponentEx
            @throw maciErrType::ComponentNotAlreadyActivatedEx
		    @return Reference to the component. 
		*/
		virtual ::CORBA::Object_ptr get_component_non_sticky (maci::Handle id, const char * component_url)
      	{ return CORBA::Object::_nil(); }

		/** 
		    Used for retrieving several services with one call. 
		    See get_service.

		    @deprecated This method is deprecated and will be
		    removed.
            @throw maciErrType::CannotGetServiceEx
		    @return A sequence of requested services.
		 */
		virtual ::maci::ObjectSeq * get_services (maci::Handle id, const ::maci::CURLSeq & service_urls, CORBA::Boolean activate, 
			maci::ulongSeq_out status)
      	{ return NULL; }

		/** 
		    Used for retrieving several components with one call. 
		    See get_component.
		    
		    @deprecated This method is deprecated and will be
		    removed.

            @throw maciErrType::CannotGetComponentEx
		    @return A sequence of requested components.
		 */
		virtual ::maci::ObjectSeq * get_components (maci::Handle id, const maci::CURLSeq & component_urls, CORBA::Boolean activate, 
			maci::ulongSeq_out status)
      	{ return NULL; }

		/** Change mortality state of an component. 
		    Component must be already active, otherwise 
		    CORBA::NO_RESOURCE exception will be thrown. 
		    The caller must be an owner of an component or 
		    have administator rights, otherwise CORBA::NO_PERMISSION 
		    exception will be thrown. 
		*/
		virtual void make_component_immortal (maci::Handle id, const char * component_url, CORBA::Boolean immortal_state)
      	{}
    
		/** 
		    Release a component. 
		    In order for this operation to be possible, 
		    the caller represented by the id must have 
		    previously successfuly requested the 
		    component via a call to get_component.

		    Releasing a component more times than 
		    requesting it should be avoided, 
		    but it produces no errors.

		    @return Number of clients that are still using the 
		    component after the operation completed. 
		    This is a useful debugging tool.
		 */
		virtual ::CORBA::Long release_component (maci::Handle id, const char * component_url)
      	{ return 0; }

		virtual void release_component_async (maci::Handle id, const char * component_url, ACS::CBlong_ptr cb, const ACS::CBDescIn& desc)
		{}

		/** 
		    Releases a component also if still referenced by other components/clients.
                    @return Number of clients that were still referencing the component 
		    after the operation completed. This is a useful debugging tool.
		 */
		virtual ::CORBA::Long force_release_component (maci::Handle id, const char * component_url)
      	{  return 0; }
    
		/** Release components.
		 */
		virtual void release_components (maci::Handle id, const ::maci::CURLSeq & component_urls)
      	{}

		/** 
		    Shutdown the Manager.

		    <B>Warning:</B> This call will also deactivate all
		    components active in the system, including startup
		    and immortal components. 
		 */
		virtual void shutdown (maci::Handle id, CORBA::ULong containers)
      	{}

		/** 
		    Get all the information that the Manager has about its known
		    containers. To invoke this method, the caller must have
		    INTROSPECT_MANAGER access rights, or it must be the object whose info
		    it is requesting.
		    
		    Calling this function does not affect the internal state of the Manager.
		    
		    @return A sequence of ContainerInfo structures
		    containing the entire Manager's knowledge about the
		    containers. If access is denied to a subset of
		    objects, the handles to those objects are set to 0. 
		*/
		virtual ::maci::ContainerInfoSeq * get_container_info (maci::Handle id, const maci::HandleSeq & h, const char * name_wc)
      	{ return NULL; }

		/** 
		    Get all the information that the Manager has about its current
		    clients. To invoke this method, the caller must have
		    INTROSPECT_MANAGER access rights, or it must be the object whose info
		    it is requesting.

		    Calling this function does not affect the internal state of the Manager.
		    
		    @return A sequence of ClientInfo structures
		    containing the entire Manager's knowledge about the
		    clients. If access is denied to a subset of objects,
		    the handles to those objects are set to 0. 
		*/
		virtual maci::ClientInfoSeq * get_client_info (maci::Handle id, const maci::HandleSeq & h, const char * name_wc)
      	{ return NULL; }
    
		/** 
		    Get all the information that the Manager has about components. To
		    invoke this method, the caller must have INTROSPECT_MANAGER access
		    rights, or it must have adequate privileges to access the component
		    (the same as with the get_component method).

		    Information about all components is returned,
		    unless the active_only parameter is set to True,
		    in which case only information about those
		    components that are currently registered with the
		    Manager and activated is returned. 

		    Calling this function does not affect the internal
		    state of the Manager. 

		    @return A sequence of ComponentInfo structures
		    containing the entire Manager's knowledge about
		    the components. If access is denied to a subset of
		    objects, the handles to those objects are set to
		    0.

		 */
		virtual maci::ComponentInfoSeq * get_component_info (maci::Handle id, const maci::HandleSeq & h, const char * name_wc,
			const char * type_wc, CORBA::Boolean active_only)
      	{ return NULL; }

		/** 
		    Restarts a component. 
            @throw maciErrType::CannotGetComponentEx
		*/
		virtual ::CORBA::Object_ptr restart_component (maci::Handle client, const char * component_url)
      	{ return CORBA::Object::_nil(); }

		/** 
		    Activation of dynamic component. 
            @throw maciErrType::IncompleteComponentSpecEx
            @throw maciErrType::InvalidComponentSpecEx
            @throw maciErrType::ComponentSpecIncompatibleWithActiveComponentEx
            @throw maciErrType::CannotGetComponentEx
		*/
		virtual maci::ComponentInfo * get_dynamic_component (maci::Handle client, const maci::ComponentSpec & c, CORBA::Boolean mark_as_default)
		{ return NULL; }

		/** 
		    Group request of dynamic components. 
            @throw maciErrType::IncompleteComponentSpecEx
            @throw maciErrType::InvalidComponentSpecEx
            @throw maciErrType::ComponentSpecIncompatibleWithActiveComponentEx
            @throw maciErrType::CannotGetComponentEx
		*/
		virtual maci::ComponentInfoSeq * get_dynamic_components (maci::Handle client, const maci::ComponentSpecSeq & components)
		{ return NULL; }

		/** 
		    Activation of a component so that it runs in the same process as
		    another given component. 
            @throw maciErrType::IncompleteComponentSpecEx
            @throw maciErrType::InvalidComponentSpecEx
            @throw maciErrType::ComponentSpecIncompatibleWithActiveComponentEx
            @throw maciErrType::CannotGetComponentEx
		*/
		virtual maci::ComponentInfo * get_collocated_component (maci::Handle client, const maci::ComponentSpec & c, CORBA::Boolean mark_as_default,
			const char * target_component)
		{ return NULL; }

		/** 
	    Returns the default component of specific type. 
        @throw maciErrType::NoDefaultComponentEx
        @throw maciErrType::CannotGetComponentEx
		*/
		virtual maci::ComponentInfo * get_default_component (maci::Handle client, const char * component_type)
      	{ return NULL; }

		/** 
	    Shutdown a container.
		*/
		virtual void shutdown_container (maci::Handle id, const char * container_name, CORBA::ULong action)
      	{}


		virtual maci::LoggingConfigurable::LogLevels get_default_logLevels() { maci::LoggingConfigurable::LogLevels ll; return ll; }
		virtual maci::LoggingConfigurable::LogLevels get_logLevels(const char*) { maci::LoggingConfigurable::LogLevels ll; return ll; }
		virtual void set_logLevels(const char*, const maci::LoggingConfigurable::LogLevels&) {}
		virtual void set_default_logLevels(const maci::LoggingConfigurable::LogLevels&) {}
		virtual void refresh_logging_config() {}

		virtual maci::stringSeq* get_logger_names() { return NULL; }
		virtual char* domain_name() { return NULL; }
  	        virtual CORBA::Boolean ping() { return false; }

	};
}

#endif // mockManager_h
