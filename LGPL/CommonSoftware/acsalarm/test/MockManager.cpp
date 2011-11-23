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
#include <MockManager.h>
#include <orbsvcs/CosNamingC.h>
#include <acsutilPorts.h>

namespace maci 
{
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
    @throw maciErrType::ComponentNotAlreadyActivatedE
    @throw maciErrType::ComponentConfigurationNotFoundEx
 	*/
	CORBA::Object_ptr MockManager::get_service (maci::Handle id, const char * curl, CORBA::Boolean activate)
	{
		// corbaloc::<hostname>:<port>/CDB
		const char* hostname = 0;
		hostname = ACSPorts::getIP();
		if (hostname == 0)
		{
			return CORBA::Object::_nil();
		}

		ACE_TCHAR corbalocRef[240];
		ACE_OS::sprintf(corbalocRef, "corbaloc::%s:%s/CDB", hostname, ACSPorts::getCDBPort().c_str());
		int  nargc = 0;
		char **nargv = 0;
		CORBA::ORB_var orb = CORBA::ORB_init (nargc, nargv, "");
		CORBA::Object_var object = orb->string_to_object(corbalocRef);
		return object._retn();
	}
}
