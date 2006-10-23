#ifndef acsContainerServices_i
#define acsContainerServices_i
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
 * "@(#) $Id: acsContainerServices.i,v 1.7 2006/10/23 15:38:21 bjeram Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * acaproni  2005-04-11  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <ACSErrTypeCommon.h>

using namespace maci;
using namespace maciErrType;

template<class T>
T* ContainerServices::getComponent(const char *name)
    throw (maciErrType::CannotGetComponentExImpl)
{ 
    CORBA::Object* obj =T::_nil();
    
    ACS_SHORT_LOG((LM_INFO,"ContainerServices::getComponent(%s)",name));
  
    try 
	{
	// Get the component as a CORBA object
	obj = getCORBAComponent(name); // obj should not be nil, but if it is the narrow will fail
	return T::_narrow(obj);   
	}
    catch (maciErrType::CannotGetComponentExImpl &ex) 
	{
	CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "ContainerServices::getComponent");
	lex.setCURL(name);
	throw lex;
	}
    catch (...) 
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ContainerServices::getComponent");
	CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				     "ContainerServices::getComponent");
	lex.setCURL(name);
	throw lex;
	}
}//getComponent


template<class T>
T* ContainerServices::getComponentNonSticky(const char *name)
    throw (maciErrType::CannotGetComponentExImpl)
{ 
    CORBA::Object* obj =T::_nil();
    
    ACS_SHORT_LOG((LM_INFO,"ContainerServices::getComponentNonSticky(%s)",name));
  
    try 
	{
	// Get the component as a CORBA object
	obj = getCORBAComponent(name); // obj should not be nil, but if it is the narrow will fail
	return T::_narrow(obj);   
	}
    catch (maciErrType::CannotGetComponentExImpl &ex) 
	{
	CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "ContainerServices::getComponentNonSticky");
	lex.setCURL(name);
	throw lex;
	}
    catch (...) 
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ContainerServices::getComponentNonSticky");
	CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				     "ContainerServices::getComponentNonSticky");
	lex.setCURL(name);
	throw lex;
	}
}//getComponentNonSticky

template<class T> T* 
ContainerServices::getDynamicComponent(maci::ComponentSpec compSpec, bool markAsDefault)
    throw (maciErrType::IncompleteComponentSpecExImpl, 
	   maciErrType::InvalidComponentSpecExImpl, 
	   maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl, 
	   maciErrType::CannotGetComponentExImpl)
{
    CORBA::Object* obj =T::_nil();

    ACS_TRACE("ContainerServices::getDynamicComponent");

    try 
	{
	// Get the component as a CORBA object
    	obj = getCORBADynamicComponent(compSpec,markAsDefault);   // obj should be null, but if it is  ..
	return T::_narrow(obj);  // ... narrow will fail
	} 
    catch (IncompleteComponentSpecExImpl &ex) 
	{
	IncompleteComponentSpecExImpl lex(ex, __FILE__, __LINE__,
					  "ContainerServices::getDynamicComponent");
	lex.setCURL(compSpec.component_name.in());
	throw lex;
    }
    catch (maciErrType::InvalidComponentSpecExImpl &ex) 
	{
	InvalidComponentSpecExImpl lex(ex, __FILE__, __LINE__,
						    "ContainerServices::getDynamicComponent");
	throw lex;
	}
    catch (maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl &ex) 
	{
	ComponentSpecIncompatibleWithActiveComponentExImpl lex(ex, __FILE__, __LINE__,
							       "ContainerServices::getDynamicComponent");
	lex.setCURL(compSpec.component_name.in());
	throw lex;
	}
    catch (maciErrType::CannotGetComponentExImpl &ex) 
	{
	CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "ContainerServices::getDynamicComponent");
	lex.setCURL(compSpec.component_name.in());
	throw lex;
	}
    catch (...) 
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ContainerServices::getDynamicComponent");
	CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				     "ContainerServices::getDynamicComponent");
	lex.setCURL(compSpec.component_name.in());
	throw lex;
	}//try-catch
}//getDynamicComponent

template<class T> T* 
ContainerServices::getCollocatedComponent(maci::ComponentSpec compSpec, bool markAsDefault, const char* targetComponent)
    throw(maciErrType::IncompleteComponentSpecExImpl, 
	  maciErrType::InvalidComponentSpecExImpl, 
	  maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl, 
	  maciErrType::CannotGetComponentExImpl)
{
    CORBA::Object* obj =T::_nil();

    ACS_TRACE("ContainerServices::getCollocatedComponent");  

    try 
	{
	// Get the component as a CORBA object
    	obj = getCORBACollocatedComponent(compSpec,markAsDefault,targetComponent); 
	return T::_narrow(obj);
	} 
    catch (maciErrType::IncompleteComponentSpecExImpl &ex) 
	{
	IncompleteComponentSpecExImpl lex(ex, __FILE__, __LINE__,
					  "ContainerServices::getCollocatedComponent");
	lex.setCURL(compSpec.component_name.in());
	throw lex;
	}
    catch (maciErrType::InvalidComponentSpecExImpl &ex) 
	{
	InvalidComponentSpecExImpl lex(ex, __FILE__, __LINE__,
				       "ContainerServices::getCollocatedComponent");
	throw lex;
	}
    catch (maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl &ex) 
	{
	ComponentSpecIncompatibleWithActiveComponentExImpl lex(ex, __FILE__, __LINE__,
							       "ContainerServices::getCollocatedComponent");
	lex.setCURL(compSpec.component_name.in());
	throw lex;
	}
    catch (maciErrType::CannotGetComponentExImpl &ex) 
	{
	CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "ContainerServices::getCollocatedComponent");
	lex.setCURL(compSpec.component_name.in());
	throw lex;
	}
    catch (...) 
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ContainerServices::getCollocatedComponent");
	CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				     "ContainerServices::getCollocatedComponent");
	lex.setCURL(compSpec.component_name.in());
	throw lex;
	}
}//getCollocatedComponent

template<class T> T* 
ContainerServices::getDefaultComponent(const char* idlType)
    throw (maciErrType::NoDefaultComponentExImpl, 
	   maciErrType::CannotGetComponentExImpl)
{
    CORBA::Object* obj =T::_nil();

    ACS_TRACE("ContainerServices::getDefaultComponent"); 

    try 
	{
	// Get the component as a CORBA object
    	obj = getCORBADefaultComponent(idlType); 
	return T::_narrow(obj);
	} 
    catch (maciErrType::NoDefaultComponentExImpl &ex) 
	{
	NoDefaultComponentExImpl lex(ex,
				     __FILE__, __LINE__,
				     "ContainerServices::getDefaultComponent");
	throw lex;
	}
    catch (maciErrType::CannotGetComponentExImpl &ex) 
	{
	CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "ContainerServices::getDefaultComponent");
	throw lex;
	}
    catch (...) 
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ContainerServices::getDefaultComponent");
	CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				     "ContainerServices::getDefaultComponent");
	throw lex;
	}
}//getDefaultComponent

#endif
