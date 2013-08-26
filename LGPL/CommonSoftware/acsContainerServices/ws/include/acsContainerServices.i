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
 * "@(#) $Id: acsContainerServices.i,v 1.18 2008/10/09 04:56:55 cparedes Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * acaproni  2005-04-11  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <ACSErrTypeCommon.h>
#include <ACSErrTypeCORBA.h>

template<class T>
T* maci::ContainerServices::getComponent(const char *name)
{ 
    CORBA::Object* obj =T::_nil();
    
    ACS_SHORT_LOG((LM_INFO,"ContainerServices::getComponent(%s)",name));
  
    try 
	{
	// Get the component as a CORBA object
	obj = getCORBAComponent(name); // obj should not be nil, but if it is the narrow will fail
	T* tmpRef = T::_narrow(obj);  
	if (tmpRef==T::_nil())
		{
		releaseComponent(name); // first we have to release the component!
		ACSErrTypeCORBA::NarrowFailedExImpl ex(__FILE__, __LINE__, "maci::ContainerServices<>::getComponent");
		ex.setNarrowType(typeid(T).name());
		throw ex;
		}//if
	return tmpRef;
	}
    catch (ACSErr::ACSbaseExImpl &ex) 
	{
	maciErrType::CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "ContainerServices::getComponent");
	lex.setCURL(name);
	throw lex;
	}
    catch (...) 
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ContainerServices::getComponent");
	maciErrType::CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				     "ContainerServices::getComponent");
	lex.setCURL(name);
	throw lex;
	}
	
}//getComponent


template<class T>
T* maci::ContainerServices::getComponentNonSticky(const char *name)
{ 
    CORBA::Object* obj =T::_nil();
    
    ACS_SHORT_LOG((LM_INFO,"ContainerServices::getComponentNonSticky(%s)",name));
  
    try 
	{
	// Get the component as a CORBA object
	obj = getCORBAComponentNonSticky(name); // obj should not be nil, but if it is the narrow will fail
	T* tmpRef = T::_narrow(obj);  
	if (tmpRef==T::_nil())
		{
		// here we do not have to release the component because it is non sticky!
		ACSErrTypeCORBA::NarrowFailedExImpl ex(__FILE__, __LINE__, "maci::ContainerServices<>::getComponentNonSticky");
		ex.setNarrowType(typeid(T).name());
		throw ex;
		}//if
	return tmpRef;  
	}
    catch (maciErrType::CannotGetComponentExImpl &ex) 
	{
	maciErrType::CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "ContainerServices::getComponentNonSticky");
	lex.setCURL(name);
	throw lex;
	}
	catch (ACSErr::ACSbaseExImpl &ex) 
	{
	maciErrType::CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "ContainerServices::getComponentNonSticky");
	lex.setCURL(name);
	throw lex;
	}
    catch (...) 
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ContainerServices::getComponentNonSticky");
	maciErrType::CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				     "ContainerServices::getComponentNonSticky");
	lex.setCURL(name);
	throw lex;
	}
}//getComponentNonSticky

template<class T> T* 
maci::ContainerServices::getDynamicComponent(maci::ComponentSpec compSpec, bool markAsDefault)
{
    CORBA::Object* obj =T::_nil();

    ACS_TRACE("maci::ContainerServices::getDynamicComponent");

    try 
    {
    	// Get the component as a CORBA object
    	obj = getCORBADynamicComponent(compSpec,markAsDefault);   // obj should be null, but if it is  ..
    	T* tmpRef = T::_narrow(obj);  
    	if (tmpRef==T::_nil())
    	{
    		// here we try to obtain name of the collocated component that we can release it
    		ACS::ACSComponent* tComp =  ACS::ACSComponent::_narrow(obj);
    		if ( tComp!=ACS::ACSComponent::_nil() )
    		{
    			char *cName = tComp->name();
    			releaseComponent(cName);
    			CORBA::string_free(cName);
    		}
    		else
    		{
    			releaseComponent(compSpec.component_name.in()); // first we have to release the component!
    		}//if-else
    		ACSErrTypeCORBA::NarrowFailedExImpl ex(__FILE__, __LINE__, "maci::ContainerServices<>::getDynamicComponent");
    		ex.setNarrowType(typeid(T).name());
    		throw ex;
    	}//if
    	return tmpRef;
    }
    catch (maciErrType::NoPermissionExImpl &ex) 
    {
    	maciErrType::NoPermissionExImpl	 lex(ex, __FILE__, __LINE__,
    			"maci::ContainerServices::getDynamicComponent");
    	throw lex;
    }
    catch (maciErrType::IncompleteComponentSpecExImpl &ex) 
    {
    	maciErrType::IncompleteComponentSpecExImpl lex(ex, __FILE__, __LINE__,
    			"maci::ContainerServices::getDynamicComponent");
    	lex.setCURL(compSpec.component_name.in());
    	throw lex;
    }
    catch (maciErrType::InvalidComponentSpecExImpl &ex) 
    {
    	maciErrType::InvalidComponentSpecExImpl lex(ex, __FILE__, __LINE__,
    			"maci::ContainerServices::getDynamicComponent");
    	throw lex;
    }
    catch (maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl &ex) 
    {
    	maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl lex(ex, __FILE__, __LINE__,
    			"maci::ContainerServices::getDynamicComponent");
    	lex.setCURL(compSpec.component_name.in());
    	throw lex;
    }
    catch (maciErrType::CannotGetComponentExImpl &ex) 
    {
    	maciErrType::CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
    			"maci::ContainerServices::getDynamicComponent");
    	lex.setCURL(compSpec.component_name.in());
    	throw lex;
    }
    catch (ACSErr::ACSbaseExImpl &ex) 
    {
    	maciErrType::CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
    			"maci::ContainerServices::getDynamicComponent");
    	lex.setCURL(compSpec.component_name.in());
    	throw lex;
    }
    catch (...) 
    {
    	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
    			"maci::ContainerServices::getDynamicComponent");
    	maciErrType::CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
    			"maci::ContainerServices::getDynamicComponent");
    	lex.setCURL(compSpec.component_name.in());
    	throw lex;
    }//try-catch
}//getDynamicComponent

template<class T> T* 
maci::ContainerServices::getCollocatedComponent(maci::ComponentSpec compSpec, bool markAsDefault, const char* targetComponent)
{
    CORBA::Object* obj =T::_nil();

    ACS_TRACE("maci::ContainerServices::getCollocatedComponent");  

    try 
	{
	// Get the component as a CORBA object
    	obj = getCORBACollocatedComponent(compSpec,markAsDefault,targetComponent); 
	T* tmpRef = T::_narrow(obj);  
    if (tmpRef==T::_nil())
		{
    	// here we try to obtain name of the collocated component that we can release it
    			ACS::ACSComponent* tComp =  ACS::ACSComponent::_narrow(obj);
    			if ( tComp!=ACS::ACSComponent::_nil() )
    			{
    				char *cName = tComp->name();
    				releaseComponent(cName);
    				CORBA::string_free(cName);
    			}
    			else
    			{
    				releaseComponent(compSpec.component_name.in()); // first we have to release the component!
    			}//if-else
		ACSErrTypeCORBA::NarrowFailedExImpl ex(__FILE__, __LINE__, "maci::ContainerServices<>::getCollocatedComponent");
		ex.setNarrowType(typeid(T).name());
		throw ex;
		}//if
	return tmpRef;
	} 
    catch (maciErrType::NoPermissionExImpl &ex) 
	{
	maciErrType::NoPermissionExImpl lex(ex, __FILE__, __LINE__,
					    "maci::ContainerServices::getCollocatedComponent");
	throw lex;
	}
    catch (maciErrType::IncompleteComponentSpecExImpl &ex) 
	{
	maciErrType::IncompleteComponentSpecExImpl lex(ex, __FILE__, __LINE__,
					  "maci::ContainerServices::getCollocatedComponent");
	lex.setCURL(compSpec.component_name.in());
	throw lex;
	}
    catch (maciErrType::InvalidComponentSpecExImpl &ex) 
	{
	maciErrType::InvalidComponentSpecExImpl lex(ex, __FILE__, __LINE__,
				       "maci::ContainerServices::getCollocatedComponent");
	throw lex;
	}
    catch (maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl &ex) 
	{
	maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl lex(ex, __FILE__, __LINE__,
							       "maci::ContainerServices::getCollocatedComponent");
	lex.setCURL(compSpec.component_name.in());
	throw lex;
	}
    catch (maciErrType::CannotGetComponentExImpl &ex) 
	{
	maciErrType::CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "maci::ContainerServices::getCollocatedComponent");
	lex.setCURL(compSpec.component_name.in());
	throw lex;
	}
	 catch (ACSErr::ACSbaseExImpl &ex) 
	{
	maciErrType::CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "maci::ContainerServices::getCollocatedComponent");
	lex.setCURL(compSpec.component_name.in());
	throw lex;
	}
    catch (...) 
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"maci::ContainerServices::getCollocatedComponent");
	maciErrType::CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				     "maci::ContainerServices::getCollocatedComponent");
	lex.setCURL(compSpec.component_name.in());
	throw lex;
	}
}//getCollocatedComponent

template<class T> T* 
maci::ContainerServices::getDefaultComponent(const char* idlType)
{
    CORBA::Object* obj =T::_nil();

    ACS_TRACE("maci::ContainerServices::getDefaultComponent"); 

    try 
	{
	// Get the component as a CORBA object
    	obj = getCORBADefaultComponent(idlType); 
	T* tmpRef = T::_narrow(obj);  
	if (tmpRef==T::_nil())
		{
		// here we try to obtain name of the default component that we can release it
		ACS::ACSComponent* tComp =  ACS::ACSComponent::_narrow(obj);
		if ( tComp!=ACS::ACSComponent::_nil() )
		{
			char *cName = tComp->name();
			releaseComponent(cName);
			CORBA::string_free(cName);
		}//if
		ACSErrTypeCORBA::NarrowFailedExImpl ex(__FILE__, __LINE__, "maci::ContainerServices<>::getDefaultComponent");
		ex.setNarrowType(typeid(T).name());
		throw ex;
		}//if
	return tmpRef;
	} 
    catch(maciErrType::NoPermissionEx &_ex)
	{
	throw maciErrType::NoPermissionExImpl(__FILE__, __LINE__,
				"maci::ContainerServices::getDefaultComponent");	      
	}
    catch (maciErrType::NoDefaultComponentExImpl &ex) 
	{
	maciErrType::NoDefaultComponentExImpl lex(ex,
				     __FILE__, __LINE__,
				     "maci::ContainerServices::getDefaultComponent");
	throw lex;
	}
    catch (maciErrType::CannotGetComponentExImpl &ex) 
	{
	maciErrType::CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "maci::ContainerServices::getDefaultComponent");
	throw lex;
	}
	catch (ACSErr::ACSbaseExImpl &ex) 
	{
	maciErrType::CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "maci::ContainerServices::getDefaultComponent");
	throw lex;
	}
    catch (...) 
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"maci::ContainerServices::getDefaultComponent");
	maciErrType::CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				     "maci::ContainerServices::getDefaultComponent");
	throw lex;
	}
}//getDefaultComponent

template <typename T>
maci::SmartPtr<T> maci::ContainerServices::getComponentSmartPtr(const char *name)
{
    return maci::SmartPtr<T>(this, true, this->getComponent<T>(name));
}//getComponentSmartPtr

template <typename T>
maci::SmartPtr<T> maci::ContainerServices::getComponentNonStickySmartPtr(const char *name)
{ 
    return maci::SmartPtr<T>(this, false, this->getComponentNonSticky<T>(name));
}//getComponentNonStickySmartPtr

template <typename T>
maci::SmartPtr<T> maci::ContainerServices::getDynamicComponentSmartPtr(maci::ComponentSpec compSpec, bool markAsDefault)
{
    return maci::SmartPtr<T>(this, true, this->getDynamicComponent<T>(compSpec, markAsDefault));
}//getDynamicComponentSmartPtr


template <typename T>
maci::SmartPtr<T> maci::ContainerServices::getCollocatedComponentSmartPtr(maci::ComponentSpec compSpec, bool markAsDefault, const char* targetComponent)
{
    return maci::SmartPtr<T>(this, true, this->getCollocatedComponent<T>(compSpec, markAsDefault, targetComponent));
}//getCollocatedComponentSmartPtr

template <typename T>
maci::SmartPtr<T> maci::ContainerServices::getDefaultComponentSmartPtr(const char* idlType)
{
    return maci::SmartPtr<T>(this, true, this->getDefaultComponent<T>(idlType));
}//getDefaultComponentSmartPtr
#endif
