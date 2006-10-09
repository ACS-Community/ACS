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
 * "@(#) $Id: acsContainerServices.i,v 1.5 2006/10/09 06:03:08 gchiozzi Exp $"
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

template<class T>
T* ContainerServices::getComponent(const char *name)
    throw (maciErrType::CannotGetComponentEx)
{ 
    CORBA::Object* obj =T::_nil();
    
    ACS_SHORT_LOG((LM_INFO,"ContainerServices::getComponent(%s)",name));
    // Get the component as a CORBA object
    try 
    {
        obj = getCORBAComponent(name);
    }
    catch (maciErrType::CannotGetComponentEx &ex) {
	maciErrType::CannotGetComponentExImpl lex(ex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	lex.setCURL(name);
	throw lex.getCannotGetComponentEx();
    }
    catch (...) {
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	maciErrType::CannotGetComponentExImpl lex(uex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	lex.setCURL(name);
	throw lex.getCannotGetComponentEx();
    }

    // Narrow the object before returning to the caller
    if (CORBA::is_nil(obj)) 
    {
      return T::_nil();
    }
    return T::_narrow(obj);
}

template<class T> T* 
ContainerServices::getDynamicComponent(maci::ComponentSpec compSpec, bool markAsDefault)
    throw (maciErrType::IncompleteComponentSpecEx, 
	   maciErrType::InvalidComponentSpecEx, 
	   maciErrType::ComponentSpecIncompatibleWithActiveComponentEx, 
	   maciErrType::CannotGetComponentEx)
{
    CORBA::Object* obj =T::_nil();
    // Get the component as a CORBA object
    try 
	{
    	obj = getCORBADynamicComponent(compSpec,markAsDefault); 
	} 
    catch (maciErrType::IncompleteComponentSpecEx &ex) {
	maciErrType::IncompleteComponentSpecExImpl lex(ex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	// @todo lex.setCURL(compSpec.component_name);
	throw lex.getIncompleteComponentSpecEx();
    }
    catch (maciErrType::InvalidComponentSpecEx &ex) {
	maciErrType::InvalidComponentSpecExImpl lex(ex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	throw lex.getInvalidComponentSpecEx();
    }
    catch (maciErrType::ComponentSpecIncompatibleWithActiveComponentEx &ex) {
	maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl lex(ex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	// @todo lex.setCURL(compSpec.component_name);
	throw lex.getComponentSpecIncompatibleWithActiveComponentEx();
    }
    catch (maciErrType::CannotGetComponentEx &ex) {
	maciErrType::CannotGetComponentExImpl lex(ex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	// @todo lex.setCURL(compSpec.component_name);
	throw lex.getCannotGetComponentEx();
    }
    catch (...) {
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	maciErrType::CannotGetComponentExImpl lex(uex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	// @todo lex.setCURL(compSpec.component_name);
	throw lex.getCannotGetComponentEx();
    }

    // Narrow the object before returning to the caller
    if (CORBA::is_nil(obj)) 
	{
	return T::_nil();
	}
    return T::_narrow(obj);
}

template<class T> T* 
ContainerServices::getCollocatedComponent(maci::ComponentSpec compSpec, bool markAsDefault, const char* targetComponent)
    throw(maciErrType::IncompleteComponentSpecEx, 
	  maciErrType::InvalidComponentSpecEx, 
	  maciErrType::ComponentSpecIncompatibleWithActiveComponentEx, 
	  maciErrType::CannotGetComponentEx)
{
    CORBA::Object* obj =T::_nil();
    // Get the component as a CORBA object
    try 
	{
    	obj = getCORBACollocatedComponent(compSpec,markAsDefault,targetComponent); 
	} 
    catch (maciErrType::IncompleteComponentSpecEx &ex) {
	maciErrType::IncompleteComponentSpecExImpl lex(ex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	// @todo lex.setCURL(compSpec.component_name);
	throw lex.getIncompleteComponentSpecEx();
    }
    catch (maciErrType::InvalidComponentSpecEx &ex) {
	maciErrType::InvalidComponentSpecExImpl lex(ex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	throw lex.getInvalidComponentSpecEx();
    }
    catch (maciErrType::ComponentSpecIncompatibleWithActiveComponentEx &ex) {
	maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl lex(ex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	// @todo lex.setCURL(compSpec.component_name);
	throw lex.getComponentSpecIncompatibleWithActiveComponentEx();
    }
    catch (maciErrType::CannotGetComponentEx &ex) {
	maciErrType::CannotGetComponentExImpl lex(ex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	// @todo lex.setCURL(compSpec.component_name);
	throw lex.getCannotGetComponentEx();
    }
    catch (...) {
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	maciErrType::CannotGetComponentExImpl lex(uex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	// @todo lex.setCURL(compSpec.component_name);
	throw lex.getCannotGetComponentEx();
    }


    // Narrow the object before returning to the caller
    if (CORBA::is_nil(obj)) 
	{
	return T::_nil();
	}
    return T::_narrow(obj);
}

template<class T> T* 
ContainerServices::getDefaultComponent(const char* idlType)
    throw (maciErrType::NoDefaultComponentEx, 
	   maciErrType::CannotGetComponentEx)
{
    CORBA::Object* obj =T::_nil();
    // Get the component as a CORBA object
    try 
	{
    	obj = getCORBADefaultComponent(idlType); 
	} 
    catch (maciErrType::NoDefaultComponentEx &ex) {
	maciErrType::NoDefaultComponentExImpl lex(ex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	throw lex.getNoDefaultComponentEx();
    }
    catch (maciErrType::CannotGetComponentEx &ex) {
	maciErrType::CannotGetComponentExImpl lex(ex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	throw lex.getCannotGetComponentEx();
    }
    catch (...) {
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	maciErrType::CannotGetComponentExImpl lex(uex,
				   __FILE__, __LINE__,
				   "ContainerServices::getComponent");
	throw lex.getCannotGetComponentEx();
    }

    // Narrow the object before returning to the caller
    if (CORBA::is_nil(obj)) 
	{
	return T::_nil();
	}
    return T::_narrow(obj);
}

#endif
