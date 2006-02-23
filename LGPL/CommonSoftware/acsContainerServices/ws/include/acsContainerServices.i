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
 * "@(#) $Id: acsContainerServices.i,v 1.3 2006/01/20 14:27:31 gchiozzi Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * acaproni  2005-04-11  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

using namespace maci;

template<class T>
T* ContainerServices::getComponent(const char *name)
{ 
    CORBA::Object* obj =T::_nil();
    
    ACS_SHORT_LOG((LM_INFO,"ContainerServices::getComponent(%s)",name));
    // Get the component as a CORBA object
    try 
    {
        obj = getCORBAComponent(name);
    } catch (...) {
        ACS_SHORT_LOG((LM_ERROR,"ContainerServices::getComponent(%s) failed",name));
        return T::_nil();
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
{
    CORBA::Object* obj =T::_nil();
    // Get the component as a CORBA object
    try 
	{
    	obj = getCORBADynamicComponent(compSpec,markAsDefault); 
	} 
    catch (...) 
	{
	ACS_SHORT_LOG((LM_ERROR,"ContainerServices::getDynamicComponent(...) failed"));
        return T::_nil();
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
{
    CORBA::Object* obj =T::_nil();
    // Get the component as a CORBA object
    try 
	{
    	obj = getCORBADefaultComponent(idlType); 
	} 
    catch (...) 
	{
        ACS_SHORT_LOG((LM_ERROR,"ContainerServices::getDefaultComponent(%s) failed",idlType));
        return T::_nil();
	}
    // Narrow the object before returning to the caller
    if (CORBA::is_nil(obj)) 
	{
	return T::_nil();
	}
    return T::_narrow(obj);
}

#endif
