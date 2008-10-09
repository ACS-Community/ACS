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
*
* "@(#) $Id: acsexmplBuildingImpl.cpp,v 1.127 2008/10/09 08:41:11 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dave 2003-08-25 release_COB on the door after we're done with it.
* gchiozzi 2003-04-09 Replaced writeXXXX with write
* oat 2002-12-17 Templatization of the ContainerImpl.get_object method
* david 2002-06-18 removed Building::invokeAction as there are no asynchronous calls in this class
* bgustafs 2002-04-09 modified for VxWorks
* almamgr 2002-04-07 Removed poa parameter from call to ConstructorEpilogue()
* blopez   2002-04-05 writeLong changed to writeString at version initialization
* gchiozzi 2002-04-04 Replaced set_sync() with getDevIO()->write<T>()
* jib/blo  2002-04-02 Created
*/

#include <acsexmplBuildingImpl.h>

//Because of bugs in the ContainerServices...we default to using this
#include <maciContainerImpl.h>

//Used to access other components, activate "OffShoot"s, etc.
#include <maciContainerServices.h>

#include <iostream>

/** @file acsexmplBuildingImpl.cpp
 *  Implementation file for Building example.
 */

ACE_RCSID(acsexmpl, acsexmplBuildingImpl, "$Id: acsexmplBuildingImpl.cpp,v 1.127 2008/10/09 08:41:11 cparedes Exp $")

using namespace baci;
using namespace maci;

// Building Constructor
Building::Building(
		   const ACE_CString &name,
		   maci::ContainerServices * containerServices) : 
    CharacteristicComponentImpl(name, containerServices),
    // Create the smart pointer for the ROsttring property
    m_version_sp(new ROstring(name+":version", getComponent()),this),
    m_door_p(acsexmplBuilding::Door::_nil())
{
    ACS_TRACE("::Building::Building");
}

void
Building::execute()
{
    ACS_SHORT_LOG((LM_INFO,"Building::execute"));
    
    /*
     * Get a reference to the front door
     */    

    /*
     * Get the component "<name>/FRONTDOOR"
     */
    ACE_CString frontDoorCobName(this->name());
    frontDoorCobName += "/FRONTDOOR";
    
    // Use container to activate the door object
    ACS_SHORT_LOG((LM_INFO, "Getting component: %s", frontDoorCobName.c_str() ));
    
    m_door_p = acsexmplBuilding::Door::_nil();

    m_door_p = getContainerServices()->getComponent<acsexmplBuilding::Door>(frontDoorCobName.c_str());    

    if (CORBA::is_nil(m_door_p.in()))
	{
	throw acsErrTypeLifeCycle::LifeCycleExImpl(__FILE__, __LINE__, "::Building::execute");
	}
    // Set current version - the devIO will write values to errcode and 
    // timestamp. In this simple example, we don't really care what values
    // it sets.
    ACS::Time timestamp;
    // Write out the version number to the property.
    m_version_sp->getDevIO()->write(rcsid_acsexmpl_acsexmplBuildingImpl, timestamp);
    
}

// Building Destructor
void Building::cleanUp()
{
    ACS_TRACE("::Building::cleanUp");

    // must release the door component
    if(CORBA::is_nil(m_door_p.in()) == false)
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "::Building::cleanUp",
		(LM_DEBUG, "Releasing /FRONTDOOR"));
	//generate the door's name again
	ACE_CString frontDoorCobName(this->name());
	frontDoorCobName += "/FRONTDOOR";

	getContainerServices()->releaseComponent(frontDoorCobName.c_str());

	// be sure to set the reference to nil
	m_door_p = acsexmplBuilding::Door::_nil();
	}
    
}

// Building Destructor
Building::~Building()
{
    ACS_TRACE("::Building::~Building");
    
    // I cannot call cleanUp() here, because the cleanUp() requires access to CORBA
    // to release the Door.
    // It is always bad practice to call cleanUp in the destructor.
    // It can be done just as an "emergency procedure" in the classes where it 
    // does not cause trouble.
}

/* --------------------- [ CORBA interface ] ----------------------*/
void
Building::openFrontDoor ()
{
    try 
	{
	//Always check to ensure the door reference is valid!
	if (CORBA::is_nil(m_door_p.in()) == false)
	    {	    
	    ACS_SHORT_LOG((LM_INFO,"*** Opening the front door ... ***"));
	    
	    // Execute the method move() of class Door
	    double pos = 0.0;
	    m_door_p->move(pos);
	    } 
	}
    catch(...)
	{
	// TBD: Exception handling 
	ACS_SHORT_LOG((LM_ERROR, "::Building::openFrontDoor"));
	}
}

void
Building::closeFrontDoor ()
{
    try 
	{
	//Always check to ensure the door reference is valid!
	if (CORBA::is_nil(m_door_p.in()) == false)
	    {
            ACS_SHORT_LOG((LM_INFO,"*** Closing the front door ... ***"));
            // Execute the method move() of class Door
            double pos = 100.0;
	    m_door_p->move(pos);    
	    }
	}
    catch(...)
	{
        // TBD: Exception handling 
        ACS_SHORT_LOG((LM_ERROR, "::Building::closeFrontDoor"));
	}
}

ACS::ROstring_ptr
Building::version ()
{
    if (m_version_sp == 0)
	{
	return ACS::ROstring::_nil();
	}
    
    ACS::ROstring_var prop = ACS::ROstring::_narrow(m_version_sp->getCORBAReference());
    return prop._retn();
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(Building)
/* ----------------------------------------------------------------*/


