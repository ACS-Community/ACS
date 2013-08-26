/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2008 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: mockContainerServicesImpl.cpp,v 1.3 2011/10/14 16:57:57 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* agrimstrup  2008-12-10  created 
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*
*   
*   PARENT CLASS
*
* 
*   DESCRIPTION
*
*
*   PUBLIC METHODS
*
*
*   PUBLIC DATA MEMBERS
*
*
*   PROTECTED METHODS
*
*
*   PROTECTED DATA MEMBERS
*
*
*   PRIVATE METHODS
*
*
*   PRIVATE DATA MEMBERS
*
*
*   FILES
*
*   ENVIRONMENT
*
*   COMMANDS
*
*   RETURN VALUES
*
*   CAUTIONS 
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

#include "vltPort.h"
#include "mockContainerServicesImpl.h"

static char *rcsId="@(#) $Id: mockContainerServicesImpl.cpp,v 1.3 2011/10/14 16:57:57 rtobar Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

//using namespace maci;

maci::MockContainerServices::MockContainerServices(ACE_CString& compName) : ContainerServices(compName, 0)
{
  map = new ACE_Map_Manager<ACE_CString, MockComponent*, ACE_SYNCH_RW_MUTEX>(10);
}


maci::MockContainerServices::~MockContainerServices()
{
  delete map;
}

CORBA::Object* maci::MockContainerServices::getCORBAComponent(const char*)
{
  return 0;
}

CORBA::Object* maci::MockContainerServices::getCORBAComponentNonSticky(const char*)
{
  return 0;
}

CORBA::Object* maci::MockContainerServices::getCORBADynamicComponent(maci::ComponentSpec, bool)
{
  return 0;
}

CORBA::Object* maci::MockContainerServices::getCORBACollocatedComponent(maci::ComponentSpec, bool, const char*)
{
  return 0;
}

CORBA::Object* maci::MockContainerServices::getCORBADefaultComponent(const char*)
{
  return 0;
}

maci::ComponentInfo maci::MockContainerServices::getComponentDescriptor(const char*)
{
  return maci::ComponentInfo();
}

ACE_CString_Vector maci::MockContainerServices::findComponents(const char*, const char*)
{
  return ACE_CString_Vector();
}

void maci::MockContainerServices::releaseComponent(const char* name)
{
  ACE_CString s(name);
  MockComponent *mc;

  if (strcmp(name, "ShaBoom") == 0)
      throw std::exception();

  if (map->find(s, mc) >= 0)
    map->unbind(s);
  else
    throw maciErrType::CannotReleaseComponentExImpl(__FILE__, __LINE__, "No such component");
}

void maci::MockContainerServices::releaseAllComponents()
{
}

CDB::DAL* maci::MockContainerServices::getCDB()
{
  return 0;
}

PortableServer::POA_var maci::MockContainerServices::getOffShootPOA()
{
  return 0;
}

ACS::OffShoot* maci::MockContainerServices::activateOffShoot(PortableServer::ServantBase*)
{
  return 0;
}

void maci::MockContainerServices::deactivateOffShoot(PortableServer::ServantBase*)
{
}

PortableServer::POA_var maci::MockContainerServices::createOffShootPOA()
{
  return 0;
}

maci::ComponentStateManager* maci::MockContainerServices::getComponentStateManager()
{
  return 0;
}

acsalarm::AlarmSource* maci::MockContainerServices::getAlarmSource()
{
  return 0;
}

/*___oOo___*/
