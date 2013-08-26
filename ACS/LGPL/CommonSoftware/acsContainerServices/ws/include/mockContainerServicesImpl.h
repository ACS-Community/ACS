#ifndef MOCKCONTAINERSERVICES_H
#define MOCKCONTAINERSERVICES_H
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
* "@(#) $Id: mockContainerServicesImpl.h,v 1.3 2011/10/14 16:57:57 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almamgr  2008-12-10  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsContainerServices.h>
#include <ace/Map_Manager.h>
#include <ace/Hash_Map_Manager.h>
#include <maciErrType.h>
#include "mockComponentImpl.h"

namespace maci {

  class MockContainerServices : public maci::ContainerServices {
  public:
    MockContainerServices(ACE_CString& compName);
    virtual ~MockContainerServices();
    CORBA::Object* getCORBAComponent(const char*);
    CORBA::Object* getCORBAComponentNonSticky(const char*);
    CORBA::Object* getCORBADynamicComponent(maci::ComponentSpec, bool);
    CORBA::Object* getCORBACollocatedComponent(maci::ComponentSpec, bool, const char*);
    CORBA::Object* getCORBADefaultComponent(const char*);
    maci::ComponentInfo getComponentDescriptor(const char*);
    ACE_CString_Vector findComponents(const char*, const char*);
    void releaseComponent(const char*);
    void releaseAllComponents();
    CDB::DAL* getCDB();
    PortableServer::POA_var getOffShootPOA();
    ACS::OffShoot* activateOffShoot(PortableServer::ServantBase*);
    void deactivateOffShoot(PortableServer::ServantBase*);
    PortableServer::POA_var createOffShootPOA();
    maci::ComponentStateManager* getComponentStateManager();
    template<class T> T* getComponent(const char *name);
    acsalarm::AlarmSource* getAlarmSource();

  private:
    ACE_Map_Manager<ACE_CString, MockComponent* ,ACE_SYNCH_RW_MUTEX> *map;
  };



};

template<class T>
T* maci::MockContainerServices::getComponent(const char *name)
{ 
  ACE_CString s(name);
  MockComponent *mc;

  if (map->find(s, mc) == -1) {
    mc = new MockComponent(s, this);
    map->bind(s,mc);
    return mc;
  } else 
    return mc;
}//getComponent

#endif /*!MOCKCONTAINERSERVICES_H*/
