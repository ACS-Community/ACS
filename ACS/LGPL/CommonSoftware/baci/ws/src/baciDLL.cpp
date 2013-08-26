/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration),
*    All rights reserved
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
* "@(#) $Id: baciDLL.cpp,v 1.95 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/03/09  modified
*/

#include <vltPort.h>

#include "acsutil.h"
#include "logging.h"
#include "baciDB.h"
#include "baciRecovery.h"
#include "baciCORBA.h"
#include "baciThread.h"
#include "baciCDBPropertySet.h"

ACE_RCSID(baci, baciDLL, "$Id: baciDLL.cpp,v 1.95 2006/09/01 02:20:54 cparedes Exp $");
 
 using namespace baci;

ACS_DLL_UNMANGLED_EXPORT bool DLLOpen(int argc, char* argv[])
{
/*
  if (argc < 2)
  {
    ACS_LOG(LM_RUNTIME_CONTEXT, "DLLOpen", (LM_ERROR,"Usage: BACI <DB root>"));
    return false;   	
  }
*/   
  // hardcoded DB root (obstain somehow via args), since CDB will be removed, etc.  !!!
  if (DBConnector::initDB(":Appl_data:alma")==false)
  {
    ACS_LOG(LM_RUNTIME_CONTEXT, "DLLOpen", (LM_ERROR, "Error initializing DB..."));
    return false;   	
  }


  // -r(ecovery switch)
  for (int i=0; i < argc; i++)
      if (ACE_OS::strncmp(argv[i], "-r", 2)==0)
	{
	  BACIRecoveryManager::loadRecovery(true);
	  break;
	}

  BACIRecoveryManager::activatorName(argv[1]);
	  
  if (BACIRecoveryManager::getInstance()==0)
  {
    ACS_LOG(LM_RUNTIME_CONTEXT, "DLLOpen", (LM_ERROR, "Error initializing recovery manager..."));
    return false;
  }

  return true;

}

ACS_DLL_UNMANGLED_EXPORT void DLLInitThreads(InitThreadFunc init, DoneThreadFunc done)
{
  BACIThread::setInitializers(init, done);
}

ACS_DLL_UNMANGLED_EXPORT void DLLSetupCORBA(CORBA::ORB_ptr orb_m,
					PortableServer::POAManager_ptr poaManager_m,
					PortableServer::POA_ptr poaRoot_m,
					PortableServer::POA_ptr poaPersistent_m,
					PortableServer::POA_ptr poaTransient_m)
{
  BACI_CORBA::createInstance(orb_m, poaManager_m, poaRoot_m, poaPersistent_m, poaTransient_m);

  if (DBConnector::getDBTable()!=0)
      {
      CDBPropertySet::createInstance(orb_m, poaManager_m, poaRoot_m);
      }
}  

ACS_DLL_UNMANGLED_EXPORT void DLLClose()
{
  if (CDBPropertySet::getInstance() != 0)
      {
      CDBPropertySet * propSet_p = CDBPropertySet::getInstance();
      delete propSet_p;
      }

  BACI_CORBA::destroyInstance();
  DBConnector::closeDB();
  BACIRecoveryManager::destroyInstance();
	
}

/*___oOo___*/
