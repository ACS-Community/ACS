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
* "@(#) $Id: baciRecovery.cpp,v 1.97 2011/09/02 11:38:54 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/02/20  redesigned
*/

#include <vltPort.h>

#include "baciRecovery.h"
#include "logging.h"

#include "acsutilTempFile.h"

ACE_RCSID(baci, baciRecovery, "$Id: baciRecovery.cpp,v 1.97 2011/09/02 11:38:54 bjeram Exp $")

 using namespace recovery;
namespace baci {

BACIRecoveryManager* BACIRecoveryManager::instance_mp = 0;
bool BACIRecoveryManager::load_m = false;
const char * BACIRecoveryManager::activatorName_mp = 0;

BACIRecoveryManager::BACIRecoveryManager() {

/*
  ACE_CString fileName;
  char *envFileName = getenv("ACS_BACI_RECOVERY_FILE");

  if (envFileName)
      fileName = envFileName;
  else
      fileName = ACE_CString(BACI_RECOVERY_FILE_NAME);
*/

   ACE_CString fileName = getTempFileName("ACS_BACI_RECOVERY_FILE", BACI_RECOVERY_FILE_NAME);

   if ((activatorName_mp!=0) && *activatorName_mp)
       {
       // replace all '/' chars in the name with ':' char
       ACE_CString activatorName = activatorName_mp;
       size_t len = activatorName.length();
       for (size_t i = 0; i < len; i++)
	 if (activatorName[i] == '/')
	   activatorName[i] = ':';
       fileName += "_" + activatorName;
       }

  ACS_DEBUG_PARAM("baci::BACIRecoveryManager::BACIRecoveryManager", "Recovery filename: '%s.'", fileName.c_str());

  store_mp = new RecoveryStore(fileName.c_str(), this->load_m);


  /*
  // debug info printout
  RecoveryStore::STORE_HASH_MAP_ENTRY *entry;
    int i = 0;
    for ( RecoveryStore::STORE_HASH_MAP_ITER hash_iter = store_mp->get_iterator();
         hash_iter.next (entry) != 0;
         hash_iter.advance ())
      {
        ACS_SHORT_LOG ((LM_INFO,
                    ACE_TEXT ("iterating (%d): [%s, %s]"),
                    i,
                    entry->ext_id_.c_str(),
                    entry->int_id_.c_str()));
        i++;
      }
  */
}
  

BACIRecoveryManager::~BACIRecoveryManager() {
  delete store_mp;
}

void
BACIRecoveryManager::loadRecovery(bool load) {
    load_m = load;
}

void
BACIRecoveryManager::activatorName(const char * activatorName)
{
  activatorName_mp = activatorName;
}


BACIRecoveryManager*
BACIRecoveryManager::getInstance(void) {
  if (instance_mp==0) 
      {
      instance_mp=new BACIRecoveryManager();
      }
     return instance_mp;
}

void
BACIRecoveryManager::destroyInstance(void) {
  if (instance_mp!=0) 
    {
      delete instance_mp;
      instance_mp = 0;
    }
}

void
BACIRecoveryManager::addRecoverableObject(RecoverableObject *object) {
  const char *state_p = object->getObjectState();
  store_mp->log(ACE_CString(object->getName()), ACE_CString(state_p));
  delete[] state_p;
}


void
BACIRecoveryManager::updateRecoverableObject(RecoverableObject *object) {
  const char *state_p = object->getObjectState();
  store_mp->update(ACE_CString(object->getName()), ACE_CString(state_p));
  delete[] state_p;
}

void
BACIRecoveryManager::removeRecoverableObject(RecoverableObject *object) {
  store_mp->remove(ACE_CString(object->getName()));
}


ACE_CString_Vector
BACIRecoveryManager::getObjectsStartingWith(const char* namePrefix) {
  ACE_CString_Vector objects;
  
  RecoveryStore::STORE_HASH_MAP_ENTRY *entry_p = 0;
  for ( RecoveryStore::STORE_HASH_MAP_ITER hashIter = store_mp->get_iterator();
	hashIter.next (entry_p) != 0;
	hashIter.advance ())
    {
      if (entry_p->ext_id_.find(namePrefix, 0)!=ACE_CString::npos)
	  {
	  objects.push_back(entry_p->ext_id_);
	  }
    }  

  return objects;
}

const char*
BACIRecoveryManager::getObjectState(const char* name) {
  ACE_CString data;
  store_mp->retrieve(ACE_CString(name), data);
  return data.rep();
}

char*
BACIRecoveryManager::generateObjectName(const char* namePrefix) {

  ACE_TCHAR *newName_p = 0;
  ACE_NEW_RETURN (newName_p, ACE_TCHAR[ACE_OS::strlen(namePrefix)+10], 0); // + 10 digits
  ACE_OS::strcpy(newName_p, namePrefix);

  ACE_CString cstring;
  cstring.set(newName_p, 0);	// do not copy

  int n = 0;
  while (store_mp->exists(cstring)==0) {
    ACE_OS::sprintf(newName_p, "%s%u", namePrefix, n++);
    cstring.set(newName_p, 0);
  }

  return newName_p;
}

 }; 

/*___oOo___*/

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: baciRecovery.cpp,v $
// Revision 1.97  2011/09/02 11:38:54  bjeram
// modified signature of getObjectState to return const char* instead char*  to get rid of warning: deprecated conversion from string constant to 'char*'.
//
// Revision 1.96  2006/09/01 02:20:54  cparedes
// small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
// Revision 1.95  2005/08/18 09:32:07  msekoran
// Replacing / chars in recovery filename with :
//
// Revision 1.94  2005/02/04 00:34:00  dfugate
// Fixed coding standards violations.
//
// Revision 1.93  2005/01/27 23:57:55  dfugate
// Fixed a number of coding standards violations as reported by the NRI.
//
// Revision 1.92  2005/01/07 18:18:02  dfugate
// Removed REVISION sections where there was no meaningful information. This is redundant as "cvs rlog" provides this as well as the comments below the LGPL disclaimer.
//
// Revision 1.91  2004/10/14 20:46:18  gchiozzi
// Cleaned up logging messages:
// - uses now only ACS_* macros and not any more ACE_* macros
// - Removed all new line characters
// - fixed problem with ACS logging that was not putting proper new line
//   characters when logging messages on stdout BEFORE the logging itself
//   was initialised.
//
