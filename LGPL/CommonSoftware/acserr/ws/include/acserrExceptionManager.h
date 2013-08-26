#ifndef ACSERREXCEPTIONMANAGER_H
#define ACSERREXCEPTIONMANAGER_H

/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: acserrExceptionManager.h,v 1.4 2004/06/16 08:58:05 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* oat       2004-01-13  created
*/

#include <map>
#include "acserrACSbaseExImpl.h"

namespace ACSErr 
{

typedef std::map<ACE_CString, ACSbaseExImpl> ExmgrMap;

class ExceptionManager
{
 public:
  static ExceptionManager* instance() { return &exmgr; }

  void pushException(ACE_CString &str, ACSbaseExImpl &ex);

  ACSbaseExImpl popException(ACE_CString &str);

  bool existException(ACE_CString &str);

 
 private:
  ExceptionManager() {}
  static ExceptionManager exmgr;

  ExmgrMap mymap;

  ACE_Recursive_Thread_Mutex m_mapMutex;
};// class ExceptionManager

};// namespace ACSErr
#endif
