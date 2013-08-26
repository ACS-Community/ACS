#include "acserrExceptionManager.h"

using namespace ACSErr;
using namespace std;


void
ExceptionManager::pushException(ACE_CString &str, ACSbaseExImpl &ex)
{
  //to be checked if this mutex is correct
  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mapMutex);
  mymap.insert(make_pair(str,ex));
}

ACSbaseExImpl
ExceptionManager::popException(ACE_CString &str)
{
  ExmgrMap::iterator it = mymap.find(str);
  // to be changed 
  // if the object is not found we throw an exception

  //if(it!=mymap.end())
  //{
      return it->second;
      //}
  //else
  //{
      // throw something
  //}
}

bool
ExceptionManager::existException(ACE_CString &str)
{
  bool retval;
  ExmgrMap::iterator it = mymap.find(str);

  if(it!=mymap.end())
    {
      retval = true;
    }
  else
    {
      retval = false;
    }

  return retval;
}

