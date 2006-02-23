#include <baciThread.h>
#include "acserrExceptionManager.h"
#include <ACSErrTypeTest.h>
#include <iostream>

using namespace baci;
using namespace ACSErrTypeTest;
using namespace std;
using namespace ACSErr;

void threadWorker(void *param_p)
{
  if(!param_p)
    {
      return;
    }

  BACIThreadParameter *baciParameter_p = (BACIThreadParameter *)param_p;
  BACIThread *myself_p = baciParameter_p->getBACIThread();
     
  ACE_CString thrName = myself_p->getName();

  try
    {
      cout << "thread worker started" << endl;
      sleep(2);
      cout << "thread worker working" << endl;
      sleep(2);
      cout << "throwing exception..." << endl;

      ACSErrTest1ExImpl err(__FILE__,__LINE__,"test1");

      ACSErr::ExceptionManager::instance()->pushException(thrName, err);
      throw err;
    }
  catch(ACSErrTest1ExImpl &ex)
    {
      cout << "Exception caught" << endl;
      ex.log();
    }

  
  cout << "thread worker ended" << endl;
  
}
  

int main()
{
// create logging proxy
  LoggingProxy m_logger (0, 0, 31, 0);
  LoggingProxy::init (&m_logger); 

  BACIThreadManager *threadManager = new BACIThreadManager();
  if(threadManager == NULL)
    {
      cout << "error creating thread manager" << endl;
      return -1;
    }

  ACE_CString threadName = "thread1";

  BACIThread *myThread = threadManager->create(threadName.c_str(), (void *)threadWorker, NULL);

  sleep(10);

  ACSbaseExImpl myerr = ACSErr::ExceptionManager::instance()->popException(threadName);

  myerr.log();

  ACE_CString str1 = "pippo";
  cout << ExceptionManager::instance()->existException(str1) << endl;
  cout << ExceptionManager::instance()->existException(threadName) << endl;

  myThread->terminate();

  if(threadManager)
    delete threadManager;

  LoggingProxy::done();
  std::cout << std::flush;

  return 0;
}
