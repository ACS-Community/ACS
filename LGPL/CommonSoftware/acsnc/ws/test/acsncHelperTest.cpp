#include "acsncHelperTest.h"
#include <maciHelper.h>
namespace nc {

HelperTest::HelperTest(const char* channelName, int argc, char *argv[]):
    Helper(channelName)
{

    ACS_TRACE("HelperTest::HelperTest");
    
	orbHelper_mp = new ORBHelper(argc, argv);
    orbHelper_mp->runOrb();
    namingContext_m = maci::MACIHelper::resolveNameService(orbHelper_mp->getORB());
    //namingContext_m = maci::MACIHelper::resolveNameService(orbHelper_mp->getORB(), "corbaloc::192.168.36.128:3001/NameService");
}

ACE_Thread_Mutex HelperTest::m_tester_mutex;
ACE_Condition_Thread_Mutex HelperTest::m_tester_condition (HelperTest::m_tester_mutex);
bool HelperTest::m_useMutex = false;

CosNotifyChannelAdmin::EventChannel_var HelperTest::getNotifyChannel(){
    return notifyChannel_m;
}

void HelperTest::createNotificationChannel(){
   
    resolveNotificationFactory();
    ACS_TRACE("HelperTest::createNotificationChannel");
    if(m_useMutex){
        ACS_SHORT_LOG((LM_INFO,"Before aquiring the mutex"));
        m_tester_mutex.acquire();
    }
    if(m_useMutex){
        ACS_SHORT_LOG((LM_INFO,"Waiting for all threads to reach this point"));
        m_tester_condition.wait ();
        m_tester_mutex.release();
    }
    ACS_SHORT_LOG((LM_INFO,"Calling the real creation of the channel"));
    //resolved first when is called alone
    Helper::createNotificationChannel();
 
}
/*
void HelperTest:useMutex(bool useMutex){
    m_useMutex = use;
}
*/
};

