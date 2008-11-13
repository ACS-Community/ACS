#include <acsutil.h>
#include <logging.h>
#include <baciThread.h>
#include <acsutilPorts.h>
#include <maciSimpleClient.h>
#include "acsncCDBProperties.h"
#include "acsncHelperTest.h"
 using namespace baci;

LoggingProxy *g_logger = 0;
maci::SimpleClient c;
int nWorker = 0;
std::string channelName1("singleChannel1");
std::string channelName2("singleChannel2");

bool nc::HelperTest::m_useMutex = true;
const char* hostname = 0;

static void creation_worker (void* param)
{   
    BACIThreadParameter* baciParameter = (BACIThreadParameter*)param;

    BACIThread* myself = baciParameter->getBACIThread();

    if (BACIThread::InitThread) BACIThread::InitThread(myself->getName().c_str());
    ACS_SHORT_LOG((LM_INFO, "Creation worker number %d", nWorker));
    nc::HelperTest * helper;
    nWorker++;
    char** myCache = (char**)malloc(10000);
    myCache[0] = (char*)malloc(10000);
    myCache[1] = (char*)malloc(10000);
    myCache[2] = (char*)malloc(10000);
    sprintf(myCache[0],"Worker_%s",myself->getName().c_str());
    sprintf(myCache[1],"-ORBInitRef");
    sprintf(myCache[2],"NameService=corbaloc::%s:3001",hostname);
    helper = new nc::HelperTest(channelName1.c_str(), 3, myCache );  
    try{
        helper->createNotificationChannel();
        if(helper->resolveNotifyChannel()){
            //log with error level to show on ref file (default level is 5
            ACS_SHORT_LOG((LM_ERROR,"NC '%s' was created for thread '%s'",channelName1.c_str(), myself->getName().c_str())); 
        }else{
            ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created for thread '%s' for an unknown reason",channelName1.c_str(), myself->getName().c_str()));  
         }
        
    }catch(NotifyMonitoringExt::NameAlreadyUsed e){
        ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created for thread '%s'",channelName1.c_str(), myself->getName().c_str()));  
       }
    
    if (BACIThread::DoneThread) BACIThread::DoneThread();

    delete baciParameter;
    myself->setStopped();

}

static void get_or_creation_worker (void* param)
{   
    BACIThreadParameter* baciParameter = (BACIThreadParameter*)param;

    BACIThread* myself = baciParameter->getBACIThread();

    if (BACIThread::InitThread) BACIThread::InitThread(myself->getName().c_str());
    ACS_SHORT_LOG((LM_INFO, "Get or creation worker number %d", nWorker));
    nc::HelperTest * helper;
    nWorker++;
    char** myCache = (char**)malloc(100);
    myCache[0] = (char*)malloc(10000);
    myCache[1] = (char*)malloc(10000);
    myCache[2] = (char*)malloc(10000);
    sprintf(myCache[0],"Worker_%s",myself->getName().c_str());
    sprintf(myCache[1],"-ORBInitRef");
    sprintf(myCache[2],"NameService=corbaloc::%s:3001",hostname);
    helper = new nc::HelperTest(channelName2.c_str(), 3, myCache );  
    if(!helper->resolveInternalNotificationChannel()){
        ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created nor resolved for thread '%s'", channelName2.c_str(), myself->getName().c_str()));  
    }else{ 
       //log with error level to show on ref file (default level is 5
        ACS_SHORT_LOG((LM_ERROR,"NC '%s' was created/resolved for thread %s",channelName2.c_str(), myself->getName().c_str()));  
       }

    if(CORBA::is_nil(helper->getNotifyChannel())){
        ACS_SHORT_LOG((LM_ERROR,"Thread '%s' couldn't get the channel '%s'", myself->getName().c_str(), channelName2.c_str()));  
    }else{
            //log with error level to show on ref file (default level is 5
        ACS_SHORT_LOG((LM_ERROR, "Thread '%s' got the channel '%s' properly", myself->getName().c_str(), channelName2.c_str()));
    }
    
    if (BACIThread::DoneThread) BACIThread::DoneThread();

    delete baciParameter;
    myself->setStopped();

}


int main(int argc, char* argv[])
{

    LoggingProxy *m_logger = new LoggingProxy(0, 0, 31, 0);
    LoggingProxy::init(m_logger);
    LoggingProxy::ProcessName(argv[0]);
    LoggingProxy::ThreadName("main");
  
    hostname = ACSPorts::getIP();
    c.init(argc,argv);
    c.login();

    g_logger = m_logger;
    BACIThreadManager * threadManager_p = new BACIThreadManager();
    ACS_SHORT_LOG((LM_INFO, "Starting."));
    //Creating threads for create channel
    int nCreationThreads = 4;
    BACIThread * creationThreads[nCreationThreads];
    std::string s;
    int i;
    for(i = 0; i < nCreationThreads; i++){
        std::stringstream ss;
        ss << "Test_creation_thread_";
        ss << (i+1);
        ss >> s;
        ACS_SHORT_LOG((LM_INFO, "Creating '%s' thread", s.c_str()));
        creationThreads[i]=threadManager_p->create(s.c_str(),
                            (void*)creation_worker, (void*)0);
    }

    for(i = 0; i < nCreationThreads; i++){
        creationThreads[i]->resume();
    }
    //Creating threads for Get or create channel
    int nGetOrCreationThreads = 4;
    BACIThread * getOrCreationThreads[nGetOrCreationThreads];
    for(i = 0; i < nGetOrCreationThreads; i++){
        std::stringstream ss;
        ss << "Test_get_or_creation_thread_";
        ss << (i+1);
        ss >> s;
        ACS_SHORT_LOG((LM_INFO, "Creating '%s' thread", s.c_str()));
        getOrCreationThreads[i]=threadManager_p->create(s.c_str(),
                            (void*)get_or_creation_worker, (void*)0);
    }

    for(i = 0; i < nGetOrCreationThreads; i++){
        getOrCreationThreads[i]->resume();
    }
    ACS_SHORT_LOG((LM_INFO, "Spawned."));

    if(nc::HelperTest::m_useMutex){
        sleep(20);
        ACS_SHORT_LOG((LM_INFO, "Broadcasting to all threads"));
        nc::HelperTest::m_tester_condition.broadcast();        
    }
    ACE_OS::sleep(30);

    ACS_SHORT_LOG((LM_INFO, "Stopping."));

    // another way
    threadManager_p->stop("Test thread");

    ACS_SHORT_LOG((LM_INFO, "Stopped."));

    ACE_OS::sleep(5);

    /********************/

    ACS_SHORT_LOG((LM_INFO, "Deleting manager."));

    delete threadManager_p;

    ACS_SHORT_LOG((LM_INFO, "Done."));
    g_logger = 0;
    LoggingProxy::done();
    //delete m_logger;

    return 0;
}

