#include <acsutil.h>
#include <logging.h>
#include <baciThread.h>
#include <acsutilPorts.h>
#include <maciSimpleClient.h>
#include "acsncCDBProperties.h"
#include "acsncHelperTest.h"

using namespace baci;
using namespace ACSErrTypeCommon;

LoggingProxy *g_logger = 0;
maci::SimpleClient c;
int nWorker = 0;
std::string channelName1("singleChannel1");
std::string channelName2("singleChannel2");
std::string channelName3("singleChannel3");
std::string channelName4("singleChannel4");
std::string domainName1("domainName1");
std::string alarmDomain("ALARMSYSTEM");

bool nc::HelperTest::m_useMutex = true;
const char* hostname = 0;

static void creation_worker (void* param)
{   
    BACIThreadParameter* baciParameter = (BACIThreadParameter*)param;

    BACIThread* myself = baciParameter->getBACIThread();

    if (BACIThread::InitThread)
    	BACIThread::InitThread(myself->getName().c_str());
    ACS_SHORT_LOG((LM_INFO, "Creation worker number %d", nWorker));
    nc::HelperTest * helper;
    nWorker++;
    char ** myCache = new char*[3];
    myCache[0] = new char[1000];
    myCache[1] = new char[20];
    myCache[2] = new char[1000];
    snprintf(myCache[0],1000,"Worker_%s",myself->getName().c_str());
    snprintf(myCache[1],20,"-ORBInitRef");
    snprintf(myCache[2],1000,"NameService=corbaloc::%s:3001",hostname);
    helper = new nc::HelperTest(channelName1.c_str(), 3, myCache );  
    try{
        helper->createNotificationChannel();
        if(helper->resolveNotifyChannel()){
            //log with error level to show on ref file (default level is 5)
            ACS_SHORT_LOG((LM_ERROR,"NC '%s' was created for thread '%s'",channelName1.c_str(), myself->getName().c_str())); 
        }else{
            ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created for thread '%s' for an unknown reason",channelName1.c_str(), myself->getName().c_str()));  
         }
        
    }catch(NotifyMonitoringExt::NameAlreadyUsed e){
        ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created for thread '%s'",channelName1.c_str(), myself->getName().c_str()));  
    } catch (NotifyMonitoringExt::NameMapError e){
    	ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created for thread '%s': Name Map Error!",channelName1.c_str(), myself->getName().c_str()));
    } catch (CORBAProblemEx){
        ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created for thread '%s': CORBAProblemEx Exception!",channelName1.c_str(), myself->getName().c_str()));
    }
    
    if (BACIThread::DoneThread)
    	BACIThread::DoneThread();

    delete baciParameter;
    for (int i=0; i<=2; i++)
    	delete [] myCache[i];
    delete [] myCache;
    myself->setStopped();
}

static void creation_worker_with_domain (void* param)
{
    BACIThreadParameter* baciParameter = (BACIThreadParameter*)param;

    BACIThread* myself = baciParameter->getBACIThread();

    if (BACIThread::InitThread)
    	BACIThread::InitThread(myself->getName().c_str());
    ACS_SHORT_LOG((LM_INFO, "Creation worker with domain number %d", nWorker));
    nc::HelperTest * helper;
    nWorker++;
    char ** myCache = new char*[3];
    myCache[0] = new char[1000];
    myCache[1] = new char[20];
    myCache[2] = new char[1000];
    snprintf(myCache[0],1000,"Worker_%s",myself->getName().c_str());
    snprintf(myCache[1],20,"-ORBInitRef");
    snprintf(myCache[2],1000,"NameService=corbaloc::%s:3001",hostname);
    helper = new nc::HelperTest(channelName3.c_str(), domainName1.c_str(), 3, myCache );
    try{
        helper->createNotificationChannel();
        if(helper->resolveNotifyChannel()){
            //log with error level to show on ref file (default level is 5)
            ACS_SHORT_LOG((LM_ERROR,"NC '%s@%s' was created for thread '%s'",channelName3.c_str(), domainName1.c_str(), myself->getName().c_str()));
        }else{
            ACS_SHORT_LOG((LM_ERROR,"NC '%s@%s' couldn't be created for thread '%s' for an unknown reason",channelName3.c_str(), domainName1.c_str(), myself->getName().c_str()));
         }

    }catch(NotifyMonitoringExt::NameAlreadyUsed e){
        ACS_SHORT_LOG((LM_ERROR,"NC '%s@%s' couldn't be created for thread '%s'",channelName3.c_str(), domainName1.c_str(), myself->getName().c_str()));
    } catch (NotifyMonitoringExt::NameMapError e){
    	ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created for thread '%s': Name Map Error!",channelName3.c_str(), myself->getName().c_str()));
    } catch (CORBAProblemEx){
    	ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created for thread '%s': CORBAProblemEx Exception!",channelName3.c_str(), myself->getName().c_str()));
    }


    if (BACIThread::DoneThread)
    	BACIThread::DoneThread();

    delete baciParameter;
    for (int i=0; i<=2; i++)
    	delete [] myCache[i];
    delete [] myCache;
    myself->setStopped();
}

static void get_or_creation_worker (void* param)
{   
    BACIThreadParameter* baciParameter = (BACIThreadParameter*)param;

    BACIThread* myself = baciParameter->getBACIThread();

    if (BACIThread::InitThread)
    	BACIThread::InitThread(myself->getName().c_str());
    ACS_SHORT_LOG((LM_INFO, "Get or creation worker number %d", nWorker));
    nc::HelperTest * helper;
    nWorker++;
    char ** myCache = new char*[3];
    myCache[0] = new char[1000];
    myCache[1] = new char[20];
    myCache[2] = new char[1000];
    snprintf(myCache[0],1000,"Worker_%s",myself->getName().c_str());
    snprintf(myCache[1],20,"-ORBInitRef");
    snprintf(myCache[2],1000,"NameService=corbaloc::%s:3001",hostname);
    helper = new nc::HelperTest(channelName2.c_str(), 3, myCache );
    try {
    	if(!helper->resolveInternalNotificationChannel()){
    		ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created nor resolved for thread '%s'", channelName2.c_str(), myself->getName().c_str()));
    	}else{
    		//log with error level to show on ref file (default level is 5)
    		ACS_SHORT_LOG((LM_ERROR,"NC '%s' was created/resolved for thread %s",channelName2.c_str(), myself->getName().c_str()));
    	}
    }catch(NotifyMonitoringExt::NameAlreadyUsed e){
    	ACS_SHORT_LOG((LM_ERROR,"NC '%s@%s' couldn't be created for thread '%s'",channelName2.c_str(), domainName1.c_str(), myself->getName().c_str()));
    } catch (NotifyMonitoringExt::NameMapError e){
    	ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created for thread '%s': Name Map Error!",channelName2.c_str(), myself->getName().c_str()));
    } catch (CORBAProblemEx){
    	ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created for thread '%s': CORBAProblemEx Exception!",channelName2.c_str(), myself->getName().c_str()));
    }

    if(CORBA::is_nil(helper->getNotifyChannel())){
        ACS_SHORT_LOG((LM_ERROR,"Thread '%s' couldn't get the channel '%s'", myself->getName().c_str(), channelName2.c_str()));
    }else{
            //log with error level to show on ref file (default level is 5)
        ACS_SHORT_LOG((LM_ERROR, "Thread '%s' got the channel '%s' properly", myself->getName().c_str(), channelName2.c_str()));
    }
    
    if (BACIThread::DoneThread)
    	BACIThread::DoneThread();

    delete baciParameter;
    for (int i=0; i<=2; i++)
    	delete [] myCache[i];
    delete [] myCache;
    myself->setStopped();

}

static void get_or_creation_worker_with_domain (void* param)
{
    BACIThreadParameter* baciParameter = (BACIThreadParameter*)param;

    BACIThread* myself = baciParameter->getBACIThread();

    if (BACIThread::InitThread)
    	BACIThread::InitThread(myself->getName().c_str());
    ACS_SHORT_LOG((LM_INFO, "Get or creation worker with domain number %d", nWorker));
    nc::HelperTest * helper;
    nWorker++;
    char ** myCache = new char*[3];
    myCache[0] = new char[1000];
    myCache[1] = new char[20];
    myCache[2] = new char[1000];
    snprintf(myCache[0],1000,"Worker_%s",myself->getName().c_str());
    snprintf(myCache[1],20,"-ORBInitRef");
    snprintf(myCache[2],1000,"NameService=corbaloc::%s:3001",hostname);
    helper = new nc::HelperTest(channelName4.c_str(), alarmDomain.c_str(), 3, myCache );
    try {
    	if(!helper->resolveInternalNotificationChannel()){
    		ACS_SHORT_LOG((LM_ERROR,"NC '%s@%s' couldn't be created nor resolved for thread '%s'", channelName4.c_str(), alarmDomain.c_str(), myself->getName().c_str()));
    	}else{
    		//log with error level to show on ref file (default level is 5)
    		ACS_SHORT_LOG((LM_ERROR,"NC '%s@%s' was created/resolved for thread %s",channelName4.c_str(), alarmDomain.c_str(), myself->getName().c_str()));
    	}
    }catch(NotifyMonitoringExt::NameAlreadyUsed e){
    	ACS_SHORT_LOG((LM_ERROR,"NC '%s@%s' couldn't be created for thread '%s'",channelName4.c_str(), domainName1.c_str(), myself->getName().c_str()));
    } catch (NotifyMonitoringExt::NameMapError e){
    	ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created for thread '%s': Name Map Error!",channelName4.c_str(), myself->getName().c_str()));
    } catch (CORBAProblemEx){
    	ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created for thread '%s': CORBAProblemEx Exception!",channelName4.c_str(), myself->getName().c_str()));
    }

    if(CORBA::is_nil(helper->getNotifyChannel())){
        ACS_SHORT_LOG((LM_ERROR,"Thread '%s' couldn't get the channel '%s@%s'", myself->getName().c_str(), channelName4.c_str(), alarmDomain.c_str()));
    }else{
            //log with error level to show on ref file (default level is 5)
        ACS_SHORT_LOG((LM_ERROR, "Thread '%s' got the channel '%s@%s' properly", myself->getName().c_str(), channelName4.c_str(), alarmDomain.c_str()));
    }

    if (BACIThread::DoneThread)
    	BACIThread::DoneThread();

    delete baciParameter;
    for (int i=0; i<=2; i++)
    	delete [] myCache[i];
    delete [] myCache;
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

    //Creating threads for create channel with domains
    int nCreationWithDomainThreads = 4;
    BACIThread * creationWithDomainThreads[nCreationWithDomainThreads];
    for(i = 0; i < nCreationWithDomainThreads; i++){
    	std::stringstream ss;
    	ss << "Test_creation_with_domain_thread_";
    	ss << (i+1);
    	ss >> s;
    	ACS_SHORT_LOG((LM_INFO, "Creating '%s' thread", s.c_str()));
    	creationWithDomainThreads[i]=threadManager_p->create(s.c_str(),
    			(void*)creation_worker_with_domain, (void*)0);
    }

   for(i = 0; i < nCreationWithDomainThreads; i++){
    	creationWithDomainThreads[i]->resume();
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

    //get_or_creation_worker_with_domain
    int nGetOrCreationWithDomainThreads = 4;
	BACIThread * getOrCreationWithDomainThreads[nGetOrCreationWithDomainThreads];
	for(i = 0; i < nGetOrCreationWithDomainThreads; i++){
		std::stringstream ss;
		ss << "Test_get_or_creation_with_domain_thread_";
		ss << (i+1);
		ss >> s;
		ACS_SHORT_LOG((LM_INFO, "Creating '%s' thread", s.c_str()));
		getOrCreationWithDomainThreads[i]=threadManager_p->create(s.c_str(),
                                (void*)get_or_creation_worker_with_domain, (void*)0);
	}

	for(i = 0; i < nGetOrCreationWithDomainThreads; i++){
		getOrCreationWithDomainThreads[i]->resume();
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

    ACS_SHORT_LOG((LM_INFO, "Deleting Thread manager."));

    delete threadManager_p;

    ACS_SHORT_LOG((LM_INFO, "Done."));
    g_logger = 0;
    LoggingProxy::done();
    //delete m_logger;

    return 0;
}

