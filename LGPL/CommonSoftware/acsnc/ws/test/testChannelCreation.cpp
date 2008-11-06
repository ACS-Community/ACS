#include <acsutil.h>
#include <logging.h>
#include <baciThread.h>
#include <acsutilPorts.h>
#include <maciSimpleClient.h>
#include <orbsvcs/Notify/MonitorControlExt/NotifyMonitoringExtC.h>
#include "acsncCDBProperties.h"
//#include "testChannelCreation.h"
 using namespace baci;

LoggingProxy *g_logger = 0;
int nWorker = 0;
maci::SimpleClient c;

static void worker (void* param)
{   
    nWorker++;
    ACS_SHORT_LOG((LM_INFO, "worker number %d", nWorker));
    BACIThreadParameter* baciParameter = (BACIThreadParameter*)param;

    BACIThread* myself = baciParameter->getBACIThread();

    if (BACIThread::InitThread) BACIThread::InitThread(myself->getName().c_str());
    
    ACE_TCHAR corbalocRef[240];
    const char* hostname = 0;
    hostname = ACSPorts::getIP();
    ACE_OS::sprintf(corbalocRef, "corbaloc::%s:%s/NameService", hostname, ACSPorts::getNamingServicePort().c_str());
  
    CORBA::Object_var obj = c.getORB()->string_to_object(corbalocRef);
    CosNaming::NamingContext_var namingContext = CosNaming::NamingContext::_narrow(obj.in());
    
    CosNaming::Name name(1);
    name.length(1);
    name[0].id = CORBA::string_dup (acscommon::NOTIFICATION_FACTORY_NAME);
	CORBA::Object_var corbaObj = namingContext->resolve(name);
	if(CORBA::is_nil(corbaObj.in()) == true)
    {
        ACS_SHORT_LOG((LM_ERROR, "resolve NotificationFactory error occured"));
        return;
    }

	NotifyMonitoringExt::EventChannelFactory_var notifyFactory = NotifyMonitoringExt::EventChannelFactory::_narrow(corbaObj.in());
	if(CORBA::is_nil(notifyFactory.in()) == true)
    {
        ACS_SHORT_LOG((LM_ERROR, "narrow new NotificationFactory error occured"));
        return;
    }
   
    CosNotification::QoSProperties initial_qos;
    CosNotification::AdminProperties initial_admin;
    CosNotifyChannelAdmin::ChannelID id;
    try{
        CosNotifyChannelAdmin::EventChannel_var ec =
            notifyFactory->create_named_channel(initial_qos,
                                         initial_admin,
                                         id,
                                       "mychannel");
    }catch(NotifyMonitoringExt::NameAlreadyUsed ex){
        ACS_SHORT_LOG((LM_ERROR, "NameAlreadyUsed Exception received on worker number %d", nWorker));
    }catch (...){
         ACS_SHORT_LOG((LM_INFO, "Unknown Exception throwed."));
    }
        ACE_Thread::yield();

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

    c.init(argc,argv);
    c.login();
    g_logger = m_logger;
    BACIThreadManager * threadManager_p = new BACIThreadManager();

    BACIThread * thread1 = threadManager_p->create("Test thread 1",
                          (void*)worker, (void*)0);
    BACIThread * thread2 = threadManager_p->create("Test thread 2",
                          (void*)worker, (void*)0);
    thread1->resume();
    thread2->resume();

    ACS_SHORT_LOG((LM_INFO, "Spawned."));

    ACE_OS::sleep(10);

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

