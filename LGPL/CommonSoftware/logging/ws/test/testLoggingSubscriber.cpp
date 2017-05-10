
#include <orbsvcs/CosNotifyCommS.h>
#include <orbsvcs/CosNotifyChannelAdminC.h>
#include <orbsvcs/CosNamingC.h>
#include <acscommonC.h>
#include "loggingHelper.h"
#include "acsThread.h"
#include "acsThreadManager.h"
#include <acsutilPorts.h>

static const int32_t LOGS_RECEIVED          = 1;
static const int32_t CONSUMER_DISCONNECTED  = 2;
static const int32_t CONSUMER_RECONNECTED   = 3;
static const int32_t LOGS_RECEIVED_AGAIN    = 4;
int32_t currentStatus = 0;

class StructuredEventConsumer_i;

/**
 * Class to wrapp all CORBA stuff related to the Notify Channel
 */
class DataCon {
public:
    CosNaming::NamingContext_var                            naming_context;
    CosNotifyChannelAdmin::EventChannel_var                 notify_channel;
    CosNotifyChannelAdmin::ConsumerAdmin_var                consumer_admin;
    CosNotifyChannelAdmin::ProxySupplier_var                proxy_supplier;
    CosNotifyChannelAdmin::StructuredProxyPushSupplier_var  structured_proxy_supplier;
    PortableServer::Servant_var<StructuredEventConsumer_i>  consumer_servant;
    CosNotifyComm::StructuredPushConsumer_var               consumer;

    CORBA::Object_ptr resolve(CosNaming::Name name)
    {
        return naming_context->resolve(name);
    }    

    bool connectProxySupplierAndConsumer()
    {
        try {
            structured_proxy_supplier->connect_structured_push_consumer(consumer.in());
            return true;
        } catch(CORBA::OBJECT_NOT_EXIST &ex) { // Proxy not exists
            //std::cerr << "Cannot connect consumer to the proxy supplier because the proxy doesn't exist" << std::endl;
            return false;
        } catch(CosEventChannelAdmin::AlreadyConnected &ex) {
            return true;
        } catch(...) {}
        return false;
    }

    bool isConnected()
    {
        try {
            structured_proxy_supplier->connect_structured_push_consumer(consumer.in());
            return true;
        } catch(CORBA::OBJECT_NOT_EXIST &ex) { // Proxy not exists
            return false;
        } catch(CosEventChannelAdmin::AlreadyConnected &ex) {
            return true;
        } catch(...) {}
        return false;
    }
};

bool reinitChannel(bool logBin,DataCon &dataCon);

/**
 * Thread to check the connection to the Logging Notify Channel. When the connection is lost
 * tries to reconnect again to the channel.
 */
class NotifyChannelChecker : public ACS::Thread
{
  public:
    NotifyChannelChecker(const ACE_CString& name,
          const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime,
          const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime,
          bool del=false) 
        : ACS::Thread(name, responseTime, sleepTime, del)
    {
        loopCounter_m = 0;
    }

    NotifyChannelChecker(const ACE_CString& name,
          const ACS::TimeInterval& responseTime,
          const ACS::TimeInterval& sleepTime,
          bool del,
          const long _thrFlags) 
        : ACS::Thread(name, responseTime, sleepTime, del, _thrFlags)
    {
        loopCounter_m = 0;
    }

    ~NotifyChannelChecker()
    {
        terminate();
    }

    void init(bool logBin, DataCon &dataCon)
    {
        this->logBin = logBin;
        this->dataCon = dataCon;
    }

    virtual void runLoop()
    {
        ++ loopCounter_m;

        dataCon.connectProxySupplierAndConsumer();

        if(dataCon.isConnected() == false)
        {
            if(currentStatus == LOGS_RECEIVED)
            {
                currentStatus = CONSUMER_DISCONNECTED;
                ACS_SHORT_LOG((LM_INFO, "Consumer disconnected"));
            } else if(currentStatus == LOGS_RECEIVED_AGAIN) {
                ACS_SHORT_LOG((LM_ERROR, "Consumer disconnected again!!"));
            }

            try {
                if(reinitChannel(logBin, dataCon))
                {
                    if(currentStatus == CONSUMER_DISCONNECTED)
                    {
                        currentStatus = CONSUMER_RECONNECTED;
                        ACS_SHORT_LOG((LM_INFO, "Consumer reconnected!"));
                    }
                }
            } catch(...) {
                //ACS_SHORT_LOG((LM_ERROR, "Some sort of error while reinitializing the connection to the channel!"));
            }
        }
    }

  protected:
    int loopCounter_m; 
    bool logBin;
    DataCon dataCon;
};




class StructuredEventConsumer_i : public virtual POA_CosNotifyComm::StructuredPushConsumer
{
public:
    // Constructor.
    StructuredEventConsumer_i(CORBA::ORB_ptr orb);

    // Override operations from StructuredPushConsumer interface.
    virtual void push_structured_event(const CosNotification::StructuredEvent& event);
    virtual void disconnect_structured_push_consumer();
    virtual void offer_change (const CosNotification::EventTypeSeq& events_added,
                                const CosNotification::EventTypeSeq& events_removed);

private:
    CORBA::ORB_var orb_;
};

// Constructor
StructuredEventConsumer_i::StructuredEventConsumer_i(CORBA::ORB_ptr orb)
    : orb_(CORBA::ORB::_duplicate(orb))
{ }

void StructuredEventConsumer_i::push_structured_event(const CosNotification::StructuredEvent& event)
{
    if(currentStatus == 0)
    {
        currentStatus = LOGS_RECEIVED;
        ACS_SHORT_LOG((LM_INFO, "Logs received in the consumer"));
    } else if(currentStatus == CONSUMER_RECONNECTED) {
        currentStatus = LOGS_RECEIVED_AGAIN;
        ACS_SHORT_LOG((LM_INFO, "Logs received again in the consumer"));
    }

/*
    const char* value;
    Logging::XmlLogRecordSeq *xmlSeq;

    if(event.remainder_of_body >>= value) {
        std::string currLog(value);
        std::cout << "log: " << currLog << std::endl;
    }

    if(event.remainder_of_body >>= xmlSeq) {
        for(int32_t i = 0;i < xmlSeq->length();++i)
        {
            std::string currLog((*xmlSeq)[i].xml);
            std::cout << "Log: " << (*xmlSeq)[i].xml << std::endl;
        }    
    }

    for (int i=0; i<event.filterable_data.length(); ++i) {
        if (event.filterable_data[i].value >>= value) {
            std::cout << event.filterable_data[i].name << "\t" << value << std::endl;
        }
    }
*/
}

void StructuredEventConsumer_i::disconnect_structured_push_consumer()
{
    CORBA::Object_var obj = orb_->resolve_initial_references ("POACurrent");
    PortableServer::Current_var current = PortableServer::Current::_narrow (obj.in());
    PortableServer::POA_var poa = current->get_POA ();
    PortableServer::ObjectId_var objectId = current->get_object_id ();
    poa->deactivate_object (objectId.in());
}

void StructuredEventConsumer_i::offer_change (const CosNotification::EventTypeSeq& events_added,
                                const CosNotification::EventTypeSeq& events_removed)
{}

bool get_channel(bool logBin,DataCon &dataCon,std::string *chName=NULL)
{
    std::string channelAndDomainName;
    CosNaming::Name name;
    name.length (1);
    if(!logBin)
        channelAndDomainName = std::string(acscommon::LOGGING_CHANNEL_XML_NAME);
    else
        channelAndDomainName = std::string(acscommon::LOGGING_CHANNEL_NAME);
    channelAndDomainName = channelAndDomainName + 
        acscommon::NAMESERVICE_BINDING_NC_DOMAIN_SEPARATOR + acscommon::ACS_NC_DOMAIN_LOGGING;

    if(chName != NULL)
    {
        *chName = channelAndDomainName;
    }

    name[0].id = CORBA::string_dup (channelAndDomainName.c_str());
    name[0].kind = CORBA::string_dup (acscommon::NC_KIND);
    CORBA::Object_var notify_channel_obj = dataCon.resolve(name); // naming_context->resolve(name);
    dataCon.notify_channel = CosNotifyChannelAdmin::EventChannel::_narrow(notify_channel_obj.in());
    if (CORBA::is_nil (dataCon.notify_channel.in())) {
        return false;
    }
    return true;
}

bool get_consumer_admin(DataCon &dataCon)
{
    CosNotifyChannelAdmin::AdminID adminid;
    CosNotifyChannelAdmin::InterFilterGroupOperator ifgop = CosNotifyChannelAdmin::AND_OP;
    dataCon.consumer_admin = dataCon.notify_channel->new_for_consumers (ifgop, adminid);
    if (CORBA::is_nil (dataCon.consumer_admin.in())) {
        return false;
    }
    return true;
}

bool get_structured_push_supplier(DataCon &dataCon)
{
    CosNotifyChannelAdmin::ProxyID proxy_id;
    dataCon.proxy_supplier = dataCon.consumer_admin->obtain_notification_push_supplier(CosNotifyChannelAdmin::STRUCTURED_EVENT,proxy_id);
    dataCon.structured_proxy_supplier = CosNotifyChannelAdmin::StructuredProxyPushSupplier::_narrow(dataCon.proxy_supplier.in());
    if (CORBA::is_nil (dataCon.structured_proxy_supplier.in())) {
        return false;
    }
    return true;
}

/**
 * Initialize the connection to the channel
 */
bool initChannel(CORBA::ORB_ptr orb,bool logBin,DataCon &dataCon)
{
    dataCon.naming_context = LoggingHelper::resolveNameService(orb);
    if (CORBA::is_nil(dataCon.naming_context.ptr()))
    {
        throw CORBA::UNKNOWN ();
    }
    ACS_SHORT_LOG((LM_INFO, "Naming Service has been acquired"));

    // Get the channel
    std::string channelName;
    if(get_channel(logBin, dataCon, &channelName))
    {
        ACS_SHORT_LOG((LM_INFO, "Channel has been acquired: %s",channelName.c_str()));
    } else {
        ACS_SHORT_LOG((LM_ERROR, "Unable to get the channel: %s",channelName.c_str()));
        return false;
    }

    // Get consumer admin
    if(get_consumer_admin(dataCon))
    {
        ACS_SHORT_LOG((LM_INFO, "Consumer Admin has been acquired"));
    } else {
        ACS_SHORT_LOG((LM_ERROR, "Unable to get consumer admin"));
        return false;
    }

    // Get structured push supplier
    if(get_structured_push_supplier(dataCon))
    {
        ACS_SHORT_LOG((LM_INFO, "Structured Push Supplier has been acquired"));
    } else {
        ACS_SHORT_LOG((LM_ERROR, "Unable to get structured proxy push supplier"));
        return false;
    }

    dataCon.consumer_servant = new StructuredEventConsumer_i(orb);
    CORBA::Object_var poa_obj = orb->resolve_initial_references("RootPOA");
    PortableServer::POA_var poa = PortableServer::POA::_narrow(poa_obj.in());
    PortableServer::ObjectId_var oid = poa->activate_object(dataCon.consumer_servant.in());
    CORBA::Object_var consumer_obj = poa->id_to_reference(oid.in());
    dataCon.consumer = CosNotifyComm::StructuredPushConsumer::_narrow(consumer_obj.in());
    

    if(dataCon.connectProxySupplierAndConsumer())
    {
        ACS_SHORT_LOG((LM_INFO, "Consumer connected to the proxy supplier"));
        return true;
    } else {
        ACS_SHORT_LOG((LM_ERROR, "Unable to connect the consumer to the proxy supplier"));
        return false;
    }
}

/**
 * Reinitialize the connection to the Notify Channel
 */
bool reinitChannel(bool logBin,DataCon &dataCon)
{
    // Get the channel
    if(false == get_channel(logBin, dataCon))
    {
        return false;
    }

    // Get consumer admin
    if(false == get_consumer_admin(dataCon))
    {
        return false;
    }

    // Get structured push supplier
    if(false == get_structured_push_supplier(dataCon))
    {
        return false;
    }

    return dataCon.connectProxySupplierAndConsumer();
}


void initLogger(LoggingProxy &logger,int argc,char* argv[])
{
    CORBA::ORB_var orb = CORBA::ORB_init(argc, argv);

    CosNaming::NamingContext_var naming_context = LoggingHelper::resolveNameService(orb.in());
    if (CORBA::is_nil(naming_context.ptr()))
    {
        throw CORBA::UNKNOWN ();
    }

    std::string channelAndDomainName;
    CosNaming::Name name;
    name.length (1);
    name[0].id = "Log";
    //name[0].id = CORBA::string_dup (channelAndDomainName.c_str());
    //name[0].kind = CORBA::string_dup (acscommon::NC_KIND);
    CORBA::Object_var log_obj = naming_context->resolve(name);

    if(log_obj.ptr() != CORBA::Object::_nil())
    {
        Logging::AcsLogService_var logSvc = Logging::AcsLogService::_narrow(log_obj.in());

        if (logSvc.ptr() != Logging::AcsLogService::_nil())
        {
            logger.setCentralizedLogger(logSvc.in());
        }
    }
}


int main(int argc, char* argv[])
{
    bool logBin = false;
    char *acsLogType = getenv("ACS_LOG_BIN");
    if (acsLogType && *acsLogType){
        if(strcmp("true", acsLogType) == 0)
            logBin = true;
    }

    // Initialize the logger
    const char *hn=ACSPorts::getIP();
    LoggingProxy::ProcessName("main");
    ACE_Log_Msg::instance()->local_host(hn);
    LoggingProxy m_logger (0, 0, 31, 0);
    LoggingProxy::init (&m_logger);
    initLogger(m_logger, argc, argv);

    ACS::ThreadManager tm;
    NotifyChannelChecker *checker = NULL;
    DataCon dataCon;

    try {
        CORBA::ORB_var orb = CORBA::ORB_init(argc, argv);
        CORBA::Object_var poa_obj = orb->resolve_initial_references("RootPOA");
        PortableServer::POA_var poa = PortableServer::POA::_narrow(poa_obj.in());

        initChannel(orb.in(), logBin, dataCon);

        checker = tm.create<NotifyChannelChecker>("Checker", 2 * 100 * 1000 * 10, 
                                                20 * 100 * 1000 * 10/*Every 2 sec checks NC status*/);
        checker->init(logBin, dataCon);
        checker->resume();

        PortableServer::POAManager_var mgr = poa->the_POAManager();
        mgr->activate();
        orb->run();
    } catch (CORBA::Exception& ex) {
        std::cerr << ex << std::endl;
        return 1;
    }

    if(checker != NULL)
    {
        tm.destroy(checker);
    }

    return 0;
}




