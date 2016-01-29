
#include "loggingGetLogger.h"
#include "loggingACEMACROS.h"
#include "loggingHelper.h"
#include <acsutilPorts.h>



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
    const char *hn=ACSPorts::getIP();
    LoggingProxy::ProcessName("main");
    ACE_Log_Msg::instance()->local_host(hn);
    LoggingProxy m_logger (0, 0, 31, 0);
    LoggingProxy::init (&m_logger);
    initLogger(m_logger, argc, argv);

    int32_t i = 0;
    while(true)
    {
        ++i;
        //for(int32_t j = 0;j < 100;++j)
            ACS_SHORT_LOG((LM_ERROR, "Generated log %d", i));
        sleep(1);
    }

    return 0;
}
