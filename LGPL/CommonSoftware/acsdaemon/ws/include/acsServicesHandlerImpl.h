#ifndef _ACS_SERVICES_HANDLER_IMPL_H_
#define _ACS_SERVICES_HANDLER_IMPL_H_

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsdaemonS.h"
#include "logging.h"
#include <acserr.h>
#include <acsdaemonErrType.h>
#include <ACSErrTypeCommon.h>
#include <acsutilPorts.h>

class ACSServicesHandlerImpl : public POA_acsdaemon::ServicesDaemon {

  public:
    
   /**
    * Constructor
    */
    ACSServicesHandlerImpl();
  
    /**
     * Destructor
     */
    virtual ~ACSServicesHandlerImpl();

    /**
     * Name retriever
     */
    const char* getName();

    const char* getType();
    
    const char* getPort();
    
    /*************************** CORBA interface *****************************/

    virtual void start_acs (
        ::CORBA::Short instance_number,
        const char * additional_command_line
      )
      ACE_THROW_SPEC ((
        CORBA::SystemException,
        ::acsdaemonErrType::FailedToStartAcsEx,
	::ACSErrTypeCommon::BadParameterEx
      ));
    virtual void stop_acs (
        ::CORBA::Short instance_number,
        const char * additional_command_line
      )
      ACE_THROW_SPEC ((
        CORBA::SystemException,
        ::acsdaemonErrType::FailedToStopAcsEx,
	::ACSErrTypeCommon::BadParameterEx
      ));

     virtual char * status_acs ( 
	 ::CORBA::Short instance_number
	 )
      ACE_THROW_SPEC ((
        CORBA::SystemException,
        ::acsdaemonErrType::FailedToGetAcsStatusEx
      ));

  private:
    std::string h_name;
    std::string h_type;

};



#endif
