#ifndef _ACS_CONTAINER_HANDLER_IMPL_H_
#define _ACS_CONTAINER_HANDLER_IMPL_H_

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsdaemonS.h"
#include "logging.h"
#include <acserr.h>
#include <acsdaemonErrType.h>
#include <ACSErrTypeCommon.h>
#include <acsutilPorts.h>

class ACSContainerHandlerImpl : public POA_acsdaemon::ContainerDaemon {

  public:
    
   /**
    * Constructor
    */
    ACSContainerHandlerImpl();
  
    /**
     * Destructor
     */
    virtual ~ACSContainerHandlerImpl();

    /**
     * Name retriever
     */
    const char* getName();

    const char* getType();
    
    const char* getPort();

    /*************************** CORBA interface *****************************/

    virtual void start_container (
        const char * container_type,
        const char * container_name,
        ::CORBA::Short instance_number,
        const char * additional_command_line
      )
      ACE_THROW_SPEC ((
        CORBA::SystemException,
        ::acsdaemonErrType::FailedToStartContainerEx,
	::ACSErrTypeCommon::BadParameterEx
      ));
    virtual void stop_container (
        const char * container_name,
        ::CORBA::Short instance_number,
        const char * additional_command_line
      )
      ACE_THROW_SPEC ((
        CORBA::SystemException,
        ::acsdaemonErrType::FailedToStopContainerEx,
	::ACSErrTypeCommon::BadParameterEx
      ));

  private:
    std::string h_name;
    std::string h_type;

};



#endif
