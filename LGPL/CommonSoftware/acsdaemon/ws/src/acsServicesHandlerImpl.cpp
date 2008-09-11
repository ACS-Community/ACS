/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@$Id: acsServicesHandlerImpl.cpp,v 1.10 2008/09/11 09:29:56 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2006-06-21 created
* agrimstr 2007-11-07 extracted service interface implementation to separate
*                     class
*/

#include "acsServicesHandlerImpl.h"

/*****************************************************************/

void CommandProcessorThread::onStart()
{
    running = true;
}

void CommandProcessorThread::stop()
{
    Request *nowreq;
    ACSErr::Completion_var comp;
    acsdaemonErrType::AcsStartServicesCompletion ok(__FILE__, __LINE__, "ACSServicesHandlerImpl::onStop");

    running = false;
    comp = ok.returnCompletion(false);

    m_mutex->acquire();
    while (!pending.empty())
    {
	nowreq = pending.front();
	pending.pop();
	nowreq->cb->done(comp.in());
	free(nowreq);
    }
    m_mutex->release();
    m_wait->signal();
}

void CommandProcessorThread::runLoop()
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::ACSErrTypeCommon::BadParameterEx
			))
{
    Request *nowreq;
    ACSErr::Completion_var comp;
    acsdaemonErrType::AcsStartServicesCompletion ok(__FILE__, __LINE__, "ACSServicesHandlerImpl::runLoop");

    while (running)
    {
        m_mutex->acquire();
        if (pending.empty())
	    m_wait->wait();

	if (!running) break;

	nowreq = pending.front();
	pending.pop();

	m_mutex->release();

	comp = ok.returnCompletion(false);
	nowreq->cb->working(comp.in());

	ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", nowreq->cmd));

	int result = ACE_OS::system(nowreq->cmd);

	if (result < 0)
	{
	    ACS_SHORT_LOG ((LM_INFO, "Result is: '%s'.", result));
 	}

	comp = ok.returnCompletion(false);
	free((void*)nowreq->cmd);
	nowreq->cb->done(comp.in());
	free(nowreq);
    }
}

void CommandProcessorThread::addRequest(Request* r)
{
    m_mutex->acquire();
    pending.push(r);
    m_mutex->release();
    m_wait->signal();
}


ACSServicesHandlerImpl::ACSServicesHandlerImpl () : h_name("ACS Services Daemon"), h_type(::acsdaemon::servicesDaemonServiceName)
{
    cmdproc = tm.create<CommandProcessorThread>(h_name.c_str());
}

ACSServicesHandlerImpl::~ACSServicesHandlerImpl (void)
{
    tm.destroy(cmdproc);
}

std::string ACSServicesHandlerImpl::getName ()
{
    return h_name;
}

std::string ACSServicesHandlerImpl::getType(void)
{
    return h_type;
}

std::string ACSServicesHandlerImpl::getPort(void)
{
    return ACSPorts::getServicesDaemonPort();
}

void ACSServicesHandlerImpl::initialize(CORBA::ORB_ptr orb)
{
    cmdproc->resume();
}

void ACSServicesHandlerImpl::dispose(CORBA::ORB_ptr orb)
{
    cmdproc->exit();
}

/************************** CORBA interface ****************************/

void
ACSServicesHandlerImpl::start_acs (
    acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number,
    const char * additional_command_line
    )
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::ACSErrTypeCommon::BadParameterEx
			))
{
    const char * cmdln = (additional_command_line ? additional_command_line : "");
    
    std::string logDirectory="~/.acs/commandcenter/";
    
    //create the directory
    std::string mkdir("mkdir -p ");
    mkdir.append(logDirectory);
    ACE_OS::system(mkdir.c_str());
    
    std::string timeStamp(getStringifiedTimeStamp().c_str());
    
    if( timeStamp.find(":") != std::string::npos)
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find(":") != std::string::npos )
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find("T") != std::string::npos)
        timeStamp.replace(timeStamp.find("T"),1,"_");
    
    char commandline[1000];
    snprintf(commandline, 1000, "acsStart -b %d %s &> %sacsStart_%s", instance_number, cmdln, logDirectory.c_str(), timeStamp.c_str());
    
    Request *newreq = new Request(acsdaemon::DaemonCallback::_duplicate(callback), strdup(commandline));
    cmdproc->addRequest(newreq);
    ACSErr::Completion_var comp;
    acsdaemonErrType::AcsStartServicesCompletion ok(__FILE__, __LINE__, "ACSServicesHandlerImpl::start_acs");
    comp = ok.returnCompletion(false);
    callback->working(comp);
}



void
ACSServicesHandlerImpl::stop_acs (
    acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number,
    const char * additional_command_line
    )
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::ACSErrTypeCommon::BadParameterEx
			))
{
    const char * cmdln = (additional_command_line ? additional_command_line : "");

    //get the directory name to store the container stdout
    std::string logDirectory="~/.acs/commandcenter/";
    
    //create the directory
    std::string mkdir("mkdir -p ");
    mkdir.append(logDirectory);
    ACE_OS::system(mkdir.c_str());

    std::string timeStamp(getStringifiedTimeStamp().c_str());

    if( timeStamp.find(":") != std::string::npos)
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find(":") != std::string::npos )
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find("T") != std::string::npos)
        timeStamp.replace(timeStamp.find("T"),1,"_");

    char command[1000];
    // execute: "acsStop -b <instance> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!
    snprintf(command, 1000, "acsStop -noShutdownLocalContainers -baseport %d %s &> %sacsStop_%s", instance_number, cmdln, logDirectory.c_str(), timeStamp.c_str());

    Request *newreq = new Request(acsdaemon::DaemonCallback::_duplicate(callback), strdup(command));
    cmdproc->addRequest(newreq);
    ACSErr::Completion_var comp;
    acsdaemonErrType::AcsStartServicesCompletion ok(__FILE__, __LINE__, "ACSServicesHandlerImpl::stop_acs");
    comp = ok.returnCompletion(false);
    callback->working(comp);


}//ACSServicesDaemonImpl::stop_acs

char * ACSServicesHandlerImpl::status_acs ( 
    ::CORBA::Short instance_number
    )
      ACE_THROW_SPEC ((
        CORBA::SystemException,
        ::acsdaemonErrType::FailedToGetAcsStatusEx
      ))
{
    int result;
    char *acsStatus=0;
    char command[100];
    std::string logFile="acsStatus_";

    logFile += getStringifiedTimeStamp().c_str();

    snprintf(command, 100, "acsStatus -b %d &> %s", instance_number, logFile.c_str());

    ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", command));

    result = ACE_OS::system(command);
    if (result < 0)
	{
	snprintf(command, 100, "rm -rf %s", logFile.c_str());
	ACE_OS::system(command);
	throw ::acsdaemonErrType::FailedToGetAcsStatusExImpl(
	    __FILE__, __LINE__, 
	    "::ACSServicesDaemonImpl::status_acs").getFailedToGetAcsStatusEx();
	}
    
    ACS_SHORT_LOG ((LM_INFO, "Reading output from: '%s'.", logFile.c_str()));
    
    ifstream outFile(logFile.c_str(), ios::in|ios::ate);
    if (outFile.is_open())
	{
	int outFileSize = outFile.tellg();
	acsStatus = new char [outFileSize+1];
	outFile.seekg (0, ios::beg);
	outFile.read (acsStatus, outFileSize);
	outFile.close();
	acsStatus[outFileSize]=0;
	snprintf(command, 100, "rm -rf %s", logFile.c_str());
	ACE_OS::system(command);
	}
    else
	{
	snprintf(command, 100, "rm -rf %s", logFile.c_str());
	ACE_OS::system(command);
	throw ::acsdaemonErrType::FailedToGetAcsStatusExImpl(
	    __FILE__, __LINE__, 
	    "::ACSServicesDaemonImpl::status_acs").getFailedToGetAcsStatusEx();
	}//if-else

    //acsStatus is deleted by CORBA
    return acsStatus;
}//ACSServicesHandlerImpl::status_acs

void ACSServicesHandlerImpl::shutdown ()
      ACE_THROW_SPEC ((
        CORBA::SystemException,
        ::maciErrType::NoPermissionEx
      ))
{
    if (h_service->isProtected())
	{
	throw ::maciErrType::NoPermissionEx();
	}
    ACS_SHORT_LOG ((LM_INFO, "Shutting down the ACS Services Daemon on remote request..."));
    h_service->shutdown(false);
}
