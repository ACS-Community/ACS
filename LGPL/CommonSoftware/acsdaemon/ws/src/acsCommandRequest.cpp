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
* "@$Id: acsCommandRequest.cpp,v 1.6 2008/10/27 21:11:23 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* azagar   2008-08-12 created
*/

#include "acsCommandRequest.h"
#include <acsdaemonErrType.h>
#include <ACSErrTypeOK.h>
#include <acsutilPorts.h>

/*********************** Command construction helper methods ***********************/

char *prepareCommand(const char *command, short instance_number, bool wait, const char *name, const char *additional_command_line, bool log) {
    char buffer[64];
    sprintf(buffer, "%s -b %d", command, instance_number);
    ACE_CString commandline = buffer;
    if (wait) commandline = commandline + " -w";
    if (name != NULL) commandline = commandline + " -n " + name;
    if (additional_command_line != NULL) commandline = commandline + " " + additional_command_line;
    if (log) {
        ACE_CString logDirectory="~/.acs/commandcenter/";

    std::string timeStamp(getStringifiedTimeStamp().c_str());

    if( timeStamp.find(":") != std::string::npos)
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find(":") != std::string::npos )
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find("T") != std::string::npos)
        timeStamp.replace(timeStamp.find("T"),1,"_");

        //create the directory
        ACE_OS::system(("mkdir -p " + logDirectory).c_str());
        commandline = commandline + " &> " + logDirectory + command + "_" + timeStamp.c_str();
    }
    return commandline.rep();
}

void execCommand(char *commandline, acsdaemon::DaemonCallback_ptr callback, RequestProcessorThread *reqproc) {
    Request *newreq = new LocalCommandRequest(callback, commandline);
    reqproc->addRequest(newreq);
    acsdaemonErrType::AcsStartServicesCompletion ok(__FILE__, __LINE__, "execCommand");
    ACSErr::Completion_var comp = ok.returnCompletion(false);
    if (callback != NULL) {
        try {
            callback->working(comp.in());
        } catch (CORBA::TIMEOUT timeout) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a 'working' callback call for local request!"));
        }
    }
}

/*********************** RequestProcessorThread ***********************/

void RequestProcessorThread::onStart()
{
    running = true;
}

void RequestProcessorThread::stop()
{
    Request *nowreq;
    running = false;
    m_mutex->acquire();
    while (!pending.empty())
    {
	nowreq = pending.front();
	pending.pop();
	nowreq->abort();
	delete nowreq;
    }
    m_mutex->release();
    m_wait->signal();
}

void RequestProcessorThread::runLoop()
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::ACSErrTypeCommon::BadParameterEx
			))
{
    Request *nowreq;
    while (running)
    {
        m_mutex->acquire();
        if (pending.empty())
	    m_wait->wait();
	if (!running) break;
	nowreq = pending.front();
	pending.pop();
	m_mutex->release();
        nowreq->execute();
	delete nowreq;
    }
}

void RequestProcessorThread::addRequest(Request* r)
{
    m_mutex->acquire();
    if (!running) {
        r->abort();
        m_mutex->release();
        return;
    }
    pending.push(r);
    m_mutex->release();
    m_wait->signal();
}

/**************************** LocalCommandRequest ****************************/

void LocalCommandRequest::abort() {
    if (callback != NULL) {
        acsdaemonErrType::ProcessingAbortedCompletion ok(__FILE__, __LINE__, "LocalCommandRequest::abort");
        ACSErr::Completion_var comp = ok.returnCompletion(false);
        try {
            callback->done(comp.in());
        } catch (CORBA::TIMEOUT timeout) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a callback call for aborted local request!"));
        }
    }
}

void LocalCommandRequest::execute() {
    ACSErr::Completion_var comp;
    ACSErrTypeOK::ACSErrOKCompletion ok;
    comp = ok.returnCompletion(false);
    if (callback != NULL) {
        try {
            callback->working(comp.in());
        } catch (CORBA::TIMEOUT timeout) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a 'working' callback call for local request!"));
        }
    }
    ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", cmd));
    int result = ACE_OS::system(cmd) >> 8;
    if (result != EC_OK) {
        ACS_SHORT_LOG ((LM_INFO, "Result is: '%d'.", result));
	ACSErr::CompletionImpl *failed;
        switch (result) {
        case EC_CANNOTCREATE:
            failed = new acsdaemonErrType::CannotCreateInstanceCompletion (__FILE__, __LINE__, "LocalCommandRequest::execute");
            break;
        case EC_CANNOTUSE:
            failed = new acsdaemonErrType::CannotUseInstanceCompletion (__FILE__, __LINE__, "LocalCommandRequest::execute");
            break;
        case EC_BADARGS:
            failed = new acsdaemonErrType::BadArgumentsCompletion (__FILE__, __LINE__, "LocalCommandRequest::execute");
            break;
        case EC_NOPORT:
            failed = new acsdaemonErrType::PortInUseCompletion (__FILE__, __LINE__, "LocalCommandRequest::execute");
            break;
        case EC_TIMEOUT:
            failed = new acsdaemonErrType::RequestProcessingTimedOutCompletion (__FILE__, __LINE__, "LocalCommandRequest::execute");
            break;
        case EC_FAILURE:
        default:
            failed = new acsdaemonErrType::FailedToProcessRequestCompletion (__FILE__, __LINE__, "LocalCommandRequest::execute");
        }
        comp = failed->returnCompletion(true);
    } else {
        comp = ok.returnCompletion(false);
    }
    if (callback != NULL) {
        try {
            callback->done(comp.in());
        } catch (CORBA::TIMEOUT timeout) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a 'done' callback call for local request!"));
        }
    }
}

/********************* DaemonRequestCallback ********************/

void DaemonRequestCallback::done(const ::ACSErr::Completion & comp) {
    ACSErr::CompletionImpl compi = comp;
//    if (!compi.isErrorFree()) compi.log();
    // this occurs right after remote service is started
    if (description.get()->callback != NULL) {
        try {
            description.get()->callback->working(description.get()->getServiceName(), description.get()->host, description.get()->instance_number, comp);
        } catch (CORBA::TIMEOUT timeout) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a 'working' callback call for daemon request!"));
        }
    }
    if (next == NULL) {
        if (description.get()->callback != NULL) {
            try {
                description.get()->callback->done(comp);
            } catch (CORBA::TIMEOUT timeout) {
                ACS_SHORT_LOG((LM_WARNING, "Failed to make a 'done' callback call for daemon request!"));
            }
        }
        delete builder;
        ACS_SHORT_LOG((LM_INFO, "Ended processing command requests!"));
    } else if (compi.isErrorFree() || builder->reuse_services) {
        builder->reqproc->addRequest(next);
    }
    // DaemonRequestCallback may deactivate itself now
    PortableServer::POA_var poa = this->_default_POA();
    PortableServer::ObjectId_var oid = poa->servant_to_id(this);
    poa->deactivate_object(oid.in()); // this will also dispose the callback object
}

void DaemonRequestCallback::working(const ::ACSErr::Completion & comp) {
    // this occurs just before remote service gets started
}

/*************************** DaemonRequest ****************************/

void DaemonRequest::abort() {
    if (description.get() != NULL && description.get()->callback != NULL) {
        acsdaemonErrType::ProcessingAbortedCompletion ok(__FILE__, __LINE__, "DaemonRequest::abort");
        ACSErr::Completion_var comp = ok.returnCompletion(false);
        try {
            description.get()->callback->working(description.get()->getServiceName(), description.get()->host, description.get()->instance_number, comp.in());
            description.get()->callback->done(comp.in());
        } catch (CORBA::TIMEOUT timeout) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a callback call for aborted daemon request!"));
        }
    }
}

void DaemonRequest::execute() {
    DaemonRequestCallback *callback = NULL;
    ACE_CString daemonRef = "corbaloc::";
    daemonRef = daemonRef + description->host + ":" + ACSPorts::getServicesDaemonPort().c_str() + "/" + ::acsdaemon::servicesDaemonServiceName;
    ACS_SHORT_LOG((LM_INFO, "Using local Services Daemon reference: '%s'", daemonRef.c_str()));
    try
    {
        CORBA::Object_var obj = builder->orb->string_to_object(daemonRef.c_str());
        if (CORBA::is_nil(obj.in()))
        {
            ACS_SHORT_LOG((LM_ERROR, "Failed to resolve reference '%s'.", daemonRef.c_str()));
            return;
        }

        acsdaemon::ServicesDaemon_var daemon = acsdaemon::ServicesDaemon::_narrow(obj.in());
        if (CORBA::is_nil(daemon.in()))
        {
            ACS_SHORT_LOG((LM_ERROR, "Failed to narrow reference '%s'.", daemonRef.c_str()));
            return;
        }

        ACS_SHORT_LOG((LM_INFO, "Requesting another daemon to startup '%s'.", acsServiceNames[service]));

/*        if (description.get()->callback != NULL)
        {
            ACSErrTypeOK::ACSErrOKCompletion ok;
            ACSErr::Completion_var comp = ok.returnCompletion(false);
            try {
                description.get()->callback->working(description.get()->getServiceName(), description.get()->host, description.get()->instance_number, comp.in());
            } catch (CORBA::TIMEOUT timeout) {
                ACS_SHORT_LOG((LM_INFO, "TIMEOUT!"));
            }
        }*/

        CommandRequestDescription *desc = description.get();
        callback = new DaemonRequestCallback(builder, next, description);
        if (description.get()->start)
        {
            switch (service) {
            case NAMING_SERVICE:
                daemon->start_naming_service(callback->ptr(), desc->instance_number);
                break;
            case NOTIFICATION_SERVICE:
                daemon->start_notification_service(desc->name, callback->ptr(), desc->instance_number);
                break;
            case CDB:
                daemon->start_xml_cdb(callback->ptr(), desc->instance_number, desc->recovery, desc->xmlcdbdir);
                break;
            case MANAGER:
                daemon->start_manager(desc->domain, callback->ptr(), desc->instance_number, desc->recovery);
                break;
            case ACS_LOG:
                daemon->start_acs_log(callback->ptr(), desc->instance_number);
                break;
            case LOGGING_SERVICE:
                daemon->start_logging_service(desc->name, callback->ptr(), desc->instance_number);
                break;
            case INTERFACE_REPOSITORY:
                daemon->start_interface_repository(desc->loadir, desc->wait, callback->ptr(), desc->instance_number);
                break;
            case UNKNOWN:
                ACS_SHORT_LOG((LM_ERROR, "Trying to start unknown service!"));
            }
        }
        else
        {
            switch (service) {
            case NAMING_SERVICE:
                daemon->stop_naming_service(callback->ptr(), desc->instance_number);
                break;
            case NOTIFICATION_SERVICE:
                daemon->stop_notification_service(desc->name, callback->ptr(), desc->instance_number);
                break;
            case CDB:
                daemon->stop_cdb(callback->ptr(), desc->instance_number);
                break;
            case MANAGER:
                daemon->stop_manager(desc->domain, callback->ptr(), desc->instance_number);
                break;
            case ACS_LOG:
                daemon->stop_acs_log(callback->ptr(), desc->instance_number);
                break;
            case LOGGING_SERVICE:
                daemon->stop_logging_service(desc->name, callback->ptr(), desc->instance_number);
                break;
            case INTERFACE_REPOSITORY:
                daemon->stop_interface_repository(callback->ptr(), desc->instance_number);
                break;
            case UNKNOWN:
                ACS_SHORT_LOG((LM_ERROR, "Trying to stop unknown service!"));
            }
        }
    }
    catch( maciErrType::NoPermissionEx &ex )
    {
        ACS_SHORT_LOG((LM_WARNING, "Daemon is running in protected mode and cannot be shut down remotely!\n"));
    }
    catch( CORBA::Exception &ex )
    {
        ACS_SHORT_LOG((LM_ERROR, "Failed."));
        ACE_PRINT_EXCEPTION (ex, ACE_TEXT ("Caught unexpected exception:"));
    }
    // if callback didn't get activated, this should dispose it
    // otherwise it will get disposed on deactivation.
    if (callback != NULL) callback->_remove_ref();
}

/************************ RequestChainBuilder *************************/

void RequestChainBuilder::addRequest(const char *service, const char **atts) {
    int i = 0;
    if (instance_number == -1 && strcasecmp(service, "acs_services_definition") == 0) {
        while (atts[i] != NULL) {
            if (strcasecmp(atts[i], "instance") == 0) instance_number = (short)atoi(atts[i+1]);
            i += 2;
        }
        return;
    }
    if (instance_number == -1) {
        instance_number = 0;
        ACS_SHORT_LOG((LM_WARNING, "Instance number has not been provided with the root node! Using 0!\n"));
    }
    CommandRequestDescription *desc = new CommandRequestDescription(service, atts, instance_number, start, callback);
    DaemonRequest *req = new DaemonRequest(this, desc);
    if (start) {
        if (head == NULL) head = req;
        if (tail != NULL) tail->next = req;
        tail = req;
    } else { // by default, service are stopped in reverse order!
        if (tail == NULL) tail = req;
        if (head != NULL) req->next = head;
        head = req;
    }
}

void RequestChainBuilder::startProcessing() {
    if (head != NULL) reqproc->addRequest(head);
}
