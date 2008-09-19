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
* "@$Id: acsCommandRequest.cpp,v 1.4 2008/09/19 13:06:44 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* azagar   2008-08-12 created
*/

#include "acsCommandRequest.h"
#include <acsdaemonErrType.h>
#include <ACSErrTypeOK.h>
#include <acsutilPorts.h>

/**************************** LocalRequest ****************************/

void LocalRequest::abort() {
    if (callback != NULL) {
        acsdaemonErrType::ProcessingAbortedCompletion ok(__FILE__, __LINE__, "LocalRequest::abort");
        ACSErr::Completion_var comp = ok.returnCompletion(false);
        try {
            callback->done(comp.in());
        } catch (CORBA::TIMEOUT timeout) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a callback call for aborted local request!"));
        }
    }
}

void LocalRequest::execute() {
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
            failed = new acsdaemonErrType::CannotCreateInstanceCompletion (__FILE__, __LINE__, "LocalRequest::execute");
            break;
        case EC_CANNOTUSE:
            failed = new acsdaemonErrType::CannotUseInstanceCompletion (__FILE__, __LINE__, "LocalRequest::execute");
            break;
        case EC_BADARGS:
            failed = new acsdaemonErrType::BadArgumentsCompletion (__FILE__, __LINE__, "LocalRequest::execute");
            break;
        case EC_NOPORT:
            failed = new acsdaemonErrType::PortInUseCompletion (__FILE__, __LINE__, "LocalRequest::execute");
            break;
        case EC_TIMEOUT:
            failed = new acsdaemonErrType::RequestProcessingTimedOutCompletion (__FILE__, __LINE__, "LocalRequest::execute");
            break;
        case EC_FAILURE:
        default:
            failed = new acsdaemonErrType::FailedToProcessRequestCompletion (__FILE__, __LINE__, "LocalRequest::execute");
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

/*********************** CommandProcessorThread ***********************/

void CommandProcessorThread::onStart()
{
    running = true;
}

void CommandProcessorThread::stop()
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

void CommandProcessorThread::runLoop()
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

void CommandProcessorThread::addRequest(Request* r)
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

/********************* RemoteRequestDaemonCallback ********************/

void RemoteRequestDaemonCallback::done(const ::ACSErr::Completion & comp) {
    ACSErr::CompletionImpl compi = comp;
//    if (!compi.isErrorFree()) compi.log();
    // this occurs right after remote service is started
    if (builder->callback != NULL) {
        try {
            builder->callback->working(service, host, builder->instance_number, comp);
        } catch (CORBA::TIMEOUT timeout) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a 'working' callback call for remote request!"));
        }
    }
    if (next == NULL) {
        if (builder->callback != NULL) {
            try {
                builder->callback->done(comp);
            } catch (CORBA::TIMEOUT timeout) {
                ACS_SHORT_LOG((LM_WARNING, "Failed to make a 'done' callback call for remote request!"));
            }
        }
        delete builder;
        ACS_SHORT_LOG((LM_INFO, "Ended processing command requests!"));
    } else if (compi.isErrorFree() || builder->reuse_services) {
        builder->cmdproc->addRequest(next);
    }
    // RemoteRequestDaemonCallback may deactivate itself now
    PortableServer::POA_var poa = this->_default_POA();
    PortableServer::ObjectId_var oid = poa->servant_to_id(this);
    poa->deactivate_object(oid.in()); // this will also dispose the callback object
}

void RemoteRequestDaemonCallback::working(const ::ACSErr::Completion & comp) {
    // this occurs just before remote service gets started
}

/*************************** RemoteRequest ****************************/

void RemoteRequest::abort() {
    if (builder->callback != NULL) {
        acsdaemonErrType::ProcessingAbortedCompletion ok(__FILE__, __LINE__, "RemoteRequest::abort");
        ACSErr::Completion_var comp = ok.returnCompletion(false);
        try {
            builder->callback->working(service, host, builder->instance_number, comp.in());
            builder->callback->done(comp.in());
        } catch (CORBA::TIMEOUT timeout) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a callback call for aborted remote request!"));
        }
    }
}

void RemoteRequest::execute() {
    RemoteRequestDaemonCallback *callback = NULL;
    ACE_CString daemonRef = "corbaloc::";
    daemonRef = daemonRef + host + ":" + ACSPorts::getServicesDaemonPort().c_str() + "/" + ::acsdaemon::servicesDaemonServiceName;
    ACS_SHORT_LOG((LM_INFO, "Using local Services Daemon reference: '%s'", daemonRef.c_str()));
    try
    {
        CORBA::Object_var obj = builder->orb->string_to_object(daemonRef.c_str());
        if (CORBA::is_nil(obj.in()))
        {
            ACS_SHORT_LOG((LM_ERROR, "Failed to resolve reference '%s'.", daemonRef.c_str()));
            return;
        }

        acsdaemon::ServicesDaemon_ACS80_var daemon = acsdaemon::ServicesDaemon_ACS80::_narrow(obj.in());
        if (CORBA::is_nil(daemon.in()))
        {
            ACS_SHORT_LOG((LM_ERROR, "Failed to narrow reference '%s'.", daemonRef.c_str()));
            return;
        }

        ACS_SHORT_LOG((LM_INFO, "Requesting a remote startup of '%s'.", service));

/*        if (builder->callback != NULL)
        {
            acsdaemonErrType::AcsStartServicesCompletion ok(__FILE__, __LINE__, "RemoteRequest::execute");
            ACSErr::Completion_var comp = ok.returnCompletion(false);
            try {
                builder->callback->working(service, host, builder->instance_number, comp.in());
            } catch (CORBA::TIMEOUT timeout) {
                ACS_SHORT_LOG((LM_INFO, "TIMEOUT!"));
            }
        }*/

        callback = new RemoteRequestDaemonCallback(builder, this);
        if (builder->start)
        {
            if (strcasecmp(service, "naming_service") == 0)
                daemon->start_naming_service(callback->ptr(), builder->instance_number);
            else if (strcasecmp(service, "notification_service") == 0)
                daemon->start_notification_service(name, callback->ptr(), builder->instance_number);
            else if (strcasecmp(service, "cdb") == 0)
                daemon->start_xml_cdb(callback->ptr(), builder->instance_number, false, 0);	// !!! TODO @todo
            else if (strcasecmp(service, "manager") == 0)
                daemon->start_manager(domain, callback->ptr(), builder->instance_number, false); /// !!! TODO @todo
            else if (strcasecmp(service, "acs_log") == 0)
                daemon->start_acs_log(callback->ptr(), builder->instance_number);
            else if (strcasecmp(service, "logging_service") == 0)
                daemon->start_logging_service(name, callback->ptr(), builder->instance_number);
            else if (strcasecmp(service, "interface_repository") == 0)
                daemon->start_interface_repository(loadir, wait, callback->ptr(), builder->instance_number);
            else
                ACS_SHORT_LOG((LM_ERROR, "Unknown service '%s'!"));
        }
        else
        {
            if (strcasecmp(service, "naming_service") == 0)
                daemon->stop_naming_service(callback->ptr(), builder->instance_number);
            else if (strcasecmp(service, "notification_service") == 0)
                daemon->stop_notification_service(name, callback->ptr(), builder->instance_number);
            else if (strcasecmp(service, "cdb") == 0)
                daemon->stop_cdb(callback->ptr(), builder->instance_number);
            else if (strcasecmp(service, "manager") == 0)
                daemon->stop_manager(domain, callback->ptr(), builder->instance_number);
            else if (strcasecmp(service, "acs_log") == 0)
                daemon->stop_acs_log(callback->ptr(), builder->instance_number);
            else if (strcasecmp(service, "logging_service") == 0)
                daemon->stop_logging_service(name, callback->ptr(), builder->instance_number);
            else if (strcasecmp(service, "interface_repository") == 0)
                daemon->stop_interface_repository(callback->ptr(), builder->instance_number);
            else
                ACS_SHORT_LOG((LM_ERROR, "Unknown service '%s'!"));
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
    RemoteRequest *req = new RemoteRequest(this, strdup(service));
    while (atts[i] != NULL) {
        if (strcasecmp(atts[i], "host") == 0) req->host = strdup(atts[i+1]);
        else if (strcasecmp(atts[i], "name") == 0) req->name = strdup(atts[i+1]);
        else if (strcasecmp(atts[i], "domain") == 0) req->domain = strdup(atts[i+1]);
        else if (strcasecmp(atts[i], "load") == 0) req->loadir = strcasecmp(atts[i+1], "true") == 0;
        else if (strcasecmp(atts[i], "wait_load") == 0) req->wait = strcasecmp(atts[i+1], "true") == 0;
        i += 2;
    }
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
    if (head != NULL) cmdproc->addRequest(head);
}
