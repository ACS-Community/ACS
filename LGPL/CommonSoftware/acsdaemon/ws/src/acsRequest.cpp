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
* "@$Id: acsRequest.cpp,v 1.20 2012/05/15 09:06:34 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* azagar   2008-08-12 created
*/

#include "acsRequest.h"
#include "acsServiceController.h"
#include <acsQoS.h>
#include <acsdaemonErrType.h>
#include <ACSErrTypeOK.h>
#include <ACSErrTypeCORBA.h>

#include <ace/Auto_Ptr.h>

/******************************* Helper methods *******************************/

#define STRDUP(a) (a == NULL ? NULL : strdup(a))
#define UNNULL(a) (a == NULL ? "" : a)

ACSServiceType acsServiceXMLTagToEnum(const char *service) {
    int i = 0;
    while (acsServices[i].xmltag != NULL) {
        if (strcasecmp(service, acsServices[i].xmltag) == 0) return (ACSServiceType)i;
        i++;
    }
    ACS_SHORT_LOG((LM_ERROR, "Unknown service '%s'!", service));
    return UNKNOWN;
}

/*************************** RequestProcessorThread ***************************/

RequestProcessorThread::RequestProcessorThread(const ACE_CString &name, const ACS::TimeInterval& responseTime, const ACS::TimeInterval& sleepTime) :
       ACS::Thread(name, responseTime, sleepTime, false, THR_DETACHED) {
    ACS_TRACE("RequestProcessorThread::RequestProcessorThread"); 
    m_mutex = new ACE_Thread_Mutex();
    m_wait = new ACE_Condition<ACE_Thread_Mutex>(*m_mutex);
}

RequestProcessorThread::~RequestProcessorThread() { 
    ACS_TRACE("RequestProcessorThread::~RequestProcessorThread");
    delete m_wait;
    delete m_mutex; 
}

void RequestProcessorThread::onStart() {
    running = true;
}

void RequestProcessorThread::stop() {
    Request *nowreq;
    running = false;
    m_mutex->acquire();
    while (!pending.empty()) {
        nowreq = pending.front();
        pending.pop();
        nowreq->abort();
        delete nowreq;
    }
    m_mutex->release();
    m_wait->signal();
}

void RequestProcessorThread::exit() {
    ACS::Thread::exit();
    stop();
}

void RequestProcessorThread::runLoop() ACE_THROW_SPEC ((CORBA::SystemException, ::ACSErrTypeCommon::BadParameterEx)) {
    Request *nowreq;
    while (running) {
        m_mutex->acquire();
        if (pending.empty()) m_wait->wait();
        if (!running) { m_mutex->release(); break; }
        nowreq = pending.front();
        pending.pop();
        m_mutex->release();
        if (nowreq->execute()) delete nowreq;
    }
}

void RequestProcessorThread::process(Request* r) {
    if (r == NULL) return;
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

/******************************* AsyncRequestThreadPool *******************************/

int AsyncRequestThreadPool::conf_threads = 1;
ACE_Thread_Mutex AsyncRequestThreadPool::mutex_;
ACE_CString AsyncRequestThreadPool::process_name;
LoggingProxy* AsyncRequestThreadPool::logger = 0;


AsyncRequestThreadPool* AsyncRequestThreadPool::instance_ = 0;

class AsyncRequestRequest : public ACE_Method_Request, public Logging::Loggable
{
public:
	AsyncRequestRequest (ACSServiceRequest* request, RequestChainContext<ACSServiceRequest> *context) :
		Logging::Loggable("AsyncRequestRequest"),
		m_request(request),
		m_context(context)
    {
        ACS_TRACE ("ActivationMethod::ActivationMethod");
    }

    virtual int call (void)
    {
        ACS_TRACE ("AsyncRequestRequest::call");
        m_request->process(m_context);
        return 0;
    }

private:
    ACSServiceRequest* m_request;
    RequestChainContext<ACSServiceRequest>* m_context;
};//class AsyncRequestRequest

class ExitMethod : public ACE_Method_Request
{
public:
    virtual int call (void)
    {
        // Cause exit.
        return -1;
    }
};//class ExitMethod


AsyncRequestThreadPool::AsyncRequestThreadPool (int n_threads) :
    Logging::Loggable("AsyncRequestThreadPool"),
    m_threads(n_threads)
{
    ACS_TRACE ("AsyncRequestThreadPool::AsyncRequestThreadPool");
    this->activate (THR_NEW_LWP|THR_JOINABLE|THR_INHERIT_SCHED, n_threads);
}

int
AsyncRequestThreadPool::svc (void)
{
    if (logger)
    {
      // in some threads the logging will be initialized two times what is not harmful
      LoggingProxy::init(logger);
      LoggingProxy::ProcessName(process_name.c_str());
    }

    ACS_TRACE ("AsyncRequestThreadPool::svc");

    while (1)
    {
        // Dequeue the next method object
        auto_ptr<ACE_Method_Request>
        request (this->activation_queue_.dequeue ());

        // Invoke the method request.
        if (request->call () == -1)
            break;
    }

    return 0;
}

int
AsyncRequestThreadPool::enqueue (ACE_Method_Request *request)
{
    ACS_TRACE ("AsyncRequestThreadPool::enqueue");
    return this->activation_queue_.enqueue (request);
}

void AsyncRequestThreadPool::shutdown()
{
	for (int i = 0; i < m_threads; i++)
    	this->enqueue (new ExitMethod ());

    // might do better implementation
    while (!this->activation_queue_.is_empty()) {
        ACE_OS::sleep(1);
    }
}

/******************************* ChainedRequest *******************************/

template <class R> void ChainedRequest<R>::process(RequestChainContext<R> *icontext) {
    context = icontext;
    context->getRequestProcessor()->process(this);
}

template <class R> void ChainedRequest<R>::complete() {
    if (context != NULL) {
        context->proceed(static_cast<R*>(this));
        context = NULL;
    }
}

/***************************** RequestChainContext ****************************/

template <class R> void RequestChainContext<R>::proceed(R *curreq) {
	ACE_Guard<ACE_Thread_Mutex> guard(mutex);

    if (inprocess) {

    	if (hasAsync && curreq->isAsync())
    	{
			asyncStartInProgress.erase(curreq->getDescription()->getACSService());
			asyncToComplete--;
    	}
    	else
    	{
    		syncPending = false;
    	}

    	//printf("hasAsync %d, curreq->isAsync() %d, asyncToComplete %d name %s\n",
    	//		hasAsync, curreq->isAsync(), asyncToComplete, curreq->getDescription()->getACSServiceName());

        if (!requestDone(curreq)) {
			failed = true;

			if (hasAsync && curreq->isAsync() && syncPending)
			{
	    		// wait for other sync to complete...
				return;
			}
			else if (hasAsync && asyncToComplete > 0)
        	{
	    		// wait for other async to complete...
				return;
        	}
        	else
        	{
        		// no async pending, can stop...
				chainAborted();
				guard.release();
				delete this;
				return;
        	}
        }
    }

	if (requests.empty()) {
		if (hasAsync && curreq->isAsync() && syncPending)
		{
    		// wait for other sync to complete...
			return;
		}
		else if (hasAsync && asyncToComplete > 0)
    	{
    		// wait for other async to complete...
    		return;
    	}
    	else
    	{
    		if (failed)
    			chainAborted();
    		else
    			chainDone();
			guard.release();
			delete this;
			return;
    	}
    } else {

		if (hasAsync && curreq->isAsync() && syncPending)
		{
    		// wait for other sync to complete...
			return;
		}

    	while (!requests.empty())
    	{
            inprocess = true;
			curreq = requests.front();

			// wait for dependent aysnc-started service to complete
			if (hasAsync)
			{
				const ACSServiceType* deps = curreq->getDescription()->getDependentService();
				bool found = false;
				int ix = 0;
				while (deps[ix] != UNKNOWN && !found)
					if (asyncStartInProgress.find(deps[ix++]) != asyncStartInProgress.end())
						found = true;

				// dependent service found, do not continue
				if (found)
					break;
			}

			requests.pop_front();

			if (curreq->getRequestType() == START_SERVICE && curreq->isAsync())
			{
				hasAsync = true;
				asyncToComplete++;
				AsyncRequestThreadPool::getInstance()->enqueue(new AsyncRequestRequest(curreq, this));
				asyncStartInProgress.insert(curreq->getDescription()->getACSService());
			}
			else
			{
	    		syncPending = true;
				curreq->process(this);
				break;
			}
    	}
    }
}

/*********************** ACS SERVICES SPECIFIC REQUESTS ***********************/
/************************ ACSServiceRequestDescription ************************/

ACSServiceRequestDescription::ACSServiceRequestDescription(ACSServiceType iservice, int iinstance_number) : service(iservice), instance_number(iinstance_number), host(NULL), name(NULL), corbalocName(NULL), domain(NULL), cdbxmldir(NULL),
		loadir(false), wait(true), recovery(false), async(acsServices[service].async) {
}

ACSServiceRequestDescription::ACSServiceRequestDescription(const ACSServiceRequestDescription &desc) : service(desc.service), instance_number(desc.instance_number), host(STRDUP(desc.host)), name(STRDUP(desc.name)), corbalocName(STRDUP(desc.corbalocName)), domain(STRDUP(desc.domain)), cdbxmldir(STRDUP(desc.cdbxmldir)),
		loadir(desc.loadir), wait(desc.wait), recovery(desc.recovery), async(desc.async) {
}

ACSServiceRequestDescription::~ACSServiceRequestDescription() {
    if (host != NULL) free((void*)host);
    if (name != NULL) free((void*)name);
    if (corbalocName != NULL) free((void*)corbalocName);
    if (domain != NULL) free((void*)domain);
    if (cdbxmldir != NULL) free ((void*)cdbxmldir);
}

void ACSServiceRequestDescription::setFromXMLAttributes(const char **atts) {
    int i = 0;
    while (atts[i] != NULL) {
        if (strcasecmp(atts[i], "host") == 0) host = strdup(atts[i+1]);
        else if (strcasecmp(atts[i], "name") == 0) name = strdup(atts[i+1]);
        else if (strcasecmp(atts[i], "domain") == 0) domain = strdup(atts[i+1]);
        else if (strcasecmp(atts[i], "cdb_xml_dir") == 0) cdbxmldir = strdup(atts[i+1]);
        else if (strcasecmp(atts[i], "config_name") == 0) cdbxmldir = strdup(atts[i+1]);
        else if (strcasecmp(atts[i], "load") == 0) loadir = strcasecmp(atts[i+1], "true") == 0;
        else if (strcasecmp(atts[i], "wait_load") == 0) wait = strcasecmp(atts[i+1], "true") == 0;
        else if (strcasecmp(atts[i], "recovery") == 0) recovery = strcasecmp(atts[i+1], "true") == 0;
        else if (strcasecmp(atts[i], "parallel") == 0) async = strcasecmp(atts[i+1], "true") == 0;
        i += 2;
    }
}

ACE_CString ACSServiceRequestDescription::prepareCommand(ACSDaemonContext *context, ACSServiceRequestType request_type, bool log) {
	// TODO usage of snprintf
    char buffer[128];
    if (service == RDB_CDB)
        sprintf(buffer, "%s \"%s\" %s -b %d", acsServices[service].script, cdbxmldir, request_type == START_SERVICE ? "-k -s" : "-k", instance_number);
    else
    	sprintf(buffer, "%s %s -b %d", acsServices[service].script, request_type == START_SERVICE ? "-k -s" : "-k", instance_number);

    ACE_CString commandline = buffer;
    if (!loadir && service == INTERFACE_REPOSITORY) commandline = commandline + " -noloadIFR";
    if (wait && request_type == START_SERVICE) commandline = commandline + " -w";
    if (recovery && (service == CDB || service == MANAGER)) commandline = commandline + " -r";
    if (name != NULL && service == NOTIFICATION_SERVICE) commandline = commandline + " -n " + name;
    if (domain != NULL && service == LOGGING_SERVICE) {
        ACS_SHORT_LOG ((LM_WARNING, "Name parameter of Logging Service startup is not yet supported!"));
    }
    if (domain != NULL && service == MANAGER) {
        ACS_SHORT_LOG ((LM_WARNING, "Domain parameter of Manager startup script is not yet supported!"));
    }
    if (cdbxmldir != NULL && service == CDB) commandline = commandline + " -d \"" + cdbxmldir + "\"";
    if (service != NAMING_SERVICE && context->hasConfigurationReference(instance_number, acsServices[NAMING_SERVICE].xmltag)) {
        commandline += " -x ";
        commandline += context->getConfigurationReference(instance_number, acsServices[NAMING_SERVICE].xmltag).c_str();
    }
    if (log) {
        ACE_CString logDirectory="~/.acs/commandcenter/";
        char * acsdata = ACE_OS::getenv("ACSDATA");
        if(acsdata != NULL)
            logDirectory = ACE_CString(acsdata) + ACE_CString("/logs/");
        ACE_CString logs = logDirectory;
        char * host = ACE_OS::getenv("HOST");
        if(host != NULL)
            logDirectory = logDirectory + ACE_CString(host) + ACE_CString("/");

        std::string timeStamp(getStringifiedTimeStamp().c_str());

        if( timeStamp.find(":") != std::string::npos)
            timeStamp.replace(timeStamp.find(":"),1,".");
        if( timeStamp.find(":") != std::string::npos )
            timeStamp.replace(timeStamp.find(":"),1,".");
        if( timeStamp.find("T") != std::string::npos)
            timeStamp.replace(timeStamp.find("T"),1,"_");

        //create the directory
        ACE_OS::system(("mkdir -p " + logDirectory).c_str());
        ACE_OS::system(("chmod 775 " + logs).c_str());
        ACE_OS::system(("chmod 775 " + logDirectory).c_str());
        commandline = commandline + " &> " + logDirectory + acsServices[service].script + "_" + timeStamp.c_str();
    }
    return commandline;
}

ACSErr::Completion_var ACSServiceRequestDescription::executeLocal(ACSDaemonContext *context, ACSServiceRequestType request_type) {
    /* sinchronously executes a system command on the local machine */
    ACE_CString command = prepareCommand(context, request_type, true);
    ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", command.c_str()));
    int result = ACE_OS::system(command.c_str()) >> 8;
    if (result != EC_OK) {
        ACS_SHORT_LOG ((LM_INFO, "Result is: '%d'.", result));
        ACSErr::CompletionImpl *failed;
        switch (result) {
        case EC_CANNOTCREATE:
            failed = new acsdaemonErrType::CannotCreateInstanceCompletion (__FILE__, __LINE__, "ACSServiceRequestDescription::executeLocal");
            break;
        case EC_CANNOTUSE:
            failed = new acsdaemonErrType::CannotUseInstanceCompletion (__FILE__, __LINE__, "ACSServiceRequestDescription::executeLocal");
            break;
        case EC_BADARGS:
            failed = new acsdaemonErrType::BadArgumentsCompletion (__FILE__, __LINE__, "ACSServiceRequestDescription::executeLocal");
            break;
        case EC_NOPORT:
            failed = new acsdaemonErrType::PortInUseCompletion (__FILE__, __LINE__, "ACSServiceRequestDescription::executeLocal");
            break;
        case EC_TIMEOUT:
            failed = new acsdaemonErrType::RequestProcessingTimedOutCompletion (__FILE__, __LINE__, "ACSServiceRequestDescription::executeLocal");
            break;
        case EC_FAILURE:
        default:
            failed = new acsdaemonErrType::FailedToProcessRequestCompletion (__FILE__, __LINE__, "ACSServiceRequestDescription::executeLocal");
        }
        return failed->returnCompletion(true);
    }
    ACSErrTypeOK::ACSErrOKCompletion ok;
    return ok.returnCompletion(false);
}

ACSErr::Completion_var ACSServiceRequestDescription::executeRemote(ACSDaemonContext *context, ACSServiceRequestType request_type, CORBA::ORB_ptr orb, acsdaemon::DaemonCallback_ptr cbptr, const char *corbaloc) {
    /* sinchronously or asinchronously propagates the request to another host */
    ACS_SHORT_LOG((LM_INFO, "Using Corba reference: '%s'", corbaloc));
    try {
        CORBA::Object_var obj = orb->string_to_object(corbaloc);
        if (CORBA::is_nil(obj.in())) {
            ACS_SHORT_LOG((LM_ERROR, "Failed to resolve reference '%s'.", corbaloc));
            ACSErrTypeCORBA::FailedToResolveServiceCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
            return failed.returnCompletion(false);
        }
        ACS_SHORT_LOG((LM_INFO, "Requesting to remotely %s '%s'.", request_type == START_SERVICE ? "start" : "stop", acsServices[service].xmltag));
        switch (service) {
        case NAMING_SERVICE: {
            acsdaemon::NamingServiceSpell_var spell = acsdaemon::NamingServiceSpell::_narrow(obj.in());
            if (CORBA::is_nil(spell.in())) {
                ACS_SHORT_LOG((LM_ERROR, "Failed to narrow reference '%s'.", corbaloc));
                acsdaemonErrType::FailedToProcessRequestCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
                return failed.returnCompletion(false);
            }
            spell = acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, spell.in());
            switch (request_type) {
            case START_SERVICE: spell->start_naming_service(cbptr, instance_number); break;
            case STOP_SERVICE: spell->stop_naming_service(cbptr, instance_number); break;
            }
            break;
        } case ALARM_SERVICE: {
            acsdaemon::AlarmServiceSpell_var spell = acsdaemon::AlarmServiceSpell::_narrow(obj.in());
            if (CORBA::is_nil(spell.in())) {
                ACS_SHORT_LOG((LM_ERROR, "Failed to narrow reference '%s'.", corbaloc));
                acsdaemonErrType::FailedToProcessRequestCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
                return failed.returnCompletion(false);
            }
            spell = acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, spell.in());
            switch (request_type) {
            case START_SERVICE: spell->start_alarm_service(cbptr, instance_number); break;
            case STOP_SERVICE: spell->stop_alarm_service(cbptr, instance_number); break;
            }
            break;
        } case NOTIFICATION_SERVICE: {
            acsdaemon::NotificationServiceSpell_var spell = acsdaemon::NotificationServiceSpell::_narrow(obj.in());
            if (CORBA::is_nil(spell.in())) {
                ACS_SHORT_LOG((LM_ERROR, "Failed to narrow reference '%s'.", corbaloc));
                acsdaemonErrType::FailedToProcessRequestCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
                return failed.returnCompletion(false);
            }
            spell = acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, spell.in());
            switch (request_type) {
            case START_SERVICE: spell->start_notification_service(UNNULL(name), cbptr, instance_number); break;
            case STOP_SERVICE: spell->stop_notification_service(UNNULL(name), cbptr, instance_number); break;
            }
            break;
        } case CDB: {
            acsdaemon::CDBSpell_var spell = acsdaemon::CDBSpell::_narrow(obj.in());
            if (CORBA::is_nil(spell.in())) {
                ACS_SHORT_LOG((LM_ERROR, "Failed to narrow reference '%s'.", corbaloc));
                acsdaemonErrType::FailedToProcessRequestCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
                return failed.returnCompletion(false);
            }
            spell = acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, spell.in());
            switch (request_type) {
            case START_SERVICE: spell->start_xml_cdb(cbptr, instance_number, recovery, UNNULL(cdbxmldir)); break;
            case STOP_SERVICE: spell->stop_cdb(cbptr, instance_number); break;
            }
            break;
        } case RDB_CDB: {
            acsdaemon::CDBSpell_var spell = acsdaemon::CDBSpell::_narrow(obj.in());
            if (CORBA::is_nil(spell.in())) {
                ACS_SHORT_LOG((LM_ERROR, "Failed to narrow reference '%s'.", corbaloc));
                acsdaemonErrType::FailedToProcessRequestCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
                return failed.returnCompletion(false);
            }
            spell = acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, spell.in());
            switch (request_type) {
            case START_SERVICE: spell->start_rdb_cdb(cbptr, instance_number, recovery, UNNULL(cdbxmldir)); break;
            case STOP_SERVICE: spell->stop_cdb(cbptr, instance_number); break;
            }
            break;
        } case MANAGER: {
            acsdaemon::ManagerSpell_var spell = acsdaemon::ManagerSpell::_narrow(obj.in());
            if (CORBA::is_nil(spell.in())) {
                ACS_SHORT_LOG((LM_ERROR, "Failed to narrow reference '%s'.", corbaloc));
                acsdaemonErrType::FailedToProcessRequestCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
                return failed.returnCompletion(false);
            }
            spell = acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, spell.in());
            switch (request_type) {
            case START_SERVICE: spell->start_manager(UNNULL(domain), cbptr, instance_number, recovery); break;
            case STOP_SERVICE: spell->stop_manager(UNNULL(domain), cbptr, instance_number); break;
            }
            break;
        } case ACS_LOG_SERVICE: {
            acsdaemon::ACSLogSpell_var spell = acsdaemon::ACSLogSpell::_narrow(obj.in());
            if (CORBA::is_nil(spell.in())) {
                ACS_SHORT_LOG((LM_ERROR, "Failed to narrow reference '%s'.", corbaloc));
                acsdaemonErrType::FailedToProcessRequestCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
                return failed.returnCompletion(false);
            }
            spell = acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, spell.in());
            switch (request_type) {
            case START_SERVICE: spell->start_acs_log(cbptr, instance_number); break;
            case STOP_SERVICE: spell->stop_acs_log(cbptr, instance_number); break;
            }
            break;
        } case LOGGING_SERVICE: {
            acsdaemon::LoggingServiceSpell_var spell = acsdaemon::LoggingServiceSpell::_narrow(obj.in());
            if (CORBA::is_nil(spell.in())) {
                ACS_SHORT_LOG((LM_ERROR, "Failed to narrow reference '%s'.", corbaloc));
                acsdaemonErrType::FailedToProcessRequestCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
                return failed.returnCompletion(false);
            }
            spell = acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, spell.in());
            switch (request_type) {
            case START_SERVICE: spell->start_logging_service(UNNULL(name), cbptr, instance_number); break;
            case STOP_SERVICE: spell->stop_logging_service(UNNULL(name), cbptr, instance_number); break;
            }
            break;
        } case INTERFACE_REPOSITORY: {
            acsdaemon::InterfaceRepositorySpell_var spell = acsdaemon::InterfaceRepositorySpell::_narrow(obj.in());
            if (CORBA::is_nil(spell.in())) {
                ACS_SHORT_LOG((LM_ERROR, "Failed to narrow reference '%s'.", corbaloc));
                acsdaemonErrType::FailedToProcessRequestCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
                return failed.returnCompletion(false);
            }
            spell = acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, spell.in());
            switch (request_type) {
            case START_SERVICE: spell->start_interface_repository(loadir, wait, cbptr, instance_number); break;
            case STOP_SERVICE: spell->stop_interface_repository(cbptr, instance_number); break;
            }
            break;
        } case UNKNOWN: {
            ACS_SHORT_LOG((LM_ERROR, "Trying to start unknown service!"));
            acsdaemonErrType::FailedToProcessRequestCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
            return failed.returnCompletion(false);
        }}
    } catch (CORBA::TIMEOUT timeout) {
        ACS_SHORT_LOG((LM_ERROR, "Object with reference '%s' did not respond in time.", corbaloc));
        acsdaemonErrType::FailedToProcessRequestCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
        return failed.returnCompletion(false);
    } catch(CORBA::OBJECT_NOT_EXIST &ex) {
        ACS_SHORT_LOG((LM_ERROR, "Server is already running but object with reference '%s' does not exist.", corbaloc));
        acsdaemonErrType::FailedToProcessRequestCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
        return failed.returnCompletion(false);
    } catch(CORBA::TRANSIENT &ex) {
        ACS_SHORT_LOG((LM_INFO, "Server not running!"));
        acsdaemonErrType::FailedToProcessRequestCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
        return failed.returnCompletion(false);
    } catch(acsdaemonErrType::ServiceAlreadyRunningEx) {
        ACS_SHORT_LOG((LM_INFO, "Failed to start service that was already running!"));
        acsdaemonErrType::ServiceAlreadyRunningCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
        return failed.returnCompletion(false);
    } catch(acsdaemonErrType::ServiceNotRunningEx) {
        ACS_SHORT_LOG((LM_INFO, "Failed to stop service that had not even been running!"));
        acsdaemonErrType::ServiceNotRunningCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
        return failed.returnCompletion(false);
    } catch(CORBA::Exception &ex) {
        ACS_SHORT_LOG((LM_ERROR, "Failed '%s'.", corbaloc));
        ACE_PRINT_EXCEPTION (ex, ACE_TEXT ("Caught unexpected exception:"));
        acsdaemonErrType::FailedToProcessRequestCompletion failed(__FILE__, __LINE__, "ACSServiceRequestDescription::executeRemote");
        return failed.returnCompletion(false);
    }
    ACSErrTypeOK::ACSErrOKCompletion ok;
    return ok.returnCompletion(false);
}

/****************************** ACSServiceRequest *****************************/

ACSServiceRequest::ACSServiceRequest(ACSDaemonContext *icontext, ACSServiceRequestTarget itarget, ACSServiceRequestType itype, ACSServiceRequestDescription *idesc, acsdaemon::DaemonCallback_ptr icallback) : context(icontext), target(itarget), request_type(itype), desc(idesc), completion(NULL), cbvar(NULL) {
    callback = icallback == NULL ? NULL : acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, icallback);
}

ACSServiceRequest::~ACSServiceRequest() {
    ACS_SHORT_LOG((LM_DEBUG, "DESTROYING '%s' ACSServiceRequest!", target == LOCAL ? "local" : target == IMP ? "imp" : "daemon"));
    delete desc;
}

acsdaemon::DaemonCallback_ptr ACSServiceRequest::cbptr() {
    if (cbvar == NULL) {
        cbvar = this->_this();
        this->_remove_ref();
    }
    return cbvar.in();
}

void ACSServiceRequest::release() {
    if (cbvar != NULL) {
        PortableServer::POA_var poa = this->_default_POA();
        PortableServer::ObjectId_var oid = poa->servant_to_id(this);
        poa->deactivate_object(oid.in()); // this will also dispose the object itself
    }
}

void ACSServiceRequest::complete() {
    if (callback != NULL) {
        try {
            callback->done(*completion);
//        } catch (CORBA::TIMEOUT timeout) {
        } catch (CORBA::Exception &ex) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a callback call for finished ACS service request!"));
        }
    }
    ChainedRequest<ACSServiceRequest>::complete();
    release();
}

void ACSServiceRequest::abort() {
    acsdaemonErrType::ProcessingAbortedCompletion aborted(__FILE__, __LINE__, "ACSServiceRequest::abort");
    ACSErr::Completion_var comp = aborted.returnCompletion(false);
    if (callback != NULL) {
        try {
            callback->done(comp.in());
//        } catch (CORBA::TIMEOUT timeout) {
        } catch (CORBA::Exception &ex) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a callback call for aborted chained ACS service request!"));
        }
    }
    completion = comp;
    complete();
}

bool ACSServiceRequest::execute() {
    ACE_CString corbaloc;
    ACSErr::Completion_var comp;
    switch (target) {
    case IMP:
        corbaloc = ACE_CString("corbaloc::") + desc->getHost() + ":" + acsServices[desc->getACSService()].impport + "/" + acsServices[desc->getACSService()].imptype;
        break;
    case DAEMON:
        corbaloc = ACE_CString("corbaloc::") + desc->getHost() + ":" + ACSPorts::getServicesDaemonPort().c_str() + "/" + acsdaemon::servicesDaemonServiceName;
        break;
    case LOCAL:
        if (callback != NULL) {
            try {
                ACSErrTypeOK::ACSErrOKCompletion ok;
                comp = ok.returnCompletion(false);
                callback->working(comp.in());
//            } catch (CORBA::TIMEOUT timeout) {
            } catch (CORBA::Exception &ex) {
                ACS_SHORT_LOG((LM_WARNING, "Failed to make a 'working' callback call for local request!"));
            }
        }
        comp = desc->executeLocal(context, request_type);
        completion = comp;
        complete();
        return true;
    }
    comp = desc->executeRemote(context, request_type, context->getORB(), cbptr(), corbaloc.c_str());
    if (comp->previousError.length() == 0) {
        // wait for callback to return and release callback in case it failed to return in time (?)
        return false;
    }
    completion = comp;
    complete();
    return false;
}

void ACSServiceRequest::done(const ACSErr::Completion &comp) {
    // this occurs right after remote service is started
    completion = &comp;
    complete();
}

void ACSServiceRequest::working(const ACSErr::Completion &comp) {
    if (callback != NULL) {
        try {
            callback->working(comp);
//        } catch (CORBA::TIMEOUT timeout) {
        } catch (CORBA::Exception &ex) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a callback call for ACS service request!"));
        }
    }
}

/************************ ACSServiceRequestChainContext ***********************/

ACSServiceRequestChainContext::ACSServiceRequestChainContext(ACSDaemonContext *icontext, ACSServiceRequestType itype, bool ireuse_services, acsdaemon::DaemonSequenceCallback_ptr icallback) : RequestChainContext<ACSServiceRequest>(icontext->getRequestProcessor()), context(icontext), request_type(itype), reuse_services(ireuse_services), instance_number(-1) {
    callback = icallback == NULL ? NULL : acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, icallback);
}

ACSServiceRequestChainContext::~ACSServiceRequestChainContext() {
    ACS_SHORT_LOG((LM_DEBUG, "DESTROYING ACSServiceRequestChainContext!"));
}

bool ACSServiceRequestChainContext::requestDone(ACSServiceRequest *request) {
    if (callback != NULL) {
        try {
            callback->working(request->getACSServiceName(), request->getHost(), request->getInstanceNumber(), *request->getCompletion());
//        } catch (CORBA::TIMEOUT timeout) {
        } catch (CORBA::Exception &ex) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a 'working' callback call for ACS service request chain!"));
        }
    }
    return request->isErrorFree() || reuse_services;
}

void ACSServiceRequestChainContext::chainDone() {
    if (callback != NULL) {
        try {
            ACSErrTypeOK::ACSErrOKCompletion ok;
            ACSErr::Completion_var comp = ok.returnCompletion(false);
            callback->done(comp.in());
//        } catch (CORBA::TIMEOUT timeout) {
        } catch (CORBA::Exception &ex) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a 'done' callback call for ACS service request chain!"));
        }
    }
    ACS_SHORT_LOG((LM_INFO, "Ended processing command requests!"));
}

void ACSServiceRequestChainContext::chainAborted() {
    if (callback != NULL) {
        try {
            acsdaemonErrType::ProcessingAbortedCompletion aborted(__FILE__, __LINE__, "ACSServiceRequest::abort");
            ACSErr::Completion_var comp = aborted.returnCompletion(false);
            callback->done(comp.in());
//        } catch (CORBA::TIMEOUT timeout) {
        } catch (CORBA::Exception &ex) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a 'done' callback call for ACS service request chain!"));
        }
    }
    ACS_SHORT_LOG((LM_INFO, "Aborted processing command requests!"));
}

void ACSServiceRequestChainContext::addRequest(const char *iservice, const char **atts) {
    int i = 0;
    if (instance_number == -1 && strcasecmp(iservice, "acs_services_definition") == 0) {
        while (atts[i] != NULL) {
            if (strcasecmp(atts[i], "instance") == 0) {
                instance_number = (short)atoi(atts[i+1]);
                if (instance_number < 0 || instance_number >= ACS_SERVICE_INSTANCES) {
                    instance_number = 0;
                    ACS_SHORT_LOG((LM_WARNING, "Instance number should be between 0 and 9! Using 0!\n"));
                }
                return;
            }
            i += 2;
        }
        return;
    }
    if (instance_number == -1) {
        instance_number = 0;
        ACS_SHORT_LOG((LM_WARNING, "Instance number has not been provided with the root node! Using 0!\n"));
    }
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(acsServiceXMLTagToEnum(iservice), instance_number);
    desc->setFromXMLAttributes(atts);
    ACSServiceRequest *request = new ACSServiceRequest(context, DAEMON, request_type, desc);
    switch (request_type) {
    case START_SERVICE: appendRequestOrdered(request); break;
    case STOP_SERVICE: prependRequestOrdered(request); break;
    }
}

