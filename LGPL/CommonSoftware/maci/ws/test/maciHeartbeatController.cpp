/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciHeartbeatController.cpp,v 1.4 2008/07/14 13:41:20 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2002/02/09  made ping timeout in mutable
* msekoran  2001/12/25  created
*/

#include <vltPort.h>

#include <tao/Messaging/Messaging.h>
#include <maciHeartbeatController.h>

#include <maciManagerImpl.h>

#include <logging.h>

/// Ping interval.
#define PING_INTERVAL 60              // seconds

/// Ping interval for Containers.
#define CONTAINER_PING_INTERVAL 30    // seconds

/// TBD intensive client (especially container) ativity can cause ping timeouts
/// and heartbeat controller considers client as inactive
/// solution: use CORBA QoS (real-time CORBA) and make ping() invocations priority high

 using namespace maci;

// Failure limit.
int HeartbeatHandler::m_failureLimit = 3;

HeartbeatHandler::HeartbeatHandler(HeartbeatController * controller, maci::Handle handle, maci::Client_ptr client) :
    m_handle(handle), m_timerID(0), m_failureCount(0), m_controller(controller)
{
    m_client = maci::Client::_duplicate(client);
}

void
HeartbeatHandler::setTimerID(long timerID)
{
    m_timerID = timerID;
}

long
HeartbeatHandler::getTimerID()
{
    return m_timerID;
}


void
HeartbeatHandler::updateClient(maci::Client_ptr client)
{
    m_failureCount = 0;
    m_client = maci::Client::_duplicate(client);
}

int
HeartbeatHandler::handle_timeout (const ACE_Time_Value &currentTime,
				  const void *arg)
{
    ACS_TRACE("maci::HeartbeatHandler::handle_timeout");
    ACS_DEBUG_PARAM("maci::HeartbeatHandler::handle_timeout", "Heartbeat check requested for: %d.", (CORBA::ULong)m_handle);


    try
    {
	bool response = m_client->ping();


	if (response)
	    // all is OK
	    m_failureCount = 0;
	else
	    // something is wrong with the client
	    m_failureCount = m_failureLimit;
    }
    catch (CORBA::TRANSIENT &ex)
    {
	m_failureCount++;
	ACS_DEBUG_PARAM("maci::HeartbeatHandler::handle_timeout", "Client with handle %d not respoding (transient exeption).", (CORBA::ULong)m_handle);
    }
    catch (CORBA::TIMEOUT &time)
    {
	m_failureCount++;
	ACS_DEBUG_PARAM("maci::HeartbeatHandler::handle_timeout", "Client with handle %d not respoding (timeout exception).", (CORBA::ULong)m_handle);
    }
    catch( CORBA::Exception &ex )
    {
	m_failureCount = m_failureLimit;
	ACS_DEBUG_PARAM("maci::HeartbeatHandler::handle_timeout", "Failed to connect to the client: %d.", (CORBA::ULong)m_handle);
	ACE_PRINT_EXCEPTION(ex, "Exception occured while connecting to the client:");
    }

    if (m_failureCount >= m_failureLimit)
    {

        ACS_DEBUG_PARAM("maci::HeartbeatHandler::handle_timeout", "Releasing 'dead' client with handle: %d.", (CORBA::ULong)m_handle);
	if (m_controller->removeClient(m_handle))
	    return -1;

	try
	{
	    m_controller->getManager()->logout(m_handle);
	}
	catch( CORBA::Exception &ex )
	{
	}

	return -1;
    }

   return 0;
}

int
HeartbeatHandler::handle_close (ACE_HANDLE, ACE_Reactor_Mask)
{
    ACS_TRACE("maci::HeartbeatHandler::handle_close");
    ACS_DEBUG_PARAM("maci::HeartbeatHandler::handle_close", "Cancelling: %d.", (CORBA::ULong)m_handle);
    delete this;
    return 0;
}


CORBA::ULong HeartbeatInitializationHandler::defaultInvocationTimeout = 2000;      // 2 seconds


HeartbeatInitializationHandler::HeartbeatInitializationHandler(CORBA::ORB_ptr orb, CORBA::ULong invocationTimeout) :
    m_orb(CORBA::ORB::_duplicate(orb)), m_invocationTimeout(invocationTimeout)
{
    // if negative default is used
    if (m_invocationTimeout == 1)
	m_invocationTimeout = defaultInvocationTimeout;
}

int
HeartbeatInitializationHandler::handle_timeout (const ACE_Time_Value &currentTime,
						const void *arg)
{
    // initialize logging
    LoggingProxy::init(ManagerImpl::m_logger);
    LoggingProxy::ThreadName("HeartbeatController");

    // initialize QoS of ORB in this thread
    // set time-out for all invocations

    if (m_invocationTimeout)
      {

	ACS_LOG(LM_RUNTIME_CONTEXT, "maci::HeartbeatInitializationHandler::handle_timeout",
		(LM_DEBUG, "Heartbeat ping() invocation timeout set to %dms", m_invocationTimeout));


	try
	  {

	    CORBA::Object_var current_object =
		m_orb->resolve_initial_references ("PolicyCurrent");         // current thread policy


	    CORBA::PolicyCurrent_var policy_current =
		CORBA::PolicyCurrent::_narrow (current_object.in ());


	    TimeBase::TimeT timeout = m_invocationTimeout*10000;	// 2s - convert 2000 millisec to ns
	    CORBA::Any any_object;
	    any_object <<= timeout;

	    CORBA::PolicyList policies (1);
	    policies.length (1);
	    policies[0] =
		m_orb->create_policy (Messaging::RELATIVE_RT_TIMEOUT_POLICY_TYPE, any_object);


	    policy_current->set_policy_overrides (policies, CORBA::SET_OVERRIDE);


	    policies[0]->destroy ();


	  }
	catch( CORBA::Exception &ex )
	  {
	    ACS_DEBUG("maci::HeartbeatInitializationHandler::handle_timeout", "Exception occured while setting PolicyCurrent.");
	    ACE_PRINT_EXCEPTION(ex, "Exception occured while setting PolicyCurrent:");
	  }

      }
    else
      {
	ACS_LOG(LM_RUNTIME_CONTEXT, "maci::HeartbeatInitializationHandler::handle_timeout",
		(LM_DEBUG, "Heartbeat ping() invocation timeout is disabled!"));
      }
    return -1;
}

int
HeartbeatInitializationHandler::handle_close (ACE_HANDLE, ACE_Reactor_Mask)
{
    ACS_TRACE("maci::HeartbeatInitialization::handle_close");
    delete this;
    return 0;
}





HeartbeatController::HeartbeatController(maci::Manager_ptr manager) : m_deactivated(false)
{
    ACS_TRACE("maci::HeartbeatController::HeartbeatController");

    m_manager = maci::Manager::_duplicate(manager);
}

HeartbeatController::~HeartbeatController()
{
    ACS_TRACE("maci::HeartbeatController::~HeartbeatController");
    stop();

    int len = 0;
    long * timerIDs = new long[m_clients.current_size()];

    HB_HASH_MAP_ENTRY *entry;
    for (HB_HASH_MAP_ITER hash_iter (m_clients);
	 hash_iter.next (entry) != 0;
	 hash_iter.advance ())
	timerIDs[len++] = entry->int_id_;

    for (int i=0; i<len; i++)
	m_timerThread.timer_queue()->cancel(timerIDs[i], 0, 0);

    delete[] timerIDs;

}

int
HeartbeatController::start(CORBA::ORB_ptr orb, CORBA::ULong invocationTimeout)
{
    ACS_TRACE("maci::HeartbeatController::start");
    int retval = m_timerThread.activate(THR_NEW_LWP);
    if (retval!=-1)
    {
	ACE_Time_Value now = ACE_OS::gettimeofday();
	m_timerThread.schedule(new HeartbeatInitializationHandler(orb, invocationTimeout), 0, now);
    }

    return retval;
}

int
HeartbeatController::stop()
{
    ACS_TRACE("maci::HeartbeatController::stop");
    if (m_deactivated)
      return 1;
    else
      m_deactivated = true;
    ACS_DEBUG("maci::HeartbeatController::stop", "Deactivating timer.");
    m_timerThread.deactivate();
    ACS_DEBUG("maci::HeartbeatController::stop", "Waiting for timer to deactivate.");
    m_timerThread.wait();     // this call is critical
    ACS_DEBUG("maci::HeartbeatController::stop", "Timer deactivated.");

    return 0;
}

int
HeartbeatController::registerClient(maci::Handle handle, maci::Client_ptr client)
{
    ACS_TRACE("maci::HeartbeatController::registerClient");

    // invalid parameters
    if (handle == 0 || CORBA::is_nil(client))
	return 1;

    ACS_DEBUG_PARAM("maci::HeartbeatHandler::registerClient", "Registering client with handle: %d.", handle);

    // client aleady exists
    long id = 0;
    if (m_clients.find(handle, id)==0)
    {
        m_clients.unbind(handle);
	m_timerThread.timer_queue()->cancel(id, 0, 0);
//	m_timerThread.mutex().signal(); !!!
    }

    HeartbeatHandler * handler;
    ACS_NEW_RETURN(handler, HeartbeatHandler(this, handle, client), -1);

    ACE_Time_Value interval(PING_INTERVAL, 0);
    if ((handle & ManagerImpl::TYPE_MASK) == ManagerImpl::CONTAINER_MASK)
	interval.sec(CONTAINER_PING_INTERVAL);

    ACE_Time_Value startTime = ACE_OS::gettimeofday ();
    startTime += interval;

    id = m_timerThread.schedule(handler, 0, interval, interval);
    if (id == -1)
    {
	delete handler;
        return 1;
    }

    if (m_clients.bind(handle, id)==-1)
    {
	m_timerThread.timer_queue()->cancel(id, 0, 0);
	return 2;
    }
    else
	return 0;

}

int
HeartbeatController::deregisterClient(maci::Handle handle)
{
    ACS_TRACE("maci::HeartbeatController::deregisterClient");

    // invalid parameters
    if (handle == 0)
	return 1;

    // retrieve data
    long id = 0;
    if (m_clients.unbind(handle, id)!=0)
	return 2;

    m_timerThread.timer_queue()->cancel(id, 0, 0);
//    m_timerThread.mutex().signal(); !!!

    return 0;
}

int
HeartbeatController::removeClient(maci::Handle handle)
{
    ACS_TRACE("maci::HeartbeatController::removeClient");

    // invalid parameters
    if (handle == 0)
	return 1;

    return m_clients.unbind(handle);
}

maci::Manager_ptr
HeartbeatController::getManager()
{
    return m_manager.ptr();
}






