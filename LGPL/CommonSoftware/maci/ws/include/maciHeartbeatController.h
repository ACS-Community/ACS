#ifndef maciHeartbeatController_H
#define maciHeartbeatController_H

/*******************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciHeartbeatController.h,v 1.81 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2002/02/09  made ping timeout in mutable
* msekoran  2001/12/24  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <maciExport.h>

#include <maciC.h>

#include <ace/Timer_Heap_T.h>
#include <ace/Timer_Queue_Adapters.h>

#include <ace/Hash_Map_Manager.h>

namespace maci {

// Forward declaration
class HeartbeatController;

/**
 * Event handler for the timer queue timeout events.
 * The handle_timeout hook method is inovked the the timer times out...
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: maciHeartbeatController.h,v 1.81 2006/09/01 02:20:54 cparedes Exp $"
 */
class HeartbeatHandler : public ACE_Event_Handler
{
  public:

    /**
     * Contructor.
     * @param controller parent controller.
     * @param handle clients handle.
     * @param client client reference.
     */
    HeartbeatHandler(HeartbeatController * controller, maci::Handle handle, maci::Client_ptr client);

    /**
     * Callback hook.
     * @param currentTime current time.
     * @return 0 to normaly proceed, -1 to indicate timer thread to call 'cancel' on this handle.
     */
    virtual int handle_timeout (const ACE_Time_Value &currentTime,
				const void *arg);

    /**
     * Callback hook, called when the timer is cancelled.
     * @return 0 is cancellation was successfull
     */
    virtual int handle_close (ACE_HANDLE, ACE_Reactor_Mask);

    /**
     * Sets timer ID.
     * @param timerID timer ID.
     */
    void setTimerID(long timerID);

    /**
     * Returns timer ID.
     * @return timer ID.
     */
    long getTimerID();

    /**
     * Updates client.
     * @param client client reference.
     */
    void updateClient(maci::Client_ptr client);

  private:

    /// Failure limit.
    static int m_failureLimit;
    
    /// Client's handle.
    maci::Handle m_handle;

    /// Thread timer id.
    long m_timerID;
    
    /// Failure count.
    int m_failureCount;
    
    /// Client's reference.
    maci::Client_var m_client;

    /// Heartbeat Controller that created me (parent).
    HeartbeatController * m_controller;
    
};

/**
 * Initialization event handler.
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: maciHeartbeatController.h,v 1.81 2006/09/01 02:20:54 cparedes Exp $"
 */
class HeartbeatInitializationHandler : public ACE_Event_Handler
{
  public:

    /// Default CORBA ping() invocation timeout.
    static CORBA::ULong defaultInvocationTimeout;

    /**
     * Constructor.
     * @param orb CORBA orb.
     * @param invocationTimeout CORBA ping() invocation timeout (0 means no timeout, 1 means use default).
     */
    HeartbeatInitializationHandler(CORBA::ORB_ptr orb, CORBA::ULong invocationTimeout);

    /**
     * Callback hook.
     * @param currentTime current time.
     * @return 0 to narmary proceed, -1 to 'self-destroy' timer.
     */
    virtual int handle_timeout (const ACE_Time_Value &currentTime,
				const void *arg);

    /**
     * Callback hook, called when the timer is cancelled.
     * @return 0 is cancellation was successfull
     */
    virtual int handle_close (ACE_HANDLE, ACE_Reactor_Mask);

  private:
    
    /// CORBA ORB.
    CORBA::ORB_var m_orb;

    /// CORBA ping() invocation timeout. 
    CORBA::ULong m_invocationTimeout;

};

/**
 * HeartbeatController is a class controlling 'presence' of the clients.
 * It periodically calls client's maci::Client::ping() method.
 * If it fails (for several times) or the return value if <code>false</code>,
 * it invokes maci::Manager::logout() method of the client.
 *
 * Manager pings its clients (both GUI clients, as well as containers) repeatedly to verify that they still exist.
 * The return value can be either "true", indicating that everything is OK with the client, of "false", indicating that client is malfunctioning.
 * If CORBA::TRANSIENT exception is thrown, the Manager should retry the ping several times, and only then shall the client be assumed to be malfunctioning.
 * If another exception is thrown, the client may be immediately assumed to be malfunctioning.
 * Once the client is found to be malfunctioning, the Manager makes an implicit logout of the client.
 *
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: maciHeartbeatController.h,v 1.81 2006/09/01 02:20:54 cparedes Exp $"
 */

class maci_EXPORT HeartbeatController
{

public:


    /**
     * Constructor.
     */
    HeartbeatController(maci::Manager_ptr manager);
    
    /**
     * Destrcutor.
     */
    ~HeartbeatController();
    
    /**
     * Starts controller.
     * @param orb CORBA orb.
     * @param invocationTimeout CORBA ping() invocation timeout (0 means no timeout, 1 means use default).
     * @return 0 on success
     */
    int start(CORBA::ORB_ptr orb, CORBA::ULong invocationTimeout);

    /**
     * Stops controller.
     * @return 0 on success
     */
    int stop();
    
    /**
     * Registers client to the heartbeat controller.
     * @return 0 on success
     */
    int registerClient(maci::Handle handle, maci::Client_ptr client);

    /**
     * Deregisters client to the heartbeat controller.
     * @return 0 on success
     */
    int deregisterClient(maci::Handle handle);

    /**
     * Returns Manager's reference.
     * @return Manager's reference
     */
    maci::Manager_ptr getManager();

  private:

    /**
     * Removes client from hash table.
     * @return 0 on success
     */
    int removeClient(maci::Handle handle);


    /// Manager's reference.
    maci::Manager_var m_manager;


    /// Trait for the queue type.
    typedef ACE_Timer_Heap_T<ACE_Event_Handler *,
	ACE_Event_Handler_Handle_Timeout_Upcall<ACE_SYNCH_RECURSIVE_MUTEX>,
	ACE_SYNCH_RECURSIVE_MUTEX>
    HB_TIMER_QUEUE;

    /// Trait for the queue iterator type.
    typedef ACE_Timer_Heap_Iterator_T<ACE_Event_Handler *,
	ACE_Event_Handler_Handle_Timeout_Upcall<ACE_SYNCH_RECURSIVE_MUTEX>,
	ACE_SYNCH_RECURSIVE_MUTEX>
    HB_TIMER_QUEUE_ITERATOR;
       
    typedef ACE_Thread_Timer_Queue_Adapter<HB_TIMER_QUEUE> HB_TIMER_THREAD;

    /// Timer thread.
    HB_TIMER_THREAD m_timerThread;

    /// Trait for the hash map type.
    typedef ACE_Hash_Map_Manager <maci::Handle, long, ACE_Recursive_Thread_Mutex> HB_HASH_MAP;
    /// Trait for the hash map iterator type.
    typedef ACE_Hash_Map_Iterator <maci::Handle, long, ACE_Recursive_Thread_Mutex> HB_HASH_MAP_ITER;
    /// Trait for the hash map entry type.
    typedef ACE_Hash_Map_Entry <maci::Handle, long> HB_HASH_MAP_ENTRY;

    /// Clients' repository.
    HB_HASH_MAP m_clients;

    // Deactivated.
    bool m_deactivated;

    friend class HeartbeatHandler;

};

 }; 

#endif  /* maciHeartbeatController_H */

// ************************************************************************
