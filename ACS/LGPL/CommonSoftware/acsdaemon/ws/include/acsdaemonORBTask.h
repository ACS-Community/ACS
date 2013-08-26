#ifndef acsdaemonORBTask_h
#define acsdaemonORBTask_h

/*******************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: acsdaemonORBTask.h,v 1.1 2009/06/12 13:32:14 msekoran Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2003/05/22  created
*/

#include <acsutil.h>
#include <logging.h>
#include <ace/Task.h>
#include <tao/ORB.h>

//namespace acsdaemon {

/**
 * Implementation of the task (set of thread workers)
 * which will later handle CORBA requests.
 * This implementation uses multithreaded TAO ORB design.
 * Example of usage:
 * <pre>
 *	ORBTask worker (orb.in(), m_logger);
 *
 *      // activate worker
 *	if (worker.activate(THR_NEW_LWP | THR_JOINABLE, nthreads) == 0)
 *          // wait until all workers do its work, i.e. until ORB is destroyed
 *	    worker.thr_mgr ()->wait ();
 *      else
 *          // report error here
 * </pre>
 */
class ORBTask : public ACE_Task_Base
{
  public:

    /**
     * Constructor of the ORBTask class.
     * @param orb CORBA ORB whose request to handle.
     * @param logger logger to be initialized in each thread.
     * @param timeToRun time in seconds to run CORBA ORB, 0 means until shutdown/destroy.
     */
    ORBTask (CORBA::ORB_ptr orb, LoggingProxy * logger = 0, unsigned int timeToRun = 0);
   
  private:

    /// Thread entry point (thread worker).
    virtual int svc (void);
 
    /// CORBA ORB whose request to handle.
    CORBA::ORB_var m_orb;

    /// Logger.
    LoggingProxy * m_logger;

    /// Time to run CORBA ORB in seconds, 0 means until shutdown/destroy.
    unsigned int m_timeToRun;

};

//}; 

#endif /* acsdaemonORBTask_h */ 

