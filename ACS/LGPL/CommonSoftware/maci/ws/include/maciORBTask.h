#ifndef maciORBTask_h
#define maciORBTask_h

/*******************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciORBTask.h,v 1.2 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2003/05/22  created
*/

#include <acsutil.h>
#include <logging.h>
#include <ace/Task.h>
#include <tao/ORB.h>

namespace maci {

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

 }; 

#endif /* maciORBTask_h */ 

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: maciORBTask.h,v $
// Revision 1.2  2006/09/01 02:20:54  cparedes
// small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
// Revision 1.1  2003/05/23 09:26:37  msekoran
// Multi-threaded servers, hierarchical COBs reactivation deadlock fixed.
//
//
// ************************************************************************
