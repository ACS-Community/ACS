/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciORBTask.cpp,v 1.4 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2003/05/22  created 
*/

#include <vltPort.h>

#include <maciORBTask.h>

 using namespace maci;

ORBTask::ORBTask (CORBA::ORB_ptr orb, LoggingProxy * logger, unsigned int timeToRun)
    :  m_orb(CORBA::ORB::_duplicate(orb)), m_logger(logger), m_timeToRun(timeToRun)
{
}

int
ORBTask::svc (void)
{
    // initialize logging
    if (m_logger)
      {
	LoggingProxy::init(m_logger);
	LoggingProxy::ThreadName("ORBTask");
      }

    
    try
      {

        // handle CORBA requests 
	if (m_timeToRun == 0)
	  {
	    this->m_orb->run ();
	    
	  }
	else
	  {
	    ACE_Time_Value tv (m_timeToRun);
	    this->m_orb->run (tv);
	    
	  }


      }
    catch( CORBA::Exception &ex )
      {
	ACE_PRINT_EXCEPTION(ex, "maci::ORBTask::svc");
	return 1;
      }

  // return error free code
  return 0;
}



// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: maciORBTask.cpp,v $
// Revision 1.4  2006/09/01 02:20:54  cparedes
// small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
// Revision 1.3  2005/09/27 08:35:10  vwang
//  change from ACE_TRY CATCH to C++ try catch
//
// Revision 1.2  2003/10/23 08:06:25  acaproni
// True native exception handling. No more extra parameters
//
// Revision 1.1  2003/05/23 09:26:37  msekoran
// Multi-threaded servers, hierarchical COBs reactivation deadlock fixed.
//
//
// ************************************************************************





