/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: acsdaemonORBTask.cpp,v 1.1 2009/06/12 13:32:14 msekoran Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2003/05/22  created 
*/

#include <acsdaemonORBTask.h>
#include <ACSErrTypeCommon.h>

//using namespace acsdaemon;

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
    catch( CORBA::SystemException &ex ) 
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
							    "acsdaemon::ORBTask::svc");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());
	
	corbaProblemEx.log();
	return 1;
	}
    catch( CORBA::Exception &ex )
      {
      ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
							    "acsdaemon::ORBTask::svc");
      corbaProblemEx.setInfo(ex._info().c_str());
      corbaProblemEx.log();
      return 1;
      }
    catch(ACSErr::ACSbaseExImpl &_ex)
	{
	
	ACSErrTypeCommon::UnexpectedExceptionExImpl ex(_ex, __FILE__, __LINE__,
						       "acsdaemon::ORBTask::svc");
	ex.log();
	return 1;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl ex(__FILE__, __LINE__,
						       "acsdaemon::ORBTask::svc");
	ex.log();
	return 1;
	}//try-catch

  // return error free code
  return 0;
}

