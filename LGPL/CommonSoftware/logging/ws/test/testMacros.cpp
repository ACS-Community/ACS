/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2005 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: testMacros.cpp,v 1.7 2007/01/02 15:35:22 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-04  created
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*   
* 
*   DESCRIPTION
*
*   FILES
*
*   ENVIRONMENT
*
*   COMMANDS
*
*   RETURN VALUES
*
*   CAUTIONS 
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

#include "logging.h"
#include <acsutilTimeStamp.h>

#include "loggingACSLogger.h"
#include "loggingHandler.h"
#include "loggingLogTrace.h"
#include "logging.h"

namespace Logging { 
   
    class TestHandler : public virtual Handler
    {
      public:
	TestHandler()
	    {setLevel(LM_TRACE);}

	~TestHandler()
	    {}

	virtual void
	log(const LogRecord& lr)
	    {
		std::string niceTime = getStringifiedUTC(lr.timeStamp).c_str();
		std::cout << lr.priority << " " << lr.message << " " << lr.file << " " << lr.line << " " << lr.method << " " << niceTime << std::endl;
	    }

	virtual std::string
	getName() const
	    {return "TestHandler";}
    };
};

int main(int argc, char *argv[])
{
    char *tooLong_p = "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";

    LoggingProxy m_logger(0, 0, 31, 0);
    LoggingProxy::init (&m_logger);


    ACS_CHECK_LOGGER;
    AUTO_TRACE("someFunc");

    ACS_SHORT_LOG((LM_INFO, "%s a %s b %s c %s d %s e %s f %s g %s h %s i %s j %s k Should never see this...\n", 
		   tooLong_p,
		   tooLong_p,
		   tooLong_p,
		   tooLong_p,
		   tooLong_p,
		   tooLong_p,
		   tooLong_p,
		   tooLong_p,
		   tooLong_p,
		   tooLong_p,
		   tooLong_p));

    
    //--------------------------------------------------------------------
    //Test ACS_LOG
    ACS_LOG(LM_RUNTIME_CONTEXT, "main",
	    (LM_INFO, "Test of formatted log 1 - %s",
	     "This is a string parameter"));
    
    ACS_LOG( LM_SOURCE_INFO, "main",
	    (LM_INFO, "Test of formatted log 2 - %s",
	     "This is a string parameter"));
    
    ACS_LOG( LM_FULL_INFO, "main",
	    (LM_INFO, "Test of formatted log 3 - %s",
	     "This is a string parameter"));
    
    //--------------------------------------------------------------------
    //Test ACS_LOG_TIME
    Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
    Logging::Handler::HandlerSmartPtr localHandler(new Logging::TestHandler());
    myLoggerSmartPtr->addHandler(localHandler);
    ACS_LOG_TIME(0, getTimeStamp(), "someRoutineName", 
		 (LM_ERROR, "%s", "The actual time..."));


    ACS_LOG_TIME(0, 132922080005000000ULL, "someRoutineName", 
		 (LM_ERROR, "%s", "Should be January 1st 2004..."));
    myLoggerSmartPtr->removeHandler(localHandler->getName());
    //--------------------------------------------------------------------

    //--------------------------------------------------------------------
    //Test Add Data
    
    char *tooLongAddData_p = 
         "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\
1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\
123456789012345678901234567890123456789012345678901last6789012345678901234567890123456789012345678901234567890";

    LoggingProxy::AddData("testTooLongValue", tooLongAddData_p);
    ACS_SHORT_LOG((LM_ERROR, "add data for this log message should be too long (max is 255 characters)"));

  //--------------------------------------------------------------------
  
  //Test Levels
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_TRACE, "Test of LM_TRACE log")); ACS_LOG( LM_FULL_INFO, "main",
	     (LM_ERROR, "Test of LM_ERROR log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_DEBUG, "Test of LM_DEBUG log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_INFO, "Test of LM_INFO log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_NOTICE, "Test of LM_NOTICE log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_WARNING, "Test of LM_WARNING log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_ERROR, "Test of LM_ERROR log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_CRITICAL, "Test of LM_CRITICAL log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_ALERT, "Test of LM_ALERT log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_EMERGENCY, "Test of LM_EMERGENCY log"));
    
    return 0;
}
