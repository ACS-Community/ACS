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
* "@(#) $Id: testMacros.cpp,v 1.16 2010/03/31 20:15:37 javarias Exp $"
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


void testAutoTraceFunc()
{
	 AUTO_TRACE("testAutoTraceFunc");
}

static void testStaticLoggingWithAudience()
{
    STATIC_LOG(Logging::BaseLog::LM_INFO, __PRETTY_FUNCTION__, 
            "Testing Static Log");
    STATIC_LOG_TO_DEVELOPER(LM_INFO, "STATIC_LOG_TO_DEVELOPER");
    STATIC_LOG_TO_OPERATOR(LM_INFO, "STATIC_LOG_TO_OPERATOR");
    STATIC_LOG_TO_SCIENCE(LM_INFO, "STATIC_LOG_TO_SCIENCE");
    STATIC_LOG_TO_SCILOG(LM_INFO, "STATIC_LOG_TO_SCILOG");
}

int main(int argc, char *argv[])
{
    char *tooLong_p = "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";

    LoggingProxy m_logger(0, 0, 31, 0);
    LoggingProxy::init (&m_logger);


    ACS_CHECK_LOGGER;
    AUTO_TRACE("someFunc");

    testAutoTraceFunc();

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
1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\
1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\
1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\
1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\
1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\
1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\
1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\
1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\
1234567890123456789LAST01234567890123456789012345678906789012345678901234567890123456789012345678901234567890";

    LoggingProxy::AddData("testTooLongValue", tooLongAddData_p);
    ACS_SHORT_LOG((LM_ERROR, "add data for this log message should be too long (max is 1024 characters)"));

  //--------------------------------------------------------------------
  
  //Test Levels
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_TRACE, "Test of LM_TRACE log")); 

    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_ERROR, "Test of LM_ERROR log"));
    ACS_LOG( LM_FULL_INFO, "main",
         (LM_DELOUSE, "Test of LM_DELOUSE log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_DEBUG, "Test of LM_DEBUG log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_INFO, "Test of LM_INFO log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_NOTICE, "Test of LM_NOTICE log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_WARNING, "Test of LM_WARNING log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_CRITICAL, "Test of LM_CRITICAL log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_ALERT, "Test of LM_ALERT log"));
    ACS_LOG( LM_FULL_INFO, "main",
	     (LM_EMERGENCY, "Test of LM_EMERGENCY log"));
   
    //Test audience macros
    LOG_TO_AUDIENCE_WITH_LOGGER(LM_INFO, 
            "Test of LOG_TO_AUDIENCE_WITH_LOGGER log",
            log_audience::OPERATOR, myLoggerSmartPtr);
    LOG_TO_DEVELOPER( LM_INFO, "Test of LOG_TO_DEVELOPER log");
    LOG_TO_DEVELOPER_WITH_LOGGER(LM_INFO, 
            "Test of LOG_TO_DEVELOPER_WITH_LOGGER",
            myLoggerSmartPtr);
    LOG_TO_OPERATOR( LM_INFO, "Test of LOG_TO_OPERATOR log");
    LOG_TO_OPERATOR_WITH_LOGGER(LM_INFO, "Test of LOG_TO_OPERATOR_WITH_LOGGER",
            myLoggerSmartPtr);
    LOG_TO_SCIENCE( LM_INFO, "Test of LOG_TO_SCIENCE log");
    LOG_TO_SCILOG( LM_INFO, "Test of LOG_TO_SCILOG log");

    testStaticLoggingWithAudience();

    return 0;
}
