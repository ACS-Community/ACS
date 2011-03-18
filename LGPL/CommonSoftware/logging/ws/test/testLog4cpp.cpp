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
 *
 * "@(#) "
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * javarias  May 7, 2010  	 created
 */

#include "loggingLog4cppACEMACROS.h"
#include "loggingLog4cpp.h"
#include "loggingXmlLayout.h"

#include "log4cpp/LayoutAppender.hh"
#include "log4cpp/OstreamAppender.hh"

static void testStaticLoggingWithAudience()
{
    LOG4CPP_STATIC_LOG(LM_INFO, __PRETTY_FUNCTION__,
            "Testing Static Log");
    LOG4CPP_STATIC_LOG_TO_DEVELOPER(LM_INFO, "STATIC_LOG_TO_DEVELOPER");
    LOG4CPP_STATIC_LOG_TO_OPERATOR(LM_INFO, "STATIC_LOG_TO_OPERATOR");
    LOG4CPP_STATIC_LOG_TO_SCIENCE(LM_INFO, "STATIC_LOG_TO_SCIENCE");
    LOG4CPP_STATIC_LOG_TO_SCILOG(LM_INFO, "STATIC_LOG_TO_SCILOG");
}

void testAutoTraceFunc()
{
	 LOG4CPP_AUTO_TRACE("testAutoTraceFunc");
}

int main (int argc, char * argv[])
{
	char *tooLong_p = "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";

	LOG4CPP_AUTO_TRACE("someFunc");

	testAutoTraceFunc();

    LOG4CPP_ACS_SHORT_LOG((LM_INFO, "%s a %s b %s c %s d %s e %s f %s g %s h %s i %s j %s k Should never see this...\n",
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

    LOG4CPP_ACS_LOG(LM_RUNTIME_CONTEXT, "main",
	    (LM_INFO, "Test of formatted log 1 - %s",
	     "This is a string parameter"));

    LOG4CPP_ACS_LOG( LM_SOURCE_INFO, "main",
	    (LM_INFO, "Test of formatted log 2 - %s",
	     "This is a string parameter"));

    LOG4CPP_ACS_LOG( LM_FULL_INFO, "main",
	    (LM_INFO, "Test of formatted log 3 - %s",
	     "This is a string parameter"));

	//Test Levels
	LOG4CPP_ACS_LOG( LM_FULL_INFO, "main",
			(LM_TRACE, "Test of LM_TRACE log"));

	LOG4CPP_ACS_LOG( LM_FULL_INFO, "main",
			(LM_ERROR, "Test of LM_ERROR log"));
	LOG4CPP_ACS_LOG( LM_FULL_INFO, "main",
			(LM_DELOUSE, "Test of LM_DELOUSE log"));
	LOG4CPP_ACS_LOG( LM_FULL_INFO, "main",
			(LM_DEBUG, "Test of LM_DEBUG log"));
	LOG4CPP_ACS_LOG( LM_FULL_INFO, "main",
			(LM_INFO, "Test of LM_INFO log"));
	LOG4CPP_ACS_LOG( LM_FULL_INFO, "main",
			(LM_NOTICE, "Test of LM_NOTICE log"));
	LOG4CPP_ACS_LOG( LM_FULL_INFO, "main",
			(LM_WARNING, "Test of LM_WARNING log"));
	LOG4CPP_ACS_LOG( LM_FULL_INFO, "main",
			(LM_CRITICAL, "Test of LM_CRITICAL log"));
	LOG4CPP_ACS_LOG( LM_FULL_INFO, "main",
			(LM_ALERT, "Test of LM_ALERT log"));
	LOG4CPP_ACS_LOG( LM_FULL_INFO, "main",
			(LM_EMERGENCY, "Test of LM_EMERGENCY log"));

	    //Test audience macros
//	    LOG4CPP_LOG_TO_AUDIENCE_WITH_LOGGER(log4cpp::Priority::INFO,
//	            "Test of LOG_TO_AUDIENCE_WITH_LOGGER log",
//	            log_audience::OPERATOR, myLoggerSmartPtr);
//	    LOG4CPP_LOG_TO_DEVELOPER(log4cpp::Priority::INFO, "Test of LOG_TO_DEVELOPER log");
//	    LOG4CPP_LOG_TO_DEVELOPER_WITH_LOGGER(log4cpp::Priority::INFO,
//	            "Test of LOG_TO_DEVELOPER_WITH_LOGGER",
//	            myLoggerSmartPtr);
//	    LOG4CPP_LOG_TO_OPERATOR(log4cpp::Priority::INFO, "Test of LOG_TO_OPERATOR log");
//	    LOG4CPP_LOG_TO_OPERATOR_WITH_LOGGER(log4cpp::Priority::INFO, "Test of LOG_TO_OPERATOR_WITH_LOGGER",
//	            myLoggerSmartPtr);
	    LOG4CPP_LOG_TO_SCIENCE(LM_INFO, "Test of LOG_TO_SCIENCE log");
	    LOG4CPP_LOG_TO_SCILOG(LM_INFO, "Test of LOG_TO_SCILOG log");

	    testStaticLoggingWithAudience();
	return 0;
}
