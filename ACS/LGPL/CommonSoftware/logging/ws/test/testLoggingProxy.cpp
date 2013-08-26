/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) UNSPECIFIED - FILL IN, 2005 
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
* "@(#) $Id: testLoggingProxy.cpp,v 1.2 2007/05/10 08:06:17 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran  2007-04-12  created
*/

#include "loggingLoggingProxy.h"
#include "logging.h"

static char *rcsId="@(#) $Id: testLoggingProxy.cpp,v 1.2 2007/05/10 08:06:17 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


int main(int argc, char *argv[])
{
  // initialize ACE logger instance
  ACE_TCHAR hostname[33];
  ACE_OS::hostname (hostname, sizeof(hostname));
  ACE_Log_Msg::instance()->local_host(hostname);

  // initialize the rest of LoggingProxy
  if (argv>0)
    LoggingProxy::ProcessName(argv[0]);
  LoggingProxy::ThreadName("main");

  #define CACHE_SIZE 5
  #define FLUSH_PERIOD 5
  LoggingProxy* m_logger = new LoggingProxy(CACHE_SIZE, 4, 8, 0, 0, FLUSH_PERIOD);
  LoggingProxy::init (m_logger);

  // ------------------------------------------------------

  ACS_SHORT_LOG((LM_INFO, "log 1"));

  #define CHECK_LOG_COUNT \
	ACE_OS::printf("LC: %d\n", m_logger->getCacheLogCount());

  CHECK_LOG_COUNT;

  for (int i = 2; i < CACHE_SIZE; i++)
  {
    ACS_SHORT_LOG((LM_INFO, "log %d", i));
    CHECK_LOG_COUNT;    
  }

  // should be 0
  ACS_SHORT_LOG((LM_INFO, "log %d - now will flush", CACHE_SIZE));
  ACE_OS::sleep(1);
  CHECK_LOG_COUNT;

  // should be 1 and 0
  ACS_SHORT_LOG((LM_INFO, "log to be auto-flushed"));
  CHECK_LOG_COUNT;
  ACE_OS::sleep(FLUSH_PERIOD+1);
  CHECK_LOG_COUNT;

  // shoule be 0
  ACS_TRACE("trace log - to be ignored");
  CHECK_LOG_COUNT;

  // should be 0, but since no log is provided, this will still go to cache
  // so it will be 1
  ACS_SHORT_LOG((LM_EMERGENCY, "emerency log - should bypass cache"));
  CHECK_LOG_COUNT; 

  // test that clean is called even if the log is not sent 
  // TRACE level is too high to be sent so it is ignored ...
  // but in the past the fileds like line, file etc. were not cleared ...
  // ... so logs from ACE+TAO which do not set those fields were corupted
  ACS_LOG(LM_FULL_INFO, "main", (LM_DEBUG, "a log that it is not sent to the CL"));
  ACE_DEBUG((LM_DEBUG, "problematic log that shall not contain non empty file, line etc. fields")); // yes should be ACE and not ACS !!!
  // ------------------------------------------------------

  LoggingProxy::done();

  delete m_logger;

  return 0;
}
