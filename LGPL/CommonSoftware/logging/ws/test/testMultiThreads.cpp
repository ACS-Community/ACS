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
* "@(#) $Id: $"
*
*
*/

#include "ace/Thread.h"
#include "loggingLoggingProxy.h"
#include "logging.h"

#define CACHE_SIZE 1
#define FLUSH_PERIOD 1

static void*
worker(void *arg)
{
	LoggingProxy::ThreadName("Thread");
	LoggingProxy* logger=(LoggingProxy*)arg;
	ACE_OS::printf("[Thread]: Before Logger initialization isInitThread = %s\n", LoggingProxy::isInitThread()?"true":"false");
	LoggingProxy::init (logger);
	ACS_SHORT_LOG((LM_INFO, "[Thread]: After Logger initialization: isInitThread = %s", LoggingProxy::isInitThread()?"true":"false"));
	//ACE_OS::sleep(2);

	for (unsigned i=0; i<1000; i++)
	{
		LOG_TO_DEVELOPER(LM_DEBUG, "Message #1");
	}


	//ACS_SHORT_LOG((LM_INFO, "[Thread]: Before LoggingProxy::done(): isInitThread = %s", LoggingProxy::isInitThread()?"true":"false"));
	//LoggingProxy::done();
	//ACS_SHORT_LOG((LM_INFO, "[Thread]: After LoggingProxy::done(): isInitThread = %s", LoggingProxy::isInitThread()?"true":"false"));
	return 0;
}

int testLoggingInThread(LoggingProxy* m_logger)
{
	ACS_SHORT_LOG((LM_INFO, "[Main]: Before Logger initialization: isInitThread = %s", LoggingProxy::isInitThread()?"true":"false"));
	LoggingProxy::init (m_logger);
	ACS_SHORT_LOG((LM_INFO, "[Main]: After Logger initialization: isInitThread = %s", LoggingProxy::isInitThread()?"true":"false"));
	ACE_thread_t  t_id;
	ACE_hthread_t t_handle;
	int ret=0;
	ret = ACE_Thread::spawn(
			(ACE_THR_FUNC)worker,
			m_logger,
			THR_JOINABLE|THR_NEW_LWP,
			&t_id,
			&t_handle,
			ACE_DEFAULT_THREAD_PRIORITY,
			0,0,0);
	if (ret==-1)
	{
		ACS_SHORT_LOG((LM_ERROR,"Error in spawning thread"));
	}

	sleep(1);
	for (unsigned i=0; i<1000; i++)
	   		{
	   			LOG_TO_DEVELOPER(LM_DEBUG, "Message in testLoggingInThread");
	   		}

	ACS_SHORT_LOG((LM_INFO,"[Main]: Waiting for thread"));
	ACE_Thread::join(t_handle);
	ACS_SHORT_LOG((LM_INFO,"[Main]: testLoggingInThread done"));
	return 0;
}


int main(int argc, char *argv[])
{
	if (argc > 0)
			LoggingProxy::ProcessName(argv[0]);
	LoggingProxy::ThreadName("Main");

	//ACS_SHORT_LOG((LM_INFO, "[Main]: Before Logger creation: isInitThread = %s", LoggingProxy::isInitThread()?"true":"false"));
	LoggingProxy* main_logger = new LoggingProxy(CACHE_SIZE, 4, 8, 0, 0, FLUSH_PERIOD);
	//ACS_SHORT_LOG((LM_INFO, "[Main]: After Logger creation: isInitThread = %s", LoggingProxy::isInitThread()?"true":"false"));

   	testLoggingInThread(main_logger);
   	//testLoggingInThread(main_logger);
   	//testLoggingInThread(main_logger);
/*
   	for (unsigned i=0; i<1000; i++)
   		{
   			LOG_TO_DEVELOPER(LM_DEBUG, "Message in the main");
   		}
*/
    delete main_logger;
   	//ACS_SHORT_LOG((LM_INFO, "[Main]: After deleting logger isInitThread = %s", LoggingProxy::isInitThread()?"true":"false"));
    return 0;
}
