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
#define N_THREADS 10

LoggingProxy* logger=0;

static void*
worker(void *arg)
{
	char msg[255];
	unsigned threadN = (unsigned)(arg);
	LoggingProxy::ThreadName("Thread");
	LoggingProxy::init (logger);
	ACS_SHORT_LOG((LM_INFO, "[Thread]: After Logger initialization: isInitThread = %s", LoggingProxy::isInitThread()?"true":"false"));

	sprintf(msg, "Message in thread #%d",threadN);
	for (unsigned i=0; i<1000; i++)
	{
		LOG_TO_DEVELOPER(LM_DEBUG, msg);
	}

	return 0;
}

int testMultiThreads()
{
	LoggingProxy::init (logger);
	ACE_thread_t  t_id[N_THREADS];
	ACE_hthread_t t_handle[N_THREADS];
	int ret=0;

	for (unsigned n=0; n<N_THREADS; n++)
	{
		ret = ACE_Thread::spawn(
				(ACE_THR_FUNC)worker,
				(void*)n,
				THR_JOINABLE|THR_NEW_LWP,
				&t_id[n],
				&t_handle[n],
				ACE_DEFAULT_THREAD_PRIORITY,
				0,0,0);
		if (ret==-1)
		{
			ACS_SHORT_LOG((LM_ERROR,"Error in spawning thread"));
		}
	}

	sleep(1);
	for (unsigned i=0; i<500; i++)
	{
		LOG_TO_DEVELOPER(LM_DEBUG, "Message in testMultiThreads");
	}

	ACS_SHORT_LOG((LM_INFO,"[Main]: Waiting for threads"));
	for (unsigned n=0; n<N_THREADS; n++)
		ACE_Thread::join(t_handle[n]);
	ACS_SHORT_LOG((LM_INFO,"[Main]: testMultiThreads done"));
	return 0;
}


int main(int argc, char *argv[])
{
	if (argc > 0)
			LoggingProxy::ProcessName(argv[0]);
	LoggingProxy::ThreadName("Main");


	logger = new LoggingProxy(CACHE_SIZE, 4, 8, 0, 0, FLUSH_PERIOD);

   	testMultiThreads();

    delete logger;

    return 0;
}
