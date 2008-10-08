/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 *
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
* who       when        what
* --------  --------    ----------------------------------------------
* sharring  2007/04/13  created 
*/
 
#include <LogStressImpl.h>
#include <sstream>
#include <string.h>
#include <unistd.h>

/**
 * The required thread service method (for ACE thread); sends messages to the logging channel.
 */
int SendingThread::svc()
{
	logStress2_p->setThreadDone(false);	
	int retVal = 0;
	char hostName[256];
	if(0 != gethostname(hostName, 255))
	{
		strcpy(hostName, "unknown host");
	}
	
	std::string componentName(logStress2_p->name());
	std::ostringstream stressMessage;
	int sleepAmount = logStress2_p->getDelay();
	for(int i = 0; i < logStress2_p->getNumTimesToLog(); i++)
	{
		stressMessage.str("");
		stressMessage << "Stress test - host: " << hostName << " component: " << componentName << " msg: " << i;
		if(NULL != stressMessage.str().c_str()) 
		{
			std::string message(stressMessage.str());
			getLogger()->log(Logging::Logger::LM_INFO, 
				message.c_str(), __FILE__, __LINE__, "SendingThread::run");
		}
		else {
			ACS_SHORT_LOG((LM_ERROR,"Stress test - problem generating message text!"));
		}
		if(sleepAmount > 0)
		    {
		    usleep(sleepAmount*1000);    /* usleep is in microseconds */
		    }
	}
	logStress2_p->setThreadDone(true);	
	return retVal;
}

/* 
 * Constructor.
 * @param name: the name of the component.
 * @param containerServices: a ContainerServices pointer to use for things like logging, getting components, etc.
 */
LogStress::LogStress(const ACE_CString &name, maci::ContainerServices * containerServices): acscomponent::ACSComponentImpl(name, containerServices)
{
	setThreadDone(false);
	std::string compName(name.c_str());
	LogStress * selfPtr = this;
	m_SendingThread_p = new SendingThread(selfPtr);
   // ACS_TRACE is used for debugging purposes
   ACS_TRACE("::LogStress::LogStress");
}

/*
 * Destructor.
 */
LogStress::~LogStress()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::LogStress::~LogStress");
    ACS_SHORT_LOG((LM_INFO,"destroying sending thread"));
    if(m_SendingThread_p) {
	    delete m_SendingThread_p;
    }
    ACS_DEBUG_PARAM("::LogStress::~LogStress", "Destroying %s...", name());
}

/******************************************************
 * Methods implementing the LogStress IDL interface. *
 ******************************************************/

/*
 * Starts a thread and returns control to the caller. The thread will log messages 
 * to the logging channel with the designated delay between msgs until the appropriate 
 * number of msgs have been sent.  
 *
 * @param numberOfTimes: the number of logs to be sent to the logging channel.
 * @param delayBetweenLogs: the time to pause between logs, in milliseconds.
 */
void LogStress::logNumTimes(int numberOfTimes, int delayBetweenLogs)
{
	// spawn a thread to do the "real work" so that we can return control to the caller ASAP
	numTimesToLog = numberOfTimes;
	delay = delayBetweenLogs;
	m_SendingThread_p->activate();
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(LogStress)
/* ----------------------------------------------------------------*/


/*___oOo___*/
