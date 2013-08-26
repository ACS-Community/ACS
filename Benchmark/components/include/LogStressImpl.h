#ifndef _LOG_STRESS2_H
#define _LOG_STRESS2_H
/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 
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
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "ace/Task.h"
#include <acscomponentImpl.h>
#include <perftestS.h>
 
class LogStress ; // forward declaration

/*
 * SendingThread - used to send the log messages
 */
class SendingThread : public ACE_Task_Base
{
	public: 

	/**
	 * Constructor for the thread.
	 * @param logstress2_ptr a pointer to the "owning" LogStress object; used 
	 *        for things like getting the component's name (which is written in the log).
	 */
	SendingThread(LogStress* logstress2_ptr)
	{
		logStress2_p = logstress2_ptr;		
	}

	/**
	 * Destructor
	 */
	~SendingThread() 
	{ 
		ACS_TRACE("SendingThread::~SendingThread"); 
	}
    
	/**
	 * Required method of ACE_Task_Base thread class.
	 */
	virtual int svc(void);

	private:
		LogStress * logStress2_p;
};

/**
 * LogStress class - implements the LogStress IDL interface.
 * Used to send logs to the logging channel.
 */
class LogStress: public virtual acscomponent::ACSComponentImpl, public POA_perftest::LogStressWithDelay
{    
	/**
	 * Make SendingThread a 'friend' so that it can call our private methods, such as
	 * getNumTimesToLog, and setThreadDone.
	 */
	friend class SendingThread;

	public:
		/**
		 * Constructor
		 * @param poa Poa which will activate this and also all other components. Developers need
		 * not be concerned with what a PortableServer does...just pass it to the superclass's
		 * constructor.
		 * @param name component's name. All components have a name associated with them so other 
		 * components and clients can access them.
		 */
		LogStress(const ACE_CString& name, maci::ContainerServices * containerServices);
    
		/**
		 * Destructor
		 */
		virtual ~LogStress();

		/* --------------------- [ CORBA interface ] ----------------------*/    
		virtual void logNumTimes(int numTimes, int delayBetweenLogs=0);
		virtual bool getThreadDone() { return threadDone; }

	private:
		int getNumTimesToLog() { return numTimesToLog; }
		int getDelay() { return delay; }
		void setThreadDone(bool done) { threadDone = done; }
		SendingThread * m_SendingThread_p;
		int numTimesToLog;
		int delay;
		bool threadDone;
};

#endif /*!_LOG_STRESS2_H*/



