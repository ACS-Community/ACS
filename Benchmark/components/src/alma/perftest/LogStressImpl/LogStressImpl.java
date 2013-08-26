/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory,
 * 2002 Copyright by ESO (in the framework of the ALMA collaboration), All
 * rights reserved
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package alma.perftest.LogStressImpl;
import java.util.logging.Logger;
import java.lang.Thread;

import alma.ACS.ComponentStates;
import alma.acs.component.ComponentLifecycle;
import alma.acs.container.ContainerServices;
import alma.perftest.LogStressWithDelayOperations;

/**
 * A very simple component that can be used to stress test the logging channel.
 * 
 * @author sharring
 */
public class LogStressImpl implements ComponentLifecycle, LogStressWithDelayOperations
{
	private ContainerServices m_containerServices;
	private Logger m_logger;
	private int numTimesToLog;
	private int delay;
	private volatile boolean threadDone;
	
	private class LogSender implements Runnable
	{
		/**
		 * Required method of Thread base class; does the actual work of the thread
		 * which in this case is to log messages (almost) as fast as possible.
		 */
		public void run() 
		{
			long i = 0;

			while(i < numTimesToLog)
			{
				m_logger.info("Java stress test msg: " + i++);
				
				if(delay != 0) 
				{
					try {
						Thread.sleep(delay);
					}
					catch(InterruptedException ex) 
					{
						m_logger.warning("Thread's sleep was interrupted");
					}
				}
			}
			threadDone = true;
		}
	}


	/////////////////////////////////////////////////////////////
	// Implementation of LogStressOperations
	/////////////////////////////////////////////////////////////
	
	public void logNumTimes(int numTimes, int delayBetweenLogs) 
	{
		m_logger.info("logNumTimes called...");
		this.threadDone = false;
		this.numTimesToLog = numTimes;
		this.delay = delayBetweenLogs;
		
		Thread sendingThread = m_containerServices.getThreadFactory().newThread(new LogSender());
		sendingThread.start();
	}

	public boolean getThreadDone() 
	{
		return threadDone;
	}

	/////////////////////////////////////////////////////////////
	// Implementation of ComponentLifecycle
	/////////////////////////////////////////////////////////////
	
	public void initialize(ContainerServices containerServices) 
	{
		m_containerServices = containerServices;
		m_logger = m_containerServices.getLogger();
		m_logger.info("initialize() called...");
		threadDone = false;
	}
    
	public void execute() {
		m_logger.info("execute() called...");
	}
    
	public void cleanUp() {
		m_logger.info("cleanUp() called..., nothing to clean up.");
	}
    
	public void aboutToAbort() {
		cleanUp();
		m_logger.info("managed to abort...");
	}

	/////////////////////////////////////////////////////////////
	// Implementation of ACSComponent
	/////////////////////////////////////////////////////////////
	
	public ComponentStates componentState() 
	{
		return m_containerServices.getComponentStateManager().getCurrentState();
	}
	
	public String name() {
		return m_containerServices.getName();
	}
}
