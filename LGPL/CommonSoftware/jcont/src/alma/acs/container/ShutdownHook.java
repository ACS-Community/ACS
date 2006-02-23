/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.container;

import java.util.logging.Logger;

/**
 * This class will be registered to the JVM Runtime as a shutdown thread to be run at shutdown, 
 * both for regular termination and for interruption like Ctrl-C (SIGINT).
 * <p>
 * TODO: look at <code>void TerminationSignalHandler(int)</code> from the C++ implementation,
 * 		and figure out what else to do at shutdown. 
 * <p>
 * Recommended reading: http://www-106.ibm.com/developerworks/ibm/library/i-signalhandling/ 
 * (also talks about signals for tougher mishaps than Ctrl-c).
 * See also {@link java.lang.Runtime#addShutdownHook(Thread)}
 * 
 * @author hsommer
 */
public class ShutdownHook extends Thread 
{
	private boolean m_regularShutdownExpected = false;

	private Logger m_logger; 
	
	private AcsContainer m_acsContainer;
	
	
	ShutdownHook(Logger logger)
	{
		super("ContainerVMShutdownHook");
        m_logger = logger;
	}
	
	
	/**
	 * @see java.lang.Runnable#run()
	 */
	public void run()
	{
		try
		{
			if (m_regularShutdownExpected)
			{
				regularTermination();
			}
			else
			{
				interruptDetected();
			}	
		}
		catch (Throwable thr)
		{
            System.err.println("failure in ShutdownHook#run: ");
            thr.printStackTrace();
		}
				
	}

	private void regularTermination()
	{
		// regular exit (unless something went wrong after setRegularShutdownExpected() had been called
		m_logger.info("about to exit the JVM...thanks for using our container, come back soon...");
	}
	
	/**
	 * Tidying up when the JVM is about to terminate due to an interrupt signal
	 */
	private void interruptDetected()
	{
		// termination
		m_logger.severe("*** container process has been interrupted - will shut down ***");
		
		if (m_acsContainer != null)
		{
			// since the OS won't wait long, use gracefully==false
			// so that the abort methods are called
             m_acsContainer.shutdown(AcsContainer.CONTAINER_EXIT << 8, false);
		}
		// currently the ORB gets shut off as well in container.shutdown,
		// so the logger is no longer available
		System.err.println("*** emergency shutdown complete, will exit... ***");
	}
	
	
	/**
	 * Must be called right before the program ends. This helps the ShutdownHook to decide 
	 * whether it's being called for a regular program termination or because of an interrupt.
	 */
	void setRegularShutdownExpected()
	{
		m_regularShutdownExpected = true;
	}
	
	/**
	 * Sets m_acsContainer.
	 */
	public void setAcsContainer(AcsContainer acsContainer)
	{
		m_acsContainer = acsContainer;
	}

}
