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

import alma.acs.shutdown.ShutdownHookBase;

/**
 * This class will be registered to the JVM Runtime as a shutdown thread to be run at shutdown, 
 * both for regular termination and for interruption like Ctrl-C (SIGINT).
 * <p>
 * TODO: look at <code>void TerminationSignalHandler(int)</code> from the C++ implementation,
 * 		and figure out what else to do at shutdown. 
 * @author hsommer
 */
public class ShutdownHook extends ShutdownHookBase 
{
	private AcsContainer m_acsContainer;
	
	ShutdownHook(Logger logger)
	{
		super(logger, "ContainerVM");
	}
	
	

	protected void regularTermination()
	{
		// regular exit (unless something went wrong after setRegularShutdownExpected() had been called
		logger.info("about to exit the JVM...thanks for using our container, come back soon...");
	}
	
	/**
	 * Tidying up when the JVM is about to terminate due to an interrupt signal
	 */
	protected void interruptDetected()
	{
		// termination
		logger.severe("*** container process has been interrupted - will shut down ***");
		
		if (m_acsContainer != null)
		{
			// since the OS won't wait long, use gracefully==false
			// so that the abort methods are called
			m_acsContainer.shutdown(AcsContainer.CONTAINER_EXIT << 8, false, false);
		}
		// The ORB gets shut off as well in container.shutdown,
		// so the logger is no longer available
		System.err.println("*** emergency shutdown complete, will exit... ***");
	}
	

	/**
	 * Sets m_acsContainer.
	 */
	public void setAcsContainer(AcsContainer acsContainer)
	{
		m_acsContainer = acsContainer;
	}

}
