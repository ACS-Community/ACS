/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2002
 * Copyright by ESO (in the framework of the ALMA collaboration)
 * and Cosylab 2002, All rights reserved
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
 */

package alma.ACS.jbaci;

import java.util.concurrent.ThreadFactory;

/**
 * BACI framework manager class.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class BACIFramework {

	/* Singleton instance, constructor and getter */
	public static BACIFramework INSTANCE = new BACIFramework();
	private BACIFramework() {}

	/**
	 * ThreadFactory to be used to create threads.
	 */
	private ThreadFactory threadFactory = null;

	/**
	 * Timer singleton instance.
	 */
	private BACITimer timer = null;

	/**
	 * Dispatcher Singleton instance.
	 */
	private BACIDispatcher dispatcher = null;
	
	/**
	 * Get timer instance (singleton pattern).
	 */
	public synchronized BACITimer getTimer()
	{
		if (timer == null)
			timer = new BACITimer(threadFactory);
		return timer;
	}

	/**
	 * Get dispatcher instance.
	 */
	public synchronized BACIDispatcher getDispatcher()
	{
		if (dispatcher == null)
			dispatcher = new BACIDispatcher(threadFactory);
		return dispatcher;
	}

	/**
	 * Initialize BACI framework not using any thread factory.
	 */
	public void initialize()
	{
		initialize(null);
	}
	
	/**
	 * Initialize BACI framework using given thread factory.
	 * @param threadFactory thread factory to be used, can be <code>null</code>.
	 */
	public void initialize(ThreadFactory threadFactory)
	{
		this.threadFactory = threadFactory;
		
		// allow reinitialization
		timer = null;
		dispatcher = null;
	}
	
	/**
	 * Shutdown BACI framework (terminate timer and dispatcher threads).
	 */
	public void shutdown()
	{
		// shutdown timer
		if (timer != null)
			timer.shutDown();
		
		// shutdown dispatcher
		if (dispatcher != null)
			dispatcher.shutdown();
		
		// free external reference, so that GC could do his work
		threadFactory = null;
	}
}
