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

import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * BACI dispatcher (thread pool)
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class BACIDispatcher {

	/**
	 * Thread pool.
	 */
	private ThreadPoolExecutor threadPool;

	/**
	 * Number of threads in thread pool.
	 */
	private static final int MAX_REQUESTS = 100;

	/**
	 * Number of requests ( in thread pool (guarantees order of execution).
	 */
	private static final int POOL_THREADS = 10;

	/**
	 * Constructor. 
	 * @param threadFactory thread factory to be used to create thread, if <code>null</code> no factory is being used
	 */
	public BACIDispatcher(ThreadFactory threadFactory)
	{
        // TODO make PriorityBlockingQueue bounded!!! (to MAX_REQUESTS)
		// TODO use PooledExecutorWithWaitInNewThreadWhenBlocked...
		if (threadFactory != null)
			threadPool = new ThreadPoolExecutor(POOL_THREADS, POOL_THREADS,
					  Long.MAX_VALUE, TimeUnit.NANOSECONDS,
					  new PriorityBlockingQueue<Runnable>(MAX_REQUESTS, new PrioritizedRunnableComparator<Runnable>()), threadFactory);
		else
			threadPool = new ThreadPoolExecutor(POOL_THREADS, POOL_THREADS,
	        								  Long.MAX_VALUE, TimeUnit.NANOSECONDS,
	        								  new PriorityBlockingQueue<Runnable>(MAX_REQUESTS, new PrioritizedRunnableComparator<Runnable>()));
		threadPool.prestartAllCoreThreads();
	}
	
	/**
	 * Execute action. 
	 * If the maximum pool size or queue size is bounded,
	 * then it is possible for incoming execute requests to block. 
	 * <code>BACIDispatcher</code> uses its own blocking policy:
	 * creating a new thread, which will call blocking execute.  
	 * @param action	action to execute.
	 * @return <code>true</code> on success.
	 */
	public boolean execute(PrioritizedRunnable action) 
	{
		try
		{
			threadPool.execute(action);
			return true;	
		}
		catch (Throwable th)
		{
			return false;
		}
	}
	
	/**
	 * Shutdown dispatcher.
	 */
	public void shutdown()
	{
		// initiate shutdown
		threadPool.shutdown();
		
		// first be kind and wait up to 3 seconds to terminate
		try {
			threadPool.awaitTermination(3, TimeUnit.SECONDS);
		} catch (InterruptedException e) {
			// noop
		}
		
		// no more "mister-nice-guy", terminate all
		threadPool.shutdownNow();
	}

}
