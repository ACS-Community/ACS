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

import EDU.oswego.cs.dl.util.concurrent.Channel;
import EDU.oswego.cs.dl.util.concurrent.PooledExecutor;

/**
 * <code>PoolExecutor</code> with added <code>WaitInNewThreadWhenBlocked</code> 
 * <code>BlockedExecutionHandler</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class PooledExecutorWithWaitInNewThreadWhenBlocked extends PooledExecutor {

	/**
	 * Implementation of new blocking policy - it blocks in a newly created thread. 
	 */
	protected class WaitInNewThreadWhenBlocked implements BlockedExecutionHandler {
		public boolean blockedAction(Runnable command) throws InterruptedException {
			final Runnable cmd = command;
			new Thread( 
				new Runnable() {
					public void run()
					{
						try
						{
							handOff_.put(cmd);
						}
						catch (InterruptedException ie)
						{
							// TODO log
						}
					}
				}, getClass().getName()+" blockedAction thread").start();
				
		  return true;
		}
	  }

	/** 
	 * Set the policy for blocked execution in a newly created thread.
	 **/
	public void waitInNewThreadWhenBlocked() {
	  setBlockedExecutionHandler(new WaitInNewThreadWhenBlocked());
	}

	/** 
	 * Create a new pool with all default settings
	 **/
	public PooledExecutorWithWaitInNewThreadWhenBlocked() {
		super();
	}

	/** 
	 * Create a new pool with all default settings except
	 * for maximum pool size.
	 **/
	public PooledExecutorWithWaitInNewThreadWhenBlocked(int maxPoolSize) {
		super(maxPoolSize);
	}

	/** 
	 * Create a new pool that uses the supplied Channel for queuing, and
	 * with all default parameter settings.
	 **/
	public PooledExecutorWithWaitInNewThreadWhenBlocked(Channel channel) {
	  super(channel, DEFAULT_MAXIMUMPOOLSIZE);
	}

	/** 
	 * Create a new pool that uses the supplied Channel for queuing, and
	 * with all default parameter settings except for maximum pool size.
	 **/
	public PooledExecutorWithWaitInNewThreadWhenBlocked(Channel channel, int maxPoolSize) {
		super(channel, maxPoolSize);
	}

}
