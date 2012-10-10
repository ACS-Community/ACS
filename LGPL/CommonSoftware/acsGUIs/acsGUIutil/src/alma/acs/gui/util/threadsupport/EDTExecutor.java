/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
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
package alma.acs.gui.util.threadsupport;

import java.util.List;
import java.util.concurrent.AbstractExecutorService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

/**
 * <code>EDTExecutor</code> executes tasks in the swing event dispatcher thread (EDT).
 * <P>
 * <code>EDTExecutor</code> is a helper that allows to submit tasks to the EDT like 
 * {@link SwingUtilities#invokeLater(Runnable)} does. The advantage is that it handles the case
 * of executing the code from inside or outside of the EDT in a transparent way.
 * <P>
 * {@link SwingWorker} has something in common with <code>EDTExecutor</code> but it is too heavy
 * and for most applications we don't need such a complexity.
 * <P>
 * We don't really need <code>EDTExecutor</code> to be a {@link ExecutorService} because it
 * is very easy but in future we might want to extend by adding more features 
 * (without reaching the same completeness of {@link SwingWorker} of course).
 * 
 * @author  acaproni
 * @since ACS 10.2
 */
public class EDTExecutor extends AbstractExecutorService {
	
	/**
	 * The singleton
	 */
	private static final ExecutorService executor = new EDTExecutor();
	
	/**
	 * A flag set is the executor has been shut down.
	 */
	protected volatile boolean hasBeenShutdown=false;
	
	/**
	 * <code>true</code> if a task has been submitted to the swing EDT.
	 */
	protected volatile boolean executingTask=false;
	
	/**
	 * The constructor is private because it is a singleton.
	 */
	private EDTExecutor() {}
	
	/**
	 * 
	 * @return The singleton
	 */
	public static ExecutorService instance() {
		return executor;
	}

	/**
	 * Shuts down the Executor: <code>EDTExecutor</code> will not submit other tasks
	 * to the EDT.
	 */
	@Override
	public void shutdown() {
		hasBeenShutdown=true;
	}

	/**
	 * Shuts down the Executor: <code>EDTExecutor</code> will not submit other tasks
	 * to the EDT.
	 * 
	 * @return <code>null</code> because this executor does not have a queue of tasks to submit. 
	 */
	@Override
	public List<Runnable> shutdownNow() {
		hasBeenShutdown=true;
		return null;
	}

	@Override
	public boolean isShutdown() {
		return hasBeenShutdown;
	}

	@Override
	public synchronized boolean isTerminated() {
		return hasBeenShutdown && !executingTask;
	}

	@Override
	public boolean awaitTermination(long timeout, TimeUnit unit)
			throws InterruptedException {
		if (isTerminated()) {
			return true;
		}
		// Wait until the timeout elapses (or a InterruptedExceprtion occurs) 
		// and check again
		boolean ret;
		try {
			unit.sleep(timeout);
		} catch (InterruptedException ie) {			
		} finally {
			ret=isTerminated();
		}
		return ret;
	}

	/**
	 * Executes the passed command in the EDT.
	 * <P>
	 * It can be called from inside or outside of the swing event dispatcher thread.
	 */
	@Override
	public void execute(Runnable command) {
		synchronized (this) {
			if (hasBeenShutdown) {
				// Do not accept further commands
				return;
			}
			executingTask=true;	
		}
		if (SwingUtilities.isEventDispatchThread()) {
			command.run();
		} else {
			SwingUtilities.invokeLater(command);
		}
		executingTask=false;
	}
	
}
