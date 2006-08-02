/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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
package alma.acs.genfw.runtime.sm;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;

import alma.acs.concurrent.DaemonThreadFactory;


/**
 * Represents an Activity in state (do-activity).
 * <p>
 * Implementation note: uses a single instance of {@link ExecutorService}
 * for cheaply reusing a thread in {@link #execute()} for all do activities. 
 * 
 * @author hsommer
 * created Apr 29, 2004 3:55:28 PM
 */
public abstract class AcsDoActivity
{
	private AcsSimpleState m_nextState;
	private AcsSimpleState m_errorState;
	private String m_name;
	
	private volatile boolean m_completed;
	
	/** 
	 * not actually used as a queue, but simply to wrap and reuse a <code>Thread</code>, 
	 * since only one do-action method is supposed to run at a time
	 * in a non-concurrent state machine.
	 * todo: or maybe it's a queue, if a composite (super) state also has a /do method ? 
	 */ 
	private static ExecutorService s_executor = Executors.newSingleThreadExecutor(new DaemonThreadFactory());

    
	/**
	 * @param name  name for the activity
	 * @param nextState  state to which the implicit "completion transition" will go.
	 * @param errorState  error state to which we'll go in case of errors.
	 */
	public AcsDoActivity(String name, AcsSimpleState nextState, AcsSimpleState errorState) 
	{
		m_completed = true;
		m_name = name;
		m_nextState = nextState;
		m_errorState = errorState;
	}
	
	/**
	 * Runs {@link #runActions()} in a separate thread and returns immediately.
	 * When the actions are completed, a transition to the next state is triggered,
	 * as specified in the constructor.
	 */
	public void execute() //throws InterruptedException 
	{
		m_completed = false;

		Runnable doActionRunner = new Runnable() {
			public void run() 
			{
				try {
					runActions();

					m_completed = true;
					// move on to next state
					m_nextState.activate("/do-activities '" + m_name + " completed.");
				}
				catch (Throwable t) {
					// todo: log
//					m_superContext.logActionFailure(stateName(), null, "initSubsysPass1", t);
					t.printStackTrace();
					
					// todo: decide if going into error state is appropriate...
					m_errorState.activate("/do-activities '" + m_name + "' failed.");
				}
			}
		};
		
		try {
			s_executor.execute(doActionRunner);
		}
		catch (RejectedExecutionException e) {
			// TODO log in better ways
			e.printStackTrace();
			m_errorState.activate("/do-activities '" + m_name + "' failed: " + e.toString());
		}
	}
	
	
    
	/**
	 * Must call the action methods associated with this activity. 
	 */
	public abstract void runActions() throws AcsStateActionException;

	
	/**
	 * Terminates the actions if they are still running. 
	 * In the current implementation, this should never be called, since we don't allow
	 * events in activitiy states which would force the termination from the current state's exit method.
	 * Implemented nontheless, just to have things complete.
	 */
	public void terminateActions()
	{
		if (!m_completed) {
			// todo: log better warning.
			System.err.println("*** /do-activities '" + m_name + "' terminated prematurely!");
			s_executor.shutdownNow();
		}
	}
}
