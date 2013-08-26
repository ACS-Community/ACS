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

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACSErr.Completion;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;

/**
 * BACI action.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public abstract class BACIAction implements PrioritizedRunnable {

	/**
	 * Action priority.
	 */
	protected BACIPriority priority;

	/**
	 * Executor.
	 */
	protected PrioritizedExecutor executor;

	/**
	 * Callback out descriptor.
	 */
	protected CBDescOut descOut;

	/**
	 * Callback.
	 */
	protected Callback callback;
		
	/**
	 * Callback dispatcher (used to dispatch non-CBvoid callbacks).
	 */
	protected CallbackDispatcher callbackDispatcher;
	
	/**
	 * Completion.
	 */
	protected Completion completion;

	/**
	 * Return value.
	 */
	protected Object returnValue;

	/**
	 * Constructor of NORMAL priority action (CBvoid callback).
	 * @param executor	executor to be used to execute action.
	 * @param callback	action callback.
	 * @param descIn	action callback in descriptor.
	 */
	public BACIAction(PrioritizedExecutor executor, CBvoid callback, CBDescIn descIn)
	{
		this(executor, callback, descIn, (CallbackDispatcher)null);
	}
	
	/**
	 * Constructor of NORMAL priority action.
	 * @param executor	executor to be used to execute action.
	 * @param callback	action callback.
	 * @param descIn	action callback in descriptor.
	 * @param callbackDispatcher	callback dispatcher (value dependend).
	 */
	public BACIAction(PrioritizedExecutor executor,
					  Callback callback, CBDescIn descIn,
					  CallbackDispatcher callbackDispatcher)
	{
		this(executor, callback, descIn, callbackDispatcher, BACIPriority.NORMAL);
	}

	/**
	 * Constructor.
	 * @param executor	executor to be used to execute action.
	 * @param callback	action callback.
	 * @param descIn	action in descriptor.
	 * @param priority	action priority.
	 */
	public BACIAction(PrioritizedExecutor executor, CBvoid callback, CBDescIn descIn, BACIPriority priority)
	{
		this(executor, callback, descIn, null, priority);
	}

	/**
	 * Constructor.
	 * @param executor	executor to be used to execute action.
	 * @param callback	action callback.
	 * @param descIn	action in descriptor.
	 * @param callbackDispatcher	callback dispatcher (value dependend).
	 * @param priority	action priority.
	 */
	public BACIAction(PrioritizedExecutor executor,
					  Callback callback, CBDescIn descIn, 
					  CallbackDispatcher callbackDispatcher, BACIPriority priority)
	{
		this.executor = executor;
		this.callback = callback;
		this.callbackDispatcher = callbackDispatcher;
		this.priority = priority;
		descOut = generateCBDescOut(descIn);
	}

	/**
	 * @see alma.ACS.jbaci.PrioritizedRunnable#getPriority()
	 */
	public BACIPriority getPriority() {
		return priority;
	}

	/**
	 * Generates callback out descriptor from in descriptor.
	 * @param descIn	callback in descriptor.
	 * @return	generated callback out descriptor.
	 */
	protected static CBDescOut generateCBDescOut(CBDescIn descIn)
	{
		return new CBDescOut(0, descIn.id_tag);
	}

	/**
	 * NOT TO BE CHANGED.
	 */
	public final void run()
	{
		try
		{
			returnValue = execute();
			
			// generate no-error completion
			if (completion == null)
				completion = CompletionUtil.generateNoErrorCompletion();
		}
		catch (AcsJException ex)
		{

			AcsJCouldntPerformActionEx cpa = new AcsJCouldntPerformActionEx(ex);
			completion = CompletionUtil.generateCompletion(cpa);
		}
		catch (Throwable th)
		{
			AcsJCouldntPerformActionEx cpa =
				new AcsJCouldntPerformActionEx("Exception caught invoking BACI action method.", th);
			completion = CompletionUtil.generateCompletion(cpa);
		}
		finally
		{
			if (!dispatch())
			{
				// TODO log (if there are retries impl., only the last...)
			}
		}
	}

	/**
	 * Dispatch method.
	 * @return	<code>true</code> if successfully dispatched.
	 */
	protected boolean dispatch()
	{
		// TODO should this be done in BACIDispatcher (we have priorities...)
		// TODO retries (or CORBA already does that)
		try
		{
			if (callbackDispatcher == null)
			{
				return CBvoidCallbackDispatcher.getInstance().dispatchCallback(
														   CallbackDispatcher.DONE_TYPE,
														   null, callback,
														   completion, descOut);
			}
			else
				return callbackDispatcher.dispatchCallback(CallbackDispatcher.DONE_TYPE,
														   returnValue, callback,
														   completion, descOut);
		}
		catch (Throwable th)
		{
			return false;
		}
	}

	/**
	 * Action to be executed - implement it.
	 * If <code>completion</code> field	is left to <code>null</code> (by this method),
	 * this class will create a no-error completion with current timestamp.
	 * Use <code>alma.ACS.jbaci.CompletionUtil</code> class to generate no-error completion.
	 * @return	value to be send via callback, can be <code>null</code>.
	 */
	public abstract Object execute() throws AcsJException;
	
	/**
	 * Sumbit action to be <code>BACIExecutor<code> to be executed. 
	 */
	public void submit()
	{
		executor.execute(this);
	}

}
