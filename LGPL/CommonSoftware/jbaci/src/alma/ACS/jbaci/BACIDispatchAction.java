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

import java.util.ArrayList;
import java.util.LinkedList;

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACSErr.Completion;

/**
 * BACI dispatch action.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class BACIDispatchAction implements PrioritizedRunnable {

	/**
	 * Dispatch failed listener interface. 
	 */
	public interface DispatchFailedListener
	{ 
		/**
		 * Called when dispatching of particulat request failed.
		 * @param action			instance of <code>BACIDispatchAction</code> dispatching request.
		 * @param failedRequest		failed request.
		 */
		public void dispatchFailed(BACIDispatchAction action, DispatchRequest failedRequest);
	}

	/**
	 * Class containing request data.  
	 */
	public class DispatchRequest
	{
		/**
		 * No value constructor.
		 * @param type	type of callback (working/done).
		 * @param completion	completion.
		 */
		public DispatchRequest(int type, Completion completion)
		{
			this(type, completion, null);
		}
		
		/**
		 * No value constructor.
		 * @param type	type of callback (working/done).
		 * @param completion	completion.
		 * @param value			value.
		 */
		public DispatchRequest(int type, Completion completion, Object value)
		{
			this.type = type;
			this.completion = completion;
			this.value = value;
		}

		/**
		 * Return value.
		 */
		public Object value;

		/**
		 * Completion.
		 */
		public Completion completion;

		/**
		 * Callback type.
		 * @see CallbackDispatcher#DONE_TYPE
		 * @see CallbackDispatcher#WORKING_TYPE
		 */
		public int type;
	}

	/**
	 * Action priority.
	 */
	protected BACIPriority priority;

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
	 * Failure limit.
	 */
	protected static final int DEFAULT_FAILURE_COUNT_LIMIT = 3;

	/**
	 * Failure limit (number of retries), 0 means until successfull.
	 */
	protected int failureCountLimit;

	/**
	 * Failure count.
	 */
	protected int failureCount = 0;

	/**
	 * Ordered dispatch queue.
	 */
	protected LinkedList queue = new LinkedList();

	/**
	 * Pending request. 
	 * NOTE: synchronized on <code>queue</code>'s monitor. 
	 */
	protected DispatchRequest pendingRequest = null;
	
	/**
	 * Flag indicating pending submit.
	 * NOTE: synchronized on <code>queue</code>'s monitor. 
	 */
	protected boolean submitPending = false;

	/**
	 * Override policy for request queue.
	 * If <code>true</code> newer request will override request in the queue.
	 * If <code>false</code> (default) requests will form a linked list.
	 */
	protected boolean overridePolicy = false;

	/**
	 * List of registered listeners.
	 */
	protected ArrayList<DispatchFailedListener> listeners = new ArrayList<DispatchFailedListener>();

	/**
	 * Constructor of NORMAL priority action (CBvoid callback).
	 * @param callback	action callback.
	 * @param descIn	action callback in descriptor.
	 */
	public BACIDispatchAction(CBvoid callback, CBDescIn descIn)
	{
		this(callback, descIn, (CallbackDispatcher)null);
	}
	
	/**
	 * Constructor of NORMAL priority action.
	 * @param callback	action callback.
	 * @param descIn	action callback in descriptor.
	 * @param callbackDispatcher	callback dispatcher (value dependend).
	 */
	public BACIDispatchAction(Callback callback, CBDescIn descIn,
					  		  CallbackDispatcher callbackDispatcher)
	{
		this(callback, descIn, callbackDispatcher, BACIPriority.NORMAL);
	}

	/**
	 * Constructor.
	 * @param callback	action callback.
	 * @param descIn	action in descriptor.
	 * @param priority	action priority.
	 */
	public BACIDispatchAction(CBvoid callback, CBDescIn descIn, BACIPriority priority)
	{
		this(callback, descIn, null, priority);
	}

	/**
	 * Constructor.
	 * @param callback	action callback.
	 * @param descIn	action in descriptor.
	 * @param callbackDispatcher	callback dispatcher (value dependend).
	 * @param priority	action priority.
	 */
	public BACIDispatchAction(Callback callback, CBDescIn descIn, 
					  		  CallbackDispatcher callbackDispatcher, BACIPriority priority)
	{
		this.callback = callback;
		this.callbackDispatcher = callbackDispatcher;
		this.priority = priority;
		descOut = generateCBDescOut(descIn);
		failureCountLimit = DEFAULT_FAILURE_COUNT_LIMIT;
	}

	/**
	 * Added working callback request to dispatch queue.
	 * @param completion	completion.
	 * @param value			value.
	 */
	public void dispatchWorkingRequest(Completion completion)
	{
		dispatchRequest(CallbackDispatcher.WORKING_TYPE, completion, null);
	}

	/**
	 * Added working callback request to dispatch queue.
	 * @param completion	completion.
	 */
	public void dispatchWorkingRequest(Completion completion, Object value)
	{
		dispatchRequest(CallbackDispatcher.WORKING_TYPE, completion, value);
	}

	/**
	 * Added done callback request to dispatch queue.
	 * @param completion	completion.
	 */
	public void dispatchDoneRequest(Completion completion)
	{
		dispatchRequest(CallbackDispatcher.DONE_TYPE, completion, null);
	}

	/**
	 * Added done callback request to dispatch queue.
	 * @param completion	completion.
	 * @param value			value.
	 */
	public void dispatchDoneRequest(Completion completion, Object value)
	{
		dispatchRequest(CallbackDispatcher.DONE_TYPE, completion, value);
	}

	/**
	 * Added request to dispatch queue.
	 */
	public void dispatchRequest(int type, Completion completion, Object value)
	{
		dispatchRequest(new DispatchRequest(type, completion, value));
	}

	/**
	 * Added request to dispatch queue (internal).
	 */
	protected void dispatchRequest(DispatchRequest request)
	{
		synchronized (queue)
		{
			// override previous requests in the queue, if override policy is set
			// (there should be max. one request in the queue anyway) 
			if (overridePolicy)
				queue.clear();
			
			// add to queue	
			queue.add(request);
			
			// initiate submit to dispatcher queue, if necessary
			if (!submitPending)
				submit();
		}
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
	 * Dispatching is done one by one - to achieve better fairness.
	 */
	public final void run()
	{
		// get request
		synchronized (queue)
		{
			if (pendingRequest != null)
				// TODO log error
				// TODO tmp
				System.err.println("IllegalStateException(pendingRequest != null)");
				
			if (!submitPending)
				// TODO log error
				// TODO tmp
				System.err.println("IllegalStateException(!submitPending)");

			pendingRequest = (DispatchRequest)queue.removeFirst();
		}
		
		// resubmit?
		if (!dispatch())
		{
			if (failed())
			{
				// TODO log dispatch failure here

				// notify
				if (listeners.size() != 0)
				{
					DispatchFailedListener[] listenerArray = null;
					synchronized (listeners)
					{
						listenerArray = new DispatchFailedListener[listeners.size()];
						listeners.toArray(listenerArray);
					}
					for (int i = 0; i < listenerArray.length; i++)
					{
						try
						{
							listenerArray[i].dispatchFailed(this, pendingRequest);
						}
						catch (Throwable th)
						{
							// TODO or even log here
							th.printStackTrace();
						}
					}
				}
			}
			else
			{
				// retry...
				synchronized (queue)
				{
					// add again to the queue, but not if override policy is set
					// (if set, older request should not override newer) 
					if (!isOverridePolicy() || queue.isEmpty())
						queue.addFirst(pendingRequest);
				}
			}
		}
		else
		{
			// reset failure counter on success
			failureCount = 0;
		}

		// this line will always be reached - up code is (has to be) exception safe
		
		synchronized (queue)
		{
			// reset
			pendingRequest = null;
			submitPending = false;
			
			// submit (BACIDispatcher.execute()) could block...
			// if non-blocking policy was used, would not be OK - there is a StackOutOfBounds risk...
			// But since there is abort policy used, this will create a new thread which will block...
			if (!queue.isEmpty())
				submit();
		}
		
	}

	/**
	 * Call this metod to notify dispatch failure.
	 * @return <code>true</code>, if dispaching is to be canceled (e.g. failure counter reached its failure limit)
	 */
	protected boolean failed()
	{
		if (failureCountLimit == 0)
		  return false;
		else
		  return (++failureCount >= failureCountLimit);
	}

	/**
	 * Dispatch method.
	 * Dispatches <code>pendingRequest</code>, should be non-<code>null</code>.
	 * @return	<code>true</code> if successfully dispatched.
	 */
	protected boolean dispatch()
	{
		try
		{
			if (callbackDispatcher == null)
			{
				((CBvoid)callback).done(pendingRequest.completion, descOut);
				return true;
			}
			else
				return callbackDispatcher.dispatchCallback(pendingRequest.type,
														   pendingRequest.value, callback,
														   pendingRequest.completion, descOut);
		}
		catch (Throwable th)
		{
			// TODO log
			th.printStackTrace();
			return false;
		}
	}

	/**
	 * Sumbit action to be <code>BACIExecutor<code> to be executed. 
	 */
	protected void submit()
	{
		synchronized (queue)
		{
			if (submitPending)
				return;

			// non blocking...
			submitPending = BACIFramework.INSTANCE.getDispatcher().execute(this);
		}
	}

	/**
	 * Get callback out-descriptor.
	 * @return callback out-descriptor.
	 */
	public CBDescOut getDescOut() {
		return descOut;
	}

	/**
	 * Get current override policy.
	 * @return current override policy.
	 */
	public boolean isOverridePolicy() {
		return overridePolicy;
	}

	/**
	 * Set current override policy.
	 * @param b override policy to set.
	 */
	public void setOverridePolicy(boolean b) {
		overridePolicy = b;
	}

	/**
	 * Set dispatching priority.
	 * @param priority dispatching priority to set.
	 */
	public void setPriority(BACIPriority priority) {
		this.priority = priority;
	}

	/**
	 * Add dispatch failed listener.
	 * @param listener	listener to listen failed dispatch notifications.
	 */
	public void addDispatchFailedListener(DispatchFailedListener listener)
	{
		synchronized (listeners)
		{
			if (!listeners.contains(listener))
				listeners.add(listener);
		}
	}

	/**
	 * Remove dispatch failed listener.
	 * @param listener	listener to remove.
	 */
	public void removeDispatchFailedListener(DispatchFailedListener listener)
	{
		synchronized (listeners)
		{
			listeners.remove(listener);
		}
	}
}

