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

package alma.ACS.impl;

import java.util.concurrent.atomic.AtomicLong;

import org.omg.CORBA.LongHolder;

import alma.ACS.CBDescIn;
import alma.ACS.Callback;
import alma.ACS.MonitorOperations;
import alma.ACS.jbaci.BACIDispatchAction;
import alma.ACS.jbaci.BACIFramework;
import alma.ACS.jbaci.BACIPriority;
import alma.ACS.jbaci.BACITimer;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.PrioritizedRunnable;
import alma.ACS.jbaci.BACIDispatchAction.DispatchRequest;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.acs.util.UTCUtility;

/**
 * Implementation of common monitor.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class CommonMonitorImpl implements MonitorOperations,  
			   BACITimer.TimerRunnable, PrioritizedRunnable,
			   BACIDispatchAction.DispatchFailedListener {

	/**
	 * Monitor timer task.
	 */
	protected Object monitorTimerTask;

	/**
	 * Monitorized property.
	 */
	protected CommonPropertyImpl property;

	/**
	 * Dispatch action.
	 */
	protected BACIDispatchAction dispatchAction;

	/**
	 * Time trigger (monitoring interval in ms).
	 */
	protected long timeTrigger;

	/**
	 * Start time (java time).
	 */
	protected long startTime;
 
 	/**
	 * Suspend/resume status.
	 */
	protected boolean isSuspended;

	/**
	 * Destruction status.
	 */
	protected boolean isDestroyed;

	/**
	 * Start time control status.
	 */
	protected boolean userControlledStartTime;

	/**
	 * Key time to process, 0 means none.
	 */
	protected AtomicLong queuedKeyTime = new AtomicLong(0);

	/**
	 * Default constructor.
	 */
	protected CommonMonitorImpl() {
	}

	/**
	 * Constructor with immediate monitor notification (synchronized monitors supported).
	 * @param property	property to be monitored, non-<code>null</code>.
	 * @param callback	callback, non-<code>null</code>.
	 * @param descIn	callback in-descriptor.
	 */
	public CommonMonitorImpl(CommonPropertyImpl property, Callback callback, CBDescIn descIn) {
		this(property, callback, descIn, 0);
	}

	/**
	 * Constructor.
	 * @param property	property to be monitored, non-<code>null</code>.
	 * @param callback	callback, non-<code>null</code>.
	 * @param descIn	callback in-descriptor.
	 * @param startTime startTime (OMG time), values less or equal to current time mean immediately,
	 * 					value 0 means that start time should be controlled automatically (synchronized monitors).  
	 */
	public CommonMonitorImpl(CommonPropertyImpl property, Callback callback, CBDescIn descIn, long startTime) {
		
		if (property == null)
			throw new NullPointerException("property == null");
		
		if (callback == null)
			throw new NullPointerException("callback == null");

		this.property = property;
		this.startTime = startTime;
		
		// create dispatch action
		dispatchAction = new BACIDispatchAction(callback, descIn, property, getPriority());
		
		// TODO 
		// make override policy configurable per instance, perhaps using a finite queue...
		// Currently the ACS mastercomp module relies on having the following line commented out,
		// since otherwise a few state change notifications are discarded.
		// this happens when changing into an activity state, which shortly afterwards changes
		// to the next state, thus producing two change events separated by a few ms only.		
//		dispatchAction.setOverridePolicy(true);
		
		dispatchAction.addDispatchFailedListener(this);

		// initializate and start
		initialize();		
	}

	/**
	 * Initialize monitor.
	 */
	protected void initialize()
	{
		// initialize monitor variables
		isSuspended = false;
		isDestroyed = false;

		// start time control		
		if (startTime == 0)
			userControlledStartTime = false;
		else
		{
			// user took over start time
			userControlledStartTime = true; 
			startTime = UTCUtility.utcOmgToJava(startTime);
		}

		// set time trigger and reschedule		
		setTimeTrigger(property.default_timer_trigger() / 10000);
	}

	/**
	 * @see alma.ACS.jbaci.BACIDispatchAction.DispatchFailedListener#dispatchFailed(alma.ACS.jbaci.BACIDispatchAction, alma.ACS.jbaci.BACIDispatchAction.DispatchRequest)
	 */
	public void dispatchFailed(
		BACIDispatchAction action,
		DispatchRequest failedRequest) {
		// destroy itself
		destroy();
	}

	/**
	 * Schedule monitor using fixed rate interval <code>timeTrigger</code>
	 * starting from <code>startTime</code> paramterer.
	 */
	public void schedule()
	{
		// cancel first...
		if (monitorTimerTask != null)
			// canceling is threadsafe (and can be done multiple times)
			BACITimer.cancel(monitorTimerTask);
		
		// do not reschedule, if suspended
		if (isSuspended || timeTrigger <= 0)
			return;

		// schedule
		monitorTimerTask = BACIFramework.INSTANCE.getTimer().executePeriodically(timeTrigger, this, startTime);
	}
	
	/**
	 * Set time trigger. 
	 * If <code>userControlledStartTime == false<code> also aligns (fixes) start time
	 * required for synchronized monitors. 
	 * @param timeTtrigger	time trigger (monitor interval in ms),
	 * 						if <= 0 monitoring is disabled but not resumed since
	 * 						there still might be on-change trigger enabled.
	 * 						<code>timeTrigger</code> is limited with lower bound
	 * 						<code>property.min_timer_trigger()</code>.
	 */
	protected void setTimeTrigger(long timeInterval)
	{
		// noop
		if (timeTrigger == timeInterval)
			return;

		if (timeInterval <= 0)
			timeTrigger = 0;
		else if (timeInterval < (property.min_timer_trigger() / 10000))
			timeTrigger = property.min_timer_trigger() / 10000;
		else
			timeTrigger = timeInterval;

		// realign start time 
		alignStartTime();		

		// reschedule
		schedule();
	}
	
	/**
	 * If <code>userControlledStartTime == false<code> aligns (fixes) start time
	 * required for synchronized monitors. 
	 */
	protected void alignStartTime()
	{
		/*
		 * synchronization of second intervals: 0.5, 1, 5, 10, 60, 300
		 * 0.5s is fired every half or round second
		 * 1s is fired every round second
		 * 5s is fired every 0, 5, 10, 15, 20, ... 55 second of a minute
		 * 10s is fired every 0, 10, 20, 30, 40, 50 second of a minute
		 * 60s is fired every round minute
		 * 300s is fired every 0, 5, 10, 15, 20, ... 55 minute of an hour
		 */
		if (!userControlledStartTime && timeTrigger > 0)
			startTime = (System.currentTimeMillis()/timeTrigger + 1) * timeTrigger;
	}
	
	/**
	 * @see alma.ACS.jbaci.PrioritizedRunnable#getPriority()
	 */
	public BACIPriority getPriority() {
		return BACIPriority.NORMAL;
	}

	/**
	 * Timer implementation - it stores time when triggered
	 * and delegates value retrival to <code>BACIExecutor</code>.
	 * @see alma.ACS.jbaci.BACITimer.TimerRunnable#timeout(long)
	 */
	public void timeout(long timeToRun) {
		// if none is queued, initiate executor otherwise override old value 
		if (queuedKeyTime.getAndSet(timeToRun) == 0)
		{
			property.getParentComponent().execute(CommonMonitorImpl.this);
		}
	}

	/**
	 * @see java.lang.Runnable#run()
	 */
	public void run() {
		long keyTime = queuedKeyTime.getAndSet(0);
		retrieveValueAndDispatch(keyTime, false);
	}

	/**
	 * Retrieve property value via cached <code>mnemonicValue</code>
	 * and add response to <code>BACIDispatchAction<code>. 
	 */
	protected void retrieveValueAndDispatch(long keyTime, boolean done)
	{
		// create new holder (done expeditiously)
		CompletionHolder completionHolder = CompletionUtil.createCompletionHolder();
		
		// retrieve value
		Object value = property.mnemonicValue(keyTime, completionHolder);

		// TODO make a copy of completion
		Completion completion = CompletionUtil.cloneCompletion(completionHolder.value);
		
		// fix time...
		completion.timeStamp = UTCUtility.utcJavaToOmg(keyTime);
		
		// TODO 
		// set monitor type and on-time code, if non-error type
		//completion.code = ACSErr.ACSErrMonitorOnTimer;
		
		// ... and dispatch
		if (!done)
			dispatchAction.dispatchWorkingRequest(completion, value);
		else
			dispatchAction.dispatchDoneRequest(completion, value);
			
		// TODO remove
		//System.out.println("Dispatched monitor (" + done + "): " + new java.util.Date(keyTime));
	}
	
	/*********************** [ Monitor ] ***********************/

	/**
	 * @see alma.ACS.MonitorOperations#start_time()
	 */
	public long start_time() {
		return UTCUtility.utcJavaToOmg(startTime);
	}

	/**
	 * @see alma.ACS.MonitorOperations#set_timer_trigger(long)
	 */
	public void set_timer_trigger(long timeInterval) {
		setTimeTrigger(timeInterval / 10000);
	}

	/**
	 * @see alma.ACS.MonitorOperations#get_timer_trigger(org.omg.CORBA.LongHolder)
	 */
	public void get_timer_trigger(LongHolder timeIntervalHolder) {
		timeIntervalHolder.value = timeTrigger * 10000;
	}

	/**
	 * @see alma.ACS.SubscriptionOperations#suspend()
	 */
	public synchronized void suspend() {
		
		// noop
		if (isSuspended)
			return;
			
		isSuspended = true;
			
		// cancel...
		if (monitorTimerTask != null)
		{
			// canceling is thradsafe (and can be done multiple times)
			BACITimer.cancel(monitorTimerTask);
			monitorTimerTask = null;
		}
			
	}

	/**
	 * @see alma.ACS.SubscriptionOperations#resume()
	 */
	public synchronized void resume() {

		// noop
		if (!isSuspended)
			return;
			
		// fix startTime to next schedule time... 
		if (!userControlledStartTime)
			alignStartTime();
		else if (timeTrigger > 0)
		{
			// consider user start time...
			long now = System.currentTimeMillis();
			startTime = startTime + ((now-startTime)/timeTrigger + 1) * timeTrigger;
		}
		
		// reschedule
		schedule();

		isSuspended = false;
	}

	/**
	 * @see alma.ACS.SubscriptionOperations#destroy()
	 */
	public synchronized void destroy() {
		
		// noop
		if (isDestroyed)
			return;
			
		isDestroyed = true;

		// remove listener not to be notified ever again
		if (dispatchAction != null)
			dispatchAction.removeDispatchFailedListener(this);

		if (!isSuspended)
			suspend();
			
		// dispatch done request
		retrieveValueAndDispatch(System.currentTimeMillis(), true);
			
		// remove from property monitor list
		property.unregisterMonitor(this);
		
	}

}
