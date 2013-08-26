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

import org.omg.CORBA.BooleanHolder;

import alma.ACS.CBDescIn;
import alma.ACS.Callback;
import alma.ACS.jbaci.BACIFramework;
import alma.ACS.jbaci.BACIPriority;
import alma.ACS.jbaci.BACITimer;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.PrioritizedRunnable;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;

/**
 * Implementation of common compareable (notifies on change) monitor.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class CommonComparableMonitorImpl 
	extends CommonMonitorImpl
	implements DataAccess.ValueChangeListener {

	/**
	 * Pool timer implementation.
	 */
	protected class PoolTimer implements BACITimer.TimerRunnable, PrioritizedRunnable
	{
		/**
		 * Key time to process, 0 means none.
		 */
		protected AtomicLong queuedKeyPoolTime = new AtomicLong(0);

		/**
		 * @see alma.ACS.jbaci.BACITimer.TimerRunnable#timeout(long)
		 */
		public void timeout(long timeToRun) {
			// if none is queued, initiate executor otherwise override old value 
			if (queuedKeyPoolTime.getAndSet(timeToRun) == 0)
			{
				property.getParentComponent().execute(this);
			}
		}
		/**
		 * @see alma.ACS.jbaci.PrioritizedRunnable#getPriority()
		 */
		public BACIPriority getPriority() {
			return CommonComparableMonitorImpl.this.getPriority();
		}

		/**
		 * @see java.lang.Runnable#run()
		 */
		public void run() {
			// TODO should be better to ACK (set to 0) at the end?
			long keyTime = queuedKeyPoolTime.getAndSet(0);

			// create new holder (done expeditiously)
			CompletionHolder completionHolder = CompletionUtil.createCompletionHolder();
		
			// TODO completion....
		
			// retrieve value
			Object value = property.mnemonicValue(keyTime, completionHolder);
			valueChanged(property.getDataAccess(), oldValue, value /*, completion */);
		}

	}

	/**
	 * Current required delta.
	 */
	protected Object deltaValue;

	/**
	 * Flag if notification has to be done on every value change.
	 * NOTE: this mode is explicitly used for non-comparable properties,
	 * i.e. properties is implementing <code>CommonComparablePropertyImpl</code>.
	 */
	protected boolean onEveryChange;

	/**
	 * Cache - casted <code>property</code> to <code>CommonComparablePropertyImpl</code>. 
	 */
	protected CommonComparablePropertyImpl comparableProperty;

	/**
	 * Last pooled (notified) value.
	 */
	protected Object oldValue;

	/**
	 * Enabled status (might be true even if disabled (if suspended)) - user status.
	 */
	protected boolean enabled;

	/**
	 * Monitor timer task.
	 */
	protected Object poolTimerTask;

	/**
	 * Default pool time (if on-change notification are not supported) in ms.
	 */
	public static final long DEFAULT_POOL_TIME = 1000;

	/**
	 * Constructor with immediate monitor notification (synchronized monitors supported).
	 * @param property	property to be monitored, non-<code>null</code>.
	 * @param callback	callback, non-<code>null</code>.
	 * @param descIn	callback in-descriptor.
	 */
	public CommonComparableMonitorImpl(CommonPropertyImpl property, Callback callback, CBDescIn descIn) {
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
	public CommonComparableMonitorImpl(
		CommonPropertyImpl property,
		Callback callback,
		CBDescIn descIn,
		long startTime) {
		super(property, callback, descIn, startTime);
		enabled = false;
		
		if (property instanceof CommonComparablePropertyImpl)
			comparableProperty = (CommonComparablePropertyImpl)property;
	}

	/**
	 * Enable on-change monitoring/pooling.
	 */
	protected synchronized void enable()
	{
		// if suspended, do not really enable
		if (isSuspended)
		{
			enabled = true;
			return;
		}
		
		// disable first (on-change support might be added, recommendedPoolTime can be changed) 
		disable();
		
		try
		{
			property.getDataAccess().addValueChangeListener(this);
		}
		catch (DataAccess.OnChangeNotSupportedException ex)
		{
			// get pool time
			long poolTime = ex.recommendedPoolTime();
			if (poolTime <= 0 || poolTime < property.minTimerTrigger)
				poolTime = DEFAULT_POOL_TIME;

			// get current value to have a value to compare with...
			oldValue = property.mnemonicValue(System.currentTimeMillis(),
											  CompletionUtil.createCompletionHolder());
			
			// determine start time to align with sync monitors - might improve performance
			long startTime = (System.currentTimeMillis()/poolTime + 1) * poolTime;
			// pool
			poolTimerTask = BACIFramework.INSTANCE.getTimer().executePeriodically(poolTime, new PoolTimer(), startTime);
		}
		
		enabled = true;
	}
	
	/**
	 * Disable on-change monitoring/pooling. 
	 */
	protected synchronized void disable()
	{
		// cancel first...
		if (poolTimerTask != null)
		{
			// canceling is thread-safe (and can be done multiple times)
			BACITimer.cancel(poolTimerTask);
			poolTimerTask = null;
		}
		else
			// remove on-change listener
			property.getDataAccess().removeValueChangeListener(this);

		enabled = false;
	}

	/**
	 * @see alma.ACS.jbaci.DataAccess.ValueChangeListener#valueChanged(alma.ACS.jbaci.DataAccess, java.lang.Object, java.lang.Object)
	 */
	public void valueChanged(
		DataAccess source,
		Object oldValue,
		Object newValue) {

		if (onEveryChange)
		{
			// if equals, return
			if (oldValue.equals(newValue))
				return;
		}
		// dispatch, if equal or more than delta
		// this code should not never be reached for non-comparable properties
		else if (comparableProperty.lessThanDelta(oldValue, newValue, deltaValue))
			return;
		
		// set new 'oldValue'
		this.oldValue = newValue;

		// TODO completion
		Completion completion = CompletionUtil.generateNoErrorCompletion();
	
		// TODO 
		// set monitor type and on-change code, if non-error type
		//completion.code = ACSErr.ACSErrMonitorOnChange;
	
		// ... and dispatch
		dispatchAction.dispatchWorkingRequest(completion, newValue);

		// TODO remove
		//System.out.println("Dispatched (on-change) monitor: " + newValue);	
	}

	/**
	 * @see alma.ACS.SubscriptionOperations#resume()
	 */
	public synchronized void resume() {
		boolean suspended = isSuspended;
		super.resume();

		// revive monitoring/pooling, if necessary
		if (suspended && enabled)
			enable();
	}

	/**
	 * @see alma.ACS.SubscriptionOperations#suspend()
	 */
	public synchronized void suspend() {
		boolean suspended = isSuspended;
		super.suspend();
		
		// store current (user) status
		boolean enabledStatus = enabled;
		if (!suspended)
			disable();
		// restore current (user) status
		enabled = enabledStatus; 
	}

	/*********************** [ Monitor<type> helpers ] ***********************/

	/**
	 * Enable on-change monitoring.
	 * @param	enable	switch to enable/disable
	 * @see alma.ACS.Monitor<type>Operations#set_value_trigger(<type>, boolean)
	 */
	public synchronized void setValueTrigger(boolean enable) {
		setValueTrigger(null, enable);
	}

	/**
	 * Enable on-delta-change monitoring, requires to operatie on <code>CommonComparableProperty</code>.
	 * @param	delta	delta value, non-<code>null</code>.
	 * @param	enable	switch to enable/disable
	 * @see alma.ACS.Monitor<type>Operations#set_value_trigger(<type>, boolean)
	 */
	public synchronized void setValueTrigger(Object delta, boolean enable) {
		
		// disable, if changing values...
		disable();
		
		if (delta == null)
			onEveryChange = true;
		else
		{
			if (comparableProperty != null)
			{			
				// Value 0 assigned to delta trigger means that a notification should be sent
				// on every change of the monitored value.
				onEveryChange = comparableProperty.noDelta(delta);
				this.deltaValue = delta;
			}
			else
			{
				// TODO issue warning
				onEveryChange = true;
			}
		}

		if (enable)
		{
			enable();
		}
		else
		{
			disable();
		}
	}

	/**
	 * @param  enableHolder holder to be set current 'enable' status of the monitor. 
	 * @return deltaValue	current delta value, can be <code>null</code>.
	 * @see alma.ACS.Monitor<type>Operations#get_value_trigger(org.omg.CORBA.<type>Holder, org.omg.CORBA.BooleanHolder)
	 */
	public Object getValueTrigger(BooleanHolder enableHolder) {
		// not perfectly thread safe...
		enableHolder.value = enabled;
		return deltaValue;
	}

}
