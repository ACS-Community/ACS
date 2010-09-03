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

import java.lang.reflect.Array;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import java.util.logging.Level;
import java.util.logging.Logger;
import alma.acs.container.ContainerServices;


import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.NO_RESOURCES;
import org.omg.PortableServer.Servant;

import alma.ACS.CBDescIn;
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACS.Monitor;
import alma.ACS.MonitorHelper;
import alma.ACS.MonitorOperations;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.OffShootOperations;
import alma.ACS.TimeSeqHolder;
import alma.ACS.jbaci.BACIAction;
import alma.ACS.jbaci.BACIPriority;
import alma.ACS.jbaci.CallbackDispatcher;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.MemoryDataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnknownEx;
import alma.acs.exceptions.AcsJException;

/**
 * Implementation of common property, i.e. type of <code>java.lang.Object</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public abstract class CommonPropertyImpl
	extends TypelessPropertyImpl implements CallbackDispatcher
	{
	/**
	 * Logger variable
	 */
	private Logger m_logger;
	
	/**
	 * Default timer trigger (in 100ns units).
	 */
	protected long defaultTimerTrigger;

	/**
	 * Min timer trigger (in 100ns units).
	 */
	protected long minTimerTrigger;
		
	/**
	 * Default value.
	 */
	protected Object defaultValue;

	/**
	 * Read-only data access.
	 */
	protected DataAccess dataAccess;

	/**
	 * Property <code>Class</code> type. 
	 */
	protected Class propertyType;

	/**
	 * History size, if 0 history is disabled. 
	 */
	protected int historySize;

	/**
	 * Pointer in history arrays, points to first empty element. 
	 */
	protected int historyPosition;

	/**
	 * <code>true</code>, if values in history arrays exceed end of arrays (circular arrays). 
	 */
	protected boolean historyTurnaround;

	/**
	 * Array of history values.
	 */
	protected Object historyValue;

	/**
	 * Array of history times (OMG standard time).
	 */
	protected long[] historyTime;

	/**
	 * List of all property monitors (needed on property destruction).
	 */
	protected Map monitors;

	/************ [ Mnemonic value retrival mechanism ] ************/

	/**
	 * Mnemonic read time key of (last) pending read.
	 */
	protected long mnemonicReadPending = 0; 

	/**
	 * Mnemonic read lock (dummy object).
	 */
	protected Object mnemonicValueRetrival = new Object();

	/**
	 * Mnemonic variables lock.
	 */
	protected ReadWriteLock mnemonicDataLock = 
		new ReentrantReadWriteLock();

	/**
	 * Time "key" (Java) if when last mnemonic retrival. 
	 */
	protected long mnemonicTime;
	
	/**
	 * Value of latest mnemonic value retrival.
	 */
	protected Object mnemonicValue;
	
	/**
	 * Completion of latest mnemonic value retrival.
	 */
	protected Completion mnemonicCompletion; 

	/***************************************************************/

	/**
	 * Constructor with memory data access.
	 * @param propertyType		property <code>Class</code> type, non-<code>null</code>.
	 * @param name				property name, non-<code>null</code>.
	 * @param parentComponent	parent component, non-<code>null</code>.
	 * @throws PropertyInitializationFailed exception is thrown on failure
	 */
	public CommonPropertyImpl(
		Class propertyType,
		String name,
		CharacteristicComponentImpl parentComponent)
		throws PropertyInitializationFailed {
		
		this(propertyType, name, parentComponent, new MemoryDataAccess());
		m_logger = parentComponent.getComponentContainerServices().getLogger();
	}

	/**
	 * Constructor.
	 * @param propertyType		property <code>Class</code> type, non-<code>null</code>.
	 * @param name				property name, non-<code>null</code>.
	 * @param parentComponent	parent component, non-<code>null</code>.
	 * @param dataAccess		read-write data access to be use, non-<code>null</code>.
	 * @throws PropertyInitializationFailed exception is thrown on failure
	 */
	public CommonPropertyImpl(
		Class propertyType,
		String name,
		CharacteristicComponentImpl parentComponent,
		DataAccess dataAccess)
		throws PropertyInitializationFailed {
		super(name, parentComponent);
		
		this.propertyType = propertyType;
		this.dataAccess = dataAccess;
	
		readCharacteristics();
	
		m_logger = parentComponent.getComponentContainerServices().getLogger();
		// TODO to be configurable
		historySize = 32;
		
		historyPosition = 0;
		historyTurnaround = false;
		historyTime = new long[historySize];
		historyValue = Array.newInstance(propertyType, historySize);
		
		// initialize data access, if required
		if (dataAccess.initializeValue())
		{
			try
			{
				CompletionHolder completionHolder = CompletionUtil.createCompletionHolder();
				dataAccess.set(defaultValue, completionHolder);
			
			}
			catch (Throwable th)
			{

				m_logger.log(Level.WARNING, "jBaci::CommonPropertyImpl::CommonPropertyImpl - Cannot create Completion Holder");
				throw new NO_RESOURCES(th.getMessage());
			}
		}
		
		// create monitor data structure
		monitors = new HashMap();
		
		// create history monitor
		registerNonCorbaMonitor(new HistoryMonitorImpl(this));
		
	}

	/**
	 * Read property characteristics.
	 * @throws PropertyInitializationFailed exception is thrown on failure
	 */
	public void readCharacteristics()
		throws PropertyInitializationFailed {
		super.readCharacteristics();
		
		try
		{
			// defaultTimerTrigger = characteristicModelImpl.getLong("default_timer_trig");
			// minTimerTrigger = characteristicModelImpl.getLong("min_timer_trig");
			defaultTimerTrigger = (long)(characteristicModelImpl.getDouble("default_timer_trig")*10000000L);
			minTimerTrigger = (long)(characteristicModelImpl.getDouble("min_timer_trig")*10000000L);
			defaultValue = readPropertyTypeCharacteristic("default_value");
		}
		catch (Throwable t)
		{
			throw new PropertyInitializationFailed("Failed to read all property characteristics.", t);
		}
	}

	/**
	 * Get property data access layer.
	 * @return	property data access.
	 */
	public DataAccess getDataAccess()
	{
		return dataAccess;
	}

	/**
	 * @see alma.ACS.PropertyImpl#destroy()
	 */
	public void destroy() {
		super.destroy();

		// destroy all monitors
		if (monitors.size() != 0)
		{
			MonitorOperations[] monitorArray = null;
			synchronized (monitors)
			{
				monitorArray = new MonitorOperations[monitors.size()];
				monitors.keySet().toArray(monitorArray);
			}
			for (int i = 0; i < monitorArray.length; i++)
			{
				try
				{
					monitorArray[i].destroy();
				}
				catch (Throwable th)
				{
					// TODO log
					m_logger.log(Level.WARNING, "jBaci::CommonPropertyImpl::destroy - cannot destroy monitorArray[].");
					throw new NO_RESOURCES(th.getMessage());
				}
			}
		}

	}

	/*********************** [ P<type> ] ***********************/

	/**
	 * @see alma.ACS.P<type>Operations#default_timer_trigger()
	 */
	public long default_timer_trigger() {
		return defaultTimerTrigger;
	}

	/**
	 * @see alma.ACS.P<type>Operations#min_timer_trigger()
	 */
	public long min_timer_trigger() {
		return minTimerTrigger;
	}

	/*********************** [ P<type> helpers ] ***********************/

	/**
	 * Read property type characteristic.
	 * @throws NoSuchCharacteristic is thrown if characterstic does not exist.
	 */
	public abstract Object readPropertyTypeCharacteristic(String name) throws NoSuchCharacteristic;

	/**
	 * Add value to the history.
	 * @param value		value to be added to the history.
	 * @param timestamp	value timestamp (OMG) to be added to the history.
	 */
	protected void addValueToHistory(Object value, long timestamp)
	{
		// if history is disabled
		if (historySize == 0)
			return;
		

		synchronized (historyValue)
		{
			// add			
			historyTime[historyPosition] = timestamp;


			if (propertyType.isPrimitive())
			{

			
				if(propertyType.isAssignableFrom(double.class))
				     Array.setDouble(historyValue, historyPosition, ((Double)value).doubleValue());
					
				else if (propertyType.isAssignableFrom(int.class))
					Array.setInt(historyValue, historyPosition, ((Integer)value).intValue());
					
				else if (propertyType.isAssignableFrom(long.class))
					Array.setLong(historyValue, historyPosition, ((Long)value).longValue());

				else if (propertyType.isAssignableFrom(short.class))
					Array.setShort(historyValue, historyPosition, ((Short)value).shortValue());
					
				else if (propertyType.isAssignableFrom(boolean.class))
					Array.setBoolean(historyValue, historyPosition, ((Boolean)value).booleanValue());

				else if (propertyType.isAssignableFrom(byte.class))
					Array.setByte(historyValue, historyPosition, ((Byte)value).byteValue());

				else if (propertyType.isAssignableFrom(float.class))
					Array.setFloat(historyValue, historyPosition, ((Float)value).floatValue());

				else if (propertyType.isAssignableFrom(char.class))
					Array.setChar(historyValue, historyPosition, ((Character)value).charValue());
					
				else
				{
					m_logger.log(Level.WARNING, "jBaci::CommonPropertyImpl::addValueToHistory - Unhandled primitive.");
				    throw new NO_RESOURCES("Unhandled primitive"); 
				}
					
			}
			else
				Array.set(historyValue, historyPosition, value);
			
			// manage control variables
			historyPosition = ++historyPosition % historySize;
			if (!historyTurnaround && historyPosition == 0) 
				historyTurnaround = true;
		}
	}

	protected Object getHistory(int lastValues, TimeSeqHolder timeSeqHolder)
	{
		// check bad parameter
		if (lastValues < 0)
			throw new BAD_PARAM("lastValues < 0");
 
		synchronized (historyValue)
		{
			int length, first;
			
			if (historyTurnaround)
		  	{
				length = historySize;
				first = historyPosition;
		  	}
			else
		  	{
				length = historyPosition;
				first = 0;
			}

			// last n values (and not first n values)
			if (lastValues > length)
				lastValues = length;
			first = (first + length - lastValues) % historySize;

			// get required number of values
			if (lastValues < length)
				length = lastValues;

			timeSeqHolder.value = new long[length];
			Object values = Array.newInstance(propertyType, length);

			// no history case
			if (length == 0)
				return values;

			// copy
			if (first + length < historySize)
			{
				System.arraycopy(historyTime, first, timeSeqHolder.value, 0,  length);
				System.arraycopy(historyValue, first, values, 0,  length);
			}
			else
			{
				int split = historySize - first;
				System.arraycopy(historyTime, first, timeSeqHolder.value, 0,  split);
				System.arraycopy(historyValue, first, values, 0,  split);
				System.arraycopy(historyTime, 0, timeSeqHolder.value, split, length - split);
				System.arraycopy(historyValue, 0, values, split,  length - split);
			}

			return values;
	  	}
		
	}

	/**
	 * @see alma.ACS.P<type>Operations#get_sync(alma.ACSErr.CompletionHolder)
	 */
	protected Object getSync(CompletionHolder completionHolder) throws AcsJException {
		try
		{
			Object retVal = dataAccess.get(completionHolder);
			// generate no-error completion, if not generated
			if (completionHolder.value == null)
				completionHolder.value = CompletionUtil.generateNoErrorCompletion();
			return retVal;
		}
		catch (AcsJException acsex)
		{
			throw new AcsJCouldntPerformActionEx("Failed to retrieve value.", acsex);
		}
	}


	/**
	 * BACI action to invoke <code>getSync</code> asynchroniously.
	 */
	protected class GetAsyncAction extends BACIAction
	{ 
		/**
		 * @see alma.ACS.jbaci.BACIAction
		 */
		public GetAsyncAction(Callback callback, CBDescIn descIn) {
			super(getParentComponent(), callback, descIn, CommonPropertyImpl.this);
		}

		/**
		 * @see alma.ACS.jbaci.BACIAction
		 */
		public GetAsyncAction(
			Callback callback,
			CBDescIn descIn,
			BACIPriority priority) {
			super(getParentComponent(), callback, descIn, CommonPropertyImpl.this, priority);
		}

		/**
		 * @see alma.ACS.jbaci.BACIAction#execute()
		 */
		public Object execute() throws AcsJException {
			try
			{
				CompletionHolder completionHolder = CompletionUtil.createCompletionHolder();
				Object retVal = getSync(completionHolder);
				// set completion 
				this.completion = completionHolder.value;
				return retVal;
			}
			catch (AcsJException acsex)
			{
				// send default value in case of failure
				this.returnValue = defaultValue;
				throw new AcsJCouldntPerformActionEx("Failed to retrieve value asynchroniously.", acsex);
			}
		}
	}

	/**
	 * @see alma.ACS.P<type>Operations#get_async(alma.ACS.CB<type>, alma.ACS.CBDescIn)
	 */
	protected void getAsync(Callback callback, CBDescIn desc) {
		new GetAsyncAction(callback, desc).submit();
	}

	
	/**
	 * Register monitor on this property (and optionally CORBA activate).
	 * Registration is needed for monitor destruction on property destruction.
	 * @param monitorImpl		monitor implementation (e.g. class implementing <code>MonitorOperations</code> interface).
	 * @param monitorServant	monitor CORBA servant (e.g. Monitor<type>POATie class). If <code>null</code> monitor will
	 * 						be treated as non-CORBA monitor and no CORBA activation will be done.
	 * 						(Not sure how to correctly pass a null, now that the method uses generics. See {@link #registerNonCorbaMonitor(MonitorOperations)}
	 * 						for an alternative call.)
	 * @return CORBA activated monitor reference, <code>null</code> if <code>monitorServant == null</code>.
	 */
	public <T extends Servant & OffShootOperations> Monitor registerMonitor(MonitorOperations monitorImpl, T monitorServant) {

		// allow activation if already in monitor list...
		Monitor monitor = null;
		if (monitorServant != null)
		{
			try
			{
				// TODO pesistent, user ID activation
				monitor = MonitorHelper.narrow(
					parentComponent.getComponentContainerServices().activateOffShoot(monitorServant)
				);
			}
			catch (Throwable th)
			{

				m_logger.log(Level.WARNING, "jBaci::CommonPropertyImpl::registerMonitor - Cannot activate Off Shoot with the monitorServant.");
				throw new NO_RESOURCES(th.getMessage());
			}
		}

		// add to list
		synchronized (monitors)
		{
			if (!monitors.containsKey(monitorImpl))
				monitors.put(monitorImpl, monitorServant);
		}
		
		return monitor;
		
	}

	/**
	 * Register monitor on this property, without corba activation.
	 * Registration is needed for monitor destruction on property destruction.
	 * <p>
	 * Note that this method was broken out from {@link #registerMonitor(MonitorOperations, Servant)}
	 * to avoid passing null as the second argument of that method, which was in conflict with the generics
	 * definition, which in turn was added there to match the one of ContainerServices.activateOffshoot.
	 * 
	 * @param monitorImpl		monitor implementation (e.g. class implementing <code>MonitorOperations</code> interface).
	 * @return CORBA activated monitor reference, <code>null</code> if <code>monitorServant == null</code>.
	 */
	public Monitor registerNonCorbaMonitor(MonitorOperations monitorImpl) {

		// allow activation if already in monitor list...
		Monitor monitor = null;

		// add to list
		synchronized (monitors)
		{
			if (!monitors.containsKey(monitorImpl))
				monitors.put(monitorImpl, null);
		}
		
		return monitor;
		
	}

	/**
	 * Unregister monitor on this property (and optionally CORBA deactivate).
	 * Should be called by <code>MonitorOperations.destroy()</code> method.
	 */
	public void unregisterMonitor(MonitorOperations monitorImpl) {
		
		Servant monitorServant = null;
		
		// remove from list
		synchronized (monitors)
		{
			monitorServant = (Servant)monitors.remove(monitorImpl);
		}
		
		// deativate CORBA monitor servant
		if (monitorServant != null)
		{
			try
			{
				parentComponent.getComponentContainerServices().deactivateOffShoot(monitorServant);
			}
			catch (Throwable th)
			{

				m_logger.log(Level.WARNING, "jBaci::CommonPropertyImpl::unregisterMonitor - Cannot deactivate Off Shoot with monitorServant");
				throw new NO_RESOURCES(th.getMessage());
			}
		}
	}

	/*********************** [ RW<type> helpers ] ***********************/

	/**
	 * @see alma.ACSErr.Completion alma.ACS.RW<type>Operations#set_sync(<type>)
	 */
	protected Completion setSync(Object value) throws AcsJException {
		// TODO should we check limits here
		try
		{
			CompletionHolder completionHolder = CompletionUtil.createCompletionHolder();
			dataAccess.set(value, completionHolder);
			// generate no-error completion, if not generated
			if (completionHolder.value == null)
				completionHolder.value = CompletionUtil.generateNoErrorCompletion();
			return completionHolder.value;
		}
		catch (AcsJException acsex)
		{
			throw new AcsJCouldntPerformActionEx("Failed to set value.", acsex);
		}
	}

	/**
	 * BACI action to invoke <code>setSync</code> asynchroniously.
	 */
	protected class SetAsyncAction extends BACIAction
	{ 
		/**
		 * Value to be set.
		 */
		Object value;
		
		/**
		 * @see alma.ACS.jbaci.BACIAction
		 */
		public SetAsyncAction(Object value, Callback callback, CBDescIn descIn) {
			super(getParentComponent(), callback, descIn, null);
			this.value = value;
		}

		/**
		 * @see alma.ACS.jbaci.BACIAction
		 */
		public SetAsyncAction(
			Object value, 
			Callback callback,
			CBDescIn descIn,
			BACIPriority priority) {
			super(getParentComponent(), callback, descIn, null, priority);
			this.value = value;
		}

		/**
		 * @see alma.ACS.jbaci.BACIAction#execute()
		 */
		public Object execute() throws AcsJException {
			try
			{
				completion = setSync(value);
			}
			catch (AcsJException acsex)
			{
				throw new AcsJCouldntPerformActionEx("Failed to set value asynchroniously.", acsex);
			}
			
			// no return value
			return null;
		}
	}

	/**
	 * @see alma.ACS.RW<type>Operations#get_async(<type>, alma.ACS.CBvoid, alma.ACS.CBDescIn)
	 */
	protected void setAsync(Object value, CBvoid callback, CBDescIn desc) {
		new SetAsyncAction(value, callback, desc).submit();
	}

	/**
	 * @see void alma.ACS.RW<type>Operations#set_nonblocking(<type>)
	 */
	protected void setNonblocking(Object value) {
		try
		{
			setSync(value);
		}
		catch (Throwable th) 
		{
			// TODO log
			m_logger.log(Level.WARNING, "jBaci::CommonPropertyImpl::setNonblocking - Cannot setSync the value.");
			throw new NO_RESOURCES(th.getMessage());
		}
	}

	/**
	 * Mnemonic value retrival.
	 * If <code>keyTime == mnemonicTime</code> cached mnemonic value is returned.
	 * @param keyTime	time (java) of mnemonic request.
	 * @param completionHolder completion holder that will be given completion.
	 * 						   NOTE: completion is passsed by reference, so do not change its value,
	 * 						   copy its value before and do it on a local copy
	 * @return current property value.
	 * @see getSync
	 */
	// TODO implement test with many threads calling this method... 
	public Object mnemonicValue(long keyTime, CompletionHolder completionHolder) {
		
		//
		// cache lookup
		//

		for (;;)
		{
		
			// read mnemonic data lock
			try	{ mnemonicDataLock.readLock().lock(); }
			catch (Throwable th)	{ 
				completionHolder.value = mnemonicCompletion; return mnemonicValue; /* return old */
			}
			try
			{
				// if same time or newer exist return cached value
				if (keyTime <= mnemonicTime)
				{
					completionHolder.value = mnemonicCompletion;
					return mnemonicValue;
				}
			}
			finally
			{
				mnemonicDataLock.readLock().unlock();
			}
			
			// read value wait, if reading is already pending			
			synchronized (mnemonicValueRetrival)
			{
			
				// lock if not newer read
				if (keyTime <= mnemonicReadPending)
				{
					try
					{ 
						mnemonicValueRetrival.wait();
					}
					catch (InterruptedException ie)	{}
					
				 	// re-read again
				 	continue;
				}
				else
				{
					mnemonicReadPending = keyTime;
					break;
				}
			}

		}
		
		//		
		// value retrival
		//

		Object retValue = null;
		try
		{
			retValue = getSync(completionHolder);
		}
		catch (AcsJException acsex)
		{
			retValue = defaultValue;
			completionHolder.value = CompletionUtil.generateCompletion(acsex);
		}
		catch (Throwable th) 
		{
			retValue = defaultValue;
			completionHolder.value = CompletionUtil.generateCompletion(
										new AcsJUnknownEx("Failed to retrieve value.", th));
		}
		
		//
		// value memorization
		//
		
		// write mnemonic data lock
		try	{ mnemonicDataLock.writeLock().lock(); }
		catch (Throwable th)	{ return mnemonicValue; /* return old */ }
		try
		{
			if (keyTime > mnemonicTime)
			{
				mnemonicTime = keyTime;
				mnemonicValue = retValue;
				mnemonicCompletion = completionHolder.value;
			}
			return mnemonicValue;
		}
		finally
		{
			mnemonicDataLock.writeLock().unlock();

			// read value wait release
			synchronized (mnemonicValueRetrival)
			{
				mnemonicValueRetrival.notifyAll();
			}
		
		}
			
	}

}
