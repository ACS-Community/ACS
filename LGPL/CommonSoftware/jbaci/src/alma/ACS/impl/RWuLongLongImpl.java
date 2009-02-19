package alma.ACS.impl;

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBlong;
import alma.ACS.CBuLongLong;
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACS.MonitoruLongLong;
import alma.ACS.MonitoruLongLongHelper;
import alma.ACS.MonitoruLongLongPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.RWuLongLongOperations;
import alma.ACS.TimeSeqHolder;
import alma.ACS.uLongLongSeqHolder;
import alma.ACS.jbaci.CallbackDispatcher;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;

public class RWuLongLongImpl 
			extends RWCommonComparablePropertyImpl
			implements RWuLongLongOperations{

	/**
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	public RWuLongLongImpl(
		String name,
		CharacteristicComponentImpl parentComponent)
		throws PropertyInitializationFailed {
		super(long.class, name, parentComponent);
	}

	/**
	 * @param name
	 * @param parentComponent
	 * @param dataAccess
	 * @throws PropertyInitializationFailed
	 */
	public RWuLongLongImpl(
		String name,
		CharacteristicComponentImpl parentComponent,
		DataAccess dataAccess)
		throws PropertyInitializationFailed {
		super(long.class, name, parentComponent, dataAccess);
	}

	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#lessThanDelta(java.lang.Object, java.lang.Object, java.lang.Object)
	 */
	public boolean lessThanDelta(Object value1, Object value2, Object delta) {
		return Math.abs(((Long)value1).longValue()-((Long)value2).longValue()) < ((Long)delta).longValue();
	}

	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#noDelta(java.lang.Object)
	 */
	public boolean noDelta(Object value) {
		return ((Long)value).longValue() == 0;
	}

	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#sum(java.lang.Object, java.lang.Object, boolean)
	 */
	public Object sum(Object value1, Object value2, boolean substract) {
		long val2 = ((Long)value2).longValue();
		if (substract)
			val2 = -val2;
		return new Long(((Long)value1).longValue() + val2);
	}

	/**
	 * @see alma.ACS.CommonPropertyImpl#readPropertyTypeCharacteristic(java.lang.String)
	 */
	public Object readPropertyTypeCharacteristic(String name)
			throws NoSuchCharacteristic {
		return new Long(characteristicModelImpl.getLong(name));

	}

	/**
	 * @see alma.ACS.RWuLongLongOperations#max_value()
	 */
	public long max_value() {
		return ((Long)maxValue).longValue();
	}

	/**
	 * @see alma.ACS.RWuLongLongOperations#min_value()
	 */
	public long min_value() {
		return ((Long)minValue).longValue();
	}

	
	/**
	 * @see alma.ACS.RWlongOperations#set_async(long, alma.ACS.CBvoid, alma.ACS.CBDescIn)
	 */
	public void set_async(long value, CBvoid callback, CBDescIn descIn) {
		setAsync(new Long(value), callback, descIn);
	}

	
	public void set_nonblocking(long value) {
		setNonblocking(new Long(value));
		
	}

	/**
	 * @see alma.ACS.RWuLongLongOperations#set_sync(long)
	 */
	public Completion set_sync(long value) {
		try
		{
			return setSync(new Long(value));
		}
		catch (AcsJException acsex)
		{
			AcsJCouldntPerformActionEx cpa =
				new AcsJCouldntPerformActionEx("Failed to set value", acsex);
			return CompletionUtil.generateCompletion(cpa);
		}
	}

	/**
	 * @see alma.ACS.PlongOperations#create_monitor(alma.ACS.CBlong, alma.ACS.CBDescIn)
	 */
	public MonitoruLongLong create_monitor(CBuLongLong cb, CBDescIn desc) {
		return create_postponed_monitor(0, cb, desc);
	}

	/**
	 * @see alma.ACS.PlongOperations#create_postponed_monitor(long, alma.ACS.CBlong, alma.ACS.CBDescIn)
	 */
	public MonitoruLongLong create_postponed_monitor(long start_time,
			CBuLongLong cb, CBDescIn desc) {
		// create monitor and its servant
		MonitoruLongLongImpl monitorImpl = new MonitoruLongLongImpl(this, cb, desc, start_time);
		MonitoruLongLongPOATie monitorTie = new MonitoruLongLongPOATie(monitorImpl);

		// register and activate		
		return MonitoruLongLongHelper.narrow(this.registerMonitor(monitorImpl, monitorTie));
	
	}

	/**
	 * @see alma.ACS.PuLongLongOperations#default_value()
	 */
	public long default_value() {
		return ((Long)defaultValue).longValue();
	}

	/**
	 * @see alma.ACS.PuLongLongOperations#get_async(alma.ACS.CBlong, alma.ACS.CBDescIn)
	 */
	public void get_async(CBuLongLong cb, CBDescIn desc) {
		getAsync(cb, desc);
	}

	/**
	 * @see alma.ACS.PuLongLongOperations#get_history(int, alma.ACS.longSeqHolder, alma.ACS.TimeSeqHolder)
	 */
	public int get_history(int n_last_values, uLongLongSeqHolder vs,
			TimeSeqHolder ts) {
		vs.value = (long[])getHistory(n_last_values, ts);
		return vs.value.length;
	}

	/**
	 * @see alma.ACS.PuLongLongOperations#get_sync(alma.ACSErr.CompletionHolder)
	 */
	public long get_sync(CompletionHolder c) {
		try
		{
			return ((Long)getSync(c)).longValue();
		}
		catch (AcsJException acsex)
		{
			AcsJCouldntPerformActionEx cpa =
				new AcsJCouldntPerformActionEx("Failed to retrieve value", acsex);
			c.value = CompletionUtil.generateCompletion(cpa);
			// return default value in case of error
			return default_value();
		}
	}

	/**
	 * @see alma.ACS.PuLongLongOperations#graph_min()
	 */
	public long graph_max() {
		return ((Long)graphMax).longValue();
	}

	/**
	 * @see alma.ACS.PuLongLongOperations#graph_min()
	 */
	public long graph_min() {
		return ((Long)graphMin).longValue();
	}

	/**
	 * @see alma.ACS.PuLongLongOperations#min_delta_trigger()
	 */
	public long min_delta_trigger() {
		return ((Long)minDeltaTrigger).longValue();
	}

	/**
	 * @see alma.ACS.PuLongLongOperations#min_step()
	 */	
	public long min_step() {
		return ((Long)minStep).longValue();
	}

	/**
	 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACSErr.Completion, alma.ACS.CBDescOut)
	 */
	public boolean dispatchCallback(int type, Object value, Callback callback,
			Completion completion, CBDescOut desc) {
		try
		{	
			if (type == CallbackDispatcher.DONE_TYPE)
				((CBlong)callback).done((int) ((Long)value).longValue(), completion, desc);
			else if (type == CallbackDispatcher.WORKING_TYPE)
				((CBlong)callback).working((int) ((Long)value).longValue(), completion, desc);
			else 
				return false;
				
			return true;
		}
		catch (Throwable th)
		{
			return false;
		}
	}
	
	
}
