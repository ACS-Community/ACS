package alma.ACS.impl;

import org.omg.CORBA.NO_IMPLEMENT;

import alma.ACS.AlarmlongLong;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBlong;
import alma.ACS.CBlongLong;
import alma.ACS.Callback;
import alma.ACS.MonitorlongLong;
import alma.ACS.MonitorlongLongHelper;
import alma.ACS.MonitorlongLongPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.ROlongLongOperations;
import alma.ACS.Subscription;
import alma.ACS.TimeSeqHolder;
import alma.ACS.longLongSeqHolder;
import alma.ACS.jbaci.CallbackDispatcher;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;

public class ROlongLongImpl
			extends ROCommonComparablePropertyImpl
			implements ROlongLongOperations {

	/**
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	public ROlongLongImpl(
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
	public ROlongLongImpl(
		String name,
		CharacteristicComponentImpl parentComponent,
		DataAccess dataAccess)
		throws PropertyInitializationFailed {
		super(long.class, name, parentComponent, dataAccess);
	}

	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#lessThanDelta(java.lang.Object,java.lang.Object,java.lang.Object)
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
		return new Long((long) (((Long)value1).longValue() + val2));
	}

	/**
	 * @see alma.ACS.CommonPropertyImpl#readPropertyTypeCharacteristic(java.lang.String)
	 */
	public Object readPropertyTypeCharacteristic(String name)
			throws NoSuchCharacteristic {
		return new Long(characteristicModelImpl.getLong(name));
	}

	/**
	 * @see alma.ACS.ROlongLongOperations#alarm_high_off()
	 */
	public long alarm_high_off() {
		return ((Long)alarmHighOff).longValue();
	}

	/**
	 * @see alma.ACS.ROlongLongOperations#alarm_high_on()
	 */
	public long alarm_high_on() {
		return ((Long)alarmHighOn).longValue();
	}

	/**
	 * @see alma.ACS.ROlongLongOperations#alarm_low_off()
	 */
	public long alarm_low_off() {
		return ((Long)alarmLowOff).longValue();
	}

	/**
	 * @see alma.ACS.ROlongLongOperations#alarm_low_on()
	 */
	public long alarm_low_on() {
		return ((Long)alarmLowOn).longValue();
	}

	/**
	 * @see alma.ACS.PlongOperations#new_subscription_Alarm(alma.ACS.AlarmlongLong, alma.ACS.CBDescIn)
	 */
	public Subscription new_subscription_Alarm(AlarmlongLong cb, CBDescIn desc) {
		throw new NO_IMPLEMENT();
	}

	/**
	 * @see alma.ACS.PlongOperations#create_monitor(alma.ACS.CBlongLong, alma.ACS.CBDescIn)
	 */
	public MonitorlongLong create_monitor(CBlongLong cb, CBDescIn desc) {
		return create_postponed_monitor(0, cb, desc);
	}

	/**
	 * @see alma.ACS.PlongOperations#create_postponed_monitor(long, alma.ACS.CBlongLong, alma.ACS.CBDescIn)
	 */
	public MonitorlongLong create_postponed_monitor(long start_time,
			CBlongLong cb, CBDescIn desc) {
		// create monitor and its servant
		MonitorlongLongImpl monitorImpl = new MonitorlongLongImpl(this, cb, desc, start_time);
		MonitorlongLongPOATie monitorTie = new MonitorlongLongPOATie(monitorImpl);

		// register and activate		
		return MonitorlongLongHelper.narrow(this.registerMonitor(monitorImpl, monitorTie));
	
	}

	/**
	 * @see alma.ACS.PlongLongOperations#default_value()
	 */
	public long default_value() {
		return ((Long)defaultValue).longValue();
	}

	/**
	 * @see alma.ACS.PlongLongOperations#get_async(alma.ACS.CBlongLong, alma.ACS.CBDescIn)
	 */
	public void get_async(CBlongLong cb, CBDescIn desc) {
		getAsync(cb, desc);
		
	}

	/**
	 * @see alma.ACS.PlongLongOperations#get_history(int, alma.ACS.longLongSeqHolder, alma.ACS.TimeSeqHolder)
	 */
	public int get_history(int n_last_values, longLongSeqHolder vs,
			TimeSeqHolder ts) {
		vs.value = (long[])getHistory(n_last_values, ts);
		return vs.value.length;
	}

	/**
	 * @see alma.ACS.PlongLongOperations#get_sync(alma.ACSErr.CompletionHolder)
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
	 * @see alma.ACS.PlongLongOperations#graph_max()
	 */
	public long graph_max() {
		return ((Long)graphMax).longValue();
	}

	/**
	 * @see alma.ACS.PlongLongOperations#graph_min()
	 */
	public long graph_min() {
		return ((Long)graphMin).longValue();
	}

	/**
	 * @see alma.ACS.PlongLongOperations#min_delta_trigger()
	 */
	public long min_delta_trigger() {
		return ((Long)minDeltaTrigger).longValue();
	}

	/**
	 * @see alma.ACS.PlongLongOperations#min_step()
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
				((CBlong)callback).done(((Integer)value).intValue(), completion, desc);
			else if (type == CallbackDispatcher.WORKING_TYPE)
				((CBlong)callback).working(((Integer)value).intValue(), completion, desc);
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
