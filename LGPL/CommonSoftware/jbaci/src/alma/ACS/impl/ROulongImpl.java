package alma.ACS.impl;

import org.omg.CORBA.NO_IMPLEMENT;

import alma.ACS.AlarmuLong;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBlong;
import alma.ACS.CBuLong;
import alma.ACS.Callback;
import alma.ACS.MonitorlongPOATie;
import alma.ACS.MonitoruLong;
import alma.ACS.MonitoruLongHelper;
import alma.ACS.MonitoruLongPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.ROuLongOperations;
import alma.ACS.Subscription;
import alma.ACS.TimeSeqHolder;
import alma.ACS.uLongSeqHolder;
import alma.ACS.jbaci.CallbackDispatcher;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;
import alma.baciErrTypeProperty.DisableAlarmsErrorEx;

/**
 * 
 * Implementation of <code>alma.ACS.ROulong</code>
 * 
 * @author javarias
 *
 */
public class ROulongImpl extends ROCommonComparablePropertyImpl implements ROuLongOperations {

	public ROulongImpl(String name, CharacteristicComponentImpl parentComponent)
			throws PropertyInitializationFailed {
		super(int.class, name, parentComponent);
	}

	public ROulongImpl(String name, CharacteristicComponentImpl parentComponent,
			DataAccess<? extends Number> dataAccess) throws PropertyInitializationFailed {
		super(int.class, name, parentComponent, dataAccess);
	}

	@Override
	public int min_delta_trigger() {
		return ((Integer)minDeltaTrigger).intValue();
	}

	@Override
	public int default_value() {
		return ((Integer)defaultValue).intValue();
	}

	@Override
	public int graph_min() {
		return ((Integer)graphMin).intValue();
	}

	@Override
	public int graph_max() {
		return ((Integer)graphMax).intValue();
	}

	@Override
	public int min_step() {
		return ((Integer)minStep).intValue();
	}

	@Override
	public int get_sync(CompletionHolder completionHolder) {
		try
		{
			return ((Integer)getSync(completionHolder)).intValue();
		}
		catch (AcsJException acsex)
		{
			AcsJCouldntPerformActionEx cpa =
				new AcsJCouldntPerformActionEx("Failed to retrieve value", acsex);
			completionHolder.value = CompletionUtil.generateCompletion(cpa);
			// return default value in case of error
			return default_value();
		}
	}

	@Override
	public void get_async(CBuLong cb, CBDescIn desc) {
		getAsync(cb, desc);
	}

	@Override
	public int get_history(int n_last_values, uLongSeqHolder vs, TimeSeqHolder ts) {
		vs.value = (int[])getHistory(n_last_values, ts);
		return vs.value.length;
	}

	@Override
	public MonitoruLong create_monitor(CBuLong cb, CBDescIn desc) {
		return create_postponed_monitor(0, cb, desc);
	}

	@Override
	public MonitoruLong create_postponed_monitor(long start_time, CBuLong cb, CBDescIn desc) {
		// create monitor and its servant
		MonitoruLongImpl monitorImpl = new MonitoruLongImpl(this, cb, desc, start_time);
		MonitoruLongPOATie monitorTie = new MonitoruLongPOATie(monitorImpl);

		// register and activate
		return MonitoruLongHelper.narrow(this.registerMonitor(monitorImpl, monitorTie));
	}

	@Override
	public boolean dispatchCallback(CallbackDispatcher.CallbackType type, Object value, Callback callback, Completion completion, CBDescOut desc) {
		try
		{	
			if (type == CallbackDispatcher.CallbackType.DONE_TYPE)
				((CBuLong)callback).done(((Integer)value).intValue(), completion, desc);
			else if (type == CallbackDispatcher.CallbackType.WORKING_TYPE)
				((CBuLong)callback).working(((Integer)value).intValue(), completion, desc);
			else 
				return false;
				
			return true;
		}
		catch (Throwable th)
		{
			return false;
		}
	}

	@Override
	public int alarm_low_on() {
		return ((Integer)alarmLowOn).intValue();
	}

	@Override
	public int alarm_low_off() {
		return ((Integer)alarmLowOff).intValue();
	}

	@Override
	public int alarm_high_on() {
		return ((Integer)alarmHighOn).intValue();
	}

	@Override
	public int alarm_high_off() {
		return ((Integer)alarmHighOff).intValue();
	}

	@Override
	public Subscription new_subscription_Alarm(AlarmuLong cb, CBDescIn desc) {
		// TODO NO_IMPLEMENT
		throw new NO_IMPLEMENT();
	}

	@Override
	public void enable_alarm_system() {
		// TODO NO_IMPLEMENT
		throw new NO_IMPLEMENT();

	}

	@Override
	public void disable_alarm_system() throws DisableAlarmsErrorEx {
		// TODO NO_IMPLEMENT
		throw new NO_IMPLEMENT();

	}

	@Override
	public boolean alarm_system_enabled() {
		// TODO NO_IMPLEMENT
		throw new NO_IMPLEMENT();
	}

	@Override
	public boolean lessThanDelta(Object value1, Object value2, Object delta) {
		return Math.abs(((Integer)value1).intValue()-((Integer)value2).intValue()) < ((Integer)delta).intValue();
	}

	@Override
	public boolean noDelta(Object value) {
		return ((Integer)value).intValue() == 0;
	}

	@Override
	public Object sum(Object value1, Object value2, boolean substract) {
		int val2 = ((Integer)value2).intValue();
		if (substract)
			val2 = -val2;
		return new Integer((int) (((Integer)value1).intValue() + val2));
	}

	@Override
	public Object readPropertyTypeCharacteristic(String name) throws NoSuchCharacteristic {
		return new Integer(characteristicModelImpl.getInteger(name));
	}

}
