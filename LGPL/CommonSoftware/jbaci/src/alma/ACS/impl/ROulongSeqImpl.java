package alma.ACS.impl;

import org.omg.CORBA.NO_IMPLEMENT;

import alma.ACS.AlarmuLong;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBlongSeq;
import alma.ACS.CBuLongSeq;
import alma.ACS.Callback;
import alma.ACS.MonitorlongPOATie;
import alma.ACS.MonitoruLong;
import alma.ACS.MonitoruLongHelper;
import alma.ACS.MonitoruLongPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.ROuLongSeqOperations;
import alma.ACS.Subscription;
import alma.ACS.TimeSeqHolder;
import alma.ACS.uLongSeqSeqHolder;
import alma.ACS.jbaci.CallbackDispatcher;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;
import alma.baciErrTypeProperty.DisableAlarmsErrorEx;

public class ROulongSeqImpl extends ROCommonComparablePropertyImpl implements ROuLongSeqOperations {

	
	
	public ROulongSeqImpl(String name, CharacteristicComponentImpl parentComponent,
			DataAccess dataAccess) throws PropertyInitializationFailed {
		super(int[].class, name, parentComponent, dataAccess);
	}

	public ROulongSeqImpl(String name, CharacteristicComponentImpl parentComponent)
			throws PropertyInitializationFailed {
		super(int[].class, name, parentComponent);
	}

	@Override
	public int min_delta_trigger() {
		return ((int[])minDeltaTrigger)[0];
	}

	@Override
	public int default_value() {
		return ((int[])defaultValue)[0];
	}

	@Override
	public int graph_min() {
		return ((int[])graphMin)[0];
	}

	@Override
	public int graph_max() {
		return ((int[])graphMax)[0];
	}

	@Override
	public int min_step() {
		return ((int[])minStep)[0];
	}

	@Override
	public int[] get_sync(CompletionHolder completionHolder) {
		try
		{
			return (int[])getSync(completionHolder);
		}
		catch (AcsJException acsex)
		{
			AcsJCouldntPerformActionEx cpa =
				new AcsJCouldntPerformActionEx(acsex);
			cpa.setProperty("message", "Failed to retrieve value");
			completionHolder.value = CompletionUtil.generateCompletion(cpa);
			// return default value in case of error
			//return default_value();
			return new int[1];
		}
	}

	@Override
	public void get_async(CBuLongSeq cb, CBDescIn desc) {
		getAsync(cb, desc);
	}

	@Override
	public int get_history(int n_last_values, uLongSeqSeqHolder vs, TimeSeqHolder ts) {
		vs.value = (int[][])getHistory(n_last_values, ts);
		return vs.value.length;
	}

	@Override
	public MonitoruLong create_monitor(CBuLongSeq cb, CBDescIn desc) {
		return create_postponed_monitor(0, cb, desc);
	}

	@Override
	public MonitoruLong create_postponed_monitor(long start_time, CBuLongSeq cb, CBDescIn desc) {
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
				((CBuLongSeq)callback).done(((int[])value), completion, desc);
			else if (type == CallbackDispatcher.CallbackType.WORKING_TYPE)
				((CBuLongSeq)callback).working(((int[])value), completion, desc);
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
		return ((int[])alarmLowOn)[0];
	}

	@Override
	public int alarm_low_off() {
		return ((int[])alarmLowOff)[0];
	}

	@Override
	public int alarm_high_on() {
		return ((int[])alarmHighOn)[0];
	}

	@Override
	public int alarm_high_off() {
		return ((int[])alarmHighOff)[0];
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
		return Math.abs(((Long)value1).longValue()-((Long)value2).longValue()) < ((Long)delta).longValue();
	}

	@Override
	public boolean noDelta(Object value) {
		return ((Integer)value).intValue() == 0;
	}

	@Override
	public Object sum(Object value1, Object value2, boolean substract) {
		long val2 = ((Long)value2).longValue();
		if (substract)
			val2 = -val2;
		return new Long(((Long)value1).longValue() + val2);
	}

	@Override
	public Object readPropertyTypeCharacteristic(String name) throws NoSuchCharacteristic {
		return (characteristicModelImpl.getIntegerSeq(name));
	}

}
