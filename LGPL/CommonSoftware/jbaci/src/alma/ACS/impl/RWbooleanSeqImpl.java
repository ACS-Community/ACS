package alma.ACS.impl;

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBboolean;
import alma.ACS.CBbooleanSeq;
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACS.Monitorboolean;
import alma.ACS.MonitorbooleanHelper;
import alma.ACS.MonitorbooleanPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.RWbooleanSeqOperations;
import alma.ACS.TimeSeqHolder;
import alma.ACS.booleanSeqSeqHolder;
import alma.ACS.jbaci.CallbackDispatcher;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;


/**
 * 
 * @author javarias
 *
 */
public class RWbooleanSeqImpl extends RWCommonComparablePropertyImpl implements RWbooleanSeqOperations {

	public RWbooleanSeqImpl(String name, CharacteristicComponentImpl parentComponent,
			DataAccess<?> dataAccess) throws PropertyInitializationFailed {
		super(boolean[].class, name, parentComponent, dataAccess);
	}

	public RWbooleanSeqImpl(String name, CharacteristicComponentImpl parentComponent)
			throws PropertyInitializationFailed {
		super(boolean[].class, name, parentComponent);
	}

	@Override
	public boolean default_value() {
		return ((Boolean)defaultValue).booleanValue();
	}

	@Override
	public boolean[] get_sync(CompletionHolder c) {
		try
		{
			return ((boolean[])getSync(c));
		}
		catch (AcsJException acsex)
		{
			AcsJCouldntPerformActionEx cpa =
				new AcsJCouldntPerformActionEx(acsex);
			c.value = CompletionUtil.generateCompletion(cpa);
			cpa.setProperty("message", "Failed to retrieve value");
			// return default value in case of error
			return new boolean[]{default_value()};
		}
	}

	@Override
	public void get_async(CBbooleanSeq cb, CBDescIn desc) {
		getAsync(cb, desc);
	}

	@Override
	public int get_history(int n_last_values, booleanSeqSeqHolder vs, TimeSeqHolder ts) {
		vs.value = (boolean[][])getHistory(n_last_values, ts);
		return vs.value.length;
	}

	@Override
	public Monitorboolean create_monitor(CBbooleanSeq cb, CBDescIn desc) {
		return create_postponed_monitor(0, cb, desc);
	}

	@Override
	public Monitorboolean create_postponed_monitor(long start_time, CBbooleanSeq cb, CBDescIn desc) {
		// create monitor and its servant
		MonitorbooleanImpl monitorImpl = new MonitorbooleanImpl(this, cb, desc, start_time);
		MonitorbooleanPOATie monitorTie = new MonitorbooleanPOATie(monitorImpl);

		// register and activate		
		return MonitorbooleanHelper.narrow(this.registerMonitor(monitorImpl, monitorTie));
	
	}

	@Override
	public boolean dispatchCallback(CallbackDispatcher.CallbackType type, Object value, Callback callback, Completion completion, CBDescOut desc) {
		try
		{	
			if (type == CallbackDispatcher.CallbackType.DONE_TYPE)
				((CBbooleanSeq)callback).done((boolean[])value, completion, desc);
			else if (type == CallbackDispatcher.CallbackType.WORKING_TYPE)
				((CBbooleanSeq)callback).working((boolean[])value, completion, desc);
			else 
				return false;
				
			return true;
		}
		catch (Exception th)
		{
			return false;
		}
	}

	@Override
	public boolean min_value() {
		return ((Boolean)min_value());
	}

	@Override
	public boolean max_value() {
		return ((Boolean)maxValue).booleanValue();
	}

	@Override
	public Completion set_sync(boolean[] value) {
		try
		{
			return setSync(value);
		}
		catch (AcsJException acsex)
		{
			AcsJCouldntPerformActionEx cpa =
				new AcsJCouldntPerformActionEx("Failed to set value", acsex);
			return CompletionUtil.generateCompletion(cpa);
		}
	}

	@Override
	public void set_async(boolean[] value, CBvoid cb, CBDescIn desc) {
		setAsync(value, cb, desc);
	}

	@Override
	public void set_nonblocking(boolean[] value) {
		setNonblocking(value);
	}

	@Override
	public boolean lessThanDelta(Object value1, Object value2, Object delta) {
		return Math.abs(((Long)value1).longValue()-((Long)value2).longValue()) < ((Long)delta).longValue();
	}

	@Override
	public boolean noDelta(Object value) {
		return ((Boolean)value) == false;
	}

	@Override
	public Object sum(Object value1, Object value2, boolean substract) {
		boolean val2 = ((Boolean)value2).booleanValue();
		if (substract)
			val2 = !val2;
		return new Boolean(((Boolean)value1).booleanValue() || val2);
	}

	@Override
	public Object readPropertyTypeCharacteristic(String name) throws NoSuchCharacteristic {
		return (characteristicModelImpl.getBooleanSeq(name));
	}

}
