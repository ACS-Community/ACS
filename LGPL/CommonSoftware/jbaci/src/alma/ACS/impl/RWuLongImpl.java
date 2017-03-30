package alma.ACS.impl;

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBlong;
import alma.ACS.CBuLong;
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACS.MonitorlongPOATie;
import alma.ACS.MonitoruLong;
import alma.ACS.MonitoruLongHelper;
import alma.ACS.MonitoruLongPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.RWuLongOperations;
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

/**
 * Implementation of RWulong
 * @author javarias
 *
 */

public class RWuLongImpl extends RWCommonComparablePropertyImpl implements RWuLongOperations {

	public RWuLongImpl(String name, CharacteristicComponentImpl parentComponent)
			throws PropertyInitializationFailed {
		super(int.class, name, parentComponent);
	}
	
	public RWuLongImpl(String name, CharacteristicComponentImpl parentComponent,
			DataAccess<Integer> dataAccess) throws PropertyInitializationFailed {
		super(int.class, name, parentComponent, dataAccess);
	}

	/**
	 * @see alma.ACS.CommonPropertyImpl#readPropertyTypeCharacteristic(java.lang.String)
	 */
	@Override
	public Object readPropertyTypeCharacteristic(String name)
		throws NoSuchCharacteristic {
		return new Long(characteristicModelImpl.getString(name));
	}

	/**
	 * @see alma.ACS.PuLongOperations#create_monitor(alma.ACS.CBlong, alma.ACS.CBDescIn)
	 */
	@Override
	public MonitoruLong create_monitor(CBuLong callback, CBDescIn descIn) {
		return create_postponed_monitor(0, callback, descIn);
	}

	/**
	 * @see alma.ACS.PuLongOperations#create_postponed_monitor(long, alma.ACS.CBlong, alma.ACS.CBDescIn)
	 */
	@Override
	public MonitoruLong create_postponed_monitor(
		long startTime,
		CBuLong callback,
		CBDescIn descIn) {
			
		// create monitor and its servant
		MonitoruLongImpl monitorImpl = new MonitoruLongImpl(this, callback, descIn, startTime);
		MonitoruLongPOATie monitorTie = new MonitoruLongPOATie(monitorImpl);

		// register and activate		
		return MonitoruLongHelper.narrow(this.registerMonitor(monitorImpl, monitorTie));
	
	}

	/**
	 * @see alma.ACS.PuLongOperations#default_value()
	 */
	@Override
	public int default_value() {
		return ((Integer)defaultValue).intValue();
	}

	/**
	 * @see alma.ACS.PuLongOperations#get_async(alma.ACS.CBlong, alma.ACS.CBDescIn)
	 */
	@Override
	public void get_async(CBuLong arg0, CBDescIn arg1) {
		getAsync(arg0, arg1);
	}

	/**
	 * @see alma.ACS.PuLongOperations#get_history(int, alma.ACS.longSeqHolder, alma.ACS.TimeSeqHolder)
	 */
	@Override
	public int get_history(
		int arg0,
		uLongSeqHolder arg1,
		TimeSeqHolder arg2) {
		arg1.value = (int[])getHistory(arg0, arg2);
		return arg1.value.length;
	}

	/**
	 * 
	 * @see alma.ACS.PuLongOperations#get_sync(alma.ACSErr.CompletionHolder)
	 */
	@Override
	public int get_sync(CompletionHolder completionHolder) {
		try
		{
			return ((Integer)getSync(completionHolder)).intValue();
		}
		catch (AcsJException acsex)
		{
			AcsJCouldntPerformActionEx cpa =
				new AcsJCouldntPerformActionEx(acsex);
			completionHolder.value = CompletionUtil.generateCompletion(cpa);
			cpa.setProperty("message", "Failed to retrieve value");
			// return default value in case of error
			return default_value();
		}
	}

	/**
	 * @see alma.ACS.PuLongOperations#graph_max()
	 */
	@Override
	public int graph_max() {
		return ((Integer)graphMax).intValue();
	}

	/**
	 * @see alma.ACS.PuLongOperations#graph_min()
	 */
	@Override
	public int graph_min() {
		return ((Integer)graphMin).intValue();
	}

	/**
	 * @see alma.ACS.PuLongOperations#min_delta_trigger()
	 */
	@Override
	public int min_delta_trigger() {
		return ((Integer)minDeltaTrigger).intValue();
	}

	/**
	 * @see alma.ACS.PuLongOperations#min_step()
	 */
	@Override
	public int min_step() {
		return ((Integer)minStep).intValue();
	}


	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#lessThanDelta(java.lang.Object, java.lang.Object, java.lang.Object)
	 */
	@Override
	public boolean lessThanDelta(Object value1, Object value2, Object delta) {
		return Math.abs(((Integer)value1).intValue()-((Integer)value2).intValue()) < ((Integer)delta).intValue();
	}

	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#noDelta(java.lang.Object)
	 */
	@Override
	public boolean noDelta(Object value) {
		return ((Integer)value).intValue() == 0;
	}

	/**
	 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACSErr.Completion, alma.ACS.CBDescOut)
	 */
	@Override
	public boolean dispatchCallback(
		CallbackDispatcher.CallbackType type,
		Object value,
		Callback callback,
		Completion completion,
		CBDescOut desc) {
		try
		{	
			if (type == CallbackDispatcher.CallbackType.DONE_TYPE)
				((CBuLong)callback).done((int) ((Integer)value).intValue(), completion, desc);
			else if (type == CallbackDispatcher.CallbackType.WORKING_TYPE)
				((CBuLong)callback).working((int) ((Integer)value).intValue(), completion, desc);
			else 
				return false;
				
			return true;
		}
		catch (Throwable th)
		{
			return false;
		}
	}

	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#sum(java.lang.Object, java.lang.Object, boolean)
	 */
	@Override
	public Object sum(Object value1, Object value2, boolean substract) {
		int val2 = ((Integer)value2).intValue();
		if (substract)
			val2 = -val2;
		return new Integer(((Integer)value1).intValue() + val2);
	}



	/**
	 * @see alma.ACS.RWuLongOperations#max_value()
	 */
	@Override
	public int max_value() {
		return ((Integer)maxValue).intValue();
	}

	/**
	 * @see alma.ACS.RWuLongOperations#min_value()
	 */
	@Override
	public int min_value() {
		return ((Integer)minValue).intValue();
	}

	/**
	 * @see alma.ACS.RWuLongOperations#set_async(long, alma.ACS.CBvoid, alma.ACS.CBDescIn)
	 */
	@Override
	public void set_async(int value, CBvoid callback, CBDescIn descIn) {
		setAsync(new Integer(value), callback, descIn);
	}

	/**
	 * @see alma.ACS.RWuLongOperations#set_nonblocking(long)
	 */
	@Override
	public void set_nonblocking(int value) {
		setNonblocking(new Integer(value));
	}

	/**
	 * @see alma.ACS.RWuLongOperations#set_sync(long)
	 */
	@Override
	public Completion set_sync(int value) {
		try
		{
			return setSync(new Integer(value));
		}
		catch (AcsJException acsex)
		{
			AcsJCouldntPerformActionEx cpa =
				new AcsJCouldntPerformActionEx("Failed to set value", acsex);
			return CompletionUtil.generateCompletion(cpa);
		}
	}
}
