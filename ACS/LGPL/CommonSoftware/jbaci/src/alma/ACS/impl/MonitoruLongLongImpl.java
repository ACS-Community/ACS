package alma.ACS.impl;

import org.omg.CORBA.BooleanHolder;
import org.omg.CORBA.IntHolder;
import org.omg.CORBA.LongHolder;

import alma.ACS.CBDescIn;
import alma.ACS.Callback;
import alma.ACS.MonitoruLongLongOperations;

public class MonitoruLongLongImpl
			extends CommonComparableMonitorImpl
			implements MonitoruLongLongOperations {
	
	/**
	 * @param property
	 * @param callback
	 * @param descIn
	 * @param start_time 
	 * @param startTime
	 */
	public MonitoruLongLongImpl(
			CommonPropertyImpl property,
			Callback callback,
			CBDescIn descIn, long start_time) {
		super(property, callback, descIn);

	}

	/**
	 * @see alma.ACS.MonitoruLongLongOperations#get_value_trigger(IntHolder, BooleanHolder)
	 */
	public void get_value_trigger(LongHolder delta, BooleanHolder enable) {
		delta.value = ((Long)getValueTrigger(enable)).longValue();
	}

	/**
	 * @see alma.ACS.MonitoruLongLongOperations#set_value_trigger(int, boolean)
	 */
	public void set_value_trigger(long delta, boolean enable) {
		setValueTrigger(new Long((long) delta), enable);
		
	}

	/**
	 * @see alma.ACS.MonitoruLongLongOperations#get_value_percent_trigger(org.omg.CORBA.DoubleHolder, org.omg.CORBA.BooleanHolder)
	 */
	public void get_value_percent_trigger(org.omg.CORBA.DoubleHolder deltaHolder, org.omg.CORBA.BooleanHolder enableHolder) {
	}

	/**
	 * @see alma.ACS.MonitoruLongLongOperations#set_value_percent_trigger(org.omg.CORBA.DoubleHolder, boolean)
	 */
	public void set_value_percent_trigger(double delta, boolean enable) {
	}
}
