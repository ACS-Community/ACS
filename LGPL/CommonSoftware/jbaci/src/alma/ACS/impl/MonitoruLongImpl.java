package alma.ACS.impl;

import org.omg.CORBA.BooleanHolder;
import org.omg.CORBA.IntHolder;

import alma.ACS.CBDescIn;
import alma.ACS.Callback;
import alma.ACS.MonitoruLongOperations;

public class MonitoruLongImpl extends CommonComparableMonitorImpl implements MonitoruLongOperations {

	public MonitoruLongImpl(CommonPropertyImpl property, Callback callback, CBDescIn descIn, long startTime) {
		super(property, callback, descIn, startTime);
	}
	
	/**
	 * @see alma.ACS.MonitorlongOperations#get_value_trigger(IntHolder, BooleanHolder)
	 */
	@Override
	public void get_value_trigger(IntHolder delta, BooleanHolder enable) {
		delta.value = ((Integer)getValueTrigger(enable)).intValue();
		
	}

	/**
	 * @see alma.ACS.MonitorlongOperations#set_value_trigger(int, boolean)
	 */
	@Override
	public void set_value_trigger(int delta, boolean enable) {
		setValueTrigger(new Integer((int) delta), enable);
	}

	/**
	 * @see alma.ACS.MonitorlongOperations#get_value_percent_trigger(org.omg.CORBA.DoubleHolder, org.omg.CORBA.BooleanHolder)
	 */
	@Override
	public void get_value_percent_trigger(org.omg.CORBA.DoubleHolder deltaHolder, org.omg.CORBA.BooleanHolder enableHolder) {
	}

	/**
	 * @see alma.ACS.MonitorlongOperations#set_value_percent_trigger(org.omg.CORBA.DoubleHolder, boolean)
	 */
	@Override
	public void set_value_percent_trigger(double delta, boolean enable) {
	}

}
