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

import org.omg.CORBA.NO_IMPLEMENT;

import alma.ACS.Alarmdouble;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBdouble;
import alma.ACS.Callback;
import alma.ACS.Monitordouble;
import alma.ACS.MonitordoubleHelper;
import alma.ACS.MonitordoublePOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.ROdoubleOperations;
import alma.ACS.Subscription;
import alma.ACS.TimeSeqHolder;
import alma.ACS.doubleSeqHolder;
import alma.ACS.jbaci.CallbackDispatcher;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;

/**
 * Implementation of <code>alma.ACS.ROdouble</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class ROdoubleImpl
	extends ROCommonComparablePropertyImpl
	implements ROdoubleOperations {

	/**
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	public ROdoubleImpl(
		String name,
		CharacteristicComponentImpl parentComponent)
		throws PropertyInitializationFailed {
		super(double.class, name, parentComponent);
	}

	/**
	 * @param name
	 * @param parentComponent
	 * @param dataAccess
	 * @throws PropertyInitializationFailed
	 */
	public ROdoubleImpl(
		String name,
		CharacteristicComponentImpl parentComponent,
		DataAccess dataAccess)
		throws PropertyInitializationFailed {
		super(double.class, name, parentComponent, dataAccess);
	}

	/**
	 * @see alma.ACS.CommonPropertyImpl#readPropertyTypeCharacteristic(java.lang.String)
	 */
	public Object readPropertyTypeCharacteristic(String name)
		throws NoSuchCharacteristic {
		return new Double(characteristicModelImpl.getDouble(name));
	}

	/**
	 * @see alma.ACS.ROdoubleOperations#alarm_high_off()
	 */
	public double alarm_high_off() {
		return ((Double)alarmHighOff).doubleValue();
	}

	/**
	 * @see alma.ACS.ROdoubleOperations#alarm_high_on()
	 */
	public double alarm_high_on() {
		return ((Double)alarmHighOn).doubleValue();
	}

	/**
	 * @see alma.ACS.ROdoubleOperations#alarm_low_off()
	 */
	public double alarm_low_off() {
		return ((Double)alarmLowOff).doubleValue();
	}

	/**
	 * @see alma.ACS.ROdoubleOperations#alarm_low_on()
	 */
	public double alarm_low_on() {
		return ((Double)alarmLowOn).doubleValue();
	}

	/**
	 * @see alma.ACS.ROdoubleOperations#new_subscription_Alarm(alma.ACS.Alarmdouble, alma.ACS.CBDescIn)
	 */
	public Subscription new_subscription_Alarm(
		Alarmdouble arg0,
		CBDescIn arg1) {
		// TODO NO_IMPLEMENT
		
		throw new NO_IMPLEMENT();
	}

	/**
	 * @see alma.ACS.PdoubleOperations#create_monitor(alma.ACS.CBdouble, alma.ACS.CBDescIn)
	 */
	public Monitordouble create_monitor(CBdouble callback, CBDescIn descIn) {
		return create_postponed_monitor(0, callback, descIn);
	}

	/**
	 * @see alma.ACS.PdoubleOperations#create_postponed_monitor(long, alma.ACS.CBdouble, alma.ACS.CBDescIn)
	 */
	public Monitordouble create_postponed_monitor(
		long startTime,
		CBdouble callback,
		CBDescIn descIn) {
			
		// create monitor and its servant
		MonitordoubleImpl monitorImpl = new MonitordoubleImpl(this, callback, descIn, startTime);
		MonitordoublePOATie monitorTie = new MonitordoublePOATie(monitorImpl);

		// register and activate		
		return MonitordoubleHelper.narrow(this.registerMonitor(monitorImpl, monitorTie));
	
	}

	/**
	 * @see alma.ACS.PdoubleOperations#default_value()
	 */
	public double default_value() {
		return ((Double)defaultValue).doubleValue();
	}

	/**
	 * @see alma.ACS.PdoubleOperations#get_async(alma.ACS.CBdouble, alma.ACS.CBDescIn)
	 */
	public void get_async(CBdouble arg0, CBDescIn arg1) {
		getAsync(arg0, arg1);
	}

	/**
	 * @see alma.ACS.PdoubleOperations#get_history(int, alma.ACS.doubleSeqHolder, alma.ACS.TimeSeqHolder)
	 */
	public int get_history(
		int arg0,
		doubleSeqHolder arg1,
		TimeSeqHolder arg2) {
		arg1.value = (double[])getHistory(arg0, arg2);
		return arg1.value.length;
	}

	/**
	 * @see alma.ACS.PdoubleOperations#get_sync(alma.ACSErr.CompletionHolder)
	 */
	public double get_sync(CompletionHolder completionHolder) {
		try
		{
			return ((Double)getSync(completionHolder)).doubleValue();
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

	/**
	 * @see alma.ACS.PdoubleOperations#graph_max()
	 */
	public double graph_max() {
		return ((Double)graphMax).doubleValue();
	}

	/**
	 * @see alma.ACS.PdoubleOperations#graph_min()
	 */
	public double graph_min() {
		return ((Double)graphMin).doubleValue();
	}

	/**
	 * @see alma.ACS.PdoubleOperations#min_delta_trigger()
	 */
	public double min_delta_trigger() {
		return ((Double)minDeltaTrigger).doubleValue();
	}

	/**
	 * @see alma.ACS.PdoubleOperations#min_step()
	 */
	public double min_step() {
		return ((Double)minStep).doubleValue();
	}


	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#lessThanDelta(java.lang.Object, java.lang.Object, java.lang.Object)
	 */
	public boolean lessThanDelta(Object value1, Object value2, Object delta) {
		return Math.abs(((Double)value1).doubleValue()-((Double)value2).doubleValue()) < ((Double)delta).doubleValue();
	}

	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#noDelta(java.lang.Object)
	 */
	public boolean noDelta(Object value) {
		return ((Double)value).doubleValue() == 0;
	}

	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#sum(java.lang.Object, java.lang.Object, boolean)
	 */
	public Object sum(Object value1, Object value2, boolean substract) {
		double val2 = ((Double)value2).doubleValue();
		if (substract)
			val2 = -val2;
		return new Double(((Double)value1).doubleValue() + val2);
	}

	/**
	 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACSErr.Completion, alma.ACS.CBDescOut)
	 */
	public boolean dispatchCallback(
		int type,
		Object value,
		Callback callback,
		Completion completion,
		CBDescOut desc) {
		try
		{	
			if (type == CallbackDispatcher.DONE_TYPE)
				((CBdouble)callback).done(((Double)value).doubleValue(), completion, desc);
			else if (type == CallbackDispatcher.WORKING_TYPE)
				((CBdouble)callback).working(((Double)value).doubleValue(), completion, desc);
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
