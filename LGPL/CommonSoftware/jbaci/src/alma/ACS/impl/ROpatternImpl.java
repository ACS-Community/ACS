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
import org.omg.CORBA.NO_RESOURCES;

import alma.ACS.Alarmpattern;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBpattern;
import alma.ACS.Callback;
import alma.ACS.Condition;
import alma.ACS.Monitorpattern;
import alma.ACS.MonitorpatternHelper;
import alma.ACS.MonitorpatternPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.ROpatternOperations;
import alma.ACS.Subscription;
import alma.ACS.TimeSeqHolder;
import alma.ACS.patternSeqHolder;
import alma.ACS.jbaci.CallbackDispatcher;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;

/**
 * Implementation of <code>alma.ACS.ROpattern</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class ROpatternImpl
	extends ROCommonPropertyImpl
	implements ROpatternOperations {

	/**
	 * @param propertyType
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	public ROpatternImpl(
		String name,
		CharacteristicComponentImpl parentComponent)
		throws PropertyInitializationFailed {
		super(int.class, name, parentComponent);
	}

	/**
	 * @param propertyType
	 * @param name
	 * @param parentComponent
	 * @param dataAccess
	 * @throws PropertyInitializationFailed
	 */
	public ROpatternImpl(
		String name,
		CharacteristicComponentImpl parentComponent,
		DataAccess dataAccess)
		throws PropertyInitializationFailed {
		super(int.class, name, parentComponent, dataAccess);
	}

	/**
	 * @see alma.ACS.CommonPropertyImpl#readPropertyTypeCharacteristic(java.lang.String)
	 */
	public Object readPropertyTypeCharacteristic(String name)
		throws NoSuchCharacteristic {
		return new Integer(characteristicModelImpl.getInteger(name));
	}

	/**
	 * @see alma.ACS.ROpatternOperations#new_subscription_Alarm(alma.ACS.Alarmpattern, alma.ACS.CBDescIn)
	 */
	public Subscription new_subscription_Alarm(
		Alarmpattern arg0,
		CBDescIn arg1) {
		// TODO NO_IMPLEMENT
		throw new NO_IMPLEMENT();
	}

	/**
	 * @see alma.ACS.PpatternOperations#default_value()
	 */
	public long default_value() {
		return ((Integer)defaultValue).intValue();
	}

	/**
	 * @see alma.ACS.PpatternOperations#bitDescription()
	 */
	public String[] bitDescription() {
		try
		{
			return characteristicModelImpl.getStringSeq("bitDescription");
		}
		catch (NoSuchCharacteristic ncs)
		{
			throw new NO_RESOURCES();
		}
	}

	/**
	 * @see alma.ACS.PpatternOperations#whenSet()
	 */
	public Condition[] whenSet() {
		try
		{
			int[] values = characteristicModelImpl.getIntegerSeq("whenSet");
			Condition[] conditions = new Condition[values.length];
			for (int i = 0; i < conditions.length; i++)
				conditions[i] = Condition.from_int(values[i]);
			return conditions;
		}
		catch (NoSuchCharacteristic ncs)
		{
			throw new NO_RESOURCES();
		}
	}

	/**
	 * @see alma.ACS.PpatternOperations#whenCleared()
	 */
	public Condition[] whenCleared() {
		try
		{
			int[] values = characteristicModelImpl.getIntegerSeq("whenCleared");
			Condition[] conditions = new Condition[values.length];
			for (int i = 0; i < conditions.length; i++)
				conditions[i] = Condition.from_int(values[i]);
			return conditions;
		}
		catch (NoSuchCharacteristic ncs)
		{
			throw new NO_RESOURCES();
		}
	}

	public long alarm_mask() {
		try
		{
			return characteristicModelImpl.getLong("alarm_mask");
		}
		catch (NoSuchCharacteristic ncs)
		{
			throw new NO_RESOURCES();
		}
	}

	public long alarm_trigger() {
		try
		{
			return characteristicModelImpl.getLong("alarm_trigger");
		}
		catch (NoSuchCharacteristic ncs)
		{
			throw new NO_RESOURCES();
		}
	}

	/**
	 * @see alma.ACS.PpatternOperations#get_sync(alma.ACSErr.CompletionHolder)
	 */
	public long get_sync(CompletionHolder completionHolder) {
		try
		{
			return ((Long)getSync(completionHolder)).longValue();
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
	 * @see alma.ACS.PpatternOperations#get_async(alma.ACS.CBpattern, alma.ACS.CBDescIn)
	 */
	public void get_async(CBpattern callback, CBDescIn descIn) {
		getAsync(callback, descIn);
	}

	/**
	 * @see alma.ACS.PpatternOperations#get_history(int, alma.ACS.patternSeqHolder, alma.ACS.TimeSeqHolder)
	 */
	public int get_history(
		int arg0,
		patternSeqHolder arg1,
		TimeSeqHolder arg2) {
		arg1.value = (long[])getHistory(arg0, arg2);
		return arg1.value.length;
	}

	/**
	 * @see alma.ACS.PpatternOperations#create_monitor(alma.ACS.CBpattern, alma.ACS.CBDescIn)
	 */
	public Monitorpattern create_monitor(CBpattern callback, CBDescIn descIn) {
		return create_postponed_monitor(0, callback, descIn);
	}

	/**
	 * @see alma.ACS.PpatternOperations#create_postponed_monitor(long, alma.ACS.CBpattern, alma.ACS.CBDescIn)
	 */
	public Monitorpattern create_postponed_monitor(
		long startTime,
		CBpattern callback,
		CBDescIn descIn) {
			
		// create monitor and its servant
		MonitorpatternImpl monitorImpl = new MonitorpatternImpl(this, callback, descIn, startTime);
		MonitorpatternPOATie monitorTie = new MonitorpatternPOATie(monitorImpl);

		// register and activate		
		return MonitorpatternHelper.narrow(this.registerMonitor(monitorImpl, monitorTie));
	
	}

	/**
	 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACS.Callback, alma.ACSErr.Completion, alma.ACS.CBDescOut)
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
				((CBpattern)callback).done(((Integer)value).intValue(), completion, desc);
			else if (type == CallbackDispatcher.WORKING_TYPE)
				((CBpattern)callback).working(((Integer)value).intValue(), completion, desc);
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
	 * @see alma.ACS.CommonComparablePropertyImpl#lessThanDelta(java.lang.Object, java.lang.Object, java.lang.Object)
	 *//*
	public boolean lessThanDelta(Object value1, Object value2, Object delta) {
		return Math.abs(((Integer)value1).intValue()-((Integer)value2).intValue()) < ((Integer)delta).intValue();
	}

	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#noDelta(java.lang.Object)
	 *//*
	public boolean noDelta(Object value) {
		return ((Integer)value).intValue() == 0;
	}

	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#sum(java.lang.Object, java.lang.Object, boolean)
	 *//*
	public Object sum(Object value1, Object value2, boolean substract) {
		double val2 = ((Integer)value2).intValue();
		if (substract)
			val2 = -val2;
		return new Double(((Integer)value1).intValue() + val2);
	}
*/
}
