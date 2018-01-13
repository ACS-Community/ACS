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

import org.omg.CORBA.Context;
import org.omg.CORBA.ContextList;
import org.omg.CORBA.DomainManager;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.NO_RESOURCES;
import org.omg.CORBA.NVList;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.Policy;
import org.omg.CORBA.Request;
import org.omg.CORBA.SetOverrideType;

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBpattern;
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACS.Condition;
import alma.ACS.Monitorpattern;
import alma.ACS.MonitorpatternHelper;
import alma.ACS.MonitorpatternPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.RWpatternOperations;
import alma.ACS.RWpattern;
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
 * Implementation of <code>alma.ACS.RWpattern</code>.
 * @author <a href="mailto:cmenayATcsrg.inf.utfsm.cl">Camilo Menay</a>
 * @author <a href="mailto:cmaureirATinf.utfsm.cl">Cristian Maureira</a>
 * @version $id$f
 */
public class RWpatternImpl
	extends CommonPropertyImpl
	implements RWpatternOperations {

	/**
	 * @param propertyType
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	public RWpatternImpl(
		String name,
		CharacteristicComponentImpl parentComponent)
		throws PropertyInitializationFailed {
		super(long.class, name, parentComponent);
	}

	/**
	 * @param propertyType
	 * @param name
	 * @param parentComponent
	 * @param dataAccess
	 * @throws PropertyInitializationFailed
	 */
	public RWpatternImpl(
		String name,
		CharacteristicComponentImpl parentComponent,
		DataAccess dataAccess)
		throws PropertyInitializationFailed {
		super(long.class, name, parentComponent, dataAccess);
	}

	/**
	 * @see alma.ACS.CommonPropertyImpl#readPropertyTypeCharacteristic(java.lang.String)
	 */
	public Object readPropertyTypeCharacteristic(String name)
		throws NoSuchCharacteristic {
		return new Long(characteristicModelImpl.getLong(name));
	}

	/**
	 * @see alma.ACS.RWPatternOperations#default_value()
	 */
	public long default_value() {
		return ((Long)defaultValue).longValue();
	}

	/**
	 * @see alma.ACS.RWPatternOperations#bitDescription()
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
	 * @see alma.ACS.RWPatternOperations#whenSet()
	 */
	public Condition[] whenSet() {
		try
		{
			// TODO check the definition of this method, and check if int[] is
			//      the right type for "whenSet".
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
	 * @see alma.ACS.RWPatternOperations#whenCleared()
	 */
	public Condition[] whenCleared() {
		try
		{
			// TODO check the definition of this method, and check if int[] is
			//      the right type for "whenCleared".
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
	 * @see alma.ACS.RWPatternOperations#get_sync(alma.ACSErr.CompletionHolder)
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
	 * @see alma.ACS.RWPatternOperations#get_async(alma.ACS.CBpattern, alma.ACS.CBDescIn)
	 */
	public void get_async(CBpattern callback, CBDescIn descIn) {
		getAsync(callback, descIn);
	}

	/**
	 * @see alma.ACS.RWPatternOperations#get_history(int, alma.ACS.patternSeqHolder, alma.ACS.TimeSeqHolder)
	 */
	public int get_history(
		int arg0,
		patternSeqHolder arg1,
		TimeSeqHolder arg2) {
		arg1.value = (long[])getHistory(arg0, arg2);
		return arg1.value.length;
	}

	/**
	 * @see alma.ACS.RWPatternOperations#create_monitor(alma.ACS.CBpattern, alma.ACS.CBDescIn)
	 */
	public Monitorpattern create_monitor(CBpattern callback, CBDescIn descIn) {
		return create_postponed_monitor(0, callback, descIn);
	}

	/**
	 * @see alma.ACS.RWPatternOperations#create_postponed_monitor(long, alma.ACS.CBpattern, alma.ACS.CBDescIn)
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
		CallbackDispatcher.CallbackType type,
		Object value,
		Callback callback,
		Completion completion,
		CBDescOut desc) {
		try
		{	
			if (type == CallbackDispatcher.CallbackType.DONE_TYPE)
				((CBpattern)callback).done(((Long)value).longValue(), completion, desc);
			else if (type == CallbackDispatcher.CallbackType.WORKING_TYPE)
				((CBpattern)callback).working(((Long)value).longValue(), completion, desc);
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
	 */
	public boolean lessThanDelta(Object value1, Object value2, Object delta) {
		// TODO Evaluate if this method is required for this type.
		//      It does not make sense to perform arismetic for pattern type.
		//      Moreover, the equivalent method is commented out in ROpatternImpl.java.
		return Math.abs(((Long)value1).longValue()-((Long)value2).longValue()) < ((Long)delta).longValue();
	}

	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#noDelta(java.lang.Object)
	 */
	public boolean noDelta(Object value) {
		// TODO Evaluate if this method is required for this type.
		//      It does not make sense to perform arismetic for pattern type.
		//      Moreover, the equivalent method is commented out in ROpatternImpl.java.
		return ((Long)value).longValue() == 0;
	}

	/*
	 * @see alma.ACS.CommonComparablePropertyImpl#sum(java.lang.Object, java.lang.Object, boolean)
	 */
	public Object sum(Object value1, Object value2, boolean substract) {
		// TODO Evaluate if this method is required for this type.
		//      It does not make sense to perform arismetic for pattern type.
		//      Moreover, the equivalent method is commented out in ROpatternImpl.java.
		long val2 = ((Long)value2).longValue();
		if (substract)
			val2 = -val2;
		return new Long(((Long)value1).longValue() + val2);
	}

	@Override
	public Completion set_sync(long value) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void set_async(long value, CBvoid cb, CBDescIn desc) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void set_nonblocking(long value) {
		setNonblocking(value);
	}
}

