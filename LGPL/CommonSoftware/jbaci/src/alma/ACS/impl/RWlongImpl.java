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

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBlong;
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACS.Monitorlong;
import alma.ACS.MonitorlongHelper;
import alma.ACS.MonitorlongPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.RWlongOperations;
import alma.ACS.TimeSeqHolder;
import alma.ACS.longSeqHolder;
import alma.ACS.jbaci.CallbackDispatcher;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;

/**
 * Implementation of <code>alma.ACS.RWlong</code>.
 * @author <a href="mailto:cmenayATcsrg.inf.utfsm.cl">Camilo Menay</a>
 * @author <a href="mailto:cmaureirATinf.utfsm.cl">Cristian Maureira</a>
 * @version $id$
 */
public class RWlongImpl
	extends RWCommonComparablePropertyImpl
	implements RWlongOperations {

	/**
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	public RWlongImpl(
		String name,
		CharacteristicComponentImpl parentComponent)
		throws PropertyInitializationFailed {
		super(int.class, name, parentComponent);
	}

	/**
	 * @param name
	 * @param parentComponent
	 * @param dataAccess
	 * @throws PropertyInitializationFailed
	 */
	public RWlongImpl(
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
	 * @see alma.ACS.PlongOperations#create_monitor(alma.ACS.CBlong, alma.ACS.CBDescIn)
	 */
	public Monitorlong create_monitor(CBlong callback, CBDescIn descIn) {
		return create_postponed_monitor(0, callback, descIn);
	}

	/**
	 * @see alma.ACS.PlongOperations#create_postponed_monitor(long, alma.ACS.CBlong, alma.ACS.CBDescIn)
	 */
	public Monitorlong create_postponed_monitor(
		long startTime,
		CBlong callback,
		CBDescIn descIn) {
			
		// create monitor and its servant
		MonitorlongImpl monitorImpl = new MonitorlongImpl(this, callback, descIn, startTime);
		MonitorlongPOATie monitorTie = new MonitorlongPOATie(monitorImpl);

		// register and activate		
		return MonitorlongHelper.narrow(this.registerMonitor(monitorImpl, monitorTie));
	
	}

	/**
	 * @see alma.ACS.PlongOperations#default_value()
	 */
	public int default_value() {
		return ((Integer)defaultValue).intValue();
	}

	/**
	 * @see alma.ACS.PlongOperations#get_async(alma.ACS.CBlong, alma.ACS.CBDescIn)
	 */
	public void get_async(CBlong arg0, CBDescIn arg1) {
		getAsync(arg0, arg1);
	}

	/**
	 * @see alma.ACS.PlongOperations#get_history(int, alma.ACS.longSeqHolder, alma.ACS.TimeSeqHolder)
	 */
	public int get_history(
		int arg0,
		longSeqHolder arg1,
		TimeSeqHolder arg2) {
		arg1.value = (int[])getHistory(arg0, arg2);
		return arg1.value.length;
	}

	/**
	 * @see alma.ACS.PlongOperations#get_sync(alma.ACSErr.CompletionHolder)
	 */
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

	/**
	 * @see alma.ACS.PlongOperations#graph_max()
	 */
	public int graph_max() {
		return ((Integer)graphMax).intValue();
	}

	/**
	 * @see alma.ACS.PlongOperations#graph_min()
	 */
	public int graph_min() {
		return ((Integer)graphMin).intValue();
	}

	/**
	 * @see alma.ACS.PlongOperations#min_delta_trigger()
	 */
	public int min_delta_trigger() {
		return ((Integer)minDeltaTrigger).intValue();
	}

	/**
	 * @see alma.ACS.PlongOperations#min_step()
	 */
	public int min_step() {
		return ((Integer)minStep).intValue();
	}


	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#lessThanDelta(java.lang.Object, java.lang.Object, java.lang.Object)
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
				((CBlong)callback).done((int) ((Integer)value).intValue(), completion, desc);
			else if (type == CallbackDispatcher.WORKING_TYPE)
				((CBlong)callback).working((int) ((Integer)value).intValue(), completion, desc);
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
	public Object sum(Object value1, Object value2, boolean substract) {
		int val2 = ((Integer)value2).intValue();
		if (substract)
			val2 = -val2;
		return new Integer(((Integer)value1).intValue() + val2);
	}



	/**
	 * @see alma.ACS.RWlongOperations#max_value()
	 */
	public int max_value() {
		return ((Integer)maxValue).intValue();
	}

	/**
	 * @see alma.ACS.RWlongOperations#min_value()
	 */
	public int min_value() {
		return ((Integer)minValue).intValue();
	}

	/**
	 * @see alma.ACS.RWlongOperations#set_async(long, alma.ACS.CBvoid, alma.ACS.CBDescIn)
	 */
	public void set_async(int value, CBvoid callback, CBDescIn descIn) {
		setAsync(new Integer(value), callback, descIn);
	}

	/**
	 * @see alma.ACS.RWlongOperations#set_nonblocking(long)
	 */
	public void set_nonblocking(int value) {
		setNonblocking(new Integer(value));
	}

	/**
	 * @see alma.ACS.RWlongOperations#set_sync(long)
	 */
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

