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

import org.omg.CORBA.NO_RESOURCES;

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBpattern;
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACS.Condition;
import alma.ACS.Monitor;
import alma.ACS.Monitorpattern;
import alma.ACS.MonitorpatternHelper;
import alma.ACS.MonitorpatternPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.OnOffSwitch;
import alma.ACS.OnOffSwitchSeqHolder;
import alma.ACS.RWOnOffSwitchOperations;
import alma.ACS.TimeSeqHolder;
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
public class RWOnOffSwitchImpl
	extends RWCommonComparablePropertyImpl
	implements RWOnOffSwitchOperations {

	/**
	 * @param propertyType
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	public RWOnOffSwitchImpl(
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
	public RWOnOffSwitchImpl(
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
	 * @see alma.ACS.PpatternOperations#default_value()
	 */
	public OnOffSwitch default_value() {
		return ((OnOffSwitch)defaultValue);
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
	 * @see alma.ACS.PpatternOperations#get_sync(alma.ACSErr.CompletionHolder)
	 */
	public OnOffSwitch get_sync(CompletionHolder completionHolder) {
		try
		{
			return ((OnOffSwitch)getSync(completionHolder));
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
		OnOffSwitchSeqHolder arg1,
		TimeSeqHolder arg2) {
		arg1.value = (OnOffSwitch[])getHistory(arg0, arg2);
		return arg1.value.length;
	}

	/**
	 * @see alma.ACS.PpatternOperations#create_monitor(alma.ACS.CBpattern, alma.ACS.CBDescIn)
	 */
	public Monitorpattern create_monitor(CBpattern callback, CBDescIn descIn) {
		return (Monitorpattern) create_postponed_monitor(0, callback, descIn);
	}

	/**
	 * @see alma.ACS.PpatternOperations#create_postponed_monitor(long, alma.ACS.CBpattern, alma.ACS.CBDescIn)
	 */
	public Monitor create_postponed_monitor(
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
	 */
	public boolean lessThanDelta(Object value1, Object value2, Object delta) {
		return Math.abs(((Integer)value1).intValue()-((Integer)value2).intValue()) < ((Integer)delta).intValue();
	}

	/**
	 * @see alma.ACS.CommonComparablePropertyImpl#noDelta(java.lang.Object)
	 */
	public boolean noDelta(Object value) {
		return ((Integer)value).intValue() == 0;
	}

	/*
	 * @see alma.ACS.CommonComparablePropertyImpl#sum(java.lang.Object, java.lang.Object, boolean)
	 */
	public Object sum(Object value1, Object value2, boolean substract) {
		double val2 = ((Integer)value2).intValue();
		if (substract)
			val2 = -val2;
		return new Double(((Integer)value1).intValue() + val2);
	}
	
	
	/**
	 * @see alma.ACS.PstringOperations#set_async(java.lang.String value, alma.ACS.CBvoid, alma.ACS.CBDescIn)
	 */
	public void set_async(OnOffSwitch value, CBvoid callback, CBDescIn desc) {
		setAsync(value, callback, desc);
	}

	/**
	 * @see alma.ACS.PstringOperations#set_nonblocking(java.lang.String)
	 */
	public void set_nonblocking(OnOffSwitch value) {
		setNonblocking(value);
	}

	/**
	 * @see alma.ACS.PstringOperations#set_sync(java.lang.String)
	 */
	public Completion set_sync(OnOffSwitch value) {
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

	public OnOffSwitch[] allStates() {
		try {
			String[] tmp = characteristicModelImpl.getStringSeq("statesDescription");
			OnOffSwitch[] ret = new OnOffSwitch[tmp.length];
			 for (int i=0; i<tmp.length; i++)
			        ret[i] = OnOffSwitch.from_int(i);
			        
			 return ret;
			
		} catch (NoSuchCharacteristic e) {
		//noop
		}
		
		return null;
	}

	public Condition[] condition() {
		try {
			int [] tmp = characteristicModelImpl.getLongSeq("statesDescription");
			Condition[] ret = new Condition[tmp.length];
			for(int i=0;i<tmp.length;i++){
				ret[i] = Condition.from_int(tmp[i]);
			}
		
		} catch (NoSuchCharacteristic e) {
			//noop
		}
		return null;
	}

	public String[] statesDescription() {
		try {
			return characteristicModelImpl.getStringSeq("statesDescription");
		} catch (NoSuchCharacteristic e) {
			//noop
		}
		return null;
	}

}

