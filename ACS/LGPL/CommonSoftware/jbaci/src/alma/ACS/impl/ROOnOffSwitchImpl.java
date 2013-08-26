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

import alma.ACS.Alarmpattern;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBpattern;
import alma.ACS.Callback;
import alma.ACS.Condition;
import alma.ACS.Monitor;
import alma.ACS.Monitorpattern;
import alma.ACS.MonitorpatternHelper;
import alma.ACS.MonitorpatternPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.OnOffSwitch;
import alma.ACS.OnOffSwitchSeqHolder;
import alma.ACS.ROOnOffSwitchOperations;
import alma.ACS.Subscription;
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
 * Implementation of <code>alma.ACS.ROdouble</code>.
 * @author <a href="mailto:cmenayATcsrg.inf.utfsm.cl">Camilo Menay</a>
 * @author <a href="mailto:cmaureirATinf.utfsm.cl">Cristian Maureira</a>
 * @version $id$
 */
public class ROOnOffSwitchImpl
	extends ROCommonComparablePropertyImpl
	implements ROOnOffSwitchOperations {

	/**
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	public ROOnOffSwitchImpl(
		String name,
		CharacteristicComponentImpl parentComponent)
		throws PropertyInitializationFailed {
		super(int.class, name, parentComponent);//check this!
	}

	/**
	 * @param name
	 * @param parentComponent
	 * @param dataAccess
	 * @throws PropertyInitializationFailed
	 */
	public ROOnOffSwitchImpl(
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
	 * @see alma.ACS.ROdoubleOperations#alarm_high_off()
	 */
	public OnOffSwitch[] alarm_off() {
		try {
			int[] temp = characteristicModelImpl.getIntegerSeq("alarm_off");
			OnOffSwitch[] ret = new OnOffSwitch[temp.length];
			
			for (int i=0;i<temp.length;i++){
				ret[i] = OnOffSwitch.from_int(temp[i]);
				
				return ret;
			}
			
		} catch (NoSuchCharacteristic e) {
			//noop
		}
			
			return null;
		
	}

	/**
	 * @see alma.ACS.ROdoubleOperations#alarm_high_on()
	 */
	public OnOffSwitch[] alarm_on() {
		try {
			int[] temp = characteristicModelImpl.getIntegerSeq("alarm_on");
			OnOffSwitch[] ret = new OnOffSwitch[temp.length];
			
			for (int i=0;i<temp.length;i++){
				ret[i] = OnOffSwitch.from_int(temp[i]);
				
				return ret;
			}
			
		} catch (NoSuchCharacteristic e) {
			//noop
		}
			
			return null;
	}



	/**
	 * @see alma.ACS.ROdoubleOperations#new_subscription_Alarm(alma.ACS.Alarmdouble, alma.ACS.CBDescIn)
	 */
	public Subscription new_subscription_AlarmEnum(
		Alarmpattern arg0,
		CBDescIn arg1) {
		
			//this.minTimerTrigger
			return null;
		// TODO NO_IMPLEMENT
		//MonitorenumpropEventDispatcher
	}

	/**
	 * @see alma.ACS.PdoubleOperations#create_monitor(alma.ACS.CBdouble, alma.ACS.CBDescIn)
	 */
	public Monitorpattern create_monitor(CBpattern callback, CBDescIn descIn) {
		return (Monitorpattern) create_postponed_monitor(0, callback, descIn);
	}

	/**
	 * @see alma.ACS.PdoubleOperations#create_postponed_monitor(long, alma.ACS.CBdouble, alma.ACS.CBDescIn)
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
	 * @see alma.ACS.PdoubleOperations#default_value()
	 */
	public OnOffSwitch default_value() {
		return ((OnOffSwitch)defaultValue);
	}

	/**
	 * @see alma.ACS.PdoubleOperations#get_async(alma.ACS.CBdouble, alma.ACS.CBDescIn)
	 */
	public void get_async(CBpattern arg0, CBDescIn arg1) {
		getAsync(arg0, arg1);
	}

	/**
	 * @see alma.ACS.PdoubleOperations#get_history(int, alma.ACS.doubleSeqHolder, alma.ACS.TimeSeqHolder)
	 */
	public int get_history(
		int arg0,
		OnOffSwitchSeqHolder arg1,
		TimeSeqHolder arg2) {
		arg1.value = (OnOffSwitch[])getHistory(arg0, arg2);
		return arg1.value.length;
	}

	/**
	 * @see alma.ACS.PdoubleOperations#get_sync(alma.ACSErr.CompletionHolder)
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
	 * @see alma.ACS.PdoubleOperations#min_delta_trigger()
	 */
	public long min_timer_trigger() {
		return minTimerTrigger;
	}

	/**
	 * @see alma.ACS.PdoubleOperations#min_step()
	 */
	public long default_timer_trigger() {
		return defaultTimerTrigger;
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
				((CBpattern)callback).done(((Long)value).longValue(), completion, desc);
			else if (type == CallbackDispatcher.WORKING_TYPE)
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

	public boolean lessThanDelta(Object value1, Object value2, Object delta) {
		//not used
		return false;
	}

	public boolean noDelta(Object value) {
		//not used
		return false;
	}

	public Object sum(Object value1, Object value2, boolean substract) {
		//not used
		return null;
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
