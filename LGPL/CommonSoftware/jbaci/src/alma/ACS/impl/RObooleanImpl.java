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

import alma.ACS.Alarmboolean;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBboolean;
import alma.ACS.Callback;
import alma.ACS.Monitorboolean;
import alma.ACS.MonitorbooleanHelper;
import alma.ACS.MonitorbooleanPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.ROBoolOperations;
import alma.ACS.RObooleanOperations;
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
 * Implementation of <code>alma.ACS.ROboolean</code>.
 * 
 * @author <a href="mailto:joseph.schwarzATbrera.inaf.it">Joseph Schwarz</a>
 * @author <a href="mailto:gchiozziATeso.org">Gianluca Chiozzi</a>
 * @author <a href="mailto:acaproniATeso.org">Alessandro Caproni</a>
 */
public class RObooleanImpl
	extends ROCommonPropertyImpl
	implements RObooleanOperations {

	/**
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	public RObooleanImpl(
		String name,
		CharacteristicComponentImpl parentComponent)
		throws PropertyInitializationFailed {
		super(boolean.class, name, parentComponent);
	}

	/**
	 * @param name
	 * @param parentComponent
	 * @param dataAccess
	 * @throws PropertyInitializationFailed
	 */
	public RObooleanImpl(
		String name,
		CharacteristicComponentImpl parentComponent,
		DataAccess dataAccess)
		throws PropertyInitializationFailed {
		super(boolean.class, name, parentComponent, dataAccess);
	}

	/**
	 * @see alma.ACS.CommonPropertyImpl#readPropertyTypeCharacteristic(java.lang.String)
	 */
	public Object readPropertyTypeCharacteristic(String name)
		throws NoSuchCharacteristic {
		return Boolean.valueOf(characteristicModelImpl.getBoolean(name));
	}

	/**
	 * @see alma.ACS.PbooleanOperations#default_value()
	 */
	public boolean default_value() {
		return ((Boolean)defaultValue).booleanValue();
	}
	
	/**
	 * @see alma.ACS.ROBooleanOperations#new_subscription_Alarm(alma.ACS.Alarmboolean, alma.ACS.CBDescIn)
	 */
	public Subscription new_subscription_Alarm(Alarmboolean cb,
			CBDescIn desc) {
		throw new NO_IMPLEMENT();
	}

	/**
	 * @see alma.ACS.PbooleanOperations#create_monitor(alma.ACS.CBboolean, alma.ACS.CBDescIn)
	 */
	public Monitorboolean create_monitor(CBboolean cb, CBDescIn desc) {
		return create_postponed_monitor(0, cb, desc);
	}

	
	/**
	 * @see alma.ACS.PbooleanOperations#create_postponed_monitor(long, alma.ACS.CBpattern, alma.ACS.CBDescIn)
	 */
	public Monitorboolean create_postponed_monitor(long start_time, CBboolean cb,CBDescIn desc) {
		// create monitor and its servant
		MonitorbooleanImpl monitorImpl = new MonitorbooleanImpl(this, cb, desc, start_time);
		MonitorbooleanPOATie monitorTie = new MonitorbooleanPOATie(monitorImpl);

		// register and activate		
		return MonitorbooleanHelper.narrow(this.registerMonitor(monitorImpl, monitorTie));
	
	}

	/**
     * @see alma.ACS.PbooleanOperations#get_async(alma.ACS.CBboolean, alma.ACS.CBDescIn)
	 */
	public void get_async(CBboolean cb, CBDescIn desc) {
		getAsync(cb, desc);
	}

	/**
	 * @see alma.ACS.PbooleanOperations#get_history(int, alma.ACS.booleanSeqHolder, alma.ACS.TimeSeqHolder)
	 */ 
	public int get_history(int n_last_values, alma.ACS.booleanSeqHolder vs, alma.ACS.TimeSeqHolder ts) {
		vs.value = (boolean[])getHistory(n_last_values, ts);
		return vs.value.length;
	}

	/**
	 * @see alma.ACS.PBooleanOperations#get_sync(alma.ACSErr.CompletionHolder)
	 */
	public boolean get_sync(CompletionHolder c) {
	try
		{
			return ((Boolean)getSync(c)).booleanValue();
		}
		catch (AcsJException acsex)
		{
			AcsJCouldntPerformActionEx cpa =
				new AcsJCouldntPerformActionEx("Failed to retrieve value", acsex);
			c.value = CompletionUtil.generateCompletion(cpa);
			// return default value in case of error
			return default_value();
		}
	}

	/**
	 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACSErr.Completion, alma.ACS.CBDescOut)
	 */
	public boolean dispatchCallback(CallbackDispatcher.CallbackType type,Object value,Callback callback,Completion completion,CBDescOut desc) {
		try
		{	
			if (type == CallbackDispatcher.CallbackType.DONE_TYPE)
				((CBboolean)callback).done(((Boolean)value).booleanValue(), completion, desc);
			else if (type == CallbackDispatcher.CallbackType.WORKING_TYPE)
				((CBboolean)callback).working(((Boolean)value).booleanValue(), completion, desc);
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
	 * @see alma.ACS.ROdoubleOperations#enable_alarm_system()
	 */
	public void enable_alarm_system() {
		// TODO NO_IMPLEMENT
		throw new NO_IMPLEMENT();
	}
	
	/**
	 * @see alma.ACS.ROdoubleOperations#disable_alarm_system() throws alma.baciErrTypeProperty.DisableAlarmsErrorEx
	 */
	public void disable_alarm_system() throws alma.baciErrTypeProperty.DisableAlarmsErrorEx {
		// TODO NO_IMPLEMENT
		throw new NO_IMPLEMENT();
	}
	
	/**
	 * @see alma.ACS.ROdoubleOperations#alarm_system_enabled()
	 */
	public boolean alarm_system_enabled() {
		// TODO NO_IMPLEMENT
		throw new NO_IMPLEMENT();
	}
	
}

