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

import java.lang.String;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBstring;
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACS.Monitor;
import alma.ACS.MonitorstringHelper;
import alma.ACS.MonitorstringPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.RWstringOperations;
import alma.ACS.TimeSeqHolder;
import alma.ACS.stringSeqHolder;
import alma.ACS.jbaci.CallbackDispatcher;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;
import alma.ACS.impl.RWCommonPropertyImpl;

public class RWstringImpl
	extends RWCommonPropertyImpl
	implements RWstringOperations {

	/**
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	public RWstringImpl(
		String name,
		CharacteristicComponentImpl parentComponent)
		throws PropertyInitializationFailed {
		super(String.class, name, parentComponent);
	}

	/**
	 * @param name
	 * @param parentComponent
	 * @param dataAccess
	 * @throws PropertyInitializationFailed
	 */
	public RWstringImpl(
		String name,
		CharacteristicComponentImpl parentComponent,
		DataAccess dataAccess)
		throws PropertyInitializationFailed {
		super(String.class, name, parentComponent, dataAccess);
	}

	/**
	 * @see alma.ACS.PCommonPropertyImpl#readPropertyTypeCharacteristic(java.lang.String)
	 */
	public Object readPropertyTypeCharacteristic(String name)
		throws NoSuchCharacteristic {
		return characteristicModelImpl.getString(name);
	}

	/**
	 * @see alma.ACS.PstringOperations#create_monitor(alma.ACS.CBstring, alma.ACS.CBDescIn)
	 */
	public Monitor create_monitor(CBstring callback, CBDescIn desc) {
		return create_postponed_monitor(0, callback, desc);
	}

	/**
	 * @see alma.ACS.PstringOperations#create_postponed_monitor(long, alma.ACS.CBstring, alma.ACS.CBDescIn)
	 */
	public Monitor create_postponed_monitor(long startTime,	CBstring callback, CBDescIn desc) {
		// create monitor and its servant
		MonitorstringImpl monitorImpl = new MonitorstringImpl(this, callback, desc, startTime);
		MonitorstringPOATie monitorTie = new MonitorstringPOATie(monitorImpl);

		// register and activate		
		return MonitorstringHelper.narrow(this.registerMonitor(monitorImpl, monitorTie));
	}

	/**
	 * @see alma.ACS.PstringOperations#default_value()
	 */
	public String default_value() {
		return (String)defaultValue;
	}

	/**
	 * @see alma.ACS.PstringOperations#get_async(alma.ACS.CBstring, alma.ACS.CBDescIn)
	 */
	public void get_async(CBstring callback, CBDescIn desc) {
		getAsync(callback, desc);
	}

	/**
	 * @see alma.ACS.PstringOperations#set_async(java.lang.String value, alma.ACS.CBvoid, alma.ACS.CBDescIn)
	 */
	public void set_async(String value, CBvoid callback, CBDescIn desc) {
		setAsync(new String(value), callback, desc);
	}

	/**
	 * @see alma.ACS.PstringOperations#set_nonblocking(java.lang.String)
	 */
	public void set_nonblocking(String value) {
		setNonblocking(value);
	}

	/**
	 * @see alma.ACS.PstringOperations#set_sync(java.lang.String)
	 */
	public Completion set_sync(String value) {
		try
		{
			return setSync(value);
		}
		catch (AcsJException acsex)
		{
			AcsJCouldntPerformActionEx cpa = new AcsJCouldntPerformActionEx("Failed to set value",acsex);	
			return CompletionUtil.generateCompletion(cpa);		
		//	AcsJCouldntPerformActionEx cpa = new AcsJCouldntPerformActionEx();
		//	return CompletionUtil.generateCompletion(cpa);
		
		}
				
	}


	/**
	 * @see alma.ACS.PstringOperations#get_history(int, alma.ACS.stringSeqHolder, alma.ACS.TimeSeqHolder)
	 */
	public int get_history(
		int length,
		stringSeqHolder values,
		TimeSeqHolder times) {
		values.value = (String[])getHistory(length, times);
		return values.value.length;
	}

	/**
	 * @see alma.ACS.PstringOperations#get_sync(alma.ACSErr.CompletionHolder)
	 */
	public String get_sync(CompletionHolder completionHolder) {
		try
		{
			return (String)getSync(completionHolder);
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
				((CBstring)callback).done((String)value, completion, desc);
			else if (type == CallbackDispatcher.WORKING_TYPE)
				((CBstring)callback).working((String)value, completion, desc);
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