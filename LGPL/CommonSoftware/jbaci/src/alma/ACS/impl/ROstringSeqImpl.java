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

import alma.ACS.AlarmstringSeq;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBstringSeq;
import alma.ACS.Callback;
import alma.ACS.MonitorstringSeq;
import alma.ACS.MonitorstringSeqHelper;
import alma.ACS.MonitorstringSeqPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.ROstringSeqOperations;
import alma.ACS.Subscription;
import alma.ACS.TimeSeqHolder;
import alma.ACS.stringSeqSeqHolder;
import alma.ACS.jbaci.CallbackDispatcher;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;

/**
 * Implementation of <code>alma.ACS.ROstringSeq</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class ROstringSeqImpl
	extends ROCommonPropertyImpl
	implements ROstringSeqOperations {

	/**
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	public ROstringSeqImpl(
		String name,
		CharacteristicComponentImpl parentComponent)
		throws PropertyInitializationFailed {
		super(String[].class, name, parentComponent);
	}

	/**
	 * @param name
	 * @param parentComponent
	 * @param dataAccess
	 * @throws PropertyInitializationFailed
	 */
	public ROstringSeqImpl(
		String name,
		CharacteristicComponentImpl parentComponent,
		DataAccess dataAccess)
		throws PropertyInitializationFailed {
		super(String[].class, name, parentComponent, dataAccess);
	}

	/**
	 * @see alma.ACS.PCommonPropertyImpl#readPropertyTypeCharacteristic(java.lang.String)
	 */
	public Object readPropertyTypeCharacteristic(String name)
		throws NoSuchCharacteristic {
		return characteristicModelImpl.getStringSeq(name);
	}

	/**
	 * @see alma.ACS.PstringSeqOperations#create_monitor(alma.ACS.CBstringSeq, alma.ACS.CBDescIn)
	 */
	public MonitorstringSeq create_monitor(CBstringSeq callback, CBDescIn desc) {
		return create_postponed_monitor(0, callback, desc);
	}

	/**
	 * @see alma.ACS.PstringSeqOperations#create_postponed_monitor(long, alma.ACS.CBstringSeq, alma.ACS.CBDescIn)
	 */
	public MonitorstringSeq create_postponed_monitor(long startTime,	CBstringSeq callback, CBDescIn desc) {
		// create monitor and its servant
		MonitorstringSeqImpl monitorImpl = new MonitorstringSeqImpl(this, callback, desc, startTime);
		MonitorstringSeqPOATie monitorTie = new MonitorstringSeqPOATie(monitorImpl);

		// register and activate		
		return MonitorstringSeqHelper.narrow(this.registerMonitor(monitorImpl, monitorTie));
	}

	/**
	 * @see alma.ACS.PstringSeqOperations#default_value()
	 */
	public String[] default_value() {
		return ((String[])defaultValue);
	}

	/**
	 * @see alma.ACS.PstringSeqOperations#get_async(alma.ACS.CBstringSeq, alma.ACS.CBDescIn)
	 */
	public void get_async(CBstringSeq callback, CBDescIn desc) {
		getAsync(callback, desc);
	}

	/**
	 * @see alma.ACS.PstringSeqOperations#get_history(int, alma.ACS.stringSeqSeqHolder, alma.ACS.TimeSeqHolder)
	 */
	public int get_history(
		int length,
		stringSeqSeqHolder values,
		TimeSeqHolder times) {
		values.value = (String[][])getHistory(length, times);
		return values.value.length;
	}

	/**
	 * @see alma.ACS.PstringSeqOperations#get_sync(alma.ACSErr.CompletionHolder)
	 */
	public String[] get_sync(CompletionHolder completionHolder) {
		try
		{
			return (String[])getSync(completionHolder);
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
	 * @see alma.ACS.ROstringSeqOperations#new_subscription_Alarm(alma.ACS.AlarmstringSeq, alma.ACS.CBDescIn)
	 */
	public Subscription new_subscription_Alarm(
		AlarmstringSeq callback,
		CBDescIn desc) {
		// TODO NO_IMPLEMENT
		throw new NO_IMPLEMENT();
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
				((CBstringSeq)callback).done((String[])value, completion, desc);
			else if (type == CallbackDispatcher.WORKING_TYPE)
				((CBstringSeq)callback).working((String[])value, completion, desc);
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
