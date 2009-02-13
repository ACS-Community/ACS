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


import alma.ACS.Bool;
import alma.ACS.BoolSeqHolder;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBfloat;
import alma.ACS.CBpattern;
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACS.Condition;
import alma.ACS.Monitor;
import alma.ACS.Monitorpattern;
import alma.ACS.MonitorpatternPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.RWBoolOperations;
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
 * Implementation of <code>alma.ACS.ROpattern</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class RWBoolImpl
	extends RWCommonPropertyImpl
	implements RWBoolOperations {
	
	/**
	 * @param propertyType
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	public RWBoolImpl(
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
	public RWBoolImpl(
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
     * @see alma.ACS.PBoolOperations#set_async(alma.acs.Bool,alma.ACS.CBpattern, alma.ACS.CBDescIn)
	 */
	public void set_async(Bool value, CBvoid cb, CBDescIn desc) {
		setAsync(value, cb, desc);
	}

	/**
	 * @see alma.ACS.RWBoolOperations#set_nonblocking(alma.acs.Bool)
	 */
	public void set_nonblocking(Bool value) {
		setNonblocking(value);
		
	}

	/**
	 * @see alma.ACS.RWBoolOperations#set_sync(alma.acs.Bool)
	 */
	public Completion set_sync(Bool value) {
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
	
	/**
	 * @see alma.ACS.PBoolOperations#allStates()
	 */
	public Bool[] allStates() {
		try {
			String[] tmp = characteristicModelImpl.getStringSeq("statesDescription");
			Bool[] ret = new Bool[tmp.length];
			 for (int i=0; i<tmp.length; i++)
			        ret[i] = Bool.from_int(i);
			        
			 return ret;
			
		} catch (NoSuchCharacteristic e) {
		//noop
		}
		return null;
	}

	/**
	 * @see alma.ACS.PBoolOperations#condition()
	 */
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

	/**
	 * @see alma.ACS.PBoolOperations#create_monitor(alma.ACS.CBpattern, alma.ACS.CBDescIn)
	 */
	public Monitorpattern create_monitor(CBpattern cb, CBDescIn desc) {
		return (Monitorpattern) create_postponed_monitor(0, cb, desc);
	}

	/**
	 * @see alma.ACS.PBoolOperations#create_postponed_monitor(long, alma.ACS.CBpattern, alma.ACS.CBDescIn)
	 */
	public Monitor create_postponed_monitor(long start_time, CBpattern cb,CBDescIn desc) {
		// create monitor and its servant
		MonitorpatternImpl monitorImpl = new MonitorpatternImpl(this, cb, desc, start_time);
		MonitorpatternPOATie monitorTie = new MonitorpatternPOATie(monitorImpl);

		// register and activate		
		return MonitorfloatHelper.narrow(this.registerMonitor(monitorImpl, monitorTie));
	
	}

	/**
	 * @see alma.ACS.PBoolOperations#default_value()
	 */
	public Bool default_value() {
		return ((Bool)defaultValue);
	}

	/**
     * @see alma.ACS.PBoolOperations#get_async(alma.ACS.CBpattern, alma.ACS.CBDescIn)
	 */
	public void get_async(CBpattern cb, CBDescIn desc) {
		getAsync(cb, desc);
	}

	/**
	 * @see alma.ACS.PBoolOperations#get_history(int, alma.ACS.BoolSeqHolder, alma.ACS.TimeSeqHolder)
	 */ 
	public int get_history(int n_last_values, BoolSeqHolder vs, TimeSeqHolder ts) {
		vs.value = (Bool[])getHistory(n_last_values, ts);
		return vs.value.length;
	}

	/**
	 * @see alma.ACS.PBoolOperations#get_sync(alma.ACSErr.CompletionHolder)
	 */
	public Bool get_sync(CompletionHolder c) {
	try
		{
			return ((Bool)getSync(c));
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
	 * @see alma.ACS.PBoolOperations#statesDescription()
	 */
	public String[] statesDescription() {
		try {
			return characteristicModelImpl.getStringSeq("statesDescription");
		} catch (NoSuchCharacteristic e) {
			//noop
		}
		return null;
	}

	/**
	 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACSErr.Completion, alma.ACS.CBDescOut)
	 */
	public boolean dispatchCallback(int type,Object value,Callback callback,Completion completion,CBDescOut desc) {
		try
		{	
			if (type == CallbackDispatcher.DONE_TYPE)
				((CBfloat)callback).done(((Float)value).floatValue(), completion, desc);
			else if (type == CallbackDispatcher.WORKING_TYPE)
				((CBfloat)callback).working(((Float)value).floatValue(), completion, desc);
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