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
import alma.ACS.CBdoubleSeq; //change
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACS.Monitordouble;
import alma.ACS.MonitordoubleHelper;
import alma.ACS.MonitordoublePOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.RWdoubleSeqOperations;
import alma.ACS.TimeSeqHolder;
import alma.ACS.doubleSeqSeqHolder;//change
import alma.ACS.jbaci.CallbackDispatcher;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;

/**
 * Implementation of <code>alma.ACS.RWdouble</code>.
 * @author <a href="mailto:cmenayATcsrg.inf.utfsm.cl">Camilo Menay</a>
 * @author <a href="mailto:cmaureirATinf.utfsm.cl">Cristian Maureira</a>
 * @version $id$
 */
public class RWdoubleSeqImpl
	extends RWCommonComparablePropertyImpl
	implements RWdoubleSeqOperations {

	/**
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	public RWdoubleSeqImpl(
		String name,
		CharacteristicComponentImpl parentComponent)
		throws PropertyInitializationFailed {
		super(double[].class, name, parentComponent);
	}//check

	/**
	 * @param name
	 * @param parentComponent
	 * @param dataAccess
	 * @throws PropertyInitializationFailed
	 */
	public RWdoubleSeqImpl(
		String name,
		CharacteristicComponentImpl parentComponent,
		DataAccess dataAccess)
		throws PropertyInitializationFailed {
		super(double[].class, name, parentComponent, dataAccess);
	}

	/**
	 * @see alma.ACS.CommonPropertyImpl#readPropertyTypeCharacteristic(java.lang.String)
	 */
	public Object readPropertyTypeCharacteristic(String name)
		throws NoSuchCharacteristic {
		return characteristicModelImpl.getDoubleSeq(name);
	}

	/**
	 * @see alma.ACS.PdoubleOperations#create_monitor(alma.ACS.CBdouble, alma.ACS.CBDescIn)
	 */
	public Monitordouble create_monitor(CBdoubleSeq callback, CBDescIn descIn) {
		return create_postponed_monitor(0, callback, descIn);
	}

	/**
	 * @see alma.ACS.PdoubleOperations#create_postponed_monitor(long, alma.ACS.CBdouble, alma.ACS.CBDescIn)
	 */
	public Monitordouble create_postponed_monitor(
		long startTime,
		CBdoubleSeq callback,
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
		return ((double[])defaultValue)[0];
	}

	/**
	 * @see alma.ACS.PdoubleOperations#get_async(alma.ACS.CBdouble, alma.ACS.CBDescIn)
	 */
	public void get_async(CBdoubleSeq arg0, CBDescIn arg1) {
		getAsync(arg0, arg1);
	}

	/**
	 * @see alma.ACS.PdoubleOperations#get_history(int, alma.ACS.doubleSeqHolder, alma.ACS.TimeSeqHolder)
	 */
	public int get_history(
		int arg0,
		doubleSeqSeqHolder arg1,
		TimeSeqHolder arg2) {
		arg1.value = (double[][])getHistory(arg0, arg2);
		return arg1.value.length;
	}

	/**
	 * @see alma.ACS.PdoubleOperations#get_sync(alma.ACSErr.CompletionHolder)
	 */
	public double[] get_sync(CompletionHolder completionHolder) {
		try
		{
			return ((double[])getSync(completionHolder));
		}
		catch (AcsJException acsex)
		{
			AcsJCouldntPerformActionEx cpa =
				new AcsJCouldntPerformActionEx("Failed to retrieve value", acsex);
			completionHolder.value = CompletionUtil.generateCompletion(cpa);
			// return default value in case of error
			//must be a double[] not an double
			//return default_value();
			return null;
		}
		
	}

	/**
	 * @see alma.ACS.PdoubleOperations#graph_max()
	 */
	public double graph_max() {
		return ((double[])graphMax)[0];
	}

	/**
	 * @see alma.ACS.PdoubleOperations#graph_min()
	 */
	public double graph_min() {
		return ((double[])graphMin)[0];
	}

	/**
	 * @see alma.ACS.PdoubleOperations#min_delta_trigger()
	 */
	public double min_delta_trigger() {
		return ((double[])minDeltaTrigger)[0];
	}

	/**
	 * @see alma.ACS.PdoubleOperations#min_step()
	 */
	public double min_step() {
		return ((double[])minStep)[0];
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
	 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACSErr.Completion, alma.ACS.CBDescOut)
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
				((CBdoubleSeq)callback).done(((double[])value), completion, desc);
			else if (type == CallbackDispatcher.CallbackType.WORKING_TYPE)
				((CBdoubleSeq)callback).working(((double[])value), completion, desc);
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
		double val2 = ((Double)value2).doubleValue();
		if (substract)
			val2 = -val2;
		return new Double(((Double)value1).doubleValue() + val2);
	}



	/**
	 * @see alma.ACS.RWdoubleOperations#max_value()
	 */
	public double max_value() {
		return ((double[])maxValue)[0];
	}

	/**
	 * @see alma.ACS.RWdoubleOperations#min_value()
	 */
	public double min_value() {
		return ((double[])minValue)[0];
	}

	/**
	 * @see alma.ACS.RWdoubleOperations#set_async(double, alma.ACS.CBvoid, alma.ACS.CBDescIn)
	 */
	public void set_async(double[] value, CBvoid callback, CBDescIn descIn) {
		setAsync(value, callback, descIn);
	}

	/**
	 * @see alma.ACS.RWdoubleOperations#set_nonblocking(double)
	 */
	public void set_nonblocking(double[] value) {
		setNonblocking(value);
	}

	/**
	 * @see alma.ACS.RWdoubleOperations#set_sync(double)
	 */
	public Completion set_sync(double[] value) {
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
	 * @see alma.ACS.RWdoubleOperations#set_async(CBdoubleSeq,CBDescIn)
	 */
	


}
