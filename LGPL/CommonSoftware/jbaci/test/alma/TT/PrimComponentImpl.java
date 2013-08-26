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

package alma.TT;

import java.util.concurrent.atomic.AtomicBoolean;

import alma.ACS.ROdouble;
import alma.ACS.ROdoubleHelper;
import alma.ACS.ROdoublePOATie;
import alma.ACS.ROfloat;
import alma.ACS.ROfloatHelper;
import alma.ACS.ROfloatPOATie;
import alma.ACS.ROlong;
import alma.ACS.ROlongHelper;
import alma.ACS.ROlongPOATie;
import alma.ACS.ROpattern;
import alma.ACS.ROpatternHelper;
import alma.ACS.ROpatternPOATie;
import alma.ACS.RWdouble;
import alma.ACS.RWdoubleHelper;
import alma.ACS.RWdoublePOATie;
import alma.ACS.RWfloat;
import alma.ACS.RWfloatHelper;
import alma.ACS.RWfloatPOATie;
import alma.ACS.RWlong;
import alma.ACS.RWlongHelper;
import alma.ACS.RWlongPOATie;
import alma.ACS.impl.CharacteristicComponentImpl;
import alma.ACS.impl.ROdoubleImpl;
import alma.ACS.impl.ROfloatImpl;
import alma.ACS.impl.ROlongImpl;
import alma.ACS.impl.ROpatternImpl;
import alma.ACS.impl.RWdoubleImpl;
import alma.ACS.impl.RWfloatImpl;
import alma.ACS.impl.RWlongImpl;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.DataAccessSupport;
import alma.ACS.jbaci.MemoryDataAccess;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;
/**
 * Implementation of <code>alma.TT.PrimTestComponent</code>.
 * @author <a href="mailto:cmenayATcsrg.inf.utfsm.cl">Camilo Menay</a>
 * @version $id$
 */
public class PrimComponentImpl extends CharacteristicComponentImpl 
	implements PrimComponentOperations {


	//protected ROdouble readback;
	//protected RWdouble current;
	protected ROpattern status;
	protected ROlong longRO;
	protected RWlong longRW;
	protected ROdouble doubleRO;
	protected RWdouble doubleRW;
	protected ROfloat floatRO;
	protected RWfloat floatRW;

	
	/**
	 * @see alma.acs.component.ComponentLifecycle#initialize(alma.acs.container.ContainerServices)
	 */
	public void initialize(ContainerServices containerServices)
		throws ComponentLifecycleException {

		super.initialize(containerServices);
		
		try
		{
			
			DataAccess currentDataAccess = new MemoryDataAccess();	
			DataAccess currentDataAccess2 = new MemoryDataAccess();	
			DataAccess currentDataAccess3 = new MemoryDataAccess();	
			DataAccess currentDataAccess4 = new MemoryDataAccess();	
			DataAccess currentDataAccess5 = new MemoryDataAccess();	
			DataAccess currentDataAccess6 = new MemoryDataAccess();	
			
			
			/* Long properties */
			RWlongImpl readbackLongRWImpl = new RWlongImpl("longRW", this, currentDataAccess);
			RWlongPOATie readbackLongRWTie = new RWlongPOATie(readbackLongRWImpl);
			longRW = RWlongHelper.narrow(this.registerProperty(readbackLongRWImpl, readbackLongRWTie));
			
			ROlongImpl readbackLongROImpl = new ROlongImpl("longRO", this, currentDataAccess2);
			ROlongPOATie readbackLongROTie = new ROlongPOATie(readbackLongROImpl);
			longRO = ROlongHelper.narrow(this.registerProperty(readbackLongROImpl, readbackLongROTie));
			/* Double properties */
			RWdoubleImpl readbackDoubleRWImpl = new RWdoubleImpl("doubleRW", this, currentDataAccess3);
			RWdoublePOATie readbackDoubleRWTie = new RWdoublePOATie(readbackDoubleRWImpl);
			doubleRW = RWdoubleHelper.narrow(this.registerProperty(readbackDoubleRWImpl, readbackDoubleRWTie));
			
			ROdoubleImpl readbackDoubleROImpl = new ROdoubleImpl("doubleRO", this, currentDataAccess4);
			ROdoublePOATie readbackDoubleROTie = new ROdoublePOATie(readbackDoubleROImpl);
			doubleRO = ROdoubleHelper.narrow(this.registerProperty(readbackDoubleROImpl, readbackDoubleROTie));
			/* Float properties */
			RWfloatImpl readbackFloatRWImpl = new RWfloatImpl("floatRW", this, currentDataAccess5);
			RWfloatPOATie readbackFloatRWTie = new RWfloatPOATie(readbackFloatRWImpl);
			floatRW = RWfloatHelper.narrow(this.registerProperty(readbackFloatRWImpl, readbackFloatRWTie));
			
			ROfloatImpl readbackFloatROImpl = new ROfloatImpl("floatRO", this, currentDataAccess6);
			ROfloatPOATie readbackFloatROTie = new ROfloatPOATie(readbackFloatROImpl);
			floatRO = ROfloatHelper.narrow(this.registerProperty(readbackFloatROImpl, readbackFloatROTie));
			
			/* Patter property */
			ROpatternImpl statusImpl = new ROpatternImpl("status", this, new StatusDataAccess());
			ROpatternPOATie statusTie = new ROpatternPOATie(statusImpl);
			status = ROpatternHelper.narrow(this.registerProperty(statusImpl, statusTie));

		}
		catch (Throwable th)
		{
			throw new ComponentLifecycleException("Failed to create properties.", th); 
		}
	}


/******************* [ Implementations of actions ] *******************/
	
	/**
	 * On/off state.
	 */
	protected AtomicBoolean onState = new AtomicBoolean(false);

	/**
	 * Implementation of 'on' method.
	 * @throws	ACS exception on any failure.
	 */
	public synchronized void onImpl() throws AcsJException
	{ 
		if (onState.getAndSet(true) == true)
			throw new AcsJCouldntPerformActionEx("Already 'on'.");
	}

	/**
	 * Implementation of 'off' method.
	 * @throws	ACS exception on any failure.
	 */
	public synchronized void offImpl() throws AcsJException
	{ 
		if (onState.getAndSet(false) == false)
			throw new AcsJCouldntPerformActionEx("Already 'off'.");
	}

	/**
	 * Implementation of 'reset' method.
	 * @throws	ACS exception on any failure.
	 */
	public void resetImpl() throws AcsJException
	{ 
		if (onState.get() == false)
			throw new AcsJCouldntPerformActionEx("In 'off' state.");
	}
	
	/******************* [ DataAccess implementations ] *******************/


	protected class StatusDataAccess extends DataAccessSupport
	{ 
		/**
		 * @see alma.ACS.jbaci.DataAccess#initializeValue()
		 */
		public boolean initializeValue() {
			return false;
		}

		/**
		 * @see alma.ACS.jbaci.DataAccess#get(alma.ACSErr.CompletionHolder)
		 */
		public Object get(CompletionHolder completionHolder)
			throws AcsJException {
			return new Integer(onState.get() ? 1 : 0);
		}

		/**
		 * @see alma.ACS.jbaci.DataAccess#set(java.lang.Object, alma.ACSErr.CompletionHolder)
		 */
		public void set(Object value, CompletionHolder completion)
			throws AcsJException {
			if ((((Integer)value).intValue() % 2) == 1)
				onImpl();
			else
				offImpl();
			// TODO no notify here...
		}
	}
	/**
	 * Implementation of readback data access (adds some random noise and responds to on/off status).
	 */
	
	
	
	/**
	 * 
	 *  Methods for returning the properties
	 *  
	 * 
	 */
	
	public RWdouble doubleRW() {
		return doubleRW;
	}

	public ROdouble doubleRO() {
		return doubleRO;
	}

	public RWfloat floatRW() {
		return floatRW;
	}

	public ROfloat floatRO() {
		return floatRO;
	}

	public RWlong longRW() {
		return longRW;
	}

	public ROlong longRO() {
		return longRO;
	}

	public ROpattern status() {
		return status;
	}



}
