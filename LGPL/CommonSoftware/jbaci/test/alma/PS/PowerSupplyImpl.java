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

package alma.PS;

import java.util.Random;
import java.util.concurrent.atomic.AtomicBoolean;

import alma.ACS.CBDescIn;
import alma.ACS.CBvoid;
import alma.ACS.impl.CharacteristicComponentImpl;
import alma.ACSErr.CompletionHolder;
import alma.ACS.ROdouble;
import alma.ACS.ROdoubleHelper;
import alma.ACS.impl.ROdoubleImpl;
import alma.ACS.ROdoublePOATie;
import alma.ACS.ROpattern;
import alma.ACS.ROpatternHelper;
import alma.ACS.impl.ROpatternImpl;
import alma.ACS.ROpatternPOATie;
import alma.ACS.RWdouble;
import alma.ACS.RWdoubleHelper;
import alma.ACS.impl.RWdoubleImpl;
import alma.ACS.RWdoublePOATie;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.DataAccessSupport;
import alma.ACS.jbaci.MemoryDataAccess;
import alma.ACS.jbaci.ReflectionBACIAction;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;

/**
 * Implementation of <code>alma.PS.PowerSupply</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class PowerSupplyImpl extends CharacteristicComponentImpl 
	implements PowerSupplyOperations {

	/**
	 * Readback property.
	 */
	protected ROdouble readback;

	/**
	 * Current property.
	 */
	protected RWdouble current;

	/**
	 * Status property.
	 */
	protected ROpattern status;

	/**
	 * @see alma.acs.component.ComponentLifecycle#initialize(alma.acs.container.ContainerServices)
	 */
	public void initialize(ContainerServices containerServices)
		throws ComponentLifecycleException {

		super.initialize(containerServices);
		
		try
		{
			// readback/current
			DataAccess currentDataAccess = new MemoryDataAccess();
			DataAccess readbackDataAccess = new ReadbackDataAccess(currentDataAccess, 10.0);
			
			// current
			RWdoubleImpl currentImpl = new RWdoubleImpl("current", this, currentDataAccess);
			RWdoublePOATie currentTie = new RWdoublePOATie(currentImpl);
			current = RWdoubleHelper.narrow(this.registerProperty(currentImpl, currentTie));

			// readback
			ROdoubleImpl readbackImpl = new ROdoubleImpl("readback", this, readbackDataAccess);
			ROdoublePOATie readbackTie = new ROdoublePOATie(readbackImpl);
			readback = ROdoubleHelper.narrow(this.registerProperty(readbackImpl, readbackTie));

			// status
			ROpatternImpl statusImpl = new ROpatternImpl("status", this, new StatusDataAccess());
			ROpatternPOATie statusTie = new ROpatternPOATie(statusImpl);
			status = ROpatternHelper.narrow(this.registerProperty(statusImpl, statusTie));
			
		}
		catch (Throwable th)
		{
			throw new ComponentLifecycleException("Failed to create properties.", th); 
		}
	}

	/*********************** [ PowerSupply ] ***********************/

	/**
	 * @see alma.PS.PowerSupplyOperations#current()
	 */
	public RWdouble current() {
		return current;
	}

	/**
	 * @see alma.PS.PowerSupplyOperations#status()
	 */
	public ROpattern status() {
		return status;
	}

	/**
	 * @see alma.PS.PowerSupplyOperations#readback()
	 */
	public ROdouble readback() {
		return readback;
	}

	/**
	 * @see alma.PS.PowerSupplyOperations#on(alma.ACS.CBvoid, alma.ACS.CBDescIn)
	 */
	public void on(CBvoid callback, CBDescIn desc) {
		new ReflectionBACIAction(this, this, getClass(), "onImpl", callback, desc).submit();
	}

	/**
	 * @see alma.PS.PowerSupplyOperations#off(alma.ACS.CBvoid, alma.ACS.CBDescIn)
	 */
	public void off(CBvoid callback, CBDescIn desc) {
		new ReflectionBACIAction(this, this, getClass(), "offImpl", callback, desc).submit();
	}

	/**
	 * @see alma.PS.PowerSupplyOperations#reset(alma.ACS.CBvoid, alma.ACS.CBDescIn)
	 */
	public void reset(CBvoid callback, CBDescIn desc) {
		new ReflectionBACIAction(this, this, getClass(), "resetImpl", callback, desc).submit();
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

	/**
	 * Status property data access - on/off status implemented.
	 */
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
	protected class ReadbackDataAccess implements DataAccess
	{ 
		/**
		 * Current signal.
		 */
		protected DataAccess current;
		
		/**
		 * Noise amplitude.
		 */
		protected double amplitude;
		
		/**
		 * Noise random generator.
		 */
		protected Random noise = new Random();

		/**
		 * Last value (used to achieve nice stepping).
		 */
		protected double lastValue = 0;

		/**
		 * Constructor.
		 * @param current	current signal (<code>DataAccess</code>).
		 */
		public ReadbackDataAccess(DataAccess current, double amplitude)
		{
			this.current = current;
			this.amplitude = amplitude;
		}
		
		/**
		 * @see alma.ACS.jbaci.DataAccess#addValueChangeListener(alma.ACS.jbaci.DataAccess.ValueChangeListener)
		 */
		public void addValueChangeListener(ValueChangeListener listener)
			throws OnChangeNotSupportedException {
			// we do not support on-change notifications,
			// as default implementation of MemoryDataAccess does
			// but can be done simply by registering ValueChangeListener on sourceSignals
			// recommend 1s pooling
			throw new OnChangeNotSupportedException(1000);
		}

		/**
		 * @see alma.ACS.jbaci.DataAccess#removeValueChangeListener(alma.ACS.jbaci.DataAccess.ValueChangeListener)
		 */
		public void removeValueChangeListener(ValueChangeListener listener) {
			// noop
		}

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
				
			double val = 0;
			
			// read current only if powersupply is 'on'
			if (onState.get())
				val = ((Double)current.get(completionHolder)).doubleValue();
			
			// stepping			
			val = (val + lastValue)/2;

			// add some noise
			val += - amplitude + 2 * amplitude * noise.nextDouble();
			val = Math.abs(val);
			lastValue = val;
			 
			// only positive readback please
			return new Double(val);
		}

		/**
		 * @see alma.ACS.jbaci.DataAccess#set(java.lang.Object, alma.ACSErr.CompletionHolder)
		 */
		public void set(Object value, CompletionHolder completion)
			throws AcsJException {
			throw new AcsJCouldntPerformActionEx("'readback' cannot be set.");
		}
	}

}
