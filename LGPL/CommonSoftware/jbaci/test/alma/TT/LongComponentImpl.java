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

import java.util.Random;
import java.util.concurrent.atomic.AtomicBoolean;

import alma.ACS.CBDescIn;
import alma.ACS.CBvoid;
import alma.ACS.ROlong;
import alma.ACS.ROlongHelper;
import alma.ACS.ROlongPOATie;
import alma.ACS.ROpattern;
import alma.ACS.ROpatternHelper;
import alma.ACS.ROpatternPOATie;
import alma.ACS.RWlong;
import alma.ACS.RWlongHelper;
import alma.ACS.RWlongPOATie;
import alma.ACS.impl.CharacteristicComponentImpl;
import alma.ACS.impl.ROlongImpl;
import alma.ACS.impl.ROpatternImpl;
import alma.ACS.impl.RWlongImpl;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.DataAccessSupport;
import alma.ACS.jbaci.MemoryDataAccess;
import alma.ACS.jbaci.ReflectionBACIAction;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;

/**
 * Implementation of <code>test.jbaci.SimpleMasterComponent</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class LongComponentImpl extends CharacteristicComponentImpl 
	implements LongComponentOperations {


	//protected ROdouble readback;
	//protected RWdouble current;
	protected ROpattern status;
	protected ROlong longReadback;
	protected RWlong longCurrent;
	//protected ROfloat floatReadback;
	//protected RWfloat floatCurrent;


	
	/**
	 * @see alma.acs.component.ComponentLifecycle#initialize(alma.acs.container.ContainerServices)
	 */
	public void initialize(ContainerServices containerServices)
		throws ComponentLifecycleException {

		super.initialize(containerServices);
		
		try
		{
//			 readback/current
			DataAccess currentDataAccess = new MemoryDataAccess();
			DataAccess readbackLongDataAccess = new ReadbackLongDataAccess(currentDataAccess, (int)10);

			//long
			//current
			RWlongImpl currentLongImpl = new RWlongImpl("longCurrent", this, currentDataAccess);
			RWlongPOATie currentLongTie = new RWlongPOATie(currentLongImpl);
			longCurrent = RWlongHelper.narrow(this.registerProperty(currentLongImpl, currentLongTie));

			// readback
			ROlongImpl readbackLongImpl = new ROlongImpl("longReadback", this, readbackLongDataAccess);
			ROlongPOATie readbackLongTie = new ROlongPOATie(readbackLongImpl);
			longReadback = ROlongHelper.narrow(this.registerProperty(readbackLongImpl, readbackLongTie));
			
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

	public ROpattern status() {
		return status;
	}
	public ROlong longReadback() {
		return longReadback;
	}	
	public RWlong longCurrent() {
		return longCurrent;
	}

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
	protected class ReadbackLongDataAccess implements DataAccess
	{ 
		/**
		 * Current signal.
		 */
		protected DataAccess current;
		
		/**
		 * Noise amplitude.
		 */
		protected int amplitude;
		
		/**
		 * Noise random generator.
		 */
		protected Random noise = new Random();

		/**
		 * Last value (used to achieve nice stepping).
		 */
		protected int lastValue = 0;

		/**
		 * Constructor.
		 * @param current	current signal (<code>DataAccess</code>).
		 */
		public ReadbackLongDataAccess(DataAccess current, int amplitude)
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
				
			int val = 0;
			
			// read current only if powersupply is 'on'
			if (onState.get())
				val = ((Integer)current.get(completionHolder)).intValue();
			
			// stepping			
			val = (val + lastValue)/2;

			// add some noise
			val += - amplitude + 2 * amplitude * noise.nextInt();
			val = Math.abs(val);
			lastValue = val;
			 
			// only positive readback please
			return new Integer(val);
		}

		/**
		 * @see alma.ACS.jbaci.DataAccess#set(java.lang.Object, alma.ACSErr.CompletionHolder)
		 */
		public void set(Object value, CompletionHolder completion)
			throws AcsJException {
			throw new AcsJCouldntPerformActionEx("'longReadback' cannot be set.");
		}
	}



}
