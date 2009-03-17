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

import alma.ACS.ROdoubleSeq;
import alma.ACS.ROdoubleSeqHelper;
import alma.ACS.ROdoubleSeqPOATie;
import alma.ACS.ROfloatSeq;
import alma.ACS.ROfloatSeqHelper;
import alma.ACS.ROfloatSeqPOATie;
import alma.ACS.ROlongSeq;
import alma.ACS.ROlongSeqHelper;
import alma.ACS.ROlongSeqPOATie;
import alma.ACS.ROpattern;
import alma.ACS.ROpatternHelper;
import alma.ACS.ROpatternPOATie;
import alma.ACS.RWdoubleSeq;
import alma.ACS.RWdoubleSeqHelper;
import alma.ACS.RWdoubleSeqPOATie;
import alma.ACS.RWfloatSeq;
import alma.ACS.RWfloatSeqHelper;
import alma.ACS.RWfloatSeqPOATie;
import alma.ACS.RWlongSeq;
import alma.ACS.RWlongSeqHelper;
import alma.ACS.RWlongSeqPOATie;
import alma.ACS.impl.CharacteristicComponentImpl;
import alma.ACS.impl.ROdoubleSeqImpl;
import alma.ACS.impl.ROfloatSeqImpl;
import alma.ACS.impl.ROlongSeqImpl;
import alma.ACS.impl.ROpatternImpl;
import alma.ACS.impl.RWdoubleSeqImpl;
import alma.ACS.impl.RWfloatSeqImpl;
import alma.ACS.impl.RWlongSeqImpl;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.DataAccessSupport;
import alma.ACS.jbaci.MemoryDataAccess;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;

/**
 * Implementation of <code>test.jbaci.SimpleMasterComponent</code>.
 * @author <a href="mailto:cmenayATcsrg.inf.utfsm.cl">Camilo Menay</a>
 * @version $id$
 */
public class SeqComponentImpl extends CharacteristicComponentImpl  
	implements SeqComponentOperations {


	//protected ROdouble readback;
	//protected RWdouble current;
	protected ROpattern status;
	protected ROlongSeq longSeqRO;
	protected RWlongSeq longSeqRW;
	protected ROdoubleSeq doubleSeqRO;
	protected RWdoubleSeq doubleSeqRW;
	protected ROfloatSeq floatSeqRO;
	protected RWfloatSeq floatSeqRW;

	
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
			ROlongSeqImpl readbackLongSeqImpl = new ROlongSeqImpl("longSeqRO", this, new readbackLongSeqDataAccess());
			ROlongSeqPOATie readbackLongSeqTie = new ROlongSeqPOATie(readbackLongSeqImpl);
			longSeqRO = ROlongSeqHelper.narrow(this.registerProperty(readbackLongSeqImpl, readbackLongSeqTie));
			
			RWlongSeqImpl readbackLongSeqRWImpl = new RWlongSeqImpl("longSeqRW", this, currentDataAccess2);
			RWlongSeqPOATie readbackLongSeqRWTie = new RWlongSeqPOATie(readbackLongSeqRWImpl);
			longSeqRW = RWlongSeqHelper.narrow(this.registerProperty(readbackLongSeqRWImpl, readbackLongSeqRWTie));
			
			ROdoubleSeqImpl readbackDoubleSeqImpl = new ROdoubleSeqImpl("doubleSeqRO", this, new readbackDoubleSeqDataAccess());
			ROdoubleSeqPOATie readbackDoubleSeqTie = new ROdoubleSeqPOATie(readbackDoubleSeqImpl);
			doubleSeqRO = ROdoubleSeqHelper.narrow(this.registerProperty(readbackDoubleSeqImpl, readbackDoubleSeqTie));
			
			RWdoubleSeqImpl readbackDoubleSeqRWImpl = new RWdoubleSeqImpl("doubleSeqRW", this, currentDataAccess);
			RWdoubleSeqPOATie readbackDoubleSeqRWTie = new RWdoubleSeqPOATie(readbackDoubleSeqRWImpl);
			doubleSeqRW = RWdoubleSeqHelper.narrow(this.registerProperty(readbackDoubleSeqRWImpl, readbackDoubleSeqRWTie));
			
			ROfloatSeqImpl readbackFloatSeqImpl = new ROfloatSeqImpl("floatSeqRO", this, new readbackFloatSeqDataAccess());
			ROfloatSeqPOATie readbackFloatSeqTie = new ROfloatSeqPOATie(readbackFloatSeqImpl);
			floatSeqRO = ROfloatSeqHelper.narrow(this.registerProperty(readbackFloatSeqImpl, readbackFloatSeqTie));
			
			RWfloatSeqImpl readbackFloatSeqRWImpl = new RWfloatSeqImpl("floatSeqRW", this, currentDataAccess3);
			RWfloatSeqPOATie readbackFloatSeqRWTie = new RWfloatSeqPOATie(readbackFloatSeqRWImpl);
			floatSeqRW = RWfloatSeqHelper.narrow(this.registerProperty(readbackFloatSeqRWImpl, readbackFloatSeqRWTie));
			
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
	protected class readbackLongSeqDataAccess implements DataAccess
	{ 
		
		/**
		 * Noise random generator.
		 */
		protected Random noise = new Random();


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
			// random
			int max = ( Math.abs(noise.nextInt()) % 30 ) +1;
			int [] a = new int[max];
			for (int i= 0;i< max; i++)
				a[i] = Math.abs(noise.nextInt()) % 10000;
			
			return a;
		}

		/**
		 * @see alma.ACS.jbaci.DataAccess#set(java.lang.Object, alma.ACSErr.CompletionHolder)
		 */
		public void set(Object value, CompletionHolder completion)
			throws AcsJException {
			throw new AcsJCouldntPerformActionEx("'RO only' cannot be set.");
		}
	}
	
	
	/**
	 * 
	 * 
	 *  DoubleSeq
	 *
	 */
	
	protected class readbackDoubleSeqDataAccess implements DataAccess
	{ 
		
		/**
		 * Noise random generator.
		 */
		protected Random noise = new Random();


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
			// random
			int max = ( Math.abs(noise.nextInt()) % 30 ) +1;
			double [] a = new double[max];
			for (int i= 0;i< max; i++)
				a[i] = Math.abs(noise.nextDouble());
			
			return a;
		}

		/**
		 * @see alma.ACS.jbaci.DataAccess#set(java.lang.Object, alma.ACSErr.CompletionHolder)
		 */
		public void set(Object value, CompletionHolder completion)
			throws AcsJException {
			throw new AcsJCouldntPerformActionEx("'RO only' cannot be set.");
		}
	}
	/**
	 * 
	 * floatSeq
	 *
	 */
	
	protected class readbackFloatSeqDataAccess implements DataAccess
	{ 
		
		/**
		 * Noise random generator.
		 */
		protected Random noise = new Random();


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
			// random
			int max = ( Math.abs(noise.nextInt()) % 30 ) +1;
			float [] a = new float[max];
			for (int i= 0;i< max; i++)
				a[i] = Math.abs(noise.nextFloat());
			
			return a;
		}

		/**
		 * @see alma.ACS.jbaci.DataAccess#set(java.lang.Object, alma.ACSErr.CompletionHolder)
		 */
		public void set(Object value, CompletionHolder completion)
			throws AcsJException {
			throw new AcsJCouldntPerformActionEx("'RO only' cannot be set.");
		}
	}
	
	
	/**
	 * 
	 *  Methods for returning the properties
	 *  
	 * 
	 */
	
	public RWdoubleSeq doubleSeqRW() {
		return doubleSeqRW;
	}

	public ROdoubleSeq doubleSeqRO() {
		return doubleSeqRO;
	}

	public RWfloatSeq floatSeqRW() {
		return floatSeqRW;
	}

	public ROfloatSeq floatSeqRO() {
		return floatSeqRO;
	}

	public RWlongSeq longSeqRW() {
		return longSeqRW;
	}

	public ROlongSeq longSeqRO() {
		return longSeqRO;
	}

	public ROpattern status() {
		return status;
	}



}
