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

package test.jbaci;

import alma.ACS.impl.CharacteristicComponentImpl;
import alma.ACS.impl.ROstringSeqImpl;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.MemoryDataAccess;
import alma.ACSErr.CompletionHolder;
import alma.ACS.ComponentStates;
import alma.ACS.ROstringSeq;
import alma.ACS.ROstringSeqHelper;
import alma.ACS.ROstringSeqPOATie;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;


/**
 * Implementation of <code>test.jbaci.SimpleMasterComponent</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class SimpleMasterComponentImpl extends CharacteristicComponentImpl 
	implements SimpleMasterComponentOperations {

	/**
	 * Current state hierarchy property.
	 */
	protected ROstringSeq currentStateHierarchy;

	/**
	 * @see alma.acs.component.ComponentLifecycle#initialize(alma.acs.container.ContainerServices)
	 */
	public void initialize(ContainerServices containerServices)
		throws ComponentLifecycleException {

		super.initialize(containerServices);
		
		try
		{
			CurrentStateHierarchyDataAccess csha = new CurrentStateHierarchyDataAccess(
				this, new String[] { "SpecialState", "BoringState", "StatelessState", "InvalidState" }, 2
			);
			// currentStateHierarchy
			ROstringSeqImpl currentStateHierarchyImpl = new ROstringSeqImpl("currentStateHierarchy", this, csha);
			ROstringSeqPOATie currentStateHierarchyTie = new ROstringSeqPOATie(currentStateHierarchyImpl);
			currentStateHierarchy = ROstringSeqHelper.narrow(this.registerProperty(currentStateHierarchyImpl, currentStateHierarchyTie));
		}
		catch (Throwable th)
		{
			throw new ComponentLifecycleException("Failed to create properties.", th); 
		}
	}

	/*********************** [ SimpleMasterComponentImpl ] ***********************/

	/**
	 * @see test.jbaci.SimpleMasterComponentOperations#currentStateHierarchy()
	 */
	public ROstringSeq currentStateHierarchy() {
		return currentStateHierarchy;
	}

	/******************* [ DataAccess implementations ] *******************/

	/**
	 * Current state hierarchy simulation <code>DataAccess</code> implementation.
	 */
	protected class CurrentStateHierarchyDataAccess extends MemoryDataAccess
		implements Runnable
	{
		/**
		 * Simulation states.
		 */
		private String[] states;
		
		/**
		 * Sequence length.
		 */
		private int sequenceLength;

		/**
		 * Simulated component.
		 */
		private CharacteristicComponentImpl characteristicComponentImpl;
				
		/**
		 * Default constructor.
		 * @param	property	property to be simulated, non-<code>null</code>.
		 * @param	states		simulation states, non-<code>null</code> and length > 0. 
		 * @param	sequenceLength	sequence length
		 */
		public CurrentStateHierarchyDataAccess(CharacteristicComponentImpl characteristicComponentImpl,
											   String[] states, int sequenceLength)
		{ 
			assert(characteristicComponentImpl != null);
			assert(states != null && states.length > 0);
			
			this.characteristicComponentImpl = characteristicComponentImpl;
			this.states = states;
			this.sequenceLength = sequenceLength;
			
			new Thread(this, "CurrentStateHierarchyDataAccess").start();
		}
		/**
		 * @see java.lang.Runnable#run()
		 */
		public void run() {
			
			CompletionHolder completionHolder = CompletionUtil.createCompletionHolder();
			int i = 0;
			
			// not nice...
			while (characteristicComponentImpl.componentState().value() < ComponentStates._COMPSTATE_ERROR)
			{
				
				// memory value set
				String[] newVal = new String[sequenceLength];
				for (int n = 0; n < sequenceLength; n++)
					newVal[n] = states[i] + String.valueOf(n);
					 			
				set(newVal, completionHolder);
				i = (++i) % states.length;
				
				// sleep for a while
				try { Thread.sleep(500); } catch (InterruptedException ie) {};
			}
		}

	}
}
