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

package alma.ACS.jbaci.enumProp.test;

import org.omg.CORBA.NO_IMPLEMENT;

import jbaciEnumPropTest.ROStates;
import jbaciEnumPropTest.ROStatesHelper;
import jbaciEnumPropTest.ROStatesOperations;
import jbaciEnumPropTest.ROStatesPOATie;
import jbaciEnumPropTest.RWStates;
import jbaciEnumPropTest.RWStatesHelper;
import jbaciEnumPropTest.RWStatesOperations;
import jbaciEnumPropTest.RWStatesPOATie;
import jbaciEnumPropTest.States;
import jbaciEnumPropTest.jbaciEnumTestComponentOperations;
import jbaciEnumPropTest.StateMachinePackage.NoSuchTransition;
import alma.ACS.impl.CharacteristicComponentImpl;
import alma.ACS.impl.CommonROEnumPropertyImpl;
import alma.ACS.impl.CommonRWEnumPropertyImpl;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.MemoryDataAccess;
import alma.ACSErr.Completion;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;

/**
 * Implementation of enumeration test component.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $Id: EnumTestComponentImpl.java,v 1.2 2009/05/04 12:45:04 msekoran Exp $
 */
public class EnumTestComponentImpl extends CharacteristicComponentImpl
		implements jbaciEnumTestComponentOperations {

	protected DataAccess dataAccess = new MemoryDataAccess();
	protected ROStates currentState;
	protected RWStates currentStateRW;
	
	/**
	 * @see alma.acs.component.ComponentLifecycle#initialize(alma.acs.container.ContainerServices)
	 */
	public void initialize(ContainerServices containerServices)
		throws ComponentLifecycleException {

		super.initialize(containerServices);
		
		try
		{
			dataAccess = new MemoryDataAccess();
	
			// currentState
			ROStatesOperations currentStateImpl =
				(ROStatesOperations)CommonROEnumPropertyImpl.createEnumProperty(
							ROStatesOperations.class,
							States.class,
							"currentState",
							this,
							dataAccess);
			ROStatesPOATie currentStatesTie = new ROStatesPOATie(currentStateImpl);
			currentState = ROStatesHelper.narrow(this.registerProperty(currentStateImpl, currentStatesTie));

			// currentStateRW
			RWStatesOperations currentStateRWImpl =
				(RWStatesOperations)CommonRWEnumPropertyImpl.createEnumProperty(
							RWStatesOperations.class,
							States.class,
							"currentStateRW",
							this,
							dataAccess);
			RWStatesPOATie currentStatesRWTie = new RWStatesPOATie(currentStateRWImpl);
			currentStateRW = RWStatesHelper.narrow(this.registerProperty(currentStateRWImpl, currentStatesRWTie));
		}
		catch (Throwable th)
		{
			throw new ComponentLifecycleException("Failed to create properties.", th); 
		}
	}
	
	public ROStates currentState() {
		return currentState;
	}

	public RWStates currentStateRW() {
		return currentStateRW;
	}

	public Completion diagnose() throws NoSuchTransition {
		return currentStateRW.set_sync(States.DIAGNOSE);
	}

	public Completion disable() throws NoSuchTransition {
		return currentStateRW.set_sync(States.DISABLED);
	}

	public Completion enable() throws NoSuchTransition {
		return currentStateRW.set_sync(States.ENABLED);
	}

	public Completion init() throws NoSuchTransition {
		return currentStateRW.set_sync(States.INITIALIZE);
	}

	public Completion off() throws NoSuchTransition {
		// noop
		return CompletionUtil.generateNoErrorCompletion();
	}

	public Completion on() throws NoSuchTransition {
		// noop
		return CompletionUtil.generateNoErrorCompletion();
	}

	public Completion shutdown() throws NoSuchTransition {
		throw new NO_IMPLEMENT();
	}

}
