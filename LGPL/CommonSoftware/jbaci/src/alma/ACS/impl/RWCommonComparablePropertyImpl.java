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
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACS.jbaci.BACIAction;
import alma.ACS.jbaci.BACIPriority;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.MemoryDataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;

/**
 * Implementation of read-write common comparable property, i.e. type of <code>java.lang.Object</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public abstract class RWCommonComparablePropertyImpl extends CommonComparablePropertyImpl {

	/**
	 * Min value.
	 */
	protected Object minValue;

	/**
	 * Max value.
	 */
	protected Object maxValue;

	/**
	 * Constructor with memory data access.
	 * @param propertyType		property <code>Class</code> type, non-<code>null</code>.
	 * @param name				property name, non-<code>null</code>.
	 * @param parentComponent	parent component, non-<code>null</code>.
	 * @throws PropertyInitializationFailed exception is thrown on failure
	 */
	public RWCommonComparablePropertyImpl(
		Class propertyType,
		String name,
		CharacteristicComponentImpl parentComponent)
		throws PropertyInitializationFailed {
		this(propertyType, name, parentComponent, new MemoryDataAccess());
	}

	/**
	 * Constructor.
	 * @param propertyType		property <code>Class</code> type, non-<code>null</code>.
	 * @param name				property name, non-<code>null</code>.
	 * @param parentComponent	parent component, non-<code>null</code>.
	 * @param dataAccess		read-write data access to be use, non-<code>null</code>.
	 * @throws PropertyInitializationFailed exception is thrown on failure
	 */
	public RWCommonComparablePropertyImpl(
		Class propertyType,
		String name,
		CharacteristicComponentImpl parentComponent,
		DataAccess dataAccess)
		throws PropertyInitializationFailed {
		super(propertyType, name, parentComponent, dataAccess);
	}

	/**
	 * Read property characteristics.
	 * @throws PropertyInitializationFailed exception is thrown on failure
	 */
	public void readCharacteristics()
		throws PropertyInitializationFailed {
		super.readCharacteristics();
		
		try
		{
			minValue = readPropertyTypeCharacteristic("min_value");
			maxValue = readPropertyTypeCharacteristic("max_value");
		}
		catch (Throwable t)
		{
			throw new PropertyInitializationFailed("Failed to read all property characteristics.", t);
		}
	}

	/**
	 * Increment action implementation.
	 * @throws AcsJException	thown on any failure.
	 */
	protected Completion incrementImpl() throws AcsJException
	{
		CompletionHolder completionHolder = CompletionUtil.createCompletionHolder(); 
		Object currentValue = getSync(completionHolder);
		// TODO completion error (type != 0) check
		// increment
		Object incrementedValue = sum(currentValue, minStep, false);
		return setSync(incrementedValue);
	}
	
	/**
	 * Decrement action implementation.
	 * @throws AcsJException	thown on any failure.
	 */
	protected Completion decrementImpl() throws AcsJException
	{
		CompletionHolder completionHolder = CompletionUtil.createCompletionHolder(); 
		Object currentValue = getSync(completionHolder);
		// TODO completion error (type != 0) check
		// decrement
		Object decrementedValue = sum(currentValue, minStep, true);
		return setSync(decrementedValue);
	}

	/*********************** [ RW<type> ] ***********************/

	/**
	 * BACI action to invoke <code>incrementImpl</code> asynchroniously.
	 */
	protected class IncrementAction extends BACIAction
	{ 
		/**
		 * @see alma.ACS.jbaci.BACIAction
		 */
		public IncrementAction(Callback callback, CBDescIn descIn) {
			super(getParentComponent(), callback, descIn, RWCommonComparablePropertyImpl.this);
		}

		/**
		 * @see alma.ACS.jbaci.BACIAction
		 */
		public IncrementAction(
			Object value, 
			Callback callback,
			CBDescIn descIn,
			BACIPriority priority) {
			super(getParentComponent(), callback, descIn, RWCommonComparablePropertyImpl.this, priority);
		}

		/**
		 * @see alma.ACS.jbaci.BACIAction#execute()
		 */
		public Object execute() throws AcsJException {
			try
			{
				completion = incrementImpl();
			}
			catch (AcsJException acsex)
			{
				throw new AcsJCouldntPerformActionEx("Failed to increment value asynchroniously.", acsex);
			}
			
			// no return value
			return null;
		}
	}

	/**
	 * @see alma.ACS.RW<type>Operations#increment(alma.ACS.CBvoid, alma.ACS.CBDescIn)
	 */
	public void increment(CBvoid callback, CBDescIn desc) {
		new IncrementAction(callback, desc).submit();
	}

	/**
	 * BACI action to invoke <code>incrementImpl</code> asynchroniously.
	 */
	protected class DecrementAction extends BACIAction
	{ 
		/**
		 * @see alma.ACS.jbaci.BACIAction
		 */
		public DecrementAction(Callback callback, CBDescIn descIn) {
			super(getParentComponent(), callback, descIn, RWCommonComparablePropertyImpl.this);
		}

		/**
		 * @see alma.ACS.jbaci.BACIAction
		 */
		public DecrementAction(
			Object value, 
			Callback callback,
			CBDescIn descIn,
			BACIPriority priority) {
			super(getParentComponent(), callback, descIn, RWCommonComparablePropertyImpl.this, priority);
		}

		/**
		 * @see alma.ACS.jbaci.BACIAction#execute()
		 */
		public Object execute() throws AcsJException {
			try
			{
				completion = decrementImpl();
			}
			catch (AcsJException acsex)
			{
				throw new AcsJCouldntPerformActionEx("Failed to decrement value asynchroniously.", acsex);
			}
			
			// no return value
			return null;
		}
	}

	/**
	 * @see alma.ACS.RW<type>Operations#decrement(alma.ACS.CBvoid, alma.ACS.CBDescIn)
	 */
	public void decrement(CBvoid callback, CBDescIn desc) {
		new IncrementAction(callback, desc).submit();
	}

}
