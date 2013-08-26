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

import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.MemoryDataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;

/**
 * Implementation of comparable common property, i.e. type of <code>java.lang.Object</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public abstract class CommonComparablePropertyImpl 
	extends CommonPropertyImpl {

	/**
	 * Min graph value.
	 */
	protected Object graphMin;

	/**
	 * Max graph value.
	 */
	protected Object graphMax;

	/**
	 * Min step value.
	 */
	protected Object minStep;

	/**
	 * Min delta value (on-change monitors).
	 */
	protected Object minDeltaTrigger;

	/**
	 * Constructor.
	 * @param propertyType		property <code>Class</code> type, non-<code>null</code>.
	 * @param name				property name, non-<code>null</code>.
	 * @param parentComponent	parent component, non-<code>null</code>.
	 * @throws PropertyInitializationFailed exception is thrown on failure
	 */
	public CommonComparablePropertyImpl(
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
	 * @param dataAccess		data access to be use, non-<code>null</code>.
	 * @throws PropertyInitializationFailed exception is thrown on failure
	 */
	public CommonComparablePropertyImpl(
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
			graphMin = readPropertyTypeCharacteristic("graph_min");
			graphMax = readPropertyTypeCharacteristic("graph_max");
			minStep = readPropertyTypeCharacteristic("min_step");
			minDeltaTrigger = readPropertyTypeCharacteristic("min_delta_trig");
		}
		catch (Throwable t)
		{
			throw new PropertyInitializationFailed("Failed to read all property characteristics.", t);
		}
	}

	/**
	 * Check if absolute difference between values is less than delta value
	 * @param value value 
	 * @param delta delta value
	 * @return true if difference between this object's value and given value is less that delta value
	 */
	public abstract boolean lessThanDelta(Object value1, Object value2, Object delta);
  
	/**
	 * Check if value equals no change (e.g. for double 0.0, int 0)
	 * @return if value equals no change
	 */
	public abstract boolean noDelta(Object value);

	/**
	 * Calculate a sum of two values.
	 * @param value1	first value.
	 * @param value2	second value.
	 * @param substract	negate value2.
	 * @return	value of (value1 + (substract ? -value2 : value2)).
	 */
	public abstract Object sum(Object value1, Object value2, boolean substract);

}
