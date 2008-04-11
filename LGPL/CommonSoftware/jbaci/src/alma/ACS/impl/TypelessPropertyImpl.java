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

import java.util.concurrent.ThreadFactory;

import alma.ACS.TypelessPropertyOperations;
import alma.ACS.jbaci.PropertyInitializationFailed;

/**
 * Implementation of <code>alma.ACS.TypelessProperty</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class TypelessPropertyImpl
	extends PropertyImpl
	implements TypelessPropertyOperations {

	/**
	 * Property description.
	 */
	protected String description;

	/**
	 * Property C-like format used by clients.
	 */
	protected String format;

	/**
	 * Property units (for numbers).
	 */
	protected String units;

	/**
	 * Property resolution (bitmask).
	 */
	protected int resolution;

	/**
	 * Constructor.
	 * @param name				property name, non-<code>null</code>.
	 * @param parentComponent	parent component, non-<code>null</code>.
	 * @throws PropertyInitializationFailed exception is thrown on failure
	 */
	public TypelessPropertyImpl(String name,
								CharacteristicComponentImpl parentComponent,
								ThreadFactory threadFactory)
		throws PropertyInitializationFailed {
		super(name, parentComponent, threadFactory);
	}

	/**
	 * Read property characteristics.
	 * @throws PropertyInitializationFailed exception is thrown on failure
	 */
	public void readCharacteristics()
		throws PropertyInitializationFailed {
		super.readCharacteristics();
		
		String characteristic = null;
		try
		{
			characteristic = "description";
			description = characteristicModelImpl.getString(characteristic);
			characteristic = "format";
			format = characteristicModelImpl.getString(characteristic);
			characteristic = "units";
			units = characteristicModelImpl.getString(characteristic);
			characteristic = "resolution";
			resolution = characteristicModelImpl.getInteger(characteristic);
		}
		catch (Throwable t)
		{
			throw new PropertyInitializationFailed("Failed to read property characteristic '" + characteristic + "'", t);
		}
	}


	/*********************** [ Property ] ***********************/

	/**
	 * @see alma.ACS.TypelessPropertyOperations#description()
	 */
	public String description() {
		return description;
	}

	/**
	 * @see alma.ACS.TypelessPropertyOperations#format()
	 */
	public String format() {
		return format;
	}

	/**
	 * @see alma.ACS.TypelessPropertyOperations#units()
	 */
	public String units() {
		return units;
	}

	/**
	 * @see alma.ACS.TypelessPropertyOperations#resolution()
	 */
	public int resolution() {
		return resolution;
	}

}
