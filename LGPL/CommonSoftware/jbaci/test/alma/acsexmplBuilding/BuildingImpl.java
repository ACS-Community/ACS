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

package alma.acsexmplBuilding;

import alma.ACS.impl.CharacteristicComponentImpl;
import alma.ACS.impl.ROstringImpl;
import alma.ACS.ROstring;
import alma.ACS.ROstringHelper;
import alma.ACS.ROstringPOATie;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;

/**
 * Implementation of <code>alma.acsexmplBuilding.Building</code>.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class BuildingImpl extends CharacteristicComponentImpl 
	implements BuildingOperations {

	/**
	 * Version property.
	 */
	protected ROstring version;

	/**
	 * @see alma.acs.component.ComponentLifecycle#initialize(alma.acs.container.ContainerServices)
	 */
	public void initialize(ContainerServices containerServices)
		throws ComponentLifecycleException {

		super.initialize(containerServices);
		
		try
		{
			// version
			ROstringImpl versionImpl = new ROstringImpl("version", this);
			ROstringPOATie versionTie = new ROstringPOATie(versionImpl);
			version = ROstringHelper.narrow(this.registerProperty(versionImpl, versionTie));
		}
		catch (Throwable th)
		{
			throw new ComponentLifecycleException("Failed to create properties.", th); 
		}
	}

	/*********************** [ Building ] ***********************/

	/**
	 * @see alma.acsexmplBuilding.BuildingOperations#version()
	 */
	public ROstring version() {
		return version;
	}

	/**
	 * @see alma.acsexmplBuilding.BuildingOperations#closeFrontDoor()
	 */
	public void closeFrontDoor() {
		// TODO implement
	}

	/**
	 * @see alma.acsexmplBuilding.BuildingOperations#openFrontDoor()
	 */
	public void openFrontDoor() {
		// TODO implement
	}

}
