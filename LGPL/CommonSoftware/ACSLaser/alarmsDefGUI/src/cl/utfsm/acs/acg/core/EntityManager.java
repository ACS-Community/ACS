/*
 *    ALMA - Atacama Large Millimeter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2009
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package cl.utfsm.acs.acg.core;

/**
 * Common interface to all the entities Managers. Implementors of this interface can perform a full
 * reload of all their internal contents from the ACS CDB, and also save their contents into the ACS
 * CDB.
 * 
 * @author rtobar
 *
 */
public interface EntityManager {

	/**
	 * Performs a full reload of the contents of the Manager. All the current internal contents of
	 * the EntityManager are cleared, and replaced by the ones that are currently available on the
	 * CDB. Therefore, this call should be done with great care.
	 */
	public void loadFromCDB();

	/**
	 * Performs a full save of the contents of the Manager to the CDB. All the modified contents of
	 * the CDB are updated, and replaced by the ones that are currently available on the
	 * EntityManager. Therefore, this call should be done with great care.
	 */
	public void saveToCDB();
}
