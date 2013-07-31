/*
 * ALMA - Atacama Large Millimiter Array
 * Copyright (c) European Southern Observatory, 2013
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

package alma.acs.monitoring.blobber;

import alma.ACSErrTypeCommon.wrappers.AcsJNoResourcesEx;

/**
 * The MonitorPointExpert encapsulates detailed knowledge about 
 * specific monitor points that is needed by the generic upper layers of the blobber.
 * <p>
 * The implementation must be provided by the lower blobber layers (outside of ACS)
 * via the BlobberPlugin.
 */
public interface MonitorPointExpert {

	/**
	 * Checks if the given monitor point is multi-valued.
	 * We must distinguish the following cases:
	 * <ul>
	 *   <li>The corresponding baci property is not of a sequence type.
	 *       This means we read a single value from it at a given time. 
	 *       Then this method should return <code>false</code>.
	 *   <li>The property is of a sequence type, and the sequence should be interpreted
	 *       as multiple values of a single property taken at a given time.
	 *       Then this method should return <code>true</code>. <br>
	 *       Note that this concept has also been referred to as "unique monitor point"
	 *       in the sense of single monitor point (but multiple values).
	 *   <li>The property is of a sequence type, and the sequence should be interpreted
	 *       as values coming from multiple logical monitor points 
	 *       that each have a single value at a given time.
	 *       Then this method should return <code>false</code>.
	 * </ul>
	 * @param propertyName The fully qualified name of the property.
	 * @return <code>true</code> if the given monitor point 
	 * @throws AcsJNoResourcesEx If the property information needed to answer this question cannot be loaded.
	 */
	public boolean isMultivaluedMonitorPoint(String propertyName) throws AcsJNoResourcesEx;
	
}
