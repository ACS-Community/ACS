/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.nsstatistics;


/** This class encapsulates the list of all suppliers of events to a particular channel returned
 * by the TAO M&C Extensions to the Notify Service.
 * @author jschwarz
 *
 */
public class ChannelSuppliers extends MCStatistics {

	public ChannelSuppliers(ChannelData parent) {
		super(parent, "SupplierNames");
	}
	
	@Override
	public String getStatistics() {
		children.clear();
		String sc[] = getMcData().list();
		for (int i = 0; i < sc.length; i++) {
			String supplierNameSimple = toSimpleName(sc[i], i);
			children.add(new ChannelParticipantName(supplierNameSimple, this));
		}
		return "Suppliers: " + getParent().getNumSuppliersAndDelta();
	}
}
