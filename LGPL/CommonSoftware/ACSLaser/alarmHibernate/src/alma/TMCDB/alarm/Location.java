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
/**
 * 
 */
package alma.TMCDB.alarm;

import java.util.Map;

import com.cosylab.cdb.jdal.hibernate.ElementValue;
import com.cosylab.cdb.jdal.hibernate.InternalElementsMap;

/**
 * @author msekoranja
 */
public class Location {

	// hierarchical support
    // must be public to be accessible, but should not have getter to be come visible as node
    // since childred can be properties and subcomponent Object is used
 	public Map<String, Object> _ = new InternalElementsMap<String, Object>();
	
	public Location(String building, String floor, String room, String mnemonic, String position) {
		if (building != null)
			_.put("building", new ElementValue(building));
		if (floor != null)
			_.put("floor", new ElementValue(floor));
		if (room != null)
			_.put("room", new ElementValue(room));
		if (mnemonic != null)
			_.put("mnemonic", new ElementValue(mnemonic));
		if (position != null)
			_.put("position", new ElementValue(position));
	}
	
}
