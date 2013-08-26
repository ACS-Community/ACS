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
import com.cosylab.cdb.jdal.hibernate.NameOverrideFeature;

/**
 * @author msekoranja
 */
public class FaultCode implements NameOverrideFeature {

	private int value;
	private boolean instant;
	
    // hierarchical support
    // must be public to be accessible, but should not have getter to be come visible as node
    // since childred can be properties and subcomponent Object is used
	public Map<String, Object> _ = new InternalElementsMap<String, Object>();

    /* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.NameOverrideFeature#getNameOverride()
	 */
	public String getNameOverride() {
		return "fault-code";
	}

	public FaultCode(int value, boolean instant,
			int priority, String cause, String action, String consequence,
			String problemDescription) {
		this.value = value;
		this.instant = instant;
		
		_.put("priority", new ElementValue(String.valueOf(priority)));
		if (cause != null)
			_.put("cause", new ElementValue(cause));
		if (action != null)
			_.put("action", new ElementValue(action));
		if (consequence != null)
			_.put("consequence", new ElementValue(consequence));
		_.put("problem-description", new ElementValue(problemDescription));
	}

	/**
	 * @return the instant
	 */
	public boolean isInstant() {
		return instant;
	}

	/**
	 * @return the value
	 */
	public int getValue() {
		return value;
	}

}
