/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarmsanalyzer.engine;

import cern.laser.source.alarmsysteminterface.FaultState;

/**
 * A collection of utility methods.
 * 
 * @author acaproni
 *
 */
public class AlarmUtils {

	/**
	 * Generate the ID out of a {@link FaultState}
	 * 
	 * @param faultState The FaultState
	 * @return The ID
	 */
	public synchronized static String getID(FaultState faultState) {
		return String.format("%s:%s:%d", 
				faultState.getFamily(),
				faultState.getMember(),
				faultState.getCode());
	}
}
