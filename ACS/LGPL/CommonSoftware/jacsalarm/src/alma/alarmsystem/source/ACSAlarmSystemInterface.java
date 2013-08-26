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
package alma.alarmsystem.source;

import java.util.Collection;

/**
 * This interface is derived from cern.laser.source.alarmsysteminterface.AlarmSystemInterface.
 * This is repeated here to avoid dependency with LASER.
 * <p>
 * @TODO Shouldn't we have a factory method for alarm sources here in this interface, 
 *       so that alarms can be produced with only the ACSAlarmSystemInterface,
 *       and without the need to use {@link ACSAlarmSystemInterfaceFactory#createFaultState(String, String, int)}?
 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
 * 
 * @author acaproni
 */
public interface ACSAlarmSystemInterface {
	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	public void setSourceName(String newSourceName);

	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	public String getSourceName();

	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	public void close();

	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	public void push(ACSFaultState state);

	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	public void push(Collection<ACSFaultState> states);

	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	public void pushActiveList(Collection<ACSFaultState> active);
}
