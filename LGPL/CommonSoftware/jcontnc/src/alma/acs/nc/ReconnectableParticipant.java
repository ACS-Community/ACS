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
package alma.acs.nc;

import gov.sandia.NotifyMonitoringExt.EventChannelFactory;

/**
 * Must be implemented by NC publishers and subscribers
 * in order to get a reconnect request from the NotifyService after a service restart.
 * <p>
 * Note that even in a push-push NC configuration there are still 
 * callbacks from the subscriber to the service and from the service to the supplier,
 * which partly explains why TAO extensions require an explicit reconnection
 * rather than relying on persistent object addresses.
 */
public interface ReconnectableParticipant {

	/**
	 * @param ecf The new EventChannelFactory reference as delivered by the NotifyService's callback.
	 */
	public void reconnect(EventChannelFactory ecf);
}
