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
import gov.sandia.NotifyMonitoringExt.EventChannelFactoryHelper;

import org.omg.CORBA.Object;

import NotifyExt.ReconnectionCallback;
import NotifyExt.ReconnectionCallbackHelper;
import NotifyExt.ReconnectionRegistry;
import NotifyExt.ReconnectionRegistryHelper;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServicesBase;
import alma.acsnc.OSReconnectionCallbackPOA;

/**
 * @TODO comment about design by TAO, and how we use it in ACS
 * @author javarias
 */
public class AcsNcReconnectionCallback extends OSReconnectionCallbackPOA {
	
	private EventChannelFactory ecf_;
	private ReconnectableSubscriber sub_;
	private int callback_id_;
	private ContainerServicesBase services;
	private volatile boolean id_is_valid_;

	public AcsNcReconnectionCallback(ReconnectableSubscriber sub){
		id_is_valid_ = false; 
		sub_=sub;
	}
	
	/**
	 * 
	 * @see NotifyExt.ReconnectionCallbackOperations#is_alive()
	 */
	@Override
	public boolean is_alive() {
		return true;
	}

	@Override
	public void reconnect(Object new_connection) {
		ecf_ = EventChannelFactoryHelper.narrow(new_connection);
		if (ecf_ != null){
			sub_.reconnect(ecf_);
		}
	}
	
	public void init(ContainerServicesBase services, EventChannelFactory ecf ) {
		if (ecf == null || services == null) {
			return; // HSO: why not IllegalArgumentException ??
		}
		this.services = services;
		
		ecf_=(EventChannelFactory) ecf._duplicate();
		try {
			ReconnectionCallback callback = ReconnectionCallbackHelper.narrow(services.activateOffShoot(this));
			ReconnectionRegistry registry = ReconnectionRegistryHelper.narrow(ecf);
			callback_id_ = registry.register_callback(callback);
			id_is_valid_ = true;
		} 
		catch (AcsJContainerServicesEx e) {
			e.printStackTrace(); // HSO: logging with repeatguard? Throw ex?
		}
	}
	
	public void disconnect(){
		if (id_is_valid_){
			ReconnectionRegistry registry = ReconnectionRegistryHelper.narrow(ecf_);
			registry.unregister_callback(callback_id_);
			/* This should never occur, but in any case*/
			try {
				services.deactivateOffShoot(this);
			} catch (AcsJContainerServicesEx e) {
			}
			id_is_valid_ = false;
		}
	}
}
