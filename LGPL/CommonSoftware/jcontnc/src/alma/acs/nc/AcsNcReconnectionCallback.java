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

import java.util.logging.Logger;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.Object;

import NotifyExt.ReconnectionCallback;
import NotifyExt.ReconnectionCallbackHelper;
import NotifyExt.ReconnectionRegistry;
import NotifyExt.ReconnectionRegistryHelper;
import gov.sandia.NotifyMonitoringExt.EventChannelFactory;
import gov.sandia.NotifyMonitoringExt.EventChannelFactoryHelper;

import alma.ACS.OffShoot;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServicesBase;
import alma.acsnc.OSReconnectionCallbackPOA;

/**
 * AcsNcReconnectionCallback implements the {@link ReconnectionCallback}
 * from TAO's notify extensions, using an inheritance structure
 * compatible with ACS's {@link OffShoot} concept.
 * The publisher and subscriber classes use this class to register themselves
 * for future {@link #reconnect(Object)} callbacks after a service restart.
 * This class encapsulates the handling of the reconnection API.
 *  
 * TODO: Merge with event callback offshoot
 * 
 * @author javarias
 */
public class AcsNcReconnectionCallback extends OSReconnectionCallbackPOA {
	
	private final Logger logger;
	private EventChannelFactory ecf_;
	private final ReconnectableParticipant parti;
	private int callback_id_;
	private ContainerServicesBase services;
	private volatile boolean id_is_valid_;

	public AcsNcReconnectionCallback(ReconnectableParticipant parti, Logger logger){
		this.parti = parti;
		this.logger = logger;
		id_is_valid_ = false; 
	}
	
	/**
	 * @see NotifyExt.ReconnectionCallbackOperations#is_alive()
	 */
	@Override
	public boolean is_alive() {
		return true;
	}

	/**
	 * Called by the NotifyService after service restart, giving the new EventChannelFactory reference.
	 * This EventChannelFactory reference is passed to the owning ReconnectableParticipant
	 * who should then use it to reconnect.
	 * 
	 * @see NotifyExt.ReconnectionCallbackOperations#reconnect(org.omg.CORBA.Object)
	 */
	@Override
	public void reconnect(Object new_connection) {
		ecf_ = EventChannelFactoryHelper.narrow(new_connection);
		if (ecf_ != null){
			parti.reconnect(ecf_);
		}
	}
	
	/**
	 * Called by the event subscriber or supplier when connecting to the NC.
	 * Corba-activates this callback object using <code>services</code> and
	 * registers it with the NotifyService's {@link ReconnectionRegistry}.
	 * <p>
	 * @throws AcsJContainerServicesEx 
	 * @throws AcsJIllegalArgumentEx 
	 * @TODO: This call does not do anything if the <code>ecf == null</code>,
	 * which is also mentioned in comments in the calling code. 
	 * This should be cleaned up, e.g. checked if at all and under which circumstances
	 * this null can happen. 
	 */
	public void registerForReconnect(ContainerServicesBase services, EventChannelFactory ecf ) throws AcsJContainerServicesEx, AcsJIllegalArgumentEx {
		// TODO: This is strange...
		if (ecf == null) {
			return; 
		}
		
		if (services == null) {
			AcsJIllegalArgumentEx ex = new AcsJIllegalArgumentEx();
			ex.setVariable("services");
			ex.setValue("null");
			throw ex;
		}
		this.services = services;
		
		ReconnectionRegistry registry = null;
		try {
			registry = ReconnectionRegistryHelper.narrow(ecf);
		} catch (BAD_PARAM ex) {
			// TODO: Or should we just return, same deal as above with ecf == null?
			AcsJIllegalArgumentEx ex2 = new AcsJIllegalArgumentEx(ex);
			ex2.setErrorDesc("The given EventChannelFactory is not a NotifyExt.ReconnectionRegistry. Make sure TAO extensions are used!");
			throw ex2;
		}
		
		// HSO: Is this _duplicate call useful, or a C++ literal translation?
		ecf_=(EventChannelFactory) ecf._duplicate();
		
		ReconnectionCallback callback = ReconnectionCallbackHelper.narrow(services.activateOffShoot(this));
		callback_id_ = registry.register_callback(callback);
		id_is_valid_ = true;
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
