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
package acs.benchmark.nc.comp;

import java.util.HashMap;
import java.util.Map;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentImplBase;
import alma.benchmark.CorbaNotifyCompBaseOperations;


/**
 * @author hsommer
 *
 * @param <T> AcsEventSubscriber or AcsEventPublisher. Allows sharing code between the two subclasses.
 */
public abstract class CorbaNotifyBaseImpl<T> extends ComponentImplBase implements CorbaNotifyCompBaseOperations
{
	/**
	 * The component stores here all NCs it is connected to, together with the publisher or subscriber object it uses.
	 * key = NC name, value = publisher or subscriber object
	 */
	protected final Map<String, T> subsOrPubs = new HashMap<String, T>();
	
	/**
	 * Flag set by interrupt method
	 */
	protected volatile boolean cancel = false;

//	@Override
//	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException {
//		super.initialize(containerServices);
//	}
//
//	@Override
//	public void cleanUp() throws AcsJComponentCleanUpEx {
//	}

	/**
	 * Encapsulates difference between AcsEventSubscriber and AcsEventPublisher.
	 */
	protected abstract T createNcParticipant(String ncName) throws AcsJContainerServicesEx;
	
	/**
	 * Encapsulates difference between AcsEventSubscriber and AcsEventPublisher.
	 * @throws AcsJIllegalStateEventEx
	 * @throws AcsJCouldntPerformActionEx 
	 */
	protected abstract void disconnectNcParticipant(T subOrPub) throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx;

	
	/**
	 * Subscriber or publisher connection.
	 */
	@Override
	public void ncConnect(String[] ncNames) throws CouldntPerformActionEx {
		Map<String, T> newSubsOrPubs = new HashMap<String, T>();
		try {
			for (String ncName : ncNames) {
				T subOrPub = createNcParticipant(ncName);
				newSubsOrPubs.put(ncName, subOrPub);
			}
			subsOrPubs.putAll(newSubsOrPubs);
		} catch (AcsJContainerServicesEx ex) {
			// disconnect those NCs that were just getting connected
			for (T subOrPub : newSubsOrPubs.values()) {
				try {
					disconnectNcParticipant(subOrPub);
				} catch (Exception ex2) {
					// ignore, since the original NC connection ex is more interesting and will be thrown
				}
			}
			throw new AcsJCouldntPerformActionEx(ex).toCouldntPerformActionEx();
		}
	}

	@Override
	public void ncDisconnect() throws CouldntPerformActionEx {
		interrupt();
		Exception lastEx = null;
		for (T subOrPub : subsOrPubs.values()) {
			try {
				disconnectNcParticipant(subOrPub);
			} catch (Exception ex) {
				lastEx = ex;
			}
		}
		if (lastEx != null) {
			throw new AcsJCouldntPerformActionEx(lastEx).toCouldntPerformActionEx();
		}
	}
	
	
	@Override
	public void interrupt() {
		cancel = true;
	}

}
