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
package acs.benchmark.nc.consumer;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.omg.CORBA.portable.IDLEntity;

import acs.benchmark.nc.CorbaNotifyBaseImpl;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.nc.AcsEventSubscriber;
import alma.acs.nc.AcsEventSubscriber.Callback;
import alma.acs.util.StopWatch;
import alma.acsnc.EventDescription;
import alma.benchmark.CorbaNotifyConsumerOperations;
import alma.benchmark.LightweightMountStatusData;
import alma.benchmark.MountStatusData;
import alma.benchmark.NcEventSpec;
import alma.benchmark.SomeOtherEventType;

public class CorbaNotifyConsumerImpl extends CorbaNotifyBaseImpl<AcsEventSubscriber<IDLEntity>> implements CorbaNotifyConsumerOperations
{

//	@Override
//	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException {
//		super.initialize(containerServices);
//	}
//
//	@Override
//	public void cleanUp() throws AcsJComponentCleanUpEx {
//		super.cleanUp();
//	}

	@Override
	protected AcsEventSubscriber<IDLEntity> createNcParticipant(String ncName) throws AcsJContainerServicesEx {
		return m_containerServices.createNotificationChannelSubscriber(ncName, IDLEntity.class);
	}

	@Override
	protected void disconnectNcParticipant(AcsEventSubscriber<IDLEntity> sub) throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx {
		sub.disconnect();
	}

	protected static class TestEventHandler<T extends IDLEntity> implements Callback<T> {
		
		private final Class<T> clazz;
		private final String antennaName; 
		private final CountDownLatch sharedEventCountdown;
		
		/**
		 * @param clazz  Class of the event (IDL struct) 
		 * @param antennaName  Expected antenna name in MountStatusData or LightweightMountStatusData events. 
		 *                     May be null for other events or if antenna name does not matter. Currently not used anyway. 
		 * @param sharedEventCountdown  Used to optionally sync on expected number of events (total, over all NCs). May be null.
		 */
		protected TestEventHandler(Class<T> clazz, String antennaName, CountDownLatch sharedEventCountdown) {
			this.clazz = clazz;
			this.antennaName = antennaName;
			this.sharedEventCountdown = sharedEventCountdown;
		}
		
		@Override
		public Class<T> getEventType() {
			return clazz;
		}

		@Override
		public void receive(T event, EventDescription eventDescrip) {
			if (sharedEventCountdown != null) {
				sharedEventCountdown.countDown();
			}
		}
	}
	
	/**
	 * Creates a typed event handler for the given eventName.
	 * To avoid errors, the String-to-handler mapping is not done in a generic way (classForName etc with guessing the Java package)
	 * but hardcoded for a few known event struct names that can be used for the tests.
	 * 
	 * @param eventName  Currently supported are "MountStatusData", "LightweightMountStatusData", "SomeOtherEventType"
	 * @return
	 * @throws IllegalArgumentException  If eventName is not supported.
	 */
	protected TestEventHandler<? extends IDLEntity> createEventHandler(String eventName, String antennaName, CountDownLatch sharedEventCountdown) {
		TestEventHandler<? extends IDLEntity> ret = null;
		
		if (eventName.equals("MountStatusData")) {
			ret= new TestEventHandler<MountStatusData>(MountStatusData.class, antennaName, sharedEventCountdown) {
				public void receive(MountStatusData event, EventDescription eventDescrip) {
					super.receive(event, eventDescrip);
					// TODO: do something with event.antennaName;
				}
			};
		}
		else if (eventName.equals("LightweightMountStatusData")) {
			ret= new TestEventHandler<LightweightMountStatusData>(LightweightMountStatusData.class, antennaName, sharedEventCountdown);
		}
		else if (eventName.equals("SomeOtherEventType")) {
			ret= new TestEventHandler<SomeOtherEventType>(SomeOtherEventType.class, antennaName, sharedEventCountdown);
		}
		else {
			throw new IllegalArgumentException("Unsupported event type '" + eventName + "'.");
		}
		return ret;
	}
	
	@Override
	public int receiveEvents(NcEventSpec[] ncEventSpecs, int processingDelayMillis, int numberOfEvents)
			throws CouldntPerformActionEx {
		
		// Set up receivers
		
		CountDownLatch sharedEventCountdown = null;
		if (numberOfEvents > 0) {
			sharedEventCountdown = new CountDownLatch(numberOfEvents);
		}

		StopWatch sw = new StopWatch();

		try {
			for (NcEventSpec ncEventSpec : ncEventSpecs) {
				AcsEventSubscriber<IDLEntity> sub = subsOrPubs.get(ncEventSpec.ncName);
				if (sub == null) {
					throw new AcsJCouldntPerformActionEx("No subscriber available for NC '" + ncEventSpec.ncName + "'.");
				}
				for (String eventName : ncEventSpec.eventNames) {
					sub.addSubscription(createEventHandler(eventName, ncEventSpec.antennaName, sharedEventCountdown));
					sub.startReceivingEvents();
				}
			}
		} catch (AcsJCouldntPerformActionEx ex) {
			throw ex.toCouldntPerformActionEx();
		} catch (Exception ex) {
			throw new AcsJCouldntPerformActionEx(ex).toCouldntPerformActionEx();
		}
		m_logger.info(ncEventSpecs.length + " subscriber(s) set up to receive events in " + sw.getLapTimeMillis() + " ms.");
		
		if (numberOfEvents > 0) {
			m_logger.info("Will wait for a total of " + numberOfEvents + " events to be received, with timeout after 10 minutes.");
			try {
				boolean cleanTermination = sharedEventCountdown.await(10, TimeUnit.MINUTES);
				if (cleanTermination) {
					m_logger.info("Received the expected " + numberOfEvents + " events in " + sw.getLapTimeMillis() + " ms.");
				}
				else {
					m_logger.warning("Unforeseen termination of event suppliers after 10 min (timeout).");
					cancel = true;
				}
			} catch (InterruptedException ex) {
				cancel = true;
			}
		}
		else {
			m_logger.info("Will return from receiveEvents now but will keep receiving events.");
		}
		
		if (cancel) {
			throw new AcsJCouldntPerformActionEx("Event receiving was interrupted or failed otherwise.").toCouldntPerformActionEx();
		}
		else {
			return (int) sw.getLapTimeMillis();
		}
	}


}
