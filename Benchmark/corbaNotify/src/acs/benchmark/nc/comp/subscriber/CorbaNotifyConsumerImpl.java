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
package acs.benchmark.nc.comp.subscriber;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import org.omg.CORBA.portable.IDLEntity;

import acs.benchmark.nc.comp.CorbaNotifyBaseImpl;

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
		AcsEventSubscriber<IDLEntity> ret = m_containerServices.createNotificationChannelSubscriber(ncName, IDLEntity.class);
		m_logger.info("Created subscriber for NC=" + ncName + "; state=" + ret.getLifecycleState());
		return ret;
	}

	@Override
	protected void disconnectNcParticipant(AcsEventSubscriber<IDLEntity> sub) throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx {
		sub.disconnect();
	}

	protected static class TestEventHandler<T extends IDLEntity> implements Callback<T> {
		
		private final Class<T> clazz;
		private final String antennaName; 
		private final int processingDelayMillis;
		private final CountDownLatch sharedEventCountdown;
		private final AtomicLong firstEventTimeMillis;
		
		/**
		 * @param clazz  Class of the event (IDL struct) 
		 * @param antennaName  Expected antenna name in MountStatusData or LightweightMountStatusData events. 
		 *                     May be null for other events or if antenna name does not matter. Currently not used anyway. 
		 * @param sharedEventCountdown  Used to optionally sync on expected number of events (total, over all NCs). May be null.
		 * @param firstEventTimeMillis  Must be initialized to <code>-1</code>. 
		 *                              The first event will set the current system time if it finds it with that -1 value.
		 *                              Other receivers may have set the time already.
		 */
		protected TestEventHandler(Class<T> clazz, String antennaName, int processingDelayMillis, CountDownLatch sharedEventCountdown, AtomicLong firstEventTimeMillis) {
			this.clazz = clazz;
			this.antennaName = antennaName;
			this.processingDelayMillis = processingDelayMillis;
			this.sharedEventCountdown = sharedEventCountdown;
			this.firstEventTimeMillis = firstEventTimeMillis;
		}
		
		@Override
		public Class<T> getEventType() {
			return clazz;
		}

		@Override
		public void receive(T event, EventDescription eventDescrip) {
			firstEventTimeMillis.compareAndSet(-1, System.nanoTime());
			if (sharedEventCountdown != null) {
				sharedEventCountdown.countDown();
			}
			if (processingDelayMillis > 0) {
				try {
					Thread.sleep(processingDelayMillis);
				} catch (InterruptedException ex) {
					ex.printStackTrace();
				}
			}
		}
	}
	
	/**
	 * Creates a typed event handler for the given eventName.
	 * To avoid errors, the String-to-handler mapping is not done in a generic way (classForName etc with guessing the Java package)
	 * but hardcoded for a few known event struct names that can be used for the tests.
	 * 
	 * @param eventName  Currently supported are "MountStatusData", "LightweightMountStatusData", "SomeOtherEventType"
	 * @param firstEventReceivedTimeNanos 
	 * @return
	 * @throws IllegalArgumentException  If eventName is not supported.
	 */
	protected TestEventHandler<? extends IDLEntity> createEventHandler(
			String eventName, 
			String antennaName, 
			int processingDelayMillis,
			CountDownLatch sharedEventCountdown, 
			AtomicLong firstEventReceivedTimeNanos ) {
		TestEventHandler<? extends IDLEntity> ret = null;
		
		if (eventName.equals("MountStatusData")) {
			ret= new TestEventHandler<MountStatusData>(MountStatusData.class, antennaName, processingDelayMillis, sharedEventCountdown, firstEventReceivedTimeNanos) {
				public void receive(MountStatusData event, EventDescription eventDescrip) {
					super.receive(event, eventDescrip);
					// TODO: do something with event.antennaName;
				}
			};
		}
		else if (eventName.equals("LightweightMountStatusData")) {
			ret= new TestEventHandler<LightweightMountStatusData>(LightweightMountStatusData.class, antennaName, processingDelayMillis, sharedEventCountdown, firstEventReceivedTimeNanos);
		}
		else if (eventName.equals("SomeOtherEventType")) {
			ret= new TestEventHandler<SomeOtherEventType>(SomeOtherEventType.class, antennaName, processingDelayMillis, sharedEventCountdown, firstEventReceivedTimeNanos);
		}
		else {
			throw new IllegalArgumentException("Unsupported event type '" + eventName + "'.");
		}
		return ret;
	}
	
	@Override
	public int receiveEvents(NcEventSpec[] ncEventSpecs, int processingDelayMillis, int numberOfEvents)
			throws CouldntPerformActionEx {
		
		m_logger.info("Will receive events on " + ncEventSpecs.length + " NC(s), with processingDelayMillis=" + processingDelayMillis
				+ ", numberOfEvents=" + (numberOfEvents > 0 ? numberOfEvents : "infinite") );
		
		// Set up receivers
		
		// sync object used to wait for numberOfEvents (if specified)
		CountDownLatch sharedEventCountdown = null;
		if (numberOfEvents > 0) {
			sharedEventCountdown = new CountDownLatch(numberOfEvents);
		}

		StopWatch sw = new StopWatch();
		
		// This field will hold the timestamp of the first event received, no matter which one of the receivers got it.
		final AtomicLong firstEventReceivedTimeNanos = new AtomicLong(-1);
		
		try {
			// iterate over NCs
			for (NcEventSpec ncEventSpec : ncEventSpecs) {
				AcsEventSubscriber<IDLEntity> sub = subsOrPubs.get(ncEventSpec.ncName);
				if (sub == null) {
					throw new AcsJCouldntPerformActionEx("No subscriber available for NC '" + ncEventSpec.ncName + "'.");
				}
//				else {
//					m_logger.info("Dealing with subscriber for NC=" + ncEventSpec.ncName + ", which is in state "+ sub.getLifecycleState());
//				}
				
				// iterate over event types
				for (String eventName : ncEventSpec.eventNames) {
					sub.addSubscription(createEventHandler(eventName, ncEventSpec.antennaName, processingDelayMillis, sharedEventCountdown, firstEventReceivedTimeNanos));
					m_logger.info("Added subscription for event=" + eventName + ", NC=" + ncEventSpec.ncName);
				}
				sub.startReceivingEvents();
			}
		} catch (AcsJCouldntPerformActionEx ex) {
			ex.printStackTrace();
			throw ex.toCouldntPerformActionEx();
		} catch (Exception ex) {
			ex.printStackTrace();
			throw new AcsJCouldntPerformActionEx(ex).toCouldntPerformActionEx();
		}
		m_logger.info(ncEventSpecs.length + " subscriber(s) set up to receive events, which took " + sw.getLapTimeMillis() + " ms.");
		
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
			long receptionTimeNanos = ( numberOfEvents > 0 && firstEventReceivedTimeNanos.longValue() > 0 
					? (System.nanoTime() - firstEventReceivedTimeNanos.longValue()) 
					: -1 );
			return (int) TimeUnit.NANOSECONDS.toMillis(receptionTimeNanos);
		}
	}


}
