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
package acs.benchmark.nc.supplier;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ThreadPoolExecutor.AbortPolicy;

import org.omg.CORBA.portable.IDLEntity;

import acs.benchmark.nc.CorbaNotifyBaseImpl;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.AcsLogLevel;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.util.StopWatch;
import alma.benchmark.CorbaNotifySupplierOperations;
import alma.benchmark.LightweightMountStatusData;
import alma.benchmark.MountStatusData;
import alma.benchmark.NcEventSpec;
import alma.benchmark.SomeOtherEventType;

public class CorbaNotifySupplierImpl extends CorbaNotifyBaseImpl<AcsEventPublisher<IDLEntity>> implements CorbaNotifySupplierOperations
{
	/**
	 * One runnable per NC. Each run() call publishes an event.
	 */
	private final List<PublishEventRunnable> runnables = new ArrayList<PublishEventRunnable>();

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
	protected AcsEventPublisher<IDLEntity> createNcParticipant(String ncName) throws AcsJContainerServicesEx {
		return m_containerServices.createNotificationChannelPublisher(ncName, IDLEntity.class);
	}

	@Override
	protected void disconnectNcParticipant(AcsEventPublisher<IDLEntity> pub) throws AcsJIllegalStateEventEx {
		pub.disconnect();
	}

	/**
	 * Runnable that can repeatedly publish the same (or an alternating series of different) events to a given NC.
	 * The run method expects to be called multiple times. If a finite number of events is specified to be published,
	 * and the {@link #setScheduledFuture(ScheduledFuture)} method has been called with the corresponding future object
	 * from an executor, then this runnable will cancel further executions once all events are sent.
	 */
	protected class PublishEventRunnable implements Runnable {
		private final Object ncName;
		/**
		 * Event data that will be published alternatingly
		 */
		private final List<IDLEntity> events;
		private final AcsEventPublisher<IDLEntity> pub;
		private final int eventsMax;
		private int eventsSent;
		private ScheduledFuture<?> future;
		private int lastEventIndex = -1;
		
		PublishEventRunnable(NcEventSpec spec, AcsEventPublisher<IDLEntity> pub, int numberOfEvents) {
			if (spec.ncName == null || spec.ncName.isEmpty()) {
				throw new IllegalArgumentException("No NC specified");
			}
			if (spec.eventNames.length == 0) {
				throw new IllegalArgumentException("No events specified for NC " + spec.ncName);
			}
			if (pub == null) {
				throw new IllegalArgumentException("AcsEventPublisher was null for NC " + spec.ncName);
			}
			this.ncName = spec.ncName;
			this.events = new ArrayList<IDLEntity>(spec.eventNames.length);
			for (String eventName : spec.eventNames) {
				IDLEntity event = createTestEvent(eventName, spec.antennaName);
				events.add(event);
			}
			this.pub = pub;
			this.eventsMax = numberOfEvents;
		}
		
		/**
		 * Support self-cancelling of repeated executions from inside the {@link #run()} method, 
		 * in case that {@link #eventsMax} is >= 0 and we have already sent the requested
		 * number of events.
		 */
		void setScheduledFuture(ScheduledFuture<?> future) {
			this.future = future;
		}
		
		/**
		 * Allows cancellation of the running event suppliers.
		 * A single call to the NC libs will not be interrupted though.
		 */
		void cancelPeriodicRuns() {
			if (future != null) {
				future.cancel(false);
			}
		}
		
		@Override
		public void run() {
			// Alternatingly publish different events, trusting that "events" is not an empty list...
			if (lastEventIndex >= events.size()-1) {
				lastEventIndex = 0;
			}
			else {
				lastEventIndex++;
			}
			try {
				pub.publishEvent(events.get(lastEventIndex));
				eventsSent++;
				if (eventsMax > 0 && eventsSent == eventsMax) {
					m_logger.info("Supplier for NC '" + ncName + "' done sending " + eventsMax + " events.");
					cancelPeriodicRuns();
				}
			} catch (AcsJException ex) {
				// TODO Auto-generated catch block
				ex.printStackTrace();
			}
		}
	}
	
	
	
	/**
	 * Factory method for event data that will be sent to the NCs.
	 * @param eventName
	 * @param antennaName  Only used for eventName == MountStatusData or LightweightMountStatusData
	 * @return 
	 * @throws IllegalArgumentException 
	 */
	protected IDLEntity createTestEvent(String eventName, String antennaName) {
		IDLEntity ret = null;
		String antennaNameToSet = ( antennaName != null && !antennaName.isEmpty() ? antennaName : "unknown" );
		if (eventName.equals("MountStatusData")) {
			MountStatusData data = new MountStatusData();
			data.antennaName = antennaNameToSet;
			// @TODO set some of the boolean / double fields
			ret = data;
		}
		else if (eventName.equals("LightweightMountStatusData")) {
			LightweightMountStatusData data = new LightweightMountStatusData();
			data.antennaName = antennaNameToSet;
			ret = data;
		}
		else if (eventName.equals("SomeOtherEventType")) {
			SomeOtherEventType data = new SomeOtherEventType();
			ret = data;
		}
		// @TODO Add support for more events types as needed
		else {
			throw new IllegalArgumentException("Unsupported event type '" + eventName + "'.");
		}
		return ret;
	}
	
	
	@Override
	public int sendEvents(NcEventSpec[] ncEventSpecs, int eventPeriodMillis, int numberOfEvents)
			throws CouldntPerformActionEx {
		
		StopWatch sw = null;
		ScheduledThreadPoolExecutor runner = null; 
		
		// Set up the runnables for all NCs
		try {
			for (NcEventSpec ncEventSpec : ncEventSpecs) {
				PublishEventRunnable runnable = new PublishEventRunnable(
						ncEventSpec, 
						this.subsOrPubs.get(ncEventSpec.ncName),
						numberOfEvents );
				runnables.add(runnable);
			}
			// multithreaded executor
			runner = new ScheduledThreadPoolExecutor(
					ncEventSpecs.length, // thread per NC
					m_containerServices.getThreadFactory(), 
					new AbortPolicy() ); //RejectedExecutionException
			
			sw = new StopWatch();
			
			// run the NC suppliers
			for (PublishEventRunnable runnable : runnables) {
				ScheduledFuture<?> future = null;
				if (eventPeriodMillis > 0) {
					// publish at fixed rate
					future = runner.scheduleAtFixedRate(runnable, 0, eventPeriodMillis, TimeUnit.MILLISECONDS);
				}
				else {
					// run continuously 
					future = runner.scheduleWithFixedDelay(runnable, 0, 1, TimeUnit.NANOSECONDS); // delay must be > 0, otherwise IllegalArgumentException
				}
				runnable.setScheduledFuture(future);
			}
			
		} catch (Exception ex) {
			m_logger.log(AcsLogLevel.SEVERE, "sendEvents call failed", ex);
			throw new AcsJCouldntPerformActionEx(ex).toCouldntPerformActionEx();
		}
		
		String msgBase = "Started publishing events on " + ncEventSpecs.length + " NC(s), sending events "
				+ ( eventPeriodMillis > 0 ? "every " + eventPeriodMillis + " ms. " : "as fast as possible. " );
		if (numberOfEvents > 0) {
			m_logger.info(msgBase + "Will now wait until " + numberOfEvents + " have been published on every NC...");
			// block until all events are sent
			runner.setContinueExistingPeriodicTasksAfterShutdownPolicy(true);
			runner.setExecuteExistingDelayedTasksAfterShutdownPolicy(true);
			runner.shutdown();
			try {
				boolean cleanTermination = runner.awaitTermination(10, TimeUnit.MINUTES); // 10 min timeout, just to clean up resources eventually.
				if (!cleanTermination) {
					m_logger.warning("Unforeseen termination of event suppliers after 10 min (timeout).");
					cancel = true;
				}
			} catch (InterruptedException ex) {
				cancel = true;
			}
		}
		else {
			m_logger.info(msgBase + "Will return and asynchronously continue publishing events, until interrupt() gets called.");
		}
		
		if (cancel) {
			throw new AcsJCouldntPerformActionEx("Event sending was interrupted or failed otherwise.").toCouldntPerformActionEx();
		}
		
		return (int) sw.getLapTimeMillis();
	}

	@Override
	public void interrupt() {
		super.interrupt();

		for (PublishEventRunnable runnable : runnables) {
			runnable.cancelPeriodicRuns();
		}
		m_logger.info("Stopped publishing events.");
	}

}
