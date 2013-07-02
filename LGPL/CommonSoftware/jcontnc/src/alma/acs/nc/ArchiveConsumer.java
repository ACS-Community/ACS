/*
 * ALMA - Atacama Large Millimiter Array Copyright (c) ESO - European Southern Observatory, 2005
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
 */
package alma.acs.nc;

import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNotification.StructuredEvent;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acsnc.EventDescription;

/**
 * ArchiveConsumer is designed solely for the purpose
 * of processing notification channel structured events sent automatically by
 * BACI properties under certain conditions. 
 * Calls are delegated to the encapsulated TweakedNCSubscriber instance that inherits from NCSubscriber.
 * This way we don't have to expose NCSubscriber methods that make no sense for this special subscriber.
 * <p>
 * These events are not IDL-defined structs! 
 * The type parameter IDLEntity is therefore cheating, to get by the checks in the base class.
 * This is OK because we intercept the events at the 'push_structured_event_called' level, 
 * before any event type information gets applied in the processing.
 * See ACS/Documents/Logging_and_Archiving.doc (currently a bit outdated) 
 * and archiveevents :: archiveeventsArchiveSupplier.cpp for detail of the sender code. 
 * Filtering using the extended trader constraint language should work.
 * <p>
 * Basically all one has to do to use
 * this class is create an ArchiveConsumer object providing an ArchiveReceiver callback,
 * and then invoke the startReceivingEvents() method. 
 * <p>
 * Note that in Alma the archiving NC has been replaced by the TMCDB monitoring framework.
 * The eventGUI is the only known Java client for the old archiving NC, thus we continue 
 * supporting this class at medium inspiration level.
 * <p>
 * 
 * @author dfugate, hsommer
 */
public class ArchiveConsumer {

	/**
	 * The client must implement this callback
	 */
	public static interface ArchiveReceiver {
		public void receive(long timeStamp, String device, String property, Object value);
	}

	private final NCSubscriber<IDLEntity> delegate;
	

	/**
	 * Creates a new instance of ArchiveConsumer.
	 * 
	 * @param services
	 * @param receiver
	 * @throws AcsJException
	 */
	public ArchiveConsumer(ArchiveReceiver receiver, ContainerServicesBase services, NamingContext namingService) throws AcsJException {
		
		delegate = new ArchiveTweakedNCSubscriber(receiver, services, namingService, ArchiveConsumer.class.getSimpleName());
	}

	public final void startReceivingEvents() throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx {
		delegate.startReceivingEvents();
	}

	public final void disconnect() throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx {
		delegate.disconnect();
	}


	private static class ArchiveTweakedNCSubscriber extends NCSubscriber<IDLEntity> {
		
		/**
		 * There is exactly one receiver that will be used by each ArchiveConsumer object.
		 */
		private final ArchiveReceiver userReceiver;

		public ArchiveTweakedNCSubscriber(ArchiveReceiver userReceiver, ContainerServicesBase services, NamingContext namingService, String clientName)
				throws AcsJException {
			super(alma.acscommon.ARCHIVING_CHANNEL_NAME.value, null, services, namingService, clientName, IDLEntity.class);
			this.userReceiver = userReceiver;

			GenericCallback internalDummyReceiver = new GenericCallback() {
				@Override
				public void receiveGeneric(Object event, EventDescription eventDescrip) {
					logger.warning("Unexpected call to 'receiveGeneric', which should not happen with our non-standard use of NCSubscriber.");
				}
			};
			addGenericSubscription(internalDummyReceiver);
		}

		@Override
		protected String getNotificationFactoryName() {
			return alma.acscommon.ARCHIVE_NOTIFICATION_FACTORY_NAME.value;
		}
	
		/**
		 * @param structuredEvent CORBA NC StructuredEvent with special format.
		 */
		@Override
		protected boolean push_structured_event_called(StructuredEvent structuredEvent) {
	
			// String containerName = "";
			String device = "?";
			String property = "?";
	
			// ArchiveSupplier codes this as "container:component:property" to avoid a few Corba Anys
			String abusedEventName = structuredEvent.header.fixed_header.event_name;
			
			String[] locationInfo = abusedEventName.split(":");
			if (locationInfo.length == 3) {
				// containerName = locationInfo[0];
				device = locationInfo[1];
				property = locationInfo[2];
			} 
			else {
				// TODO: error log with repeat guard
			}
			
			// extract the useful info
			Long timeStamp = (Long) anyAide.corbaAnyToObject(structuredEvent.filterable_data[0].value); // Property name is "time_stamp"
			Object value = anyAide.corbaAnyToObject(structuredEvent.filterable_data[1].value); // Property name is "value"
	
			// give it to the receiver
			userReceiver.receive(timeStamp, device, property, value);
			
			return false;
		}

	}	
	
}
