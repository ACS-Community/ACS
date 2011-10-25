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

/**
 * @author dfugate
 * @version $Id: ArchiveConsumer.java,v 1.15 2011/10/25 14:17:26 hsommer Exp $
 */

import java.lang.reflect.Method;
import java.util.logging.Level;

import org.omg.CosNotification.StructuredEvent;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;

/**
 * ArchiveConsumer is a a Consumer-derived class designed solely for the purpose
 * of processing notification channel structured events sent automatically by
 * BACI properties under certain conditions. Basically all one has to do to use
 * this class is create an ArchiveConsumer object providing an object with
 * implements "receive(Long timeStamp, String device, String parameter, Object
 * value)" and then invoke the consumerReady() method. Since archive events do
 * not contain complex IDL structs, filtering using the extended trader
 * constraint language should work as well.
 * <p>
 * HSO 2009: Note that the base class Consumer is getting replaced with NCSubscriber, 
 * and also that in Alma this ArchiveConsumer is being replaced with the new TMCDB monitoring code.
 * 
 * @author dfugate
 */
public class ArchiveConsumer extends Consumer {

	/**
	 * There is exactly one receive method that will be invoked per
	 * ArchiveConsumer object.
	 */
	private final Method receiveMethod_m;

	/**
	 * There is exactly one receiver that will be used by each ArchiveConsumer
	 * object.
	 */
	private final Object receiver_m;

	
	/**
	 * Creates a new instance of ArchiveConsumer
	 * 
	 * @param services
	 *           This is used to access ACS logging system.
	 * @param receiver
	 *           An object which implements a method called "receive". The
	 *           "receive" method must accept four parameters which
	 *           are: timeStamp(long), device(string), parameter(string), 
	 *           and value(Object).
	 * @throws AcsJException
	 *            Thrown on any <I>really bad</I> error conditions encountered.
	 */
	public ArchiveConsumer(ContainerServicesBase services, Object receiver) throws AcsJException {
		// call the super.
		super(alma.acscommon.ARCHIVING_CHANNEL_NAME.value, services);

		// check to ensure receiver is capable to processing the event
		Class<?> receiverClass = receiver.getClass();
		Method receiveMethod = null;

		// receive will have four parameters
		Class<?>[] parm = { Long.class, String.class, String.class, Object.class };

		// if this fails we know that the developer has not defined "receive"
		// correctly at not at all. we can do nothing more
		try {
			receiveMethod = receiverClass.getMethod(RECEIVE_METHOD_NAME, parm);
		} catch (Exception ex) { // NoSuchMethodException, SecurityException
			// Well the method doesn't exist...that sucks!
			AcsJIllegalArgumentEx ex2 = new AcsJIllegalArgumentEx();
			ex2.setVariable("receiver");
			ex2.setErrorDesc("Object of type '" + receiverClass.getName() + "' does not implement a visible and correct '" + 
					RECEIVE_METHOD_NAME + "' method which it needs to receive events.");
			m_logger.log(Level.FINE, "Bad monitor event receiver!", ex2);
			throw ex2;
		}
		
		// save the receive method for later event dispatching
		receiveMethod_m = receiveMethod;

		// save the receiver for later use.
		receiver_m = receiver;
	}

	/**
	 * Overridden
	 * 
	 * @return string
	 */
	protected String getChannelKind() {
		// because archive channels are registered differently
		// in the CORBA naming service than ICD-style channels
		return alma.acscommon.ARCHIVING_CHANNEL_KIND.value;
	}

	/**
	 * Overridden.
	 * 
	 * @return string
	 */
	protected String getNotificationFactoryName() {
		return alma.acscommon.ARCHIVE_NOTIFICATION_FACTORY_NAME.value;
	}

	/**
	 * Overridden so that super constructor subscribes to all events. 
	 * @throws AcsJException 
	 */
	protected void configSubscriptions() {
		// calling addsubscription on null automatically subscribes
		// to all event types.
		try {
		addSubscription(null);
		} catch (Exception e) {
			String msg = "Failed to subscribe to archive events: ";
			msg = msg + e.getMessage();
			m_logger.severe(msg);
		}

		return;
	}

	/**
	 * Overridden.
	 * <p>
	 * @TODO: add reference to documentation about the expected event data, i.e.
	 * timeStamp, device, parameter, value.
	 * 
	 * @param structuredEvent CORBA NC StructuredEvent
	 */
	public void push_structured_event(StructuredEvent structuredEvent) {
		try {
			String[] eventNames = structuredEvent.header.fixed_header.event_name.split(":");

			// extract the useful info
			//Long timeStamp = new Long(structuredEvent.filterable_data[0].value.extract_ulonglong());
			Long timeStamp = (Long) m_anyAide.corbaAnyToObject(structuredEvent.filterable_data[0].value);
			Object value = m_anyAide.corbaAnyToObject(structuredEvent.filterable_data[1].value);

			String device = eventNames[1];
			String parameter = eventNames[2];
			// String containerName = eventNames[0];

			// organize it into an argument list to be sent to the receive method.
			// order is critical here
			Object[] arg = { timeStamp, device, parameter, value };

			// try sending it to the receiver.
			receiveMethod_m.invoke(receiver_m, arg);
		} catch (java.lang.IllegalAccessException e) {
			// should never happen...
			String msg = "Failed to process an event on the '" + m_channelName + "' channel because: ";
			msg = msg + e.getMessage();
			m_logger.warning(msg);
		} catch (java.lang.reflect.InvocationTargetException e) {
			// should never happen...
			String msg = "Failed to process an event on the '" + m_channelName + "' channel because: ";
			msg = msg + e.getMessage();
			m_logger.warning(msg);
		}
	}

}
