/*
 * ALMA - Atacama Large Millimiter Array (c) Associated Universities Inc., 2002
 * (c) European Southern Observatory, 2002 Copyright by ESO (in the framework of
 * the ALMA collaboration), All rights reserved
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
 * 
 * Helper.java
 * 
 * Created on March 4, 2003, 10:41 AM
 * 
 * This class is intended to be a prototype for the Java notification channel
 * based in part on the C++ Helper class in acsnc.
 */

package alma.acs.nc;

import java.util.logging.Logger;

import org.omg.CORBA.IntHolder;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextHelper;
import org.omg.CosNotifyChannelAdmin.EventChannel;
import org.omg.CosNotifyChannelAdmin.EventChannelFactory;
import org.omg.CosNotifyChannelAdmin.EventChannelFactoryHelper;
import org.omg.CosNotifyChannelAdmin.EventChannelHelper;

import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.acs.container.AdvancedContainerServices;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;

/**
 * This class provides methods useful to both supplier and consumer objects.
 */
public class Helper {
	/**
	 * Creates a new instance of Helper.
	 * 
	 * @param services A reference to the ContainerServices
	 * @throws AcsJException Generic ACS exception will be thrown if anything in this class is broken.
	 */
	public Helper(ContainerServicesBase services) throws AcsJException {
		if (services == null) {
			// make sure this code is being run within a container or client
			String reason = "Null reference obtained for the ContainerServices!";
			throw new AcsJBadParameterEx(reason);
		}

		// save a local reference to the container services
		m_services = services;

		// immediately grab a logger
		m_logger = m_services.getLogger();

		// create a helper object used to retrieve channel properties
		m_channelProperties = new ChannelProperties(services);

		// the only reason these are invoked so we know immediately if something
		// is going to fail in this class.
		getNamingService();
	}

	
	/**
	 * Returns a reference to the Container Services which are provided by a
	 * container or client.
	 * 
	 * @return A valid reference to the ContainerServices instance.
	 */
	public ContainerServicesBase getContainerServices() {
		return m_services;
	}

	/**
	 * Returns a reference to the Naming Service.
	 * <p>
	 * @TODO: The dependence of NC libs on the NamingService should be expressed in a less hidden form 
	 * than using a property, which leads to dangerous runtime-only failures if missing.
	 * It would already be an improvement if we would obtain the NamingService reference
	 * from {@link AdvancedContainerServices#getORB()}.
	 * A more radical change would be to integrate the NC classes into the ContainerServices.
	 * 
	 * @return Valid reference to the Naming Service.
	 * @throws AcsJException
	 *            Thrown when there's a bad corbaloc given for the Naming Service
	 *            or the reference cannot be narrowed.
	 */
	public NamingContext getNamingService() throws AcsJException {
		// Most likely the first call to this method...
		if (m_nContext == null) {
			// acsStartJava always add this Java property for us
			String nameCorbaloc = System.getProperty(m_nameJavaProp);
			// make sure the end-user is using acsStartJava
			if (nameCorbaloc == null) {
				String reason = "Null reference obtained from the Java property '" + m_nameJavaProp
						+ "' for the Naming Service corbaloc!";
				throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason);
			}

			// get the unnarrowed reference to the Naming Service
			org.omg.CORBA.Object tempCorbaObject = getContainerServices().getAdvancedContainerServices().corbaObjectFromString(nameCorbaloc);
			if (tempCorbaObject == null) {
				// very bad situation. without the naming service we cannot do anything.
				String reason = "Null reference obtained for the Naming Service, corbaloc=" + nameCorbaloc;
				throw new alma.ACSErrTypeCORBA.wrappers.AcsJFailedToResolveServiceEx(reason);
			}

			m_nContext = NamingContextHelper.narrow(tempCorbaObject);
			if (m_nContext == null) {
				// very bad situation. without the naming service we cannot do anything.
				String reason = "Unable to narrow Naming Service reference to the correct type!";
				throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(reason);
			}
		}
		return m_nContext;
	}

	
	/**
	 * This method gets a reference to the event channel. If it is not already
	 * registered with the naming service, it is created.
	 * 
	 * @return Reference to the event channel specified by channelName.
	 * @param channelName
	 *           Name of the event channel registered with the CORBA Naming
	 *           Service
	 * @param channelKind
	 *           Kind of the channel as registered with the CORBA naming service.
	 * @param notifyFactoryName
	 *           Name of the notification service as registered with the CORBA
	 *           naming service.
	 * @throws AcsJException
	 *            Standard ACS Java exception.
	 */
	protected EventChannel getNotificationChannel(String channelName, String channelKind, String notifyFactoryName) 
		throws AcsJException 
	{
		// return value
		EventChannel retValue = null;

		try {
//			m_logger.fine("Will create notification channel " + channelName);
			NameComponent[] t_NameSequence = { new NameComponent(channelName, channelKind) };
			retValue = EventChannelHelper.narrow(getNamingService().resolve(t_NameSequence));
		} 
		catch (org.omg.CosNaming.NamingContextPackage.NotFound e) {
			// No other suppliers have created the channel yet...create it
			m_logger.info("Attempting to create the " + channelName + " channel.");
			return createNotificationChannel(channelName, channelKind, notifyFactoryName);
		} 
		catch (org.omg.CosNaming.NamingContextPackage.CannotProceed e) {
			// Think there is virtually no chance of this every happening but...
			throw new AcsJUnexpectedExceptionEx(e);
		} 
		catch (org.omg.CosNaming.NamingContextPackage.InvalidName e) {
			// Think there is virtually no chance of this every happening but...
			throw new AcsJUnexpectedExceptionEx(e);
		}
		
		return retValue;
	}

	/**
	 * Creates a notification channel (using quality of service and
	 * administrative properties specified by configQofS() and configAdminProps()
	 * respectively). Then registers this channel with the naming service. Should
	 * only be invoked when the channel this supplier is attempting to connect to
	 * does not exist.
	 * 
	 * @return Reference to the newly created channel.
	 * @param channelName
	 *           Name of the channel to create.
	 * @param channelKind
	 *           Kind of the channel as registered with the CORBA naming service.
	 * @param notifyFactoryName
	 *           Name of the notification service as registered with the CORBA
	 *           naming service.
	 * @throws AcsJException
	 *            Standard ACS Java exception.
	 */
	protected EventChannel createNotificationChannel(String channelName, String channelKind, String notifyFactoryName) 
		throws AcsJException 
	{
		// return value
		EventChannel retValue = null;
		try {
			// get the Notification Factory first.
			NameComponent[] t_NameFactory = { new NameComponent(notifyFactoryName, /* alma.acscommon.NOTIFICATION_FACTORY_NAME.value, */"") };
			EventChannelFactory notifyFactory = EventChannelFactoryHelper.narrow(getNamingService().resolve(t_NameFactory));

			// create the channel
			// here we use the channel properties taken directly from our channel properties helper object. 
			// presumably these values come from the ACS configuration database.
			retValue = notifyFactory.create_channel(m_channelProperties.configQofS(channelName),
					m_channelProperties.configAdminProps(channelName), new IntHolder());
			// sanity check
			if (retValue == null) {
				// a null reference implies we cannot go any further
				String reason = "Null reference obtained for the '" + channelName + "' channel!";
				throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason); // @todo: more specific ex type
			}

			// register our new channel with the naming service
			NameComponent[] t_NameChannel = { new NameComponent(channelName, channelKind) };
			getNamingService().rebind(t_NameChannel, retValue);
		} catch (org.omg.CosNaming.NamingContextPackage.NotFound e) {
			String reason = "The CORBA Notification Service is not registered in the Naming Service: ";
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(reason + e.getMessage());
		} catch (org.omg.CosNaming.NamingContextPackage.CannotProceed e) {
			// Think there is virtually no chance of this every happening but...
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(e.getMessage());
		} catch (org.omg.CosNaming.NamingContextPackage.InvalidName e) {
			// Think there is virtually no chance of this every happening but...
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(e.getMessage());
		} catch (org.omg.CosNotification.UnsupportedAdmin e) {
			String reason = "The administrative properties specified for the '" + channelName
					+ "' channel are unsupported: ";
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(reason + e.getMessage());
		} catch (org.omg.CosNotification.UnsupportedQoS e) {
			String reason = "The quality of service properties specified for the '" + channelName
					+ "' channel are unsupported: ";
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(reason + e.getMessage());
		}
		return retValue;
	}

	/**
	 * <b>Destroys the channel and unregisters it from the naming service. ONLY
	 * USE THIS METHOD IF YOU KNOW FOR CERTAIN THERE IS ONLY ONE SUPPLIER FOR THE
	 * CHANNEL!!! Use this method with extreme caution as it's likely to become
	 * deprecated in future versions of ACS!</b>
	 * 
	 * @param channelName
	 *           name of the channel as registered int the CORBA notification
	 *           service
	 * @param channelKind
	 *           Kind of the channel as registered with the CORBA naming service.
	 * @param channelRef
	 *           reference to the channel being destroyed
	 * @throws AcsJException
	 *            Thrown when the channel isn't registered with the Naming
	 *            Service.
	 * @warning this method assumes
	 */
	protected void destroyNotificationChannel(String channelName, String channelKind, EventChannel channelRef)
			throws AcsJException 
	{
		try {
//			m_logger.fine("Will destroy notification channel " + channelName);
			// destroy the remote CORBA object
			channelRef.destroy();

			// unregister our channel with the naming service
			NameComponent[] t_NameChannel = { new NameComponent(channelName, channelKind) };
			getNamingService().unbind(t_NameChannel);
		} catch (org.omg.CosNaming.NamingContextPackage.NotFound e) {
			// Think there is virtually no chance of this every happening but...
			String msg = "Cannot unbind the '" + channelName + "' channel from the Naming Service!";
			m_logger.severe(msg);
		} catch (org.omg.CosNaming.NamingContextPackage.CannotProceed e) {
			// Think there is virtually no chance of this every happening but...
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(e.getMessage());
		} catch (org.omg.CosNaming.NamingContextPackage.InvalidName e) {
			// Think there is virtually no chance of this every happening but...
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(e.getMessage());
		}
	}

	
	/**
	 * Provides access to the information about the channel contained within the
	 * ACS CDB
	 * 
	 * @return This class's channel properties member.
	 */
	public ChannelProperties getChannelProperties() {
		return m_channelProperties;
	}

	// --------------------------------------------------------------------------

	/**
	 * In a running system, there can be only one reference to the Naming Service.
	 * <p>
	 * @TODO: This static variable is causing problems inside JUnit tests (discovered 2007-11). 
	 *        As a workaround, it must be nulled in the TestCase's tearDown method.
	 *        We should check if it really needs to be static, and if so, whether
	 *        we could introduce a cleanUp() method in the NC librarires somewhere
	 *        which would null this variable.
	 */
	public static NamingContext m_nContext;

	// /Java property name for the CORBA Naming Service corbaloc.
	private static final String m_nameJavaProp = "ORBInitRef.NameService";

	// / Access to the component's name along with the logging service.
	private final ContainerServicesBase m_services;

	// / Our own personal logger
	private final Logger m_logger;

	// / Provides access to channel's quality of service properties
	private final ChannelProperties m_channelProperties;
}
