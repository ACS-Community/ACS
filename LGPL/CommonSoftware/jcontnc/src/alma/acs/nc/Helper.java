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

import java.util.HashMap;
import java.util.Random;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.text.ParseException;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.IntHolder;
import org.omg.CosNaming.Binding;
import org.omg.CosNaming.BindingIteratorHolder;
import org.omg.CosNaming.BindingListHolder;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextHelper;
import org.omg.CosNotification.Property;
import org.omg.CosNotification.PropertyError;
import org.omg.CosNotification.UnsupportedAdmin;
import org.omg.CosNotification.UnsupportedQoS;
import org.omg.CosNotifyChannelAdmin.ChannelNotFound;

import com.cosylab.CDB.DAO;
import com.cosylab.cdb.client.CDBAccess;
import com.cosylab.cdb.client.DAOProxy;
import com.cosylab.util.WildcharMatcher;

import gov.sandia.NotifyMonitoringExt.EventChannel;
import gov.sandia.NotifyMonitoringExt.EventChannelFactory;
import gov.sandia.NotifyMonitoringExt.EventChannelFactoryHelper;
import gov.sandia.NotifyMonitoringExt.EventChannelHelper;
import gov.sandia.NotifyMonitoringExt.NameAlreadyUsed;
import gov.sandia.NotifyMonitoringExt.NameMapError;

import alma.ACSErrTypeCORBA.wrappers.AcsJNarrowFailedEx;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;
import alma.ACSErrTypeCommon.wrappers.AcsJGenericErrorEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.AcsNCTraceLog.LOG_NC_ChannelCreatedRaw_OK;
import alma.AcsNCTraceLog.LOG_NC_ChannelCreated_ATTEMPT;
import alma.AcsNCTraceLog.LOG_NC_ChannelCreated_OK;
import alma.AcsNCTraceLog.LOG_NC_ChannelDestroyed_OK;
import alma.AcsNCTraceLog.LOG_NC_TaoExtensionsSubtypeMissing;
import alma.acs.container.AdvancedContainerServices;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.AcsLogLevel;
import alma.acs.util.StopWatch;
import alma.acscommon.ACS_NC_DOMAIN_ARCHIVING;
import alma.acscommon.ACS_NC_DOMAIN_LOGGING;
import alma.acscommon.ARCHIVE_NOTIFICATION_FACTORY_NAME;
import alma.acscommon.LOGGING_NOTIFICATION_FACTORY_NAME;
import alma.acscommon.NAMESERVICE_BINDING_NC_DOMAIN_DEFAULT;
import alma.acscommon.NAMESERVICE_BINDING_NC_DOMAIN_SEPARATOR;
import alma.acscommon.NC_KIND;
import alma.acscommon.NC_KIND_NCSUPPORT;


/**
 * This class provides methods useful to both supplier and consumer objects, 
 * where it is associated with exactly one Notification Channel.
 * It can also be used without any notification channel.
 * Never use a Helper instance for more than once NC!
 */
public class Helper {

	/**
	 * In a running system, there can be only one reference to the Naming Service.
	 */
	private final NamingContext m_nContext;

	/**
	 * Java property name for the CORBA Naming Service corbaloc.
	 * This property is set in the acsstartup :: acsStartJava.
	 * <p>
	 * @TODO: Check if we can eliminate this property if we use something similar as in
	 * jmanager's {@link com.cosylab.acs.maci.plug.NamingServiceRemoteDirectory}.
	 */
	private static final String m_nameJavaProp = "ORBInitRef.NameService";

	
	/**
	 * Access to the component's name along with the logging service.
	 */
	private final ContainerServicesBase m_services;

	// Our own personal logger
	protected final Logger m_logger;
	
	/**
	 * A Helper instance serves only one NC.
	 */
	protected final String channelName;
	
	/**
	 * The optional NC domain name, e.g. "ALARMSYSTEM".
	 */
	protected final String domainName;

    /**
     * Creation time of the channel to be used in order to know the restart of the Notify Service
     */
    protected Date channelTimestamp = new Date(0);
	
	/**
	 * channelId is used to reconnect to channel in case that Notify Server crashes
	 */
	private int channelId;
	
	// Provides access to channel's quality of service properties
	private final ChannelProperties m_channelProperties;
	
	/**
	 * Cached notify factory name where our NC is or should be hosted.
	 * Avoids unnecessary CDB calls.
	 */
	private volatile String ncFactoryName;
	
	/**
	 * TODO
	 */
	private EventChannelFactory notifyFactory;

	/**
	 * timestamp in ms when config error was found. Initialized to 0L.
	 * <p>
	 * Used similar to a log repeat guard, because the OMC was flooded with NC config problem logs 
	 * (as of 2008-03-06). 
	 */
	private Long channelConfigProblemTimestamp = 0L;

	/**
	 * Random number generator, used to randomize client names 
	 * when used as server-side proxy names.
	 * This class is static and thus shared among the subscribers and suppliers of one client or component,
	 * to reduce the risk that two subscribers or suppliers create
	 * Helper instances at the same time (system time is used as random generator seed) 
	 * and thereby produce identical sequences of random numbers, 
	 * which would defy the purpose of avoiding name clashes on the server.
	 */
	protected static final Random random = new Random(System.currentTimeMillis());

	public static String extractChannelName(String channelWithDomainName) throws AcsJIllegalArgumentEx {
		return splitChannelAndDomainName(channelWithDomainName)[0];
	}
	
	public static String extractDomainName(String channelWithDomainName) throws AcsJIllegalArgumentEx {
		return splitChannelAndDomainName(channelWithDomainName)[1];
	}

	private static String[] splitChannelAndDomainName(String channelWithDomainName) throws AcsJIllegalArgumentEx {
		String[] ret = channelWithDomainName.split(NAMESERVICE_BINDING_NC_DOMAIN_SEPARATOR.value);
		if (ret.length == 2) {
			return ret;
		}
		else {
			AcsJIllegalArgumentEx ex = new AcsJIllegalArgumentEx();
			ex.setVariable("channelWithDomainName");
			ex.setValue(channelWithDomainName);
			ex.setErrorDesc("No unique NC - domain separation found in NC binding.");
			throw ex;
		}
	}

	public static String combineChannelAndDomainName(String channelName, String domainName) {
		String ret = channelName;
		ret += NAMESERVICE_BINDING_NC_DOMAIN_SEPARATOR.value;
		if (domainName != null) {
			ret += domainName;
		}
		else {
			ret += NAMESERVICE_BINDING_NC_DOMAIN_DEFAULT.value;
		}
		return ret;
	}

	
	/**
	 * Same as {@link #Helper(String, String, ContainerServicesBase, NamingContext)} but without the NC domain;
	 * the default NC domain ({@link NAMESERVICE_BINDING_NC_DOMAIN_DEFAULT.value}) will be used.
	 * 
	 * @param channelName
	 * @param services
	 * @param namingService
	 * @throws AcsJException
	 */
	public Helper(String channelName, ContainerServicesBase services, NamingContext namingService) throws AcsJException {
		this(channelName, null, services, namingService);
	}
	
	/**
	 * Creates a new instance of Helper.
	 * @param channelName The NC that this Helper is made for.
	 * @param domainName The optional NC domain, or <code>null</code>.
	 * @param services A reference to the ContainerServices
	 * @param namingService Reference to the naming service.
	 * @throws AcsJException Generic ACS exception will be thrown if anything in this class is broken.
	 */
	public Helper(String channelName, String domainName, ContainerServicesBase services, NamingContext namingService) throws AcsJException {
		
		this.channelName = channelName;
		this.domainName = domainName;
		
		if (services == null) {
			AcsJBadParameterEx ex = new AcsJBadParameterEx();
			ex.setReason("Null reference obtained for the ContainerServices!");
			throw ex;
		}

		// save a local reference to the container services
		m_services = services;

		// immediately grab a logger
		m_logger = m_services.getLogger();

		// create a helper object used to retrieve channel properties
		m_channelProperties = new ChannelProperties(services);

		m_nContext = namingService;

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
	 * <b>This method should only be used by specialized framework code, 
	 * not by application code that uses NCs.</b>
	 * It retrieves and returns a reference to the Naming Service based on the
	 * Java property <code>ORBInitRef.NameService</code>.
	 * <p>
	 * Even specialized code that has an AcsManagerProxy instance available, 
	 * e.g. via an AdvancedComponentClient,
	 * should use the AcsManagerProxy to get the NamingContext reference:
	 * <pre>
	 * nctx = NamingContextHelper.narrow(AdvancedComponentClient.getAcsManagerProxy().get_service("NameService", false);
	 * </pre>.
	 * <p>
	 * 
	 * @return Valid reference to the Naming Service.
	 * @throws AcsJException
	 *            Thrown when there's a bad corbaloc given for the Naming Service
	 *            or the reference cannot be narrowed.
	 * @see #getNamingService()
	 * @deprecated To be removed once we have fixed the leftover usages in modules 
	 *             jcontnc, laser-core, acssamp, acssampGUI.
	 */
	public static NamingContext getNamingServiceInitial(ContainerServicesBase cs) throws AcsJException {
		
		// acsStartJava always adds this Java property for us
		String nameCorbaloc = System.getProperty(m_nameJavaProp);

		// make sure the end-user is using acsStartJava
		if (nameCorbaloc == null || nameCorbaloc.trim().isEmpty()) {
			AcsJBadParameterEx ex = new AcsJBadParameterEx();
			ex.setReason("Missing Java property '" + m_nameJavaProp + "' for the Naming Service corbaloc!");
			throw ex;
		}

		// get the unnarrowed reference to the Naming Service
		org.omg.CORBA.Object tempCorbaObject = cs.getAdvancedContainerServices().corbaObjectFromString(nameCorbaloc);
		if (tempCorbaObject == null) {
			// very bad situation. without the naming service we cannot do anything.
			Throwable cause = new Throwable("Null reference obtained for the Naming Service, corbaloc=" + nameCorbaloc);
			//AcsJFailedToResolveServiceEx ex = new AcsJFailedToResolveServiceEx();
			// @TODO add parameter "service" and "reason" to definition of AcsJFailedToResolveServiceEx
			// ex.setReason("Null reference obtained for the Naming Service, corbaloc=" + nameCorbaloc);
			throw new alma.ACSErrTypeCORBA.wrappers.AcsJFailedToResolveServiceEx(cause);
		}

		NamingContext ret = NamingContextHelper.narrow(tempCorbaObject);
		if (ret == null) {
			// very bad situation. without the naming service we cannot do anything.
			Throwable cause = new Throwable("Unable to narrow Naming Service reference to the correct type!");
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(cause);
		}
		
		return ret;
	}

	/**
	 * Returns a reference to the Naming Service.
	 * <p>
	 * @return Valid reference to the Naming Service.
	 */
	public NamingContext getNamingService() {
		return m_nContext;
	}
	
	
	/**
	 * The TAO extension's reconnect() methods call this (via NCSubscriber etc),
	 * so that we call again {@link org.omg.CosNotifyChannelAdmin.EventChannelFactoryOperations#get_event_channel(int)}.
	 */
	public EventChannel getNotificationChannel(EventChannelFactory ecf)
	{
		EventChannel ec = null;
		try {
			ec = EventChannelHelper.narrow( ecf.get_event_channel(channelId) );
		} catch (ChannelNotFound e) {
			// I cannot recover the channel using the ID
		}
		return ec;
	}

	/**
	 * This method gets a reference to the event channel. If it is not already
	 * registered with the naming service, it is created.
	 * 
	 * @return Reference to the event channel specified by channelName. Never null.
	 * @param channelKind
	 *           Kind of the channel as registered with the CORBA naming service ("channels").
	 * @param notifyFactoryName
	 *           Name of the notification service as registered with the CORBA
	 *           naming service.
	 * @throws AcsJException
	 *            Standard ACS Java exception.
	 */
	protected EventChannel getNotificationChannel(String notifyFactoryName) 
		throws AcsJException
	{
		String channelKind = NC_KIND.value;
		
		// return value
		EventChannel retValue = null;
		NameComponent[] t_NameSequence = { new NameComponent(combineChannelAndDomainName(channelName, domainName), channelKind) };
		// (retryNumberAttempts * retrySleepSec) = the time before we give up to get a reference or create the channel if 
		// a channel of the given name supposedly gets created already (due to race conditions with other clients).
		int retryNumberAttempts = 20;
		int retrySleepSec = 2;
		do {
			try {
				// @TODO move the check for existing channel from naming service to the NC factory,
				// now that we use the TAO extension with named NCs.
				// The only advantage of still using the naming service is that the naming service is a real system-wide singleton
				// and can return also channels that were by mistake created from a different notify service factory than the one configured in the CDB.
				initializeNotifyFactory(notifyFactoryName);
				retValue = EventChannelHelper.narrow(getNamingService().resolve(t_NameSequence));
			} 
			catch (org.omg.CosNaming.NamingContextPackage.NotFound e) {
				// No other consumers or suppliers have registered the channel yet...
				// This can mean that the channel has never been created, or that it is currently being created but has not yet been registered.
			} 
			catch (org.omg.CosNaming.NamingContextPackage.CannotProceed e) {
				// Think there is virtually no chance of this every happening but...
				throw new AcsJUnexpectedExceptionEx(e);
			} 
			catch (org.omg.CosNaming.NamingContextPackage.InvalidName e) {
				// Think there is virtually no chance of this every happening but...
				throw new AcsJUnexpectedExceptionEx(e);
			}
	
			if (retValue == null) {
				// Couldn't get the channel object from the naming service.
				// Let's try to create it, which may fail if some other consumer or supplier is currently doing the same,
				// but only because we use the TAO extensions that support named NCs.
				try {
					retValue = createNotificationChannel(channelKind, notifyFactoryName);
				} catch (NameAlreadyUsed ex) {
					m_logger.log(Level.INFO, "NC '" + channelName + "' seems to be getting created. Will wait and try again in " + retrySleepSec + " seconds.", ex);
					try {
						Thread.sleep(retrySleepSec*1000);
					} catch (InterruptedException ex1) {
						// too bad
					}
				}
			}

            // The channel could be resolved from the Naming Service
			else {
                // Get the channel timestamp located into the Naming Service or set it to the current time
                initChannelTimestamp();
//				System.out.println("*** Got NC " + channelName + " from the naming service");
			}
		} while (retValue == null && --retryNumberAttempts >= 0);
		
		if (retValue == null) {
			AcsJGenericErrorEx ex = new AcsJGenericErrorEx();
			ex.setErrorDesc("Giving up to get reference to channel " + channelName);
			throw ex;
		}
		
		return retValue;
	}
	
	
	/**
	 * @param notifyFactoryName
	 * @throws AcsJException AcsJCORBAProblemEx if the NotifyService reference cannot be retrieved from the NamingService;
	 *                       AcsJNarrowFailedEx if the NotifyService is not of the required TAO extension type.
	 */
	protected void initializeNotifyFactory(String notifyFactoryName) 
		throws AcsJException
	{
		if (notifyFactory == null){
			final String standardEventFactoryId = org.omg.CosNotifyChannelAdmin.EventChannelFactoryHelper.id();
			final String specialEventFactoryId = gov.sandia.NotifyMonitoringExt.EventChannelFactoryHelper.id();
			
			// get the Notification Factory first.
			NameComponent[] t_NameFactory = { new NameComponent(notifyFactoryName, "") };
			org.omg.CORBA.Object notifyFactoryObj = null;
			//notifyFactory = null;
			try {
				notifyFactoryObj = getNamingService().resolve(t_NameFactory);
			}
			catch (org.omg.CosNaming.NamingContextPackage.NotFound ex) {
				String reason = "The CORBA Notification Service '" + notifyFactoryName + 
							"' is not registered in the Naming Service: " + ex.why.toString();
				AcsJCORBAProblemEx ex2 = new AcsJCORBAProblemEx();
				ex2.setInfo(reason);
				throw ex2;
			} catch (org.omg.CosNaming.NamingContextPackage.CannotProceed e) {
				// Think there is virtually no chance of this every happening but...
				Throwable cause = new Throwable(e.getMessage());
				throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
			} catch (org.omg.CosNaming.NamingContextPackage.InvalidName e) {
				// Think there is virtually no chance of this every happening but...
				Throwable cause = new Throwable(e.getMessage());
				throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
			}
			
			// narrow the notification factory to the TAO extension subtype
			try {
				notifyFactory = EventChannelFactoryHelper.narrow(notifyFactoryObj);
			} catch (BAD_PARAM ex) {
				if (notifyFactoryObj._is_a(standardEventFactoryId)) {
					LOG_NC_TaoExtensionsSubtypeMissing.log(m_logger, notifyFactoryName, specialEventFactoryId, standardEventFactoryId);
				}
				else {
					LOG_NC_TaoExtensionsSubtypeMissing.log(m_logger, notifyFactoryName, specialEventFactoryId, "???");
				}
				AcsJNarrowFailedEx ex2 = new AcsJNarrowFailedEx(ex);
				ex2.setNarrowType(specialEventFactoryId);
				throw ex2;
			}
		}
	}
	
	/**
	 * Tries to create a notification channel (using quality of service and administrative properties 
	 * specified by configQofS() and configAdminProps() respectively).
	 * If this succeeds, then registers this channel with the naming service.
	 * <p>
	 * Should only be invoked when the channel that this supplier or consumer is attempting to connect to
	 * does not exist.
	 * However even with prior check for the existence of this channel, a race condition with other suppliers or consumers
	 * can lead to multiple attempts to create the same channel, which will result in <code>NameAlreadyUsed</code> exception.
	 * <p>
	 * Design note: Currently the TAO notification extensions are used to synch channel creation with other clients
	 * by supplying the channel name to the factory.
	 * If we want to use only standard NC factories then we'd have to implement our own locking mechanisms in all 
	 * ACS consumer and supplier classes, see http://jira.alma.cl/browse/COMP-2808
	 * 
	 * @return Reference to the newly created channel.
	 * @param channelKind
	 *           Kind of the channel as registered with the CORBA naming service.
	 * @param notifyFactoryName
	 *           Name of the notification service as registered with the CORBA naming service.
	 * @throws AcsJException
	 *            Standard ACS Java exception.
	 * @throws NameAlreadyUsed thrown if the channel of this name already exists.
	 */
	protected EventChannel createNotificationChannel(String channelKind, String notifyFactoryName) 
		throws AcsJException, NameAlreadyUsed
	{
		LOG_NC_ChannelCreated_ATTEMPT.log(m_logger, channelName, notifyFactoryName);
		
		// return value
		EventChannel retValue = null;
		channelId = -1; // to be assigned by factory
		
		StopWatch stopwatch = new StopWatch();
		try {
			
			initializeNotifyFactory(notifyFactoryName);
			
			// create the channel
			// here we use the channel properties taken directly from our channel properties helper object. 
			// presumably these values come from the ACS configuration database.
			IntHolder channelIdHolder = new IntHolder();
			retValue = createNotifyChannel_internal(
					m_channelProperties.configQofS(channelName),
					m_channelProperties.configAdminProps(channelName), 
					channelIdHolder);
			
			// The fact that we got here without exception means that the channel name was not used yet.
			
			// sanity check
			if (retValue == null) {
				// a null reference implies we cannot go any further
				Throwable cause = new Throwable("Null reference obtained for the '" + channelName + "' channel!");
				throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(cause); // TODO: more specific ex type
			}
			channelId = channelIdHolder.value;

			// register our new channel with the naming service
			try {
				NameComponent[] t_NameChannel = { new NameComponent(
						combineChannelAndDomainName(channelName, domainName), channelKind) };
				getNamingService().rebind(t_NameChannel, retValue);

                // Create an entry into the Naming Service to store the timestamp of the channel in order to allow
                // subscribers to reconnect to the channel (ICT-4730)
                int maxNumAttempts = 10;
                int nAttempts = maxNumAttempts;
                boolean timestampCreated = setChannelTimestamp(retValue);
                while(false == timestampCreated && nAttempts > 0) {
					try {
						Thread.sleep(2000);
					} catch (InterruptedException ex1) {
						// too bad
					}
                    nAttempts--;
                    timestampCreated = setChannelTimestamp(retValue);
                }

                if(false == timestampCreated) {
                    Throwable cause = new Throwable("Failed to register the timestamp of the channel '" + channelName 
                            + "' into the Naming Service after " + String.valueOf(maxNumAttempts) + " attempts");
                    throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(cause); // TODO: more specific ex type
                }
			}
			catch (org.omg.CosNaming.NamingContextPackage.NotFound ex) {
				// Corba spec: "If already bound, the previous binding must be of type nobject; 
				//              otherwise, a NotFound exception with a why reason of not_object is raised."
				String reason = "Failed to register the new channel '" + channelName + 
					"' with the Naming Service: " + ex.why.toString();
				AcsJCORBAProblemEx ex2 = new AcsJCORBAProblemEx(ex);
				ex2.setInfo(reason);
				throw ex2;
			}

		} catch (org.omg.CosNaming.NamingContextPackage.CannotProceed e) {
			// Think there is virtually no chance of this every happening but...
			Throwable cause = new Throwable(e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		} catch (org.omg.CosNaming.NamingContextPackage.InvalidName e) {
			// Think there is virtually no chance of this every happening but...
			Throwable cause = new Throwable(e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		} catch (org.omg.CosNotification.UnsupportedQoS e) {
			Throwable cause = new Throwable("The quality of service properties specified for the '" + channelName
					+ "' channel are unsupported: " + e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		}
		
		LOG_NC_ChannelCreated_OK.log(m_logger, channelName, channelId, notifyFactoryName, stopwatch.getLapTimeMillis());
		
		return retValue;
	}

    /**
     *
     */
    protected boolean setChannelTimestamp(EventChannel eventChannel) {
        Date timestamp = new Date();
        DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd_HH:mm:ss");
        String id = combineChannelAndDomainName(channelName, domainName) + "-" + dateFormat.format(timestamp);
        String kind = NC_KIND_NCSUPPORT.value; 

        try {
			NameComponent[] t_NameChannel = { new NameComponent(id, kind) };
			getNamingService().rebind(t_NameChannel, eventChannel);
            channelTimestamp = timestamp;
            return true;
		} catch (org.omg.CosNaming.NamingContextPackage.NotFound e) {
            m_logger.log(AcsLogLevel.ERROR, "Rebinding channel '" + channelName + "' in order to store the timestamp threw a NotFound exception", e);
	    } catch (org.omg.CosNaming.NamingContextPackage.CannotProceed e) {
            m_logger.log(AcsLogLevel.ERROR, "Rebinding channel '" + channelName + "' in order to store the timestamp threw a CannotProceed exception", e);
        } catch (org.omg.CosNaming.NamingContextPackage.InvalidName e) {
            m_logger.log(AcsLogLevel.ERROR, "Rebinding channel '" + channelName + "' in order to store the timestamp threw a InvalidName exception", e);
        }
        return false;
    }

    /**
     * Returns the current channel's timestamp registered in the Naming Service.
     */
    public Date getChannelTimestamp() {
        Date timestamp = new Date(0);

        BindingListHolder bl = new BindingListHolder();
        BindingIteratorHolder bi = new BindingIteratorHolder();

        String chNameAndDomain = combineChannelAndDomainName(channelName, domainName);

        try {
            // Get names of all objects bound in the naming service
            getNamingService().list(-1, bl, bi);

            // Extract the useful binding information Id and Kind
            for (Binding binding : bl.value) {
                if(binding.binding_name[0].kind.equals(NC_KIND_NCSUPPORT.value)) { 
                    if(binding.binding_name[0].id.startsWith(chNameAndDomain)) {
                        String sts = binding.binding_name[0].id.substring(chNameAndDomain.length() + 1); 
                        DateFormat df = new SimpleDateFormat("yyyy-MM-dd_HH:mm:ss");
                        try {
                            timestamp = df.parse(sts);
                        } catch(ParseException e) {
                        }
                    }
                }
            }
        } catch(Exception e) {
        }

        return timestamp;
    }

    /**
     * Initialize the channel timestamp attribute by retrieving its value from the Naming Service. If after
     * 10 attempts the timestamp cannot be retrieved then it's set to the current time.
     */
    public void initChannelTimestamp() {
        int nAttempts = 10;
        channelTimestamp = getChannelTimestamp();
        while(channelTimestamp.getTime() == 0 && nAttempts > 0) {
            channelTimestamp = getChannelTimestamp();
            nAttempts--;
            try {
                Thread.sleep(2000);
            } catch(InterruptedException ex1) {}
        }

        if(channelTimestamp.getTime() == 0) {
            channelTimestamp = new Date();
            DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd_HH:mm:ss");
            m_logger.log(AcsLogLevel.WARNING, "Timestamp of NC '" + channelName + "' couldn't be retrieved from the Naming Service. Initialized to: " 
                    + dateFormat.format(channelTimestamp));
        }
    }

    /**
     * Returns the last timestamp retrieved from the Naming Service
     */
    public Date getLastRegisteredChannelTimestamp() {
        return channelTimestamp;
    }

    /**
     * Set the last channel timestamp
     */
    public void setLastRegisteredChannelTimestamp(Date timestamp) {
        channelTimestamp = timestamp;
    }

	/**
	 * Broken out from {@link #createNotificationChannel(String, String, String)}
	 * to give tests better control about the timing when this call to the event factory is made.
	 * @throws NameAlreadyUsed if the call to NotifyFactory#create_named_channel fails with this exception.
	 * @throws AcsJCORBAProblemEx if the TAO extension throws a NameMapError or if the QoS attributes cause a UnsupportedAdmin.
	 */
	protected EventChannel createNotifyChannel_internal(Property[] initial_qos, Property[] initial_admin, IntHolder channelIdHolder) 
		throws NameAlreadyUsed, UnsupportedQoS, AcsJNarrowFailedEx, AcsJCORBAProblemEx {
		
		EventChannel ret = null;
		
		StopWatch stopwatch = new StopWatch();
		
		try {
			// The TAO extension of the notify factory that we use declares only the plain EventChannel type, 
			// even though it creates the TAO-extension subtype.
			org.omg.CosNotifyChannelAdmin.EventChannel eventChannelBaseType = notifyFactory.create_named_channel(
					initial_qos,
					initial_admin, 
					channelIdHolder, 
					channelName);
			
			LOG_NC_ChannelCreatedRaw_OK.log(m_logger, channelName, channelIdHolder.value, stopwatch.getLapTimeMillis());
		
			// re-create the client side corba stub, to get the extension subtype
			ret = gov.sandia.NotifyMonitoringExt.EventChannelHelper.narrow(eventChannelBaseType);
		} catch (BAD_PARAM ex) {
			LOG_NC_TaoExtensionsSubtypeMissing.log(m_logger, channelName, EventChannel.class.getName(), org.omg.CosNotifyChannelAdmin.EventChannelHelper.id());
			AcsJNarrowFailedEx ex2 = new AcsJNarrowFailedEx(ex);
			ex2.setNarrowType(EventChannelHelper.id());
			throw ex2;
		} catch (NameMapError ex) {
			String msg = "Got a TAO extension-specific NameMapError exception that means the TAO NC extension is not usable. Bailing out since we need the extension.";
			m_logger.log(AcsLogLevel.ERROR, msg, ex);
			AcsJCORBAProblemEx ex2 = new AcsJCORBAProblemEx(ex);
			ex2.setInfo(msg);
			throw ex2;
		} catch (UnsupportedAdmin ex) {
			AcsJCORBAProblemEx ex2 = new AcsJCORBAProblemEx(ex);
			ex2.setInfo(createUnsupportedAdminLogMessage(ex));
			throw ex2;
		}
		return ret;
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
	 *           reference to the channel being destroyed. 
	 *           Here we use the plain OMG type instead of the TAO extension subtype, because the extensions are 
	 *           not used for channel destruction.
	 * @throws AcsJException
	 *            Thrown when the channel isn't registered with the Naming
	 *            Service.
	 * @warning this method assumes
	 */
	protected void destroyNotificationChannel(String channelKind, org.omg.CosNotifyChannelAdmin.EventChannel channelRef)
			throws AcsJException 
	{
		try {
			// destroy the remote CORBA object
			channelRef.destroy();

			// unregister our channel with the naming service
			NameComponent[] t_NameChannel = { new NameComponent(
					combineChannelAndDomainName(channelName, domainName), channelKind) };
			getNamingService().unbind(t_NameChannel);
		} catch (org.omg.CosNaming.NamingContextPackage.NotFound e) {
			// Think there is virtually no chance of this every happening but...
			String msg = "Cannot unbind the '" + channelName + "' channel from the Naming Service!";
			m_logger.severe(msg);
		} catch (org.omg.CosNaming.NamingContextPackage.CannotProceed e) {
			// Think there is virtually no chance of this every happening but...
			Throwable cause = new Throwable(e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		} catch (org.omg.CosNaming.NamingContextPackage.InvalidName e) {
			// Think there is virtually no chance of this every happening but...
			Throwable cause = new Throwable(e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		}
		
		LOG_NC_ChannelDestroyed_OK.log(m_logger, channelName, ""); // TODO use notif.service name 

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

	/**
	 * Gets the notification channel factory name for the given channel/domain of this Helper class.
	 * <p>
	 * Details:
	 * <ul>
	 *   <li>First tries to use the factory name cached from a previous call.
	 *   <li>NC domain ARCHIVING (ArchivingChannel) is mapped to ArchiveNotifyEventChannelFactory.
	 *   <li>NC domain LOGGING (LoggingChannel) is mapped to LoggingNotifyEventChannelFactory.
	 *   <li>If the CDB contains MACI/Channels/NotificationServiceMapping data, we try to find 
	 *       a matching notify service first based on NC name, then on NC domain, then default.
	 *   <li>If no factory has been found, we use NotifyEventChannelFactory.
	 *   <li>Otherwise if the factory name found does not end in NotifyEventChannelFactory, 
	 *       we append NotifyEventChannelFactory. 
	 * </ul>
	 * @return notification channel factory name.
	 */
	public String getNotificationFactoryNameForChannel() 
	{
		// try local cache
		if (ncFactoryName != null) {
			return ncFactoryName;
		}
		
		// factory name, with or without the "NotifyEventChannelFactory" suffix
		String ncFactoryNameTmp = "";
		
		// We use hard-coded mappings for logging and archiving system NCs.
		// As described in ICT-494, these mappings can at the moment not be overridden by CDB mappings.
		// If we want to allow that, we'd have to also read the CDB mappings from the acsstartup scripts 
		// that create these system NCs, or alternatively have these NCs created on demand instead of during 
		// notify service startup. Then we could move the following code after the CDB access, 
		// to be executed only as a fallback if no CDB mapping was found for the system NCs.
		if (domainName != null) {
			if (domainName.equals(ACS_NC_DOMAIN_ARCHIVING.value)) {
				ncFactoryNameTmp = ARCHIVE_NOTIFICATION_FACTORY_NAME.value;
			}
			else if (domainName.equals(ACS_NC_DOMAIN_LOGGING.value)) {
				ncFactoryNameTmp = LOGGING_NOTIFICATION_FACTORY_NAME.value;
			}
		}
		
		// lazy initialization of CDB access
		DAOProxy channelsDAO = null;
		
		if (ncFactoryNameTmp.isEmpty()) {
			try {
				CDBAccess cdbAccess = new CDBAccess(m_services.getAdvancedContainerServices().getORB(), m_logger);
				cdbAccess.setDAL(m_services.getCDB());
				channelsDAO = cdbAccess.createDAO("MACI/Channels");
			} catch (Throwable th) {
				// keep track of when this occurs
				Long timeLastError = channelConfigProblemTimestamp;
				channelConfigProblemTimestamp = System.currentTimeMillis();
				// don't log this too often (currently only once)
				if (timeLastError == 0l) {
					m_logger.log(AcsLogLevel.CONFIG, "Config issue for channel '" + channelName + "'. " + 
							"Failed to get MACI/Channels DAO from CDB. Will use default notification service.");
				}
			}
		}
		
		// query CDB... 
		if (channelsDAO != null) {
			// if channel mapping exists take it, wildchars are also supported
			try {
				// Note that in spite of the NC domain name being appended in the name service mappings,
				// we still configure simple NC names in the CDB.
				String[] channelNameList = channelsDAO.get_string_seq("NotificationServiceMapping/Channels_");
				for (String pattern : channelNameList) {
					String regExpStr = WildcharMatcher.simpleWildcardToRegex(pattern);
					
					if (Pattern.matches(regExpStr, channelName)) {
						ncFactoryNameTmp = channelsDAO.get_string("NotificationServiceMapping/Channels_/" + pattern + "/NotificationService");
						break;
					}
				}
			} catch (Throwable th) {
				m_logger.finer("No Channel to NotificationService mapping found for channel: " + channelName);
			}

			// try domain mapping, if given
			if (ncFactoryNameTmp.isEmpty() && domainName != null)
			{
				try {
					ncFactoryNameTmp = channelsDAO.get_string("NotificationServiceMapping/Domains/" + domainName + "/NotificationService");
					if (m_logger.isLoggable(Level.FINEST)) {
						m_logger.finest("NC '" + channelName + "' of domain '" + domainName + "' mapped to NotificationService '" + ncFactoryNameTmp + "'.");
					}
				} catch (Throwable th) {
					if (m_logger.isLoggable(Level.FINER)) {
						m_logger.finer("No Domain to NotificationService mapping found for domain/channel: " + domainName + "/" + channelName);
					}
				}
			}
	
			// use default from CDB
			if (ncFactoryNameTmp.isEmpty()) {
				try {
					ncFactoryNameTmp = channelsDAO.get_string("NotificationServiceMapping/DefaultNotificationService");
				} catch (Throwable th) {
					m_logger.finer("No NotificationServiceMapping/DefaultNotificationService attribute found, returning hardcoded default.");
				}
			}
		}

		if (!ncFactoryNameTmp.endsWith(alma.acscommon.NOTIFICATION_FACTORY_NAME.value)) {
			// If we found nothing in the CDB, we default to "NotifyEventChannelFactory".
			// Or if the CDB data did not contain the magical service name suffix, we add it here (see http://jira.alma.cl/browse/COMP-9260)
			ncFactoryNameTmp += alma.acscommon.NOTIFICATION_FACTORY_NAME.value;
		}
		
		ncFactoryName = ncFactoryNameTmp;
		return ncFactoryName;
	}

	/**
	 * The following returns a map where each key is the name of an event and the
	 * value is the maximum amount of time (in floating point seconds) an event receiver has 
	 * to process the event before a warning message is logged.
	 * 
	 * @param channelName name of the channel
	 * @return HashMap described above
	 */
	public HashMap<String, Double> getEventHandlerTimeoutMap() {
		
		// initialize the return value
		HashMap<String, Double> retVal = new HashMap<String, Double>();

		// data access object to traverse the ACS CDB
		DAO dao = null;

		// keys into the DAO
		String[] keys = null;

		// get the dao for the channel...
		// ...if this fails, just return.
		try {
			dao = m_services.getCDB().get_DAO_Servant(
					"MACI/Channels/" + channelName);
		} catch (Exception e) {
			m_logger.finer("No CDB entry found for '" + channelName + "' channel");
			return retVal;
		}

		// names of all the events
		try {
			keys = dao.get_string_seq("Events");
		} catch (Exception e) {
			m_logger.finer("CDB entry found for '" + channelName
					+ "' but no Events element.");
			return retVal;
		}

		// sanity check on the number of events
		if (keys.length == 0) {
			m_logger.finer("No event definitions found for the '" + channelName
					+ "' within the CDB.");
			return retVal;
		}

		// populate the hashmap
		for (int i = 0; i < keys.length; i++) {
			// determine the value location
			String timeoutLocation = "Events/" + keys[i] + "/MaxProcessTime";
			// get the value (floating point seconds)
			try {
				double value = dao.get_double(timeoutLocation);
				retVal.put(keys[i], new Double(value));
			} catch (Exception e) {
				e.printStackTrace();
				m_logger
						.severe("Could not convert 'MaxProcessTime' to floating "
								+ "point seconds for the '"
								+ channelName
								+ "' channel and '" + keys[i] + "' event type.");
			}
		}

		return retVal;
	}
	
	public EventChannelFactory getNotifyFactory() {
		return notifyFactory;
	}
	
	/**
	 * Corba spec: If the implementation of the target object is not capable of supporting 
	 * any of the requested administrative property settings, the UnsupportedAdmin exception is raised. 
	 * This exception has associated with it a list of name-value pairs of which each name 
	 * identifies an administrative property whose requested setting could not be satisfied, 
	 * and each associated value the closest setting for that property that could be satisfied.
	 * @param ex
	 * @return a String that contains the information from UnsupportedAdmin
	 */
	public String createUnsupportedAdminLogMessage(UnsupportedAdmin ex) {
		StringBuilder sb = new StringBuilder();
		sb.append("Caught " + ex.getMessage() + ": The administrative properties specified for the '" + channelName 
				+ "' channel are not supported: ");
		if (ex.admin_err != null) {
			for (PropertyError propertyError : ex.admin_err) {
				sb.append("code=" + propertyError.code).append("; ");
				sb.append("name=" + propertyError.name);
				// TODO: Figure out what type (inside the Any) the range values are and add something like the following
				// sb.append("available_low=" + propertyError.available_range.low_val.extract_long());
				// or use alma.acs.nc.AnyAide.#corbaAnyToObject(Any)
			}
		}
		return sb.toString();
	}
	
	/**
	 * Appends a random number to the given client name
	 * and replaces '/' in the name with '_'.
	 * <p>
	 * This is used when setting names on NC proxy objects via the TAO extension API.
	 * It reduces the risk of creating a new object with an existing name,
	 * because TAO has a memory bug and will not delete the badly named object
	 * even if it throws the correct NameAlreadyUsed exception.
	 * Replacing of slashes is done so that clients of the TAO MC extension API
	 * can suppress uninteresting parts of the pathname without mutilating the client name itself, 
	 * see http://ictjira.alma.cl/browse/ICT-3551.
	 * <p>
	 * This method is synchronized just to overcome residual doubts about the thread safety
	 * of random#nextInt.
	 * 
	 * @param clientName
	 * @return "clientName-randomNumber"
	 * @see #random
	 */
	public static synchronized String createRandomizedClientName(String clientName) {
		StringBuffer clientNameSB = new StringBuffer();
		clientNameSB.append(clientName.replace("/", "_"));
		clientNameSB.append('-');
		clientNameSB.append(String.format("%05d", random.nextInt(Integer.MAX_VALUE)));
		return clientNameSB.toString();
	}
	
}
