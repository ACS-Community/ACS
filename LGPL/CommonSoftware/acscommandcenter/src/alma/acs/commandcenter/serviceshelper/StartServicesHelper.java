/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2012
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
package alma.acs.commandcenter.serviceshelper;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.omg.CORBA.StringHolder;

import alma.ACSErr.Completion;
import alma.ACSErrTypeCommon.BadParameterEx;
import alma.ACSErrTypeCommon.NoResourcesEx;
import alma.acs.commandcenter.serviceshelper.TMCDBServicesHelper.AcsServiceToStart;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import alma.acs.util.AcsLocations;
import alma.acsdaemon.DaemonSequenceCallback;
import alma.acsdaemon.DaemonSequenceCallbackHelper;
import alma.acsdaemon.DaemonSequenceCallbackPOA;
import alma.acsdaemon.ServiceDefinitionBuilder;
import alma.acsdaemon.ServicesDaemon;
import alma.acsdaemon.ServicesDaemonHelper;

/**
 * An helper to start the ACS services defined in the TMCDB with the 
 * services daemon.
 * <P>
 * The reference to the services daemon is acquired before starting/stopping services
 * and released when the operation terminates.
 * 
 * @author acaproni
 *
 */
public class StartServicesHelper {

	/**
     * The callback to notify the listeners about the progress of the async start/stop
     * of services.
     * 
     * @author acaproni
     *
     */
    public class DaemonSequenceCallbackImpl extends DaemonSequenceCallbackPOA
    {
    	/**
    	 * The logger
    	 */
        private final AcsLogger logger;
        
        /**
         * <code>true</code> if the callback is used while starting services
         */
        private final boolean startingServices;

        /**
         * C'tor.
         * 
         * @param logger The logger 
         * @param starting <code>true</code> means that the callback is used while starting services
         */
        public DaemonSequenceCallbackImpl(AcsLogger logger, boolean starting) {
        	if (logger==null) {
        		throw new IllegalArgumentException("The logger can't be null");
        	}
            this.logger = logger;
            this.startingServices=starting;
        }

        public void done(Completion comp) {
            logger.log(AcsLogLevel.DEBUG,"Done: comp=" + comp.timeStamp);
            notifyListenersDone(comp.type, comp.code);
        }

        public void working(String service, String host, short instance_number, Completion comp) {
            logger.log(AcsLogLevel.DEBUG,"Working: service=" + service + " host=" + host + " instance=" + instance_number + " comp=" + comp.timeStamp);
            notifyListenersStartStop(startingServices, service, host, instance, comp.type, comp.code);
        }
        
        /**
    	 * Notify all the listeners about the progresses while starting/stopping of services.
    	 * <P> 
    	 * This method is executed by {@link #working(String, String, short, Completion)}
    	 * to notify the listener that a service started or stopped.
    	 *  
    	 * @param starting <code>true</code> means starting of services;
    	 * 				   <code>false</code> means stopping.
    	 * @param serviceName The name of the service. 
    	 * @param hostName The name of the host where the service started.		
    	 * @param instance The number of the ACS instance
    	 * @param errorType The type of error (0 means no error)
    	 * @param errorCode The code of the error
    	 */
    	private void notifyListenersStartStop(
    			final boolean starting, 
    			final String serviceName, 
    			final String hostName,
    			final int instance, 
    			final int errorType, 
    			final int errorCode)  {
    		
    		if (listeners.isEmpty()) {
    			// Nobody to notify!
    			return;
    		}
    		Thread listenersUpdaterThread=contSvcs.getThreadFactory().newThread(new Runnable() {
    			public void run() {
    				synchronized (listeners) {
    					for (ServicesListener listener: listeners) {
   							// The working method of the callback has been invoked
							if (starting) {
								listener.serviceStarted(
										serviceName,
										hostName, 
										instance, 
										errorType,errorCode);
							} else {
								listener.serviceStopped(
										serviceName,
										hostName, 
										instance,
										errorType,										
										errorCode);
							}
    					}
    				}
    			}
    		});
    		listenersUpdaterThread.setName("ListenersUpdaterStartStopThread");
    		listenersUpdaterThread.setDaemon(true);
    		listenersUpdaterThread.start();
    	}
    	
    	/**
    	 * Notify all the listeners that the operation has terminated.
    	 * <P> 
    	 * This method is executed by {@link #done(Completion)}
    	 * to notify the listener that the starting of stopping of the
    	 * services terminated.
    	 *  
    	 * @param errorType The type of error (0 means no error)
    	 * @param errorCode The code of the error
    	 */
    	private void notifyListenersDone(
    			final int errorType, 
    			final int errorCode)  {
    		
    		if (listeners.isEmpty()) {
    			// Nobody to notify!
    			return;
    		}
    		Thread listenersUpdaterThread=contSvcs.getThreadFactory().newThread(new Runnable() {
    			public void run() {
    				synchronized (listeners) {
    					for (ServicesListener listener: listeners) {
   							listener.done(errorType, errorCode);
    					}
    				}
    			}
    		});
    		listenersUpdaterThread.setName("ListenersUpdaterDoneThread");
    		listenersUpdaterThread.setDaemon(true);
    		listenersUpdaterThread.start();
    	}
    }
    
    /**
     * An object to hold the definitins of the services to start.
     * 
     * @author acaproni
     *
     */
    public class AlarmServicesDefinitionHolder {
    	/**
    	 * The XML describing the services to start/stop
    	 */
    	public final String xmlServicesDefinition;
    	
    	/**
    	 * The immutable list of services to start as it has been retrieved
    	 * from the TMCDB
    	 */
    	public final List<AcsServiceToStart> services;
    	
    	/**
    	 * Constructor 
    	 * 
    	 * @param xml The xml describing the services to start/stop
    	 * @param services The list of services to start read from the TMCDB
    	 */
    	public AlarmServicesDefinitionHolder(String xml, List<AcsServiceToStart> services) {
    		xmlServicesDefinition=xml;
    		this.services=services;
    	}
    }

	/**
	 * The listener of events generated starting and stopping 
	 * the services.
	 * 
	 * @author acaproni
	 *
	 */
	public interface ServicesListener {
		
		/**
		 * Invoked when start/stop of services terminated.
		 * 
		 * @param errorType The type of error (0 means no error)
		 * @param errorCode The code of the error
		 */
		public void done(int errorType, int errorCode);
		
		/**
		 * Invoked when the service with the passed name has been
		 * successfully started.
		 * 
		 * @param name The name of the service
		 * @param host The name of the host where the services has been started/stopped
		 * @param instance The number of the ACS instance
		 * @param errorType The type of error (0 means no error)
		 * @param errorCode The code of the error
		 */
		public void serviceStarted(String name, String host, int instance, int errorType, int errorCode);
		
		/**
		 * Invoked when the service with the passed name has been
		 * successfully stopped.
		 * 
		 * @param name The name of the service
		 * @param host The name of the host where the services has been started/stopped
		 * @param instance The number of the ACS instance
		 * @param errorType The type of error (0 means no error)
		 * @param errorCode The code of the error
		 */
		public void serviceStopped(String name, String host, int instance, int errorType, int errorCode);
	}
	
	/**
	 * The exception returned when an error happens getting the services daemon.
	 * 
	 * @author acaproni
	 *
	 */
	public class GettingDaemonException extends Exception {
		
		/**
		 * C'tor
		 * 
		 * @see Exception#Exception(String)
		 */
		public GettingDaemonException(String errorStr) {
			super(errorStr);
		}
		
		/**
		 * C'tor
		 * 
		 * @see Exception#Exception(String, Throwable)
		 */
		public GettingDaemonException(String message, Throwable cause) {
	        super(message, cause);
	    }
	}
	
	/**
	 * Encapsulate the exception returned by the daemon
	 * 
	 * 
	 * @author acaproni
	 *
	 */
	public class DaemonException extends Exception {
		
		/**
		 * C'tor
		 * 
		 * @see Exception#Exception(String)
		 */
		public DaemonException(String errorStr) {
			super(errorStr);
		}
		
		/**
		 * C'tor
		 * 
		 * @see Exception#Exception(String, Throwable)
		 */
		public DaemonException(String message, Throwable cause) {
	        super(message, cause);
	    }
	}
	
	/**
	 * The exception in case of error from the TMCDB
	 * 
	 * 
	 * @author acaproni
	 *
	 */
	public class TMCDBException extends Exception {
		
		/**
		 * C'tor
		 * 
		 * @see Exception#Exception(String)
		 */
		public TMCDBException(String errorStr) {
			super(errorStr);
		}
		
		/**
		 * C'tor
		 * 
		 * @see Exception#Exception(String, Throwable)
		 */
		public TMCDBException(String message, Throwable cause) {
	        super(message, cause);
	    }
	}
	
	/**
	 * The listener of events
	 */
	private final Set<ServicesListener> listeners = 
			Collections.synchronizedSet(new HashSet<ServicesListener>()); 
	
	/**
	 * The session to access the TMCDB
	 */
	private final Session session;
	
	/**
	 * The name of the configuration to get the list of services from
	 * the tmcdb
	 */
	private final String configurationName;
	
	/**
	 * The ContainerServices
	 */
	private final ContainerServices contSvcs;
	
	/**
	 * The host where the service daemon runs
	 */
	private final String daemonHost;
	
	/**
	 * The ACS instance
	 */
	private final int instance;
	
	/** 
	 * Constructor.
	 * 
	 * @param Session The hibernate session to read data from the TMCDB 
	 * @param configurationName The name of the configuration 
	 * @param contSvcs The {@link ContainerServices}
	 * @param daemonHost The host where the service daemon runs
	 * @param instance The ACS instance
	 */ 
	public StartServicesHelper(
			Session session, 
			String configurationName, 
			ContainerServices contSvcs,
			String daemonHost,
			int instance) {
		if (session==null) {
			throw new IllegalArgumentException("The name of the configuration can't be null nor empty!");
		}
		this.session=session;
		if (configurationName==null || configurationName.isEmpty()) {
			throw new IllegalArgumentException("The Session can't be NULL nor empty!");
		}
		this.configurationName=configurationName;
		if (contSvcs==null) {
			throw new IllegalArgumentException("The ContainerServices can't be null!");
		}
		this.contSvcs=contSvcs;
		if (daemonHost==null || daemonHost.isEmpty()) {
			throw new IllegalArgumentException("The host address of the service daemon can't be NULL nor empty!");
		}
		this.daemonHost=daemonHost;
		this.instance=instance;
	}

	/** 
	 * Reads from the TMCDB the list of ACS services to start. 
	 * The definition of the services and the host where we want to deploy them 
	 * is sent to an acs daemon in order to build an XML representation 
	 * that can be later used by the daemons to start all the services. 
	 * <P>
	 * Using this method is optional and expected only for special cases (in conjunction with {@link #startACSServices}), 
	 * because method {@link #startACSServices} includes this step.
	 * <P>
	 * This method delegates to {@link #internalGetServicesDescritpion(ServicesDaemon, ServiceDefinitionBuilder)}
	 * @return A struct with the XML representation of the services to start and the services to start read from the TMCDB
	 * @throws GettingDaemonException In case of error getting the daemon
	 * @throws DaemonException In case of error from the daemon
	 * @throws TMCDBException If the list of service read from the TMCDB is empty
	 * 
	 * @see #internalGetServicesDescritpion(ServicesDaemon, ServiceDefinitionBuilder)
	 */ 
	public AlarmServicesDefinitionHolder getServicesDescription() 
			throws GettingDaemonException, DaemonException, TMCDBException {
		// Get the reference to the daemon
		ServicesDaemon daemon = getServicesDaemon();
		
		return internalGetServicesDescription(daemon);
	}
	
	/**
	 * Return the the XML string describing the services to start by reading the services from the
	 * TMCDB.
	 *  
	 * @param daemon That services daemon
	 * @return The XML and the list of services describing the services to start
	 * @throws HibernateException In case of error reading the services from the TMCDB
	 * @throws DaemonException In case of error from the daemon
	 * @throws TMCDBException If the list of service read from the TMCDB is empty
	 */
	private AlarmServicesDefinitionHolder internalGetServicesDescription (
			ServicesDaemon daemon) 
					throws HibernateException, DaemonException, TMCDBException {
		if (daemon==null) {
			throw new IllegalArgumentException("The daemon can't be null");
		}
		// Get the service definition builder for the current instance
		ServiceDefinitionBuilder srvDefBuilder=null;
		try {
			srvDefBuilder=daemon.create_service_definition_builder((short)instance);
		} catch (Throwable t) {
			throw new DaemonException("Error getting the service definition builder", t);
		}
		// Get the services from the TMCDB
		List<AcsServiceToStart> services = getServicesList();
		if (services.isEmpty()) {
			throw new TMCDBException("No services to start from TMCDB");
		}
		// Add the services to the service definition builder
		try { 
			for (AcsServiceToStart svc: services) {
				switch (svc.serviceType) {
				case MANAGER: {
					srvDefBuilder.add_manager(svc.hostName, "", false);
					break;
				}
				case ALARM: {
					srvDefBuilder.add_alarm_service(svc.hostName);
					break;
				}
				case CDB: {
					srvDefBuilder.add_xml_cdb(svc.hostName, false, "");
					break;
				}
				case IFR: {
					srvDefBuilder.add_interface_repository(svc.hostName, true, false);
					break;
				}
				case LOGGING: {
					srvDefBuilder.add_logging_service(svc.hostName, svc.serviceName);
					break;
				}
				case LOGPROXY: {
					srvDefBuilder.add_acs_log(svc.hostName);
					break;
				}
				case NAMING: {
					srvDefBuilder.add_naming_service(svc.hostName);
					break;
				}
				case NOTIFICATION: {
					srvDefBuilder.add_notification_service(svc.serviceName, svc.hostName);
					break;
				}
				default: {
					throw new Exception("Unknown type of service to start: "+svc.serviceType+", on "+svc.hostName);
				}
				}
			}
		} catch (Throwable t) {
			throw new DaemonException("Error adding services to the daemon", t);
		}
		String svcsXML = srvDefBuilder.get_services_definition();
		StringHolder errorStr = new StringHolder();
		if (!srvDefBuilder.is_valid(errorStr)) {
			// Error in the XML
			throw new DaemonException("Error in the services definition: "+errorStr.value);
		}
		return new AlarmServicesDefinitionHolder(svcsXML, Collections.unmodifiableList(services));
	}

	/** 
	 * Starts the services whose XML definition is in the parameter. 
	 * <P> 
	 * The real starting of services is delegated to {@link #internalStartServices(ServicesDaemon, ServiceDefinitionBuilder, String)}
	 * 
	 * The starting of the services is delegated to a acs service daemon. 
	 * @param xmlListOfServices The XML describing the list of services to start. 
	 * It is generally returned by getServicesDescription() 
	 * 
	 * @throws GettingDaemonException in case of error getting the services daemon
	 * @throws DaemonException In case of error from the daemon
	 */ 
	public void startACSServices(String xmlListOfServices) 
			throws GettingDaemonException, DaemonException {
		if (xmlListOfServices==null || xmlListOfServices.isEmpty()) {
			throw new IllegalArgumentException("The XML list of services can't be null nor empty");
		}
		// Get the reference to the daemon
		ServicesDaemon daemon = getServicesDaemon();
		// Get the service definition builder for the current instance
		ServiceDefinitionBuilder srvDefBuilder=null;
		try {
			srvDefBuilder=daemon.create_service_definition_builder((short)instance);
		} catch (Throwable t) {
			throw new DaemonException("Error getting the service definition builder", t);
		}
		try {
			srvDefBuilder.add_services_definition(xmlListOfServices);
		} catch (Throwable t) {
			throw new DaemonException("Error adding the list of services to the daemon", t);
		}
		StringHolder errorStr = new StringHolder();
		if (!srvDefBuilder.is_valid(errorStr)) {
			// Error in the XML
			throw new DaemonException("Invalid XML list of services: "+errorStr.value);
		}
		internalStartServices(daemon, xmlListOfServices);
	}

	/** 
	 * Starts the services as they are defined in the TMCDB. 
	 * This method is a convenience that before first gets the list of services from the TMCDB 
	 * by calling getServicesDescription() and then executes startACSServices(String xmlListOfServices)
	 * <P> 
	 * The real starting of services is delegated to {@link #internalStartServices(ServicesDaemon, ServiceDefinitionBuilder, String)}
	 * 
	 * @return A struct with the An XML representation of the started services, to be used in {@link #stopServices}
	 * 			and a list of services as they have been read from the TMCDB
	 * 
	 * @throws GettingDaemonException in case of error getting the services daemon
	 * @throws HibernateException In case of error reading the services from the TMCDB
	 * @throws DaemonExceptionIn case of error from the daemon
	 * @throws TMCDBException If the list of services read from the TMCDB is empty
	 * @see AlarmServicesDefinitionHolder 
	 */
	public AlarmServicesDefinitionHolder startACSServices() throws 
	GettingDaemonException, HibernateException, DaemonException, TMCDBException {
		// Get the reference to the daemon
		ServicesDaemon daemon = getServicesDaemon();
		// Get the services from the TMCDB
		AlarmServicesDefinitionHolder holder=internalGetServicesDescription(daemon);
		// Start the services
		internalStartServices(daemon, holder.xmlServicesDefinition);
		return holder;
	}
	
	/**
	 * Asks the daemon to start the services whose description is in the passed XML.
	 * <P>
	 * This method assumes that the XML has already been validated 
	 * by {@link ServiceDefinitionBuilder#is_valid(StringHolder)}
	 * 
	 * @param daemon The daemon to start the services
	 * @param builder The builder to validate the XML;
	 *                 If <code>null</code> an instance is retrieved from the daemon.
	 * @param svcsXML The XML string describing the services to start
	 * 
	 * @throws DaemonException In case of a bad parameter in the method to start the services
	 * 						   or instantiating the callback
	 */
	private void internalStartServices(
			ServicesDaemon daemon, 
			String svcsXML) throws DaemonException {
		if (daemon==null) {
			throw new IllegalArgumentException("The daemon can't be null");
		}
		if (svcsXML==null || svcsXML.isEmpty()) {
			throw new IllegalArgumentException("The XML list of services can't be null nor empty");
		}
		 DaemonSequenceCallbackImpl callback = new DaemonSequenceCallbackImpl(contSvcs.getLogger(),true);
         DaemonSequenceCallback daemonSequenceCallback = null;
        		 
         try {
        	 daemonSequenceCallback = DaemonSequenceCallbackHelper.narrow(contSvcs.activateOffShoot(callback));
         } catch (Throwable t) {
  			throw new DaemonException("Error starting the callback", t);
  		}
         try {
        	 daemon.start_services(svcsXML, true, daemonSequenceCallback);
         } catch (Throwable t) {
 			throw new DaemonException("Error starting the services", t);
 		}
	}

	/** 
	 * Stops the services whose definition is in the passed parameter. 
	 * 
	 * @param xmlListOfServices The XML describing the list of services to stop.
	 * @throws GettingDaemonException In case of error getting the services daemon 
	 * @throws DaemonException In case of error from the services daemon
	 */ 
	public void stopServices(String xmlListOfServices) 
	throws GettingDaemonException, DaemonException {
		if (xmlListOfServices==null || xmlListOfServices.isEmpty()) {
			throw new IllegalArgumentException("The XML list of services can't be null nor empty");
		}
		// Get the reference to the daemon
		ServicesDaemon daemon = getServicesDaemon();
		
		DaemonSequenceCallbackImpl callback = new DaemonSequenceCallbackImpl(contSvcs.getLogger(),false);
        DaemonSequenceCallback daemonSequenceCallback = null;
        try {
        	daemonSequenceCallback = DaemonSequenceCallbackHelper.narrow(contSvcs.activateOffShoot(callback));
        } catch (Throwable t) {
        	throw new DaemonException("Error instantiating the callback", t);
        }
        try {
        	daemon.stop_services(xmlListOfServices, daemonSequenceCallback);
        } catch (Throwable t) {
        	throw new DaemonException("Error stopping services", t);
        }
	}
	
	/**
	 * Add a listener to be notified about the progress of the start/stop of services.
	 *  
	 * @param listener The not <code>null</code> listener to add.
	 */
	public void addServiceListener(ServicesListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		listeners.add(listener);
	}
	

	/**
	 * Remove the passed listener of the start/stop of services.
	 *  
	 * @param listener The not <code>null</code> listener to remove.
	 * @return <code>true</code> if this set contained the specified element
	 */
	public boolean  removeServiceListener(ServicesListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		return listeners.remove(listener);
	}
	
	/**
	 * Get the list of services to start from the TMCDB by delegating to {@link TMCDBServicesHelper}
	 * @return The list of services to start.
	 * @throws Exception In case of error getting the list of services from the TMCDB
	 */
	private List<AcsServiceToStart> getServicesList() throws HibernateException {
		TMCDBServicesHelper tmcdbHelper = new TMCDBServicesHelper(contSvcs.getLogger(), session);
		return tmcdbHelper.getServicesList(configurationName);
	}
	
	/**
	 * Connects to the services daemon building the corba loc from its
	 * host address in ({@link #daemonHost}.
	 * 
	 * @return the reference to the services daemon
	 * 
	 * @throws GettingDaemonException in case of error getting the services daemon
	 */
	private ServicesDaemon getServicesDaemon() throws GettingDaemonException {
		ServicesDaemon daemon;
		String daemonLoc = AcsLocations.convertToServicesDaemonLocation(daemonHost);
		try {
			org.omg.CORBA.Object object = 
					contSvcs.getAdvancedContainerServices().getORB().string_to_object(daemonLoc);
			daemon = ServicesDaemonHelper.narrow(object);
			if (daemon == null)
				throw new GettingDaemonException("Received null trying to retrieve acs services daemon on "+daemonHost);
			if (daemon._non_existent()) // this may be superfluous with daemons but shouldn't hurt either
				throw new GettingDaemonException("Acs services daemon not existing on "+daemonHost);
		} catch (Throwable t) {
			throw new GettingDaemonException("Error getting the services daemon "+t.getMessage(), t);
		}
		return daemon;
	}
}
