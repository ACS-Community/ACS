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
package alma.acs.eventbrowser.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang.StringUtils;
import org.omg.CORBA.ORB;
import org.omg.CosNaming.Binding;
import org.omg.CosNaming.BindingIteratorHolder;
import org.omg.CosNaming.BindingListHolder;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextHelper;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.NotFound;
import org.omg.CosNotifyChannelAdmin.AdminNotFound;
import org.omg.CosNotifyChannelAdmin.ChannelNotFound;
import org.omg.CosNotifyChannelAdmin.EventChannel;
import org.omg.CosNotifyChannelAdmin.EventChannelFactory;
import org.omg.CosNotifyChannelAdmin.EventChannelFactoryHelper;
import org.omg.CosNotifyChannelAdmin.EventChannelHelper;
import org.omg.DynamicAny.DynAnyFactory;
import org.omg.DynamicAny.DynAnyFactoryHelper;

import gov.sandia.CosNotification.NotificationServiceMonitorControl;
import gov.sandia.CosNotification.NotificationServiceMonitorControlHelper;

import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.ClientLogManager;
import alma.acs.nc.ArchiveConsumer;
import alma.acs.nc.Helper;
import alma.acs.util.AcsLocations;

/**
 * @author jschwarz, hsommer
 *
 * $Id: EventModel.java,v 1.39 2013/02/22 15:36:44 hsommer Exp $
 */
public class EventModel {
	
	private final static String eventGuiId = "eventGUI";

	/**
	 * Singleton instance, used by GUI layer classes to access the model.
	 */
	private static EventModel modelInstance;
	
	/**
	 * Key = service ID (e.g. "AlarmNotifyEventChannelFactory") <br>
	 * Value = NotifyServiceData object
	 * <p>
	 * This is the root of our data model. From here we can go to NCs and their attributes.
	 */
	private final Map<String, NotifyServiceData> notifyServices;
	
	private AdvancedComponentClient acc;
	private final ORB orb;
	private final Logger m_logger;
	private final ContainerServices cs;
	private final DynAnyFactory dynAnyFactory;
	private final NamingContext nctx;
	
	private final ArrayBlockingQueue<EventData> equeue = new ArrayBlockingQueue<EventData>(50000);
	private final ArrayBlockingQueue<ArchiveEventData> archQueue = new ArrayBlockingQueue<ArchiveEventData>(100000);

	/**
	 * key = name of NotifyService or NC; value=int[] {consumerCount, supplierCount}
	 * <p>
	 * TODO: The handling for consumers seems to be based on the number of admin objects, 
	 *       which is not always the same as the number of Consumers (new NCSubscriber differs!) 
	 */
	private final HashMap<String, int[]> lastConsumerAndSupplierCount;
	
	/**
	 * Consumers used by the eventGUI to subscribe to events on various NCs.
	 * <p>
	 * Todo: we could keep the consumer reference in the matching ChannelData object
	 * and eliminate this map.
	 */
	private final HashMap<String, AdminConsumer> consumerMap;
	
	/**
	 * Subscribes to monitoring/archiving events.
	 */
	private ArchiveConsumer archiveConsumer;
	
	
	/**
	 * TODO: Remove singleton pattern and either register domain objects in a high (shared) node of IEclipseContexts,
	 * to be created by the Eclipse DI container, 
	 * or turn the entire domain layer into an OSGI service (which is again a singleton...).
	 * See also http://www.eclipse.org/forums/index.php/t/333467/,
	 * http://blog.maxant.co.uk/pebble/2011/08/04/1312486560000.html
	 * <p>
	 * TODO: move the detection of NotifyService out of the ctor, to make the application start faster.
	 * This would require a first-time service-detection-independently-of-NCs mechanism. 
	 * <p>
	 * We declare <code>Throwable</code> so that this constructor can catch, print and re-throw even the nastiest errors,
	 * which in case of VerifyError etc are otherwise not well shown by the Eclipse container.
	 */
	private EventModel() throws Throwable {
		
		try {
			notifyServices = new HashMap<String, NotifyServiceData>(); 
			lastConsumerAndSupplierCount = new HashMap<String, int[]>();
			consumerMap = new HashMap<String, AdminConsumer>();
	
			m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(eventGuiId, false);
			ClientLogManager.getAcsLogManager().suppressRemoteLogging();

			String managerLoc = AcsLocations.figureOutManagerLocation();

			acc = new AdvancedComponentClient(m_logger, managerLoc, eventGuiId) {
				@Override
				protected void initAlarmSystem() {
					m_logger.fine("The eventGUI suppresses initialization of the alarm system libraries, to cut the unnecessary dependency on CERN AS jar files.");
				}
				@Override 
				protected void tearDownAlarmSystem() {
					// nothing. Overloaded to avoid "java.lang.IllegalStateException: Trying close with null ContainerServicesBase"
				}
			};

			cs = acc.getContainerServices();
			orb = acc.getAcsCorba().getORB();
			
			dynAnyFactory = DynAnyFactoryHelper.narrow(orb.resolve_initial_references("DynAnyFactory"));
			
			nctx = NamingContextHelper.narrow(
					acc.getAcsManagerProxy().get_service("NameService", false)
			);
			
			discoverNotifyServicesAndChannels(true);
			
			// todo: make this on-demand
			getArchiveConsumer();
			
		} catch (Throwable thr) {
			thr.printStackTrace();
			throw thr;
		}
		m_logger.info("*** EventModel() ctor finished ***");
	}


	public synchronized static EventModel getInstance() throws Throwable {
		if (modelInstance == null) 
			modelInstance = new EventModel();
		return modelInstance;
	}
	
	/**
	 * @TODO: Logger should be created separately and get injected
	 */
	public Logger getLogger() {
		return m_logger;
	}
	
	/**
	 * Gets the event queue, for read and write access.
	 */
	public BlockingQueue<EventData> getEventQueue() {
		return equeue;
	}
	
	public BlockingQueue<ArchiveEventData> getArchQueue() {
		return archQueue;
	}
	

	/**
	 * Discovers services and bindings, retrieving only once the list of all
	 * bindings from the naming service.
	 * 
	 * @param discoverServicesIndependentlyOfChannels
	 *            If <code>true</code>, notify services will be searched for as
	 *            an extra step, which will discover them early even when they
	 *            don't host NCs.
	 *            If <code>false</code>, notify services will only be discovered on demand 
	 *            when we find one of their channels.
	 */
	private synchronized void discoverNotifyServicesAndChannels(boolean discoverServicesIndependentlyOfChannels) {
		BindingListHolder bl = new BindingListHolder();
		BindingIteratorHolder bi = new BindingIteratorHolder();
		
		// Get names of all objects bound in the naming service
		nctx.list(-1, bl, bi);
		
		// Extract the useful binding information Id and Kind
		Map<String, String> bindingMap = new HashMap<String, String>();
		for (Binding binding : bl.value) {
			bindingMap.put(binding.binding_name[0].id, binding.binding_name[0].kind);
		}
		
		// notify services
		if (discoverServicesIndependentlyOfChannels) {
			discoverNotifyServices(bindingMap);
		}
		
		// channels (NCs)
		discoverChannels(bindingMap);
	}

	/**
	 * Checks the naming service for notify service instances and stores / updates them in {@link #notifyServices}.
	 * This update does not include the NCs owned by the notify services, so that {@link #discoverChannels(Binding[])} 
	 * should be called after this. 
	 * <p>
	 * Currently this discovery is "expensive" because a lot of other corba objects found in the naming service must be queried 
	 * to check if they are notify service instances. See code comment about dedicated 'kind' marker.
	 * <p>
	 * This method is broken out from {@link #discoverNotifyServicesAndChannels(boolean)} to make it more readable. 
	 * It should be called only from there, to keep services and NCs aligned.
	 * 
	 * @param bindingMap Name service bindings in the form key = bindingName, value = bindingKind
	 */
	private synchronized void discoverNotifyServices(Map<String, String> bindingMap) {
		
		ArrayList<String> newNotifyServiceNames = new ArrayList<String>(10); // used for local logging only
		Set<String> oldServiceIds = new HashSet<String>(notifyServices.keySet()); // used to detect services that have disappeared
		
		for (String bindingName : bindingMap.keySet()) {
			String bindingKind = bindingMap.get(bindingName);
			try {
				// Currently ACS does not use a unique 'kind' value when binding NotifyService instances (see email HSO to ACA 20121126).
				// Adding this would save us unnecessary "_is_a" calls when discovering notify service instances.
				// The fact that NotifyServices have an empty kind field we can use to skip those objects from the NS that are surely not NotifyServices.
				if (bindingKind.isEmpty()) {
					NameComponent nc_array[] = { new NameComponent(bindingName, "") };

					// Get the object reference from the naming service.
					// We skip the manager for access to services, because since ACS 10.2 only specially registered services
					// would be available in the get_service call and we want more flexibility for this special tool.
					org.omg.CORBA.Object obj = nctx.resolve(nc_array);

					if (obj != null && obj._is_a("IDL:omg.org/CosNotifyChannelAdmin/EventChannelFactory:1.0")) {
						// our current naming service entry maps indeed to a notify service
						oldServiceIds.remove(bindingName);
						if (!notifyServices.containsKey(bindingName)) {
							EventChannelFactory efact = EventChannelFactoryHelper.narrow(obj);
							String displayName = simplifyNotifyServiceName(bindingName);
							newNotifyServiceNames.add(displayName);
							NotificationServiceMonitorControl nsmc = null;
							try {
								nsmc = getMonitorControl(bindingName);
							} catch (Exception ex) {
								m_logger.log(Level.WARNING, "Failed to obtain the MonitorControl object for service '" + bindingName + "'.", ex);
							}
							NotifyServiceData notifyServiceData = new NotifyServiceData(displayName, bindingName, efact, nsmc);
							notifyServices.put(bindingName, notifyServiceData);
						}
						else {
							// Todo: Should we always update the map, in case the corba objects have been relocated with a change of reference?
						}
					}
				}
			} catch (Exception ex) {
				m_logger.log(Level.WARNING, "Failed to check whether service '" + bindingName + "' is a NotifyService.", ex);
			}
		}
		if (!newNotifyServiceNames.isEmpty()) {
			Collections.sort(newNotifyServiceNames);
			m_logger.info("Found " + newNotifyServiceNames.size() + " notify service instances: " + StringUtils.join(newNotifyServiceNames, ' '));
		}
		if (!oldServiceIds.isEmpty()) {
			for (String oldServiceId : oldServiceIds) {
				notifyServices.remove(oldServiceId);
			}
			m_logger.info("Removed " + oldServiceIds.size() + " notify service instances: " + StringUtils.join(oldServiceIds, ' '));
		}
	}
	
	
	/**
	 * Checks the naming service for NC instances and stores / updates them under the matching service from {@link #notifyServices}.
	 * It does not query the NCs for consumers etc. though.
	 * <p>
	 * This method is broken out from {@link #discoverNotifyServicesAndChannels(boolean)} to make it more readable. 
	 * It should be called only from there, to keep services and NCs aligned.
	 */
	private synchronized void discoverChannels(Map<String, String> bindingMap) {
		
		// known NC names, used to detect NCs that have disappeared. 
		Set<String> oldNcNames = new HashSet<String>();
		for (NotifyServiceData nsData : notifyServices.values()) {
			for (ChannelData channelData : nsData.getChannels()) {
				oldNcNames.add(channelData.getQualifiedName());
			}
		}

		// Note that it is useless to retrieve the NC from the notify service (get_event_channel(ncId) etc), 
		// because the limited notification service API will not tell us the channel name
		// even if we used the TAO extensions (where the NC names show up in the statistics but are not accessible through the API).
		// However, we can get the NC names from the MC object, to be integrated with the other MCStatistics retrieval.
		// Then we still need to obtain the NC corba reference separately in order to know about admin object details, 
		// with the NC ref coming either from the naming service or perhaps from the notify service directly if we can match ncIDs with the MC data.
		for (String bindingName : bindingMap.keySet()) {
			String bindingKind = bindingMap.get(bindingName);
			if (bindingKind.equals(alma.acscommon.NC_KIND.value)) {
				String channelName = bindingName;
				try {
					// Check if we already know this NC. 
					// The channelName must be unique across notify services (required for naming service registration), 
					// so that we can simply search our maps.
					ChannelData channelData = getNotifyServicesRoot().findChannel(channelName);
					if (channelData != null) {
						oldNcNames.remove(channelData.getQualifiedName());
						channelData.setIsNewNc(false);
					}
					else {
//						System.out.println("*** New NC " + channelName);
						// A new NC. This will happen at startup, and later as NCs get added.
						// Currently the NC-to-service mapping is based on conventions and CDB data, using the Helper class from jcontnc.
						Helper notifyHelper = new Helper(cs, nctx);
						String serviceId = notifyHelper.getNotificationFactoryNameForChannel(channelName);
						NotifyServiceData service = notifyServices.get(serviceId);
						if (service == null) {
							// If we do not auto-discover services as part of refreshing NCs, then a new NC hosted in a new service
							// brings us here. We try to find the new service.
							discoverNotifyServices(bindingMap);
							service = notifyServices.get(serviceId);
							String msg = "Unknown notify service '" + simplifyNotifyServiceName(serviceId) + "' required for NC '" + channelName + "'. ";
							if (service != null) {
								m_logger.info(msg + "Added the new service.");
							}
							else {
								m_logger.warning(msg + "Failed to add the new service.");
							}
						}
						if (service != null) {
							EventChannel nc = resolveNotificationChannel(channelName);
							ChannelData cdata = new ChannelData(nc, channelName, service);
							cdata.setIsNewNc(true);
							service.addChannel(channelName, cdata);
						}
					}
				}
				catch (Exception ex) {
					m_logger.log(Level.WARNING, "Failed to map NC '" + channelName + "' to its notify service.", ex);
				}
			}
		}
		
		
		// In addition to checking the name service for NCs, we also check the services and their MC objects. 
		// Some system NCs are currently not getting registered in the naming service, and we want to display them too.
		for (NotifyServiceData service : notifyServices.values()) {

			// First set the missing ncId on new ChannelData objects
			if (!service.getNewChannels().isEmpty()) { // check first if we can avoid the get_all_channels() remote call
				int[] ncIds = service.getEventChannelFactory().get_all_channels();
				for (int ncId : ncIds) {
					if (service.getChannelById(ncId) == null) {
						// our ncId is new... try to find matching known new NC
						EventChannel newNc = null;
						try {
							newNc = service.getEventChannelFactory().get_event_channel(ncId);
						} catch (ChannelNotFound ex) {
							ex.printStackTrace(); // should never happen since we just got this Id from the service
						}
						ChannelData matchedNc = service.getChannelByCorbaRef(newNc);
						if (matchedNc != null) {
							if (matchedNc.isNewNc()) {
								matchedNc.setNcId(ncId);
								m_logger.fine("Service " + service.getName() + ": Matched ncId=" + ncId + " to new NC '" + matchedNc.getName() + "'.");
							}
							else {
								m_logger.warning("Service " + service.getName() + ": Matched ncId=" + ncId + " to NC '" + matchedNc.getName() + "', but expect this NC to be new, which it isn't.");
							}
						}
						else {
							// ncId is the Id of a new NC that was not registered in the naming service (unless the corberef-based match failed).
							// We don't create a ChannelData object yet, because we may get its real name below from the MC.
						}
					}
				}
			}
			
			// Then check the service's MC object for new (hopefully named) NCs.
			MCProxy mcProxy = new MCProxy(service, m_logger);
			List<String> ncNamesFromMc = mcProxy.listChannels();
			for (String ncNameFromMc : ncNamesFromMc) {
				if (service.getChannelByName(ncNameFromMc) == null) {
					// The NC was not listed in the naming service under the name we got from MC. Possibilities are:
					// (a) It is not registered in the naming service at all;
					// (b) It is properly registered in the naming service with its human-readable name, but it was created without using the TAO extensions
					//     and therefore we get only its integer ID from the MC API. 
					try {
						// Try if we have an ID instead of a name 
						int ncId = Integer.parseInt(ncNameFromMc); // see NumberFormatException catch below
						service.getEventChannelFactory().get_event_channel(ncId); // see ChannelNotFound catch below
						// The NC was created without TAO extension name. 
						// Check if we know this NC already from the naming service or from a previous round here.
						ChannelData channelData = service.getChannelById(ncId);
						if (channelData == null) {
							// First time we see this NC. It is not in the naming service and there is nowhere we can get a decent name for it.
							// We simply use the Id as its name.
							// If this is a problem, then this NC should be created with TAO extension name, or should be registered in the NS.
							m_logger.info("Service " + service.getName() + ": Found unnamed NC with ID='" + ncId + "' that is not registered in the naming service."); 
							String newNcName = Integer.toString(ncId);
							EventChannel unnamedNcCorbaRef = null;
							try {
								unnamedNcCorbaRef = service.getEventChannelFactory().get_event_channel(ncId);
							} catch (ChannelNotFound ex) {
								ex.printStackTrace(); // should not happen since we just got the ncId from the service
							}
							ChannelData newNcData = new ChannelData(unnamedNcCorbaRef, Integer.toString(ncId), service);
							newNcData.markUnsubscribable();
							newNcData.setIsNewNc(true);
							newNcData.setNcId(ncId);
							service.addChannel(newNcName, newNcData);
						}
					} catch (NumberFormatException ex) {
						// The name was not an integer, and therefore also for sure not an ID.
						// This means the NC was created using the TAO extensions, and yet it was not registered in the naming service.
						m_logger.info("Service " + service.getName() + ": Found NC with name='" + ncNameFromMc + "' that is not registered in the naming service. This case was thought to not exist in practice and is therefore not supported yet by the eventGUI. Please inform ACS.");
					} catch (ChannelNotFound ex) {
						m_logger.warning("Strange NC '" + service.getName() + "/" + ncNameFromMc + 
								"' reported by MC is not listed in the naming service and has an integer name that is not a channel ID.");
					}
				}
				else if (!bindingMap.containsKey(ncNameFromMc)) {
					// The NC is not in the naming service, but was added based on MC data in a previous round.
					ChannelData nc = service.getChannelByName(ncNameFromMc);
					nc.setIsNewNc(false);
					oldNcNames.remove(nc.getQualifiedName());
				}
			}

			//			for (String statName : nsData.getMc().get_statistic_names()) {
//				System.out.println(statName);
//			}
			
		}
		
		if (!oldNcNames.isEmpty()) {
			// TODO: Change this when the eventGUI or other tools offers manual NC destruction
			// Actually it could also be that an additional NotifyService with unused NCs was stopped, but in practice this does not happen.
			m_logger.warning("Lost " + oldNcNames.size() + " NCs, which should not happen as we never destroy an NC even if it no longer gets used: " + StringUtils.join(oldNcNames, ' '));
			
			for (String oldNcQualifiedName : oldNcNames) {
				String oldNcName = oldNcQualifiedName.substring(oldNcQualifiedName.indexOf('#')+1);
				NotifyServiceData oldNcService = getNotifyServicesRoot().findHostingService(oldNcName);
				ChannelData oldNcData = oldNcService.getChannelByName(oldNcName);
				closeSelectedConsumer(oldNcData);
				oldNcService.removeChannel(oldNcName);
			}
		}
	}
	

	/**
	 * Resolves the TAO monitor-control object that corresponds to the given notify service name.
	 */
	private NotificationServiceMonitorControl getMonitorControl(String notifyBindingName) 
			throws CannotProceed, org.omg.CosNaming.NamingContextPackage.InvalidName, NotFound {

		String name = "MC_" + notifyBindingName;
		
		NameComponent[] ncomp = new NameComponent[1];
		ncomp[0] = new NameComponent(name, "");

		NotificationServiceMonitorControl nsmc = NotificationServiceMonitorControlHelper.narrow(nctx.resolve(ncomp));

		return nsmc;
	}
	
	
	/**
	 * Simplifies the notify service ID by cutting off the trailing "NotifyEventChannelFactory".
	 * For the default NC service, whose ID is only this suffix, it returns "DefaultNotifyService".
	 * @param id
	 * @return "Logging", "Alarm", "DefaultNotifyService", "MyRealtimeNotifyService", ...
	 */
	private String simplifyNotifyServiceName(String id) {
		String displayName = id.substring(0, id.indexOf("NotifyEventChannelFactory"));
		
		if (displayName.equals("")) {
			displayName = "DefaultNotifyService";
		}
		return displayName;
	}


	public NotifyServices getNotifyServicesRoot() {
		return new NotifyServices(notifyServices);
	}



	/**
	 * Called by NotifyServiceUpdateJob (single/periodic refresh of service summary / channel tree).
	 */
	public synchronized void getChannelStatistics() {
		
		// TODO:  Make service-discovery-independently-of-channels configurable (preferences etc),
		// to not ignore user-defined services that start late until they host NCs.
		discoverNotifyServicesAndChannels(false);
		
		for (NotifyServiceData nsData : notifyServices.values()) {
			
			// iterate over NCs
			// TODO: Rely only on MC data and skip these 'get_all_consumeradmins' calls.
			for (ChannelData channelData : nsData.getChannels()) {
				String channelName = channelData.getName();
				EventChannel ec = channelData.getCorbaRef();
				int[] consAndSupp = {0,0}; // initial or previous count of consumers / suppliers
				if (channelData.isNewNc()) {
					lastConsumerAndSupplierCount.put(channelName, consAndSupp);
				} 
				else if (lastConsumerAndSupplierCount.containsKey(channelName)) {
					consAndSupp = lastConsumerAndSupplierCount.get(channelName);
				}
				
				final String[] roleNames = {"consumer", "supplier"};
				int [] adminCounts = new int[2];
				int [] adminDeltas = new int[2];
				adminCounts[0] = ec.get_all_consumeradmins().length;
				adminCounts[1] = ec.get_all_supplieradmins().length;

//				for (int consumerAdminId : ec.get_all_consumeradmins()) {
//					try {
//						ec.get_consumeradmin(consumerAdminId);
//					} catch (AdminNotFound ex) {
//						ex.printStackTrace();
//					}
//				}
				
				// same code for consumer and supplier
				for (int i = 0; i < adminCounts.length; i++) {
					String cstr = channelData.getQualifiedName();
					int cdiff = adminCounts[i] - consAndSupp[i];
					if (cdiff != 0) {
						if (cdiff > 0) {
							cstr += " has added " + cdiff + " " + roleNames[i];
						} 
						else if (cdiff < 0) {
							cstr += " has removed " + (-cdiff) + " " + roleNames[i];
						}
						cstr += (Math.abs(cdiff)!=1 ? "s." : ".");
						m_logger.info(cstr);
					}
					adminDeltas[i] = cdiff;
				}
				lastConsumerAndSupplierCount.put(channelName, adminCounts);
				//m_logger.info("Channel: " + channelName + " has " + adminCounts[0] + " consumers and " + adminCounts[1] + " suppliers.");

				channelData.setNumberConsumers(adminCounts[0]);
				channelData.setNumberSuppliers(adminCounts[1]);
				channelData.setDeltaConsumers(adminDeltas[0]);
				channelData.setDeltaSuppliers(adminDeltas[1]);
			}
		}
	}
	
	
	/**
	 * Resolves a notification channel in the naming service.
	 * 
	 * @return Reference to the event channel specified by channelName.
	 * @param channelName
	 *           Name of the event channel registered with the CORBA Naming Service
	 * @throws AcsJException
	 *            Standard ACS Java exception.
	 */
	protected EventChannel resolveNotificationChannel(String channelName) throws AcsJException {

		EventChannel retValue = null;

		try {
//			m_logger.fine("Will create notification channel " + channelName);
			NameComponent[] t_NameSequence = { new NameComponent(channelName, alma.acscommon.NC_KIND.value) };
			retValue = EventChannelHelper.narrow(nctx.resolve(t_NameSequence));
		} 
		catch (org.omg.CosNaming.NamingContextPackage.NotFound e) {
			// No other suppliers have created the channel yet...create it
			m_logger.info("The " + channelName + " channel has not been created yet.");
			throw new AcsJUnexpectedExceptionEx(e);
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
	
	
	public ContainerServices getContainerServices() {
		return cs;
	}
	

	private AdminConsumer getAdminConsumer(String channelName) throws AcsJException {
		synchronized (consumerMap) {
			if (!consumerMap.containsKey(channelName)) {
				AdminConsumer adm = new AdminConsumer(channelName, cs, nctx, equeue);
				consumerMap.put(channelName, adm);
				return adm;
			}
			return consumerMap.get(channelName);
		}
	}

	
	/**
	 * Called from SubscribeNCHandler. 
	 * @throws AcsJException 
	 */
	public void addChannelSubscription(ChannelData channelData ) throws AcsJException {
		String channelName = channelData.getName();
		
		AdminConsumer consumer = null;
		synchronized (consumerMap) {
			// The subscription flag and the existence of a consumer for that NC must be in sync
			if (channelData.isSubscribed() != consumerMap.containsKey(channelName)) {
				m_logger.warning("Inconsistent state between channel flag and consumer map. Will not subscribe to " + channelName);
				return;
			}
			else if (channelData.isSubscribed()) {
				m_logger.warning("Already subscribed to " + channelName + ". Ignoring subscription request.");
				return;
			}
			
			channelData.setSubscribed(true);
			consumer = getAdminConsumer(channelName);
		}
		consumer.startReceivingEvents();
	}
	

	/**
	 * TODO: Call this from mouse menu of Archiving NC instead of in the beginning.
	 * (It used to be called once a minute from a thread of the event list / archiving list parts.)
	 * 
	 * Creates on demand an ArchiveConsumer and stores its reference in field {@link #archiveConsumer}.
	 */
	private synchronized void getArchiveConsumer() {
		if (archiveConsumer == null) {
			try {
				archiveConsumer = new ArchiveConsumer(new ArchiveReceiver(archQueue), cs, nctx);
				archiveConsumer.startReceivingEvents();
				m_logger.info("Subscribed to monitoring/archiving events.");
			} catch (AcsJException ex) {
				m_logger.log(Level.WARNING, "Failed to subcribe to monitoring/archiving events.", ex);
			}
		}
	}
	
	public DynAnyFactory getDynAnyFactory() {
		return dynAnyFactory;
	}
	
	public void closeSelectedConsumer(ChannelData channelData) {
		String channelName = channelData.getName();
		
		synchronized (consumerMap) {
			if (consumerMap.containsKey(channelName)) {
				AdminConsumer consumer = consumerMap.get(channelName);
				try {
					consumer.disconnect();
				} catch (Exception ex) {
					m_logger.log(Level.WARNING, "Failed to close subscriber: ", ex);
				}
				consumerMap.remove(channelName);
			}
		}
		channelData.setSubscribed(false);
	}

	public void closeAllConsumers() {
		
		synchronized (consumerMap) {
			for (ChannelData channelData : getNotifyServicesRoot().getAllChannels()) {
				if (channelData.isSubscribed()) {
					closeSelectedConsumer(channelData);
				}
			}
		}
	}
	
	/**
	 * TODO: Call from mouse menu handler for the archiving NC
	 */
	public synchronized void closeArchiveConsumer() {
		if (archiveConsumer != null) {
			try {
				archiveConsumer.disconnect();
				m_logger.info("Closed subscriber for baci monitoring events");
			} catch (Exception ex) {
				m_logger.log(Level.WARNING, "Got an exception disconnecting the archive consumer.", ex);
			}
			archiveConsumer = null;
		}
	}

	public void tearDown() {
		try {
			closeAllConsumers();
			closeArchiveConsumer();
			acc.tearDown();
		} catch (Exception ex) {
			m_logger.log(Level.WARNING, "Error in EventModel#tearDown: ", ex);
		}
	}

	/**
	 * Note that this method may be called very frequently, to determine whether menu items are selected/enabled, thus it must be fast.
	 */
	public boolean isSubscribed(ChannelData channelData) {
		return channelData.isSubscribed();
	}
	
}
