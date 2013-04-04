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
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.NotFound;
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
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.ClientLogManager;
import alma.acs.nc.ArchiveConsumer;
import alma.acs.nc.Helper;
import alma.acs.util.AcsLocations;
import alma.acs.util.StopWatch;

/**
 * @author jschwarz
 *
 * $Id: EventModel.java,v 1.39 2013/02/22 15:36:44 hsommer Exp $
 */
public class EventModel {
	
	private final static String eventGuiId = "eventGUI";

	/**
	 * Singleton instance, used by GUI layer classes to access the model.
	 */
	private static EventModel modelInstance;
	
	private final List<NotifyServiceData> notifyServices;
	private AdvancedComponentClient acc;
	private final ORB orb;
	private final Logger m_logger;
	private final ContainerServices cs;
	private final DynAnyFactory dynAnyFactory;
	private final NamingContext nctx;
	
	public static final int MAX_NUMBER_OF_CHANNELS = 100;
	
	private final ArrayBlockingQueue<EventData> equeue = new ArrayBlockingQueue<EventData>(50000);
	private final ArrayBlockingQueue<ArchiveEventData> archQueue = new ArrayBlockingQueue<ArchiveEventData>(100000);

	/**
	 * maps each event channel name to the event channel
	 */
	private final HashMap<String, EventChannel> channelMap; 
	
	/**
	 * key = name of NotifyService; value=int[] {consumerCount, supplierCount}
	 * <p>
	 * TODO: The handling for consumers seems to be based on the number of admin objects, 
	 *       which is not always the same as the number of Consumers (new NCSubscriber differs!) 
	 */
	private final HashMap<String, int[]> lastConsumerAndSupplierCount;
	
	private HashSet<String> subscribedChannels; // all channels whose events the user wishes to monitor/display
	
	private final ArrayList<AdminConsumer> readyConsumers;
	
	/**
	 * Consumers used by the eventGUI to subscribe to events on various NCs.
	 */
	private final HashMap<String, AdminConsumer> consumerMap;
	
	private ArchiveConsumer archiveConsumer;
	
	
	/**
	 * TODO: Remove singleton pattern and either register domain objects in a high (shared) node of IEclipseContexts,
	 * to be created by the Eclipse DI container, 
	 * or turn the entire domain layer into an OSGI service (which is again a singleton...).
	 * See also http://www.eclipse.org/forums/index.php/t/333467/,
	 * http://blog.maxant.co.uk/pebble/2011/08/04/1312486560000.html
	 * <p>
	 * TODO: move the detection of NotifyService out of the ctor, to make the application start faster, 
	 * but also to react to new or disappeared notifyservices during the application lifetime.
	 * Detecting all services, NCs, and reading out the participant information should probably become a 
	 * job (http://www.vogella.com/articles/EclipseJobs/article.html) triggered by the respective views.
	 * <p>
	 * We declare <code>Throwable</code> so that this constructor can catch, print and re-throw even the nastiest errors,
	 * which in case of VerifyError etc are otherwise not well shown by the Eclipse container.
	 */
	private EventModel() throws Throwable {
		
		try {
			notifyServices = Collections.synchronizedList(new ArrayList<NotifyServiceData>()); 
			channelMap = new HashMap<String, EventChannel>();
			lastConsumerAndSupplierCount = new HashMap<String, int[]>();
			consumerMap = new HashMap<String, AdminConsumer>();
			readyConsumers = new ArrayList<AdminConsumer>();
			subscribedChannels = new HashSet<String>();
	
			m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(eventGuiId, false);
			ClientLogManager.getAcsLogManager().suppressRemoteLogging();

			setupAcsConnections();

			cs = acc.getContainerServices();
			orb = acc.getAcsCorba().getORB();
			
			dynAnyFactory = DynAnyFactoryHelper.narrow(orb.resolve_initial_references("DynAnyFactory"));
			
			Helper notifyHelper = new Helper(cs); // todo get NS from the manager instead of this Helper
			nctx = notifyHelper.getNamingService();
			
			getAllNotifyServices();
			
		} catch (Throwable thr) {
			thr.printStackTrace();
			throw thr;
		}
		m_logger.info("*** EventModel() ctor finished ***");
	}

	/**
	 * Broken out from c'tor.
	 * @throws Exception
	 */
	private void setupAcsConnections() throws Exception {
		
		String managerLoc = AcsLocations.figureOutManagerLocation();

		acc = new AdvancedComponentClient(m_logger, managerLoc, eventGuiId) {
			@Override
			protected void initAlarmSystem() {
				m_logger.info("The eventGUI suppresses initialization of the alarm system libraries, to cut the unnecessary dependency on CERN AS jar files.");
			}
			@Override 
			protected void tearDownAlarmSystem() {
				// nothing. Overloaded to avoid "java.lang.IllegalStateException: Trying close with null ContainerServicesBase"
			}
		};
	}

	/**
	 */
	private NotificationServiceMonitorControl getMonitorControl(String notifyBindingName) throws CannotProceed, org.omg.CosNaming.NamingContextPackage.InvalidName, NotFound  {

		NotificationServiceMonitorControl nsmc;
		NameComponent[] ncomp = new NameComponent[1];

		String name;
		if (notifyBindingName == "") 
			name = "MC_NotifyEventChannelFactory"; // default notify service isn't named, but MC is!
		else
			name = "MC_"+notifyBindingName;
		ncomp[0] = new NameComponent(name,"");

		nsmc = NotificationServiceMonitorControlHelper.narrow(nctx.resolve(ncomp));

		return nsmc;
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
	 * Broken out from the c'tor.
	 */
	private void getAllNotifyServices() {
		
		ArrayList<String> efactNames = new ArrayList<String>(10);

		BindingListHolder bl = new BindingListHolder();
		BindingIteratorHolder bi = new BindingIteratorHolder();
		
		// Get names of all objects bound in the naming service
		nctx.list(-1, bl, bi);
		for (Binding binding : bl.value) {
			String id = binding.binding_name[0].id;
			String bindingKind = binding.binding_name[0].kind;
			try {
				// Currently ACS does not use a unique 'kind' value when binding NotifyService instances (see email HSO to ACA 20121126).
				// However, NCs are registered using kind value == alma.acscommon.NC_KIND.value.
				// NotifyServices have an empty kind field, which we can use to skip those objects from the NS that are surely not NotifyServices.
				if (bindingKind != null && bindingKind.isEmpty()) {
					NameComponent nc_array[] = { new NameComponent(id, "") };

					// Get the object reference from the naming service.
					// We skip the manager for access to services, because since ACS 10.2 only specially registered services
					// would be available in the get_service call and we want more flexibility for this special tool.
					org.omg.CORBA.Object obj = nctx.resolve(nc_array);

					if (obj != null && obj._is_a("IDL:omg.org/CosNotifyChannelAdmin/EventChannelFactory:1.0")) {
						String displayName = simplifyNotifyServiceName(id);
						EventChannelFactory efact = EventChannelFactoryHelper.narrow(obj);
						efactNames.add(displayName);
						NotificationServiceMonitorControl nsmc = null;
						try {
							nsmc = getMonitorControl(id);
						} catch (Exception ex) {
							m_logger.log(Level.WARNING, "Failed to obtain the MonitorControl object for service '" + id + "'.", ex);
						}
						notifyServices.add(new NotifyServiceData(displayName, id, efact, nsmc, new int[2], new int[2]));
					}
				}
			} catch (Exception ex) {
				m_logger.log(Level.WARNING, "Failed to check whether service '" + id + "' is a NotifyService.", ex);
			}
		}
		Collections.sort(efactNames);
		m_logger.info("Found " + efactNames.size() + " notify service instances: " + StringUtils.join(efactNames, ' '));
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
	 * @param id
	 * @return
	 */
	private String simplifyNotifyServiceName(String id) {
		String displayName = id.substring(0,id.indexOf("NotifyEventChannelFactory"));
		
		if (displayName.equals("")) {
			displayName = "DefaultNotifyService";
		}
		return displayName;
	}

	/**
	 * TODO: Check if we can eliminate class NotifyServices
	 * and return a list of NotifyServiceData.
	 */
	public NotifyServices getNotifyServiceTotals() {
		return new NotifyServices(notifyServices);
	}

	/**
	 * Counts consumers and suppliers for every NC in every NotifyService,
	 * and stores the results in {@link #lastConsumerAndSupplierCount}.
	 * <p>
	 * TODO: Use TAO extensions so that in error messages we can use the NC name instead of the NC index number.
	 */
	private void calculateNotifyServiceTotals() {

		for (NotifyServiceData nsData : notifyServices) {
			
			EventChannelFactory efact = nsData.getEventChannelFactory();
			String efactName = nsData.getName();
			
			int nconsumers = 0;
			int nsuppliers = 0;
			int[] ncIds = efact.get_all_channels();
			
			for (int ncId : ncIds) {
				try {
					EventChannel nc = efact.get_event_channel(ncId);
					// TODO: These calculations depend on the assumption that 1 consumer admin ==> 1 consumer
					//       This is no longer true with NCSubscriber. Must check number of supplier proxies, 
					//       and deduct the 1 marker/dummy proxy, see module jcontnc 
					nconsumers += nc.get_all_consumeradmins().length;
					nsuppliers += nc.get_all_supplieradmins().length;
				} catch (ChannelNotFound ex) {
					m_logger.log(AcsLogLevel.WARNING, "Failed to get NC '" + ncId + "'.", ex);
				}
			}
			//System.out.printf("%s consumers: %d\n", efactName, nconsumers);
			//System.out.printf("%s suppliers: %d\n", efactName, nsuppliers);
			//System.out.printf("Number of channels for: %s %d \n", efactName, ncIds.length);
			
			nsData.setNumberConsumers(nconsumers);
			nsData.setNumberSuppliers(nsuppliers);
			
			// The 0 values are an initialization, to display the change in numbers of consumers and suppliers
			lastConsumerAndSupplierCount.put(efactName, new int[]{0,0});
		}
	}

	public synchronized ArrayList<ChannelData> getChannelStatistics() {
		
		// TODO: Why call this, when we anyway recount the consumers etc below?
		calculateNotifyServiceTotals();
		
		ArrayList<ChannelData> chdatList = new ArrayList<ChannelData>();
		
		BindingListHolder bl = new BindingListHolder();
		BindingIteratorHolder bi = new BindingIteratorHolder();
		
		// TODO: why call naming service and not get NCs from the known NotifyService instances?
		nctx.list(-1, bl, bi);
		for (Binding binding : bl.value) {
			if (binding.binding_name[0].kind.equals(alma.acscommon.NC_KIND.value)) {
				String channelName = binding.binding_name[0].id;
				EventChannel ec;
				try {
					ec = getNotificationChannel(channelName);

					int[] consAndSupp = {0,0}; // initial or previous count of consumers / suppliers
					if (!channelMap.containsKey(channelName)) {
						// a new NC
						channelMap.put(channelName, ec);
						if (subscribedChannels.contains(channelName)) {// user wants to subscribe
							AdminConsumer consumer = getAdminConsumer(channelName);
							consumerMap.put(channelName, consumer); // user *has* subscribed
						}
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

					// same code for consumer and supplier
					for (int i = 0; i < adminCounts.length; i++) {
						String cstr = channelName;
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
	
					// ?? this we could skip had we gotten the NC from the NotifyService...
					Helper notifyHelper = new Helper(cs, nctx); // helper per NC, to avoid "java.lang.IllegalArgumentException: Helper for NC 'CONTROL_REALTIME' cannot be used also for 'MY_OTHER_NC'."
					String notifyServiceName = simplifyNotifyServiceName(notifyHelper.getNotificationFactoryNameForChannel(channelName));
					
					for (NotifyServiceData nsData : notifyServices) {
						if (nsData.getName().equals(notifyServiceName)) {
							ChannelData cdata = new ChannelData(channelName, nsData, adminCounts, adminDeltas);
							nsData.addChannelAndConfirm(channelName, cdata);
							break;
						}
					}
				} catch (AcsJException e) {
					m_logger.log(AcsLogLevel.SEVERE, "Can't find channel " + channelName, e);
				}
			}
		}
		return chdatList;	
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
	 * COMMENTED OUT: @param notifyFactoryName
	 *           Name of the notification service as registered with the CORBA
	 *           naming service.
	 * @throws AcsJException
	 *            Standard ACS Java exception.
	 */
	protected EventChannel getNotificationChannel(String channelName) 
		throws AcsJException 
	{
		// return value
		EventChannel retValue = null;

		try {
//			m_logger.fine("Will create notification channel " + channelName);
			NameComponent[] t_NameSequence = { new NameComponent(channelName, alma.acscommon.NC_KIND.value) };
			retValue = EventChannelHelper.narrow(nctx.resolve(t_NameSequence));
		} 
		catch (org.omg.CosNaming.NamingContextPackage.NotFound e) {
			// No other suppliers have created the channel yet...create it
			m_logger.info("The " + channelName + " channel has not been created yet.");
//			return createNotificationChannel(channelName, channelKind, notifyFactoryName);
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
	
	public void refreshChannelSubscriptions() {
		HashMap<String, AdminConsumer> consumers = getAllSelectedConsumers();

		if (consumers != null) {
			for (AdminConsumer consumer : consumers.values()) {
				try {
					if (!readyConsumers.contains(consumer)) {
						consumer.startReceivingEvents();
						readyConsumers.add(consumer);
					}
				} catch (AcsJException ex) {
					m_logger.log(Level.WARNING, "Failed to start AdminConsumer ", ex);
				}
			}
		}
	}


	public synchronized AdminConsumer getAdminConsumer(String channelName) throws AcsJException {
		if (!consumerMap.containsKey(channelName)) {
			AdminConsumer adm = new AdminConsumer(channelName, cs, nctx, equeue);
			consumerMap.put(channelName, adm);
			subscribedChannels.add(channelName);
			return adm;
		}
		return consumerMap.get(channelName);
	}

	/**
	 * The intention here is to return an array list of all consumers whose events the user
	 * wants to monitor in the EventListView. "channelMap" indicates what channels are live;
	 * "subscribedChannels" lists all channel that are to be (are being) monitored.
	 * @return all consumers so selected
	 */
	public HashMap<String,AdminConsumer> getAllSelectedConsumers() {
		int channelsProcessed = 0;
		StopWatch sw = new StopWatch(m_logger);
		
		// Get all NCs from the naming service.
		// TODO: Since we already have the references to the NotifyServices, 
		// wouldn't it be easier to get the NC refs from them?
		BindingListHolder bl = new BindingListHolder();
		BindingIteratorHolder bi = new BindingIteratorHolder();
		nctx.list(-1, bl, bi);
		for (Binding binding : bl.value) {
			if (binding.binding_name[0].kind.equals(alma.acscommon.NC_KIND.value)) {
				String channelName = binding.binding_name[0].id;
				//m_logger.info("Found NC: " + channelName);
				try {
					EventChannel ec = getNotificationChannel(channelName);
					if (!channelMap.containsKey(channelName) ) {
						channelMap.put(channelName, ec);
					}
					synchronized (this) {
						if (subscribedChannels.contains(channelName) && !consumerMap.containsKey(channelName)) {
							AdminConsumer consumer = getAdminConsumer(channelName);
							consumerMap.put(channelName, consumer);
							channelsProcessed++;
						}
					}
				} catch (AcsJException e) {
					m_logger.log(AcsLogLevel.SEVERE, "Can't find channel"+channelName, e);
				}
			}
		}
		sw.logLapTime("create " + channelsProcessed + " channels.");
		getArchiveConsumer();
		return consumerMap;
	}
	
	public synchronized void addChannelSubscription(String channelName) {
		subscribedChannels.add(channelName);
	}
	
	public synchronized void subscribeToAllChannels() {
		for (String channelName : channelMap.keySet()) {
			subscribedChannels.add(channelName);
		}
	}

	/**
	 * Creates on demand an ArchiveConsumer and stores its reference in field {@link #archiveConsumer}.
	 */
	private synchronized void getArchiveConsumer() {
		if (archiveConsumer == null) {
				try {
					archiveConsumer = new ArchiveConsumer(new ArchiveReceiver(archQueue), cs, nctx);
					archiveConsumer.startReceivingEvents();
				} catch (AcsJException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
		}
	}
	
	public DynAnyFactory getDynAnyFactory() {
		return dynAnyFactory;
	}
	
	public synchronized void closeSelectedConsumer(String channelName, boolean deselect) {
		if (consumerMap.containsKey(channelName)) {
			AdminConsumer consumer = consumerMap.get(channelName);
			try {
				consumer.disconnect();
			} catch (Exception ex) {
				m_logger.log(Level.WARNING, "Failed to close subscriber: ", ex);
			}
			consumerMap.remove(channelName);
		}
		if (deselect && !subscribedChannels.remove(channelName))
			m_logger.log(Level.WARNING,"Couldn't remove "+channelName+" from list of selected subscribers.");
	}

	public synchronized void closeAllConsumers() {
		for (String channelName : subscribedChannels) {
			closeSelectedConsumer(channelName, false);
		}
		subscribedChannels = new HashSet<String>(MAX_NUMBER_OF_CHANNELS*5); // reset the list
	}
	
	public void closeArchiveConsumer() {
		if (archiveConsumer != null) {
			try {
				archiveConsumer.disconnect();
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
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Note that this method may be called very frequently, to determine whether menu items are selected/enabled, thus it must be fast.
	 */
	public boolean isSubscribed(String channelName) { 
		return subscribedChannels.contains(channelName);
	}
	
}
