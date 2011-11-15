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

import gov.sandia.CosNotification.NotificationServiceMonitorControl;
import gov.sandia.CosNotification.NotificationServiceMonitorControlHelper;
import gov.sandia.CosNotification.NotificationServiceMonitorControlPackage.InvalidName;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;
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

import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.AcsManagerProxy;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.nc.ArchiveConsumer;
import alma.acs.nc.Helper;
import alma.acs.util.StopWatch;
import alma.maciErrType.wrappers.AcsJCannotGetComponentEx;
import alma.maciErrType.wrappers.AcsJComponentConfigurationNotFoundEx;
import alma.maciErrType.wrappers.AcsJComponentNotAlreadyActivatedEx;
import alma.maciErrType.wrappers.AcsJNoPermissionEx;

/**
 * @author jschwarz
 *
 * $Id: EventModel.java,v 1.33 2011/11/15 16:13:55 jschwarz Exp $
 */
public class EventModel {
	private final ORB orb;
	private final Logger m_logger;


	private final AdvancedComponentClient compClient;
	private final ContainerServices cs;
	private final Helper h;
	private final NamingContext nctx;
	private final AcsManagerProxy mproxy;
	private final static String eventGuiId = "eventGUI";
	private HashMap<String, EventChannel> channelMap; // maps each event channel name to the event channel
	private HashMap<String, int[]> lastConsumerAndSupplierCount;
	private HashSet<String> subscribedChannels; // all channels whose events the user wishes to monitor/display
	private static EventModel modelInstance;
	private ArrayList<AdminConsumer> readyConsumers;
	private HashMap<String, AdminConsumer> consumerMap;
	private static DynAnyFactory dynAnyFactory = null;
	public static final int MAX_NUMBER_OF_CHANNELS = 100;
	private NotificationServiceMonitorControl nsmc;
	private ArchiveConsumer archiveConsumer;
	private AdvancedComponentClient acc;
	private ArrayList<EventChannelFactory> efacts;
	private ArrayList<String> efactNames;
	private ArrayList<String> notifyBindingNames;
	private ArrayList<NotifyServiceData> clist;
	private NotifyServices notifyServices;

	private EventModel() throws Exception {
		
		m_logger = setupAcsConnections();
		channelMap = new HashMap<String, EventChannel>(MAX_NUMBER_OF_CHANNELS);
		lastConsumerAndSupplierCount = new HashMap<String, int[]>(MAX_NUMBER_OF_CHANNELS);
		consumerMap = new HashMap<String, AdminConsumer>(MAX_NUMBER_OF_CHANNELS);
		readyConsumers = new ArrayList<AdminConsumer>(MAX_NUMBER_OF_CHANNELS*5);
		subscribedChannels = new HashSet<String>(MAX_NUMBER_OF_CHANNELS*5);
		compClient = acc;
		mproxy = compClient.getAcsManagerProxy();

		cs = compClient.getContainerServices();
		orb = compClient.getAcsCorba().getORB();
		
		dynAnyFactory = DynAnyFactoryHelper.narrow(orb.resolve_initial_references("DynAnyFactory"));
		
		h = new Helper(cs);
		nctx = h.getNamingService();
		getAllNotifyServices();
		clist = new ArrayList<NotifyServiceData>(10);
	}

	/**
	 * @throws Exception
	 */
	private AcsLogger setupAcsConnections() throws Exception {
		String connectionString;
		String managerHost;
		int acsInstance = 0;
		managerHost = "localhost";
		try {
			connectionString = System.getProperty("ACS.Manager",System.getenv("MANAGER_REFERENCE"));
			System.setProperty("ORBInitRef.NameService", System.getenv("ACS_NAME_SERVICE"));
			String temp = connectionString.substring("corbaloc::".length());
			int endIndex = temp.indexOf(":");
			managerHost = temp.substring(0, endIndex);
			acsInstance = (Integer.parseInt(temp.substring(endIndex+1, temp.indexOf("/"))) - 3000)/100;
		} catch (Exception e) {
			if ((connectionString = System.getProperty("ACS.Manager")) == null) { // Joe's Linux box in Garching is the default!
				acsInstance = 0;
				managerHost = "alma";
				connectionString = getConnectionString(managerHost, acsInstance);
			}
		}
		AcsLogger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(eventGuiId, false);
		ClientLogManager.getAcsLogManager().suppressRemoteLogging();

		acc = null;
		try {

			acc = new AdvancedComponentClient(m_logger, connectionString, eventGuiId) {
              @Override
               protected void initAlarmSystem() {
                   m_logger.info("The eventGUI suppresses initialization of the alarm system libraries, to cut the unnecessary dependency on CERN AS jar files.");
               } 
};
		} catch (Exception e) {
			if (PlatformUI.isWorkbenchRunning()) {
				MessageDialog dialog = new MessageDialog(null,"Can't create client",null,"The eventGUI client cannot be created. You might want to check the ACS host and instance.\n"+
						"ACS instance used: "+acsInstance+"; Looked for manager on host: "+managerHost,
						MessageDialog.ERROR, new String[]{"OK"}, 0);
				dialog.open();
				IStatus status = new Status(IStatus.ERROR, eventGuiId,0, "Couldn't create component client", e);
				Platform.getLog(Platform.getBundle(eventGuiId)).log(status);
			}
			else {
				m_logger.log(Level.SEVERE,"Can't create advanced component client.",e);
			}
			throw(e);
		}
		return logger;
	}

	/**
	 * This routine returns an array of NotificationServiceMonitorControl objects (provided by the
	 * ACE/TAO Monitoring extensions) by resolving the IORs for these objects that are stored in
	 * the $ACS_TMP/ACS_INSTANCE.x/iors/ directory. 
	 * @throws org.omg.CosNaming.NamingContextPackage.InvalidName 
	 * @throws CannotProceed
	 * @throws InvalidName
	 */
	private NotificationServiceMonitorControl getMonitorControl(String notifyBindingName) throws CannotProceed, org.omg.CosNaming.NamingContextPackage.InvalidName  {

		NotificationServiceMonitorControl nsmc;
		NameComponent[] ncomp = new NameComponent[1];

		String name = "MC_"+notifyBindingName;
		ncomp[0] = new NameComponent(name,"");

		try {
			nsmc = NotificationServiceMonitorControlHelper.narrow(nctx.resolve(ncomp));
		} catch (NotFound e) {
			m_logger.info("Can't find an Monitor & Control instance corresponding to: "+notifyBindingName);
			nsmc = null;
		} 

		return nsmc;
	}
	
	private String getConnectionString(String managerHost, int acsInstance) {
		String port = Integer.toString(3000+acsInstance*100);
		return "corbaloc::"+managerHost+":"+port+"/Manager";
	}
	
	public synchronized static EventModel getInstance() throws Exception {
		if (modelInstance == null) 
			modelInstance = new EventModel();
		return modelInstance;
	}
	
	public Logger getLogger() {
		return m_logger;
	}
	
	/**
		 * @throws AcsJException
		 * @throws AcsJComponentNotAlreadyActivatedEx
		 * @throws AcsJCannotGetComponentEx
		 * @throws AcsJComponentConfigurationNotFoundEx
		 * @throws AcsJNoPermissionEx
		 */
		private NotifyServices getAllNotifyServices() throws AcsJException,
				AcsJComponentNotAlreadyActivatedEx, AcsJCannotGetComponentEx,
				AcsJComponentConfigurationNotFoundEx, AcsJNoPermissionEx {
			efacts = new ArrayList<EventChannelFactory>(10);
			efactNames = new ArrayList<String>(10);
			notifyBindingNames = new ArrayList<String>(10);
			
			notifyServices = NotifyServices.getInstance();
	
			Helper h = new Helper(cs);
			NamingContext nctx = h.getNamingService();BindingListHolder bl = new BindingListHolder();
			BindingIteratorHolder bi = new BindingIteratorHolder();
			
			nctx.list(-1, bl, bi);
			for (Binding binding : bl.value) {
				String serviceKind = alma.acscommon.NC_KIND.value;
				if (binding.binding_name[0].kind.equals(serviceKind)) continue; // Channels are not services! Let's avoid unnecessary exceptions.
	
				String id = binding.binding_name[0].id;
				//System.out.println("Binding name[0].id: "+binding.binding_name[0].id);
				org.omg.CORBA.Object obj = null;
				try {
					obj = mproxy.get_service(id, false);
				} catch (Exception e) {
					e.printStackTrace();
				}
				if (obj != null && obj._is_a("IDL:omg.org/CosNotifyChannelAdmin/EventChannelFactory:1.0")) {
	//				System.out.println("Binding name[0].id: "+binding.binding_name[0].id);
	//				System.out.println("Binding name[0].kind: "+binding.binding_name[0].kind);
					String displayName = simplifyNotifyServiceName(id);
					EventChannelFactory efact = EventChannelFactoryHelper.narrow(mproxy.get_service(id, false));
					efacts.add(efact);
					efactNames.add(displayName);
					notifyBindingNames.add(id);
					try {
						nsmc = getMonitorControl(id);
					} catch (CannotProceed e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (org.omg.CosNaming.NamingContextPackage.InvalidName e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					notifyServices.addService(new NotifyServiceData(displayName,id, efact, nsmc, new int[2], new int[2]));
				}
			}
	
			m_logger.info("Number of notify service instances found = "+efacts.size());
			return notifyServices;
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

	public ArrayList<NotifyServiceData> getNotifyServiceTotals() throws AcsJException {
		if (clist.isEmpty())
			calculateNotifyServiceTotals();
		return clist;
	}

	/**
	 * 
	 * @throws AcsJException
	 * @throws AcsJComponentNotAlreadyActivatedEx
	 * @throws AcsJCannotGetComponentEx
	 * @throws AcsJComponentConfigurationNotFoundEx
	 * @throws AcsJNoPermissionEx
	 */
	private void calculateNotifyServiceTotals() throws AcsJException,
			AcsJComponentNotAlreadyActivatedEx, AcsJCannotGetComponentEx,
			AcsJComponentConfigurationNotFoundEx, AcsJNoPermissionEx {

		clist = notifyServices.getServices();
		for (int i = 0; i < clist.size(); i++) {
			int nconsumers;
			int nsuppliers;
			int[] chans;
			
			NotifyServiceData nsData = clist.get(i);
			EventChannelFactory efact = nsData.getEventChannelFactory();
			String efactName = nsData.getName();
			chans = efact.get_all_channels();
			nconsumers = 0;
			nsuppliers = 0;
			for (int j = 0; j < chans.length; j++) {
				try {
					nconsumers += efact.get_event_channel(chans[j]) // TODO: These calculations depend on assumption that 1 consumer admin ==> 1 consumer
							.get_all_consumeradmins().length;
					nsuppliers += efact.get_event_channel(chans[j])
							.get_all_supplieradmins().length;
				} catch (ChannelNotFound e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

			}
//			System.out.printf("%s consumers: %d\n", efactName, nconsumers);
//			System.out.printf("%s suppliers: %d\n", efactName, nsuppliers);
//			System.out.printf("Number of channels for: %s %d \n", efactName,
//					chans.length);
			nsData.setNumberConsumers(nconsumers);
			nsData.setNumberSuppliers(nsuppliers);
			lastConsumerAndSupplierCount.put(efactName, new int[]{0,0});
		}
	}

	public ArrayList<ChannelData> getChannelStatistics() {
		
		try {
			calculateNotifyServiceTotals();
		} catch (AcsJException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		ArrayList<ChannelData> chdatList = new ArrayList<ChannelData>();
		BindingListHolder bl = new BindingListHolder();
		BindingIteratorHolder bi = new BindingIteratorHolder();
		final String[] roleNames = {"consumer","supplier"};
		
		nctx.list(-1, bl, bi);
		for (Binding binding : bl.value) {
			String serviceIsChannelKind = alma.acscommon.NC_KIND.value;
			if (binding.binding_name[0].kind.equals(serviceIsChannelKind)) {
				String channelName = binding.binding_name[0].id;
				EventChannel ec;
				try {
					ec = getNotificationChannel(channelName, alma.acscommon.NC_KIND.value);

					AdminConsumer consumer = null;
					int[] consAndSupp = {0,0};
					if (!channelMap.containsKey(channelName)) {
						channelMap.put(channelName, ec);
						if (subscribedChannels.contains(channelName)) {
							consumer = getAdminConsumer(channelName);
							consumerMap.put(channelName, consumer);
						}
						lastConsumerAndSupplierCount.put(channelName,
								consAndSupp);
					} else {
						if (lastConsumerAndSupplierCount.containsKey(channelName))
							consAndSupp = lastConsumerAndSupplierCount.get(channelName);
					}
					int [] adminCounts = new int[2];
					int [] adminDeltas = new int[2];
					adminCounts[0] = ec.get_all_consumeradmins().length;
					adminCounts[1] = ec.get_all_supplieradmins().length;

					for (int i = 0; i < adminCounts.length; i++) {
						String cstr = channelName;
						int cdiff = adminCounts[i] - consAndSupp[i];
						if (cdiff != 0) {
							if (cdiff > 0) {
								cstr += " has added " + cdiff + " "
										+ roleNames[i];
							} else if (cdiff < 0) {
								cstr += " has removed " + (-cdiff) + " "
										+ roleNames[i];
							}
							cstr += (Math.abs(cdiff)!=1 ? "s." : ".");
							m_logger.info(cstr);
						}
						adminDeltas[i] = cdiff;
					}
					lastConsumerAndSupplierCount.put(channelName,adminCounts);
					//m_logger.info("Channel: "+channelName+" has "+adminCounts[0]+" consumers and "+adminCounts[1]+" suppliers.");
					String notifyServiceName = simplifyNotifyServiceName(h.getNotificationFactoryNameForChannel(channelName));
					for (Iterator<NotifyServiceData> iterator = clist.iterator(); iterator
							.hasNext();) {
						NotifyServiceData nsData = (NotifyServiceData) iterator.next();
						if (nsData.getName().equals(notifyServiceName)) {
							ChannelData cdata = new ChannelData(channelName, nsData, adminCounts, adminDeltas);
							nsData.addChannelAndConfirm(channelName, cdata);
							break;
						}						
					}
				} catch (AcsJException e) {
					m_logger.log(AcsLogLevel.SEVERE, "Can't find channel"+channelName, e);
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
	protected EventChannel getNotificationChannel(String channelName, String channelKind) 
		throws AcsJException 
	{
		// return value
		EventChannel retValue = null;

		try {
//			m_logger.fine("Will create notification channel " + channelName);
			NameComponent[] t_NameSequence = { new NameComponent(channelName, channelKind) };
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
	
	public String[] getCorbaServices() {

		return orb.list_initial_services();
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
						consumer.consumerReady();
						readyConsumers.add(consumer);
					}
				} catch (AcsJException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
	}


	public AdminConsumer getAdminConsumer(String channelName) throws AcsJException {
		if (!consumerMap.containsKey(channelName)) {
			AdminConsumer adm = new AdminConsumer(channelName,cs);
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
		BindingListHolder bl = new BindingListHolder();
		BindingIteratorHolder bi = new BindingIteratorHolder();
		nctx.list(-1, bl, bi);
		for (Binding binding : bl.value) {
			String serviceKind = alma.acscommon.NC_KIND.value;
			if (binding.binding_name[0].kind.equals(serviceKind)) {
				String channelName = binding.binding_name[0].id;
				//System.out.println("Channel: "+channelName);
				EventChannel ec;
				try {
					ec = getNotificationChannel(channelName, alma.acscommon.NC_KIND.value);

					AdminConsumer consumer = null;
					if (!channelMap.containsKey(channelName) ) {
						channelMap.put(channelName, ec);
					}
					if (subscribedChannels.contains(channelName) && !consumerMap.containsKey(channelName)) {
							consumer = getAdminConsumer(channelName);
							consumerMap.put(channelName, consumer);
							channelsProcessed++;
					}
				} catch (AcsJException e) {
					m_logger.log(AcsLogLevel.SEVERE, "Can't find channel"+channelName, e);
				}
			}
		}
		sw.logLapTime(" create "+channelsProcessed+" channels ");
		getArchiveConsumer();
		return consumerMap;
	}
	
	public void addChannelSubscription(String channelName) {
		subscribedChannels.add(channelName);
	}
	
	public void subscribeToAllChannels() {
		for (String channelName : channelMap.keySet()) {
			subscribedChannels.add(channelName);
		}
	}

	public void getArchiveConsumer() {
		synchronized (this ){
			if (archiveConsumer == null)
				try {
					archiveConsumer = new ArchiveConsumer(cs,new ArchiveReceiver());
					archiveConsumer.consumerReady();
				} catch (AcsJException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
		}
	}
	
	public static DynAnyFactory getDynAnyFactory() {
		return dynAnyFactory;
	}
	
	public synchronized void closeSelectedConsumer(String channelName, boolean deselect) {
		if (consumerMap.containsKey(channelName)) {
			AdminConsumer consumer = consumerMap.get(channelName);
			consumer.disconnect();
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
			archiveConsumer.disconnect();
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

	public boolean isSubscribed(String channelName) { // TODO: Is this going to work for purposes of disabling the "Subscribe to channel" option?
		return subscribedChannels.contains(channelName);
	}
	
}
