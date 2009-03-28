package alma.acs.eventbrowser.model;

import gov.sandia.CosNotification.NotificationServiceMonitorControl;
import gov.sandia.CosNotification.NotificationServiceMonitorControlHelper;
import gov.sandia.CosNotification.NotificationServiceMonitorControlPackage.InvalidName;

import java.util.ArrayList;
import java.util.HashMap;
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
import org.omg.CosNotification.Property;
import org.omg.CosNotifyChannelAdmin.AdminNotFound;
import org.omg.CosNotifyChannelAdmin.ChannelNotFound;
import org.omg.CosNotifyChannelAdmin.ConsumerAdmin;
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
import alma.acs.logging.ClientLogManager;
import alma.acs.nc.Helper;

public class EventModel {
	private final ORB orb;
	private final Logger m_logger;


	private final AdvancedComponentClient compClient;
	private final ContainerServices cs;
	private final Helper h;
	private final NamingContext nctx;
	private final AcsManagerProxy mproxy;
	private final EventChannelFactory nsvc;
	private final EventChannelFactory lsvc;	// For logging service
	private final static String eventGuiId = "eventGUI";
	private final EventChannelFactory alsvc;
	private final EventChannelFactory arsvc;
	private HashMap<String, EventChannel> channelMap; // maps each event channel name to the event channel
	private static EventModel modelInstance;
	private ArrayList<AdminConsumer> consumers;
	private ArrayList<AdminConsumer> readyConsumers;
	private HashMap<String, AdminConsumer> consumerMap;
	private static DynAnyFactory dynAnyFactory = null;
	public static final int MAX_NUMBER_OF_CHANNELS = 100;
	private NotificationServiceMonitorControl nsmc;

	private EventModel() throws Exception {
		String connectionString;
		String managerHost;
		
		int acsInstance;
		if ((connectionString = System.getProperty("ACS.Manager")) == null) { // Joe's Linux box in Garching is the default!
			acsInstance = 0;
			managerHost = "pc013018";
			connectionString = getConnectionString(managerHost, acsInstance);
		}
		else {
			String temp = connectionString.substring("corbaloc::".length());
			int endIndex = temp.indexOf(":");
			managerHost = temp.substring(0, endIndex);
			acsInstance = (Integer.parseInt(temp.substring(endIndex+1, temp.indexOf("/"))) - 3000)/100;
		}
		m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(eventGuiId, false);
		ClientLogManager.getAcsLogManager().suppressRemoteLogging();
		AdvancedComponentClient acc = null;
		try {
			acc = new AdvancedComponentClient(m_logger, connectionString, eventGuiId);
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
				System.out.println("Can't create advanced component client.");
				e.printStackTrace();
			}
			throw(e);
		}
		channelMap = new HashMap<String, EventChannel>(MAX_NUMBER_OF_CHANNELS);
		consumerMap = new HashMap<String, AdminConsumer>(MAX_NUMBER_OF_CHANNELS);
		consumers = new ArrayList<AdminConsumer>(MAX_NUMBER_OF_CHANNELS*5); // a guess at the possible limit to the number of consumers
		readyConsumers = new ArrayList<AdminConsumer>(MAX_NUMBER_OF_CHANNELS*5);
		compClient = acc;
		mproxy = compClient.getAcsManagerProxy();

		nsvc = EventChannelFactoryHelper.narrow(mproxy.get_service(alma.acscommon.NOTIFICATION_FACTORY_NAME.value, true));
		lsvc = EventChannelFactoryHelper.narrow(mproxy.get_service(alma.acscommon.LOGGING_NOTIFICATION_FACTORY_NAME.value, true));
		alsvc = EventChannelFactoryHelper.narrow(mproxy.get_service("AlarmNotifyEventChannelFactory", true));
		arsvc = EventChannelFactoryHelper.narrow(mproxy.get_service(alma.acscommon.ARCHIVE_NOTIFICATION_FACTORY_NAME.value, true));
		cs = compClient.getContainerServices();
		orb = compClient.getAcsCorba().getORB();
		
		dynAnyFactory = DynAnyFactoryHelper.narrow(orb.resolve_initial_references("DynAnyFactory"));
		
		h = new Helper(cs);
		nctx = h.getNamingService();
		nsmc = NotificationServiceMonitorControlHelper.narrow(nctx.resolve(new NameComponent[]{new NameComponent("TAO_MonitorAndControl","")}));
		getServiceTotals(); // temporarily, for testing
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
	
	public ArrayList<ChannelData> getServiceTotals() {
		ArrayList<ChannelData> clist = new ArrayList<ChannelData>();
		final EventChannelFactory[] efacts = {nsvc, lsvc, alsvc, arsvc};
		final String[] efactNames = {"Notification", "Logging", "Alarm", "Property Archiving"};

		int nconsumers;
		int nsuppliers;
		for (int i = 0; i < efacts.length; i++) {
			int[] chans;
			chans = efacts[i].get_all_channels();
			nconsumers = 0;
			nsuppliers = 0;
			for (int j = 0; j < chans.length; j++) {
				try {
					nconsumers += efacts[i].get_event_channel(chans[j]) // TODO: These calculations depend on assumption that 1 consumer admin ==> 1 consumer
							.get_all_consumeradmins().length;
					nsuppliers += efacts[i].get_event_channel(chans[j])
							.get_all_supplieradmins().length;
				} catch (ChannelNotFound e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

			}
//			System.out.printf("%s consumers: %d\n", efactNames[i], nconsumers);
//			System.out.printf("%s suppliers: %d\n", efactNames[i], nsuppliers);
//			System.out.printf("Number of channels for: %s %d \n", efactNames[i],
//					chans.length);
			clist.add(new ChannelData(efactNames[i], nconsumers, nsuppliers));
		}

		return clist;
	}
	
	public ArrayList<ChannelData> getChannelStatistics() {
		ArrayList<ChannelData> clist = new ArrayList<ChannelData>();
		BindingListHolder bl = new BindingListHolder();
		BindingIteratorHolder bi = new BindingIteratorHolder();
		nctx.list(-1, bl, bi);
		for (Binding binding : bl.value) {
			String serviceKind = alma.acscommon.NC_KIND.value;
			if (binding.binding_name[0].kind.equals(serviceKind)) {
				String channelName = binding.binding_name[0].id;
				System.out.println("Channel: "+binding.binding_name[0].id);
				EventChannel ec;
				try {
					ec = getNotificationChannel(channelName, alma.acscommon.NC_KIND.value);

					AdminConsumer consumer = null;
					if (!channelMap.containsKey(channelName)) {
						channelMap.put(channelName, ec);
						consumer = getAdminConsumer(channelName);
						consumers.add(consumer);
					}
					int[] admins = ec.get_all_consumeradmins();
					int numConsumers = admins.length;
					int numSuppliers = admins.length;
					System.out.println("... has "+numConsumers+" consumers and "+numSuppliers+" suppliers.");
					clist.add(new ChannelData(channelName, numConsumers, numSuppliers));
				} catch (AcsJException e) {
					m_logger.severe("Can't find channel "+channelName);
					e.printStackTrace();
				}

			}
			printMonitoringResults(nsmc);
		}
		return clist;	
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
	
	public String[] getServices() {

		return orb.list_initial_services();
	}
	
	public ContainerServices getContainerServices() {
		return cs;
	}
	
	public void refreshChannelSubscriptions() {
		consumers = getAllConsumers();

		if (consumers != null) {
			for (AdminConsumer consumer : consumers) {
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
			return adm;
		}
		return consumerMap.get(channelName);
	}

	public ArrayList<AdminConsumer> getAllConsumers() {
		BindingListHolder bl = new BindingListHolder();
		BindingIteratorHolder bi = new BindingIteratorHolder();
		nctx.list(-1, bl, bi);
		for (Binding binding : bl.value) {
			String serviceKind = alma.acscommon.NC_KIND.value;
			if (binding.binding_name[0].kind.equals(serviceKind)) {
				String channelName = binding.binding_name[0].id;
				System.out.println("Channel: "+binding.binding_name[0].id);
				EventChannel ec;
				try {
					ec = getNotificationChannel(channelName, alma.acscommon.NC_KIND.value);

					AdminConsumer consumer = null;
					if (!channelMap.containsKey(channelName)) {
						channelMap.put(channelName, ec);
						consumer = getAdminConsumer(channelName);
						consumers.add(consumer);
					}
				} catch (AcsJException e) {
					m_logger.severe("Can't find channel "+channelName);
					e.printStackTrace();
				}
			}
		}
		return consumers;
	}
	
	public static DynAnyFactory getDynAnyFactory() {
		return dynAnyFactory;
	}
	
	private void printMonitoringResults(NotificationServiceMonitorControl mc) { //alma/ACS-8.0/TAO/ACE_wrappers/build/linux/TAO/orbsvcs/orbsvcs/Notify/MonitorControlExt/NotifyMonitoringExt.idl
		// 
		if (true) return; // TODO -- Fix this method, which throws InvalidName exception
		try {
			m_logger.info("EventChannelFactoryNames: "+mc.get_statistic(gov.sandia.NotifyMonitoringExt.EventChannelFactoryNames.value));
			m_logger.info("ActiveEventChannelCount: "+mc.get_statistic(gov.sandia.NotifyMonitoringExt.ActiveEventChannelCount.value));
			m_logger.info("ActiveEventChannelNames: "+mc.get_statistic(gov.sandia.NotifyMonitoringExt.ActiveEventChannelNames.value));
		} catch (InvalidName e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void closeAllConsumers() {
		ArrayList<AdminConsumer>consumers = getAllConsumers();
		for (AdminConsumer consumer : consumers) {
			if (consumer != null) {
				consumer.disconnect();
			}
		}
		
	}

	
}
