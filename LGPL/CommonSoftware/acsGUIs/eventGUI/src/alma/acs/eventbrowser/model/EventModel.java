package alma.acs.eventbrowser.model;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.omg.CORBA.IntHolder;
import org.omg.CORBA.ORB;
import org.omg.CosNaming.Binding;
import org.omg.CosNaming.BindingIteratorHolder;
import org.omg.CosNaming.BindingListHolder;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNotification.Property;
import org.omg.CosNotifyChannelAdmin.AdminLimitExceeded;
import org.omg.CosNotifyChannelAdmin.AdminNotFound;
import org.omg.CosNotifyChannelAdmin.ClientType;
import org.omg.CosNotifyChannelAdmin.ConsumerAdmin;
import org.omg.CosNotifyChannelAdmin.EventChannel;
import org.omg.CosNotifyChannelAdmin.EventChannelFactory;
import org.omg.CosNotifyChannelAdmin.EventChannelFactoryHelper;
import org.omg.CosNotifyChannelAdmin.EventChannelHelper;
import org.omg.CosNotifyChannelAdmin.ProxySupplier;

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

	public EventModel() throws Exception {
		m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("eventGUI", false);
		compClient = new AdvancedComponentClient(m_logger, "corbaloc::pc013018:3000/Manager", "eventGUI");
		mproxy = compClient.getAcsManagerProxy();
		nsvc = EventChannelFactoryHelper.narrow(mproxy.get_service("NotifyEventChannelFactory", true));
		cs = compClient.getContainerServices();
		orb = compClient.getAcsCorba().getORB();
		h = new Helper(cs);
		nctx = h.getNamingService();
	}
	
	public ArrayList<ChannelData> getChannelStatistics() {
		ArrayList<ChannelData> clist = new ArrayList<ChannelData>();
		int[] chans = nsvc.get_all_channels();
		System.out.println("Number of channels: "+chans.length);

		BindingListHolder bl = new BindingListHolder();
		BindingIteratorHolder bi = new BindingIteratorHolder();
		nctx.list(-1, bl, bi);
		for (Binding binding : bl.value) {
			if (binding.binding_name[0].kind.equals(alma.acscommon.NC_KIND.value)) {
				String channelName = binding.binding_name[0].id;
				System.out.println("Channel: "+binding.binding_name[0].id);
				EventChannel ec;
				try {
					ec = getNotificationChannel(channelName, alma.acscommon.NC_KIND.value);
					int numConsumers = ec.get_all_consumeradmins().length;
					int numSuppliers = ec.get_all_supplieradmins().length;
					System.out.println("... has "+numConsumers+" consumers and "+numSuppliers+" suppliers.");
//					int[] admins = ec.get_all_consumeradmins();
//					ConsumerAdmin adm = ec.get_consumeradmin(admins[0]);
//					int[] pulls = adm.pull_suppliers();
//					ClientType arg0 = ClientType.ANY_EVENT;
//					IntHolder ih = new IntHolder();
//					ProxySupplier ps = adm.obtain_notification_pull_supplier(arg0, ih);
//					System.out.println(ps);
					Property[] props = ec.get_admin();
					for (int j = 0; j < props.length; j++) {
						System.out.println(props[j].name+ props[j].value);
					}
					clist.add(new ChannelData(channelName, numConsumers, numSuppliers));
				} catch (AcsJException e) {
					m_logger.severe("Can't find channel "+channelName);
					e.printStackTrace();
//				} catch (AdminNotFound e) {
//					// TODO Auto-generated catch block
//					e.printStackTrace();
//				} catch (AdminLimitExceeded e) {
//					// TODO Auto-generated catch block
//					e.printStackTrace();
				}

			}
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
	
}
