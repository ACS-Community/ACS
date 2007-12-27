package alma.acs.eventbrowser.model;

import java.util.logging.Logger;

import org.omg.CORBA.ORB;
import org.omg.CosNaming.NamingContext;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.ClientLogManager;
import alma.acs.nc.Helper;

public class EventModel {
	private final AcsCorba acsCorba;
	private final ORB orb;
	private final Logger m_logger;
	private final AdvancedComponentClient compClient;
	private final ContainerServices cs;
	private final Helper h;
	private final NamingContext nctx;

	public EventModel() throws Exception {
		m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("eventGUI", false);
		compClient = new AdvancedComponentClient(m_logger, "corbaloc::pc013018:3000/Manager", "eventGUI");
		acsCorba = compClient.getAcsCorba();
		cs = compClient.getContainerServices();
		orb = acsCorba.getORB();
		h = new Helper(cs);
		nctx = h.getNamingService();
	}
	
	public String[] getServices() {

		return orb.list_initial_services();
	}
	
}
