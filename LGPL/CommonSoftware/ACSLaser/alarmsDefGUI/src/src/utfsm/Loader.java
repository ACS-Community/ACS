package utfsm;

import java.util.logging.Logger;
import com.cosylab.acs.laser.dao.ACSAlarmDAOImpl;
import com.cosylab.acs.laser.dao.ConfigurationAccessor;
import com.cosylab.acs.laser.dao.ConfigurationAccessorFactory;
import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.acs.logging.ClientLogManager;
import alma.alarmsystem.AlarmService;
import alma.acs.logging.AcsLogger;

public class Loader extends ComponentClientTestCase {


	  public Loader(String name) throws Exception {
		super(name);
	} 
	  
	 private AlarmService alarmservice;
     private ContainerServices m_containerServices;
	 private AdvancedComponentClient m_client;
	 private ACSAlarmDAOImpl alarmDAO;
  
	 /* To Load the Alarm from the CDB using JDAL */
	public void load() throws Exception{
		try{
		Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("Loader",true);
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null) {		  
		  System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
		  System.exit(-1);
		}
		m_client = new AdvancedComponentClient(logger,managerLoc,"Getting the Alarms from CDB");
		ConfigurationAccessor conf = ConfigurationAccessorFactory.getInstance(m_client.getContainerServices());
		assertNotNull("A null ConfigurationAccessor", conf);     
		alarmDAO=new ACSAlarmDAOImpl(m_client.getContainerServices().getLogger());
		assertNotNull("The AlarmDAO is null", alarmDAO);     
		alarmDAO.setConfAccessor(conf);
		alarmDAO.loadAlarms(); 
	}
	
	catch(Exception e){}
}
	  public static void main(String[] args){
		  try{
		  Loader l = new Loader("Loader Test");
		  l.load();
		  } catch(Exception e){} 
	  }  
}
