package utfsm;


import java.util.HashMap;
import java.util.Set;
import java.util.logging.Logger;

import cern.laser.business.data.Source;

import com.cosylab.acs.laser.dao.ACSAlarmDAOImpl;
import com.cosylab.acs.laser.dao.ConfigurationAccessor;
import com.cosylab.acs.laser.dao.ConfigurationAccessorFactory;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.acs.logging.ClientLogManager;


public class TestAlarmDAO extends ComponentClientTestCase {

 
    private ACSAlarmDAOImpl alarmDAO;

  public TestAlarmDAO() throws Exception {
      super("TestAlarmDAO");
  }
  
  
  private ContainerServices m_containerServices;
  private AdvancedComponentClient m_client;
	 
  public void setUp() throws Exception {
      super.setUp();
      Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("DemoTest",true);
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null) {		  
		  System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
		  System.exit(-1);
		}
		m_client = new AdvancedComponentClient(logger,managerLoc,"TestSendLaserSources");
		m_containerServices = m_client.getContainerServices();
        ConfigurationAccessor conf;
        conf = ConfigurationAccessorFactory.getInstance(m_containerServices);
        assertNotNull("Got a null ConfigurationAccessor", conf);
        alarmDAO=new ACSAlarmDAOImpl(m_containerServices.getLogger());
        assertNotNull("AlarmDAO is null", alarmDAO);
        alarmDAO.setConfAccessor(conf);
        alarmDAO.loadAlarms();
  
  }

  public void tearDown() throws Exception {
      super.tearDown();
  }

  
  public void testGetSources() throws Exception {
      HashMap<String,cern.laser.business.data.Source> sources = alarmDAO.getSources();
      assertNotNull(sources);
      assertEquals("There should be only one source and not "+sources.size(),1, sources.size());
      Set<String> keys =sources.keySet();
      assertNotNull(keys);
      assertEquals("Invalid number of keys", 1, keys.size());
      for (String key: keys) {
          Source src = sources.get(key);
          assertNotNull(src);
          assertEquals(src.getSourceId(), key);
          assertEquals("SOURCE", src.getDescription());
          assertEquals("ALARM_SYSTEM_SOURCES", src.getName());
      }
  }
 
  
  public static void main(String[] args){
	  try{
	  TestAlarmDAO t = new TestAlarmDAO();
	  t.setUp();
	  t.testGetSources();
	  } catch(Exception e){}
  }

}