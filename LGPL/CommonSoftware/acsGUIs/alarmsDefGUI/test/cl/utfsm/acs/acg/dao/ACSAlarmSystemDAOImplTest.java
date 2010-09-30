package cl.utfsm.acs.acg.dao;

import java.util.List;

import alma.alarmsystem.alarmmessage.generated.AlarmSystemConfiguration;
import alma.alarmsystem.alarmmessage.generated.ConfigurationProperty;
import cl.utfsm.acs.acg.core.AcsInformation;
import cl.utfsm.acs.acg.core.DAOManager;
import junit.framework.TestCase;

public class ACSAlarmSystemDAOImplTest extends TestCase{
	AcsInformation _acsInfo;
	DAOManager _daoManager;
	ACSAlarmSystemDAOImpl _alarmSystemDAO;

	public void setUp() throws Exception {
		_acsInfo = new AcsInformation("Test client");
		_daoManager = new DAOManager(_acsInfo.getContainerServices());
		_daoManager.connect();
		_alarmSystemDAO = (ACSAlarmSystemDAOImpl)_daoManager.getAlarmSystemDAO();
	}
	
	public void testGetConfiguration() {
		AlarmSystemConfiguration asc = _alarmSystemDAO.getConfiguration();
		ConfigurationProperty[] cps = asc.getConfigurationProperty();
		for (ConfigurationProperty cp : cps) {
			System.out.println(cp.getContent());
		}
		List<ConfigurationProperty> cpsL = _alarmSystemDAO.loadConfigurations();
		for (ConfigurationProperty cp : cpsL) {
			System.out.println(cp.getName()+"_"+ cp.getContent());
		}
	}

	public void tearDown() throws Exception {
		_acsInfo.disconnect();
	}
}
