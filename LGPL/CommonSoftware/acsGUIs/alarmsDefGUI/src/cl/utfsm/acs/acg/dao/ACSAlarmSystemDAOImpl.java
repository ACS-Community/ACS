package cl.utfsm.acs.acg.dao;

import java.util.HashMap;
import java.util.logging.Logger;

import com.cosylab.acs.laser.dao.ConfigurationAccessor;

public class ACSAlarmSystemDAOImpl {
	static final String ALARM_SYSTEM_CONFIGURATION_PATH = "/Alarms/Administrative/AlarmSystemConfiguration";
	Logger logger;
	ConfigurationAccessor conf;
	HashMap<String,String> configs=new HashMap<String,String>();
	
	public ACSAlarmSystemDAOImpl(Logger log) {
		if (log==null) {
			throw new IllegalArgumentException("Invalid null logger");
		}
		logger = log;
	}
	
	public void setConfAccessor(ConfigurationAccessor conf)
	{
		this.conf = conf;
	}
	
	public void loadConfiguration(){
		if(conf == null)
			throw new IllegalStateException("Missing dal");
		String xml;
		try {
			xml = conf.getConfiguration(ALARM_SYSTEM_CONFIGURATION_PATH);
		} catch (Throwable t) {
			throw new RuntimeException("Couldn't read alarm system XML", t);
		}
		System.out.println(xml);
	}
}
