package cl.utfsm.acs.acg.dao;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Logger;

import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;

import alma.acs.alarmsystem.generated.AlarmSystemConfiguration;
import alma.acs.alarmsystem.generated.ConfigurationProperty;

import cl.utfsm.acs.acg.dao.ConfigurationAccessor;

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
	
	public AlarmSystemConfiguration getConfiguration(){
		if (conf==null)
			throw new IllegalStateException("null configuration accessor");
		AlarmSystemConfiguration asc;
		String xml;
		try {
			xml = conf.getConfiguration(ALARM_SYSTEM_CONFIGURATION_PATH);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
		StringReader FFReader = new StringReader(xml);
		Unmarshaller FF_unmarshaller = new Unmarshaller(AlarmSystemConfiguration.class);
		FF_unmarshaller.setValidation(false);
		try {
			asc = (AlarmSystemConfiguration)FF_unmarshaller.unmarshal(FFReader);
		} catch (MarshalException e) {
			e.printStackTrace();
			return null;
		} catch (ValidationException e) {
			e.printStackTrace();
			return null;
		}
		try {
			asc.validate();
		} catch (ValidationException e) {
			e.printStackTrace();
		}
		return asc;
	}
	
	public List<ConfigurationProperty> loadConfigurations() {
		configs.clear();
		AlarmSystemConfiguration asc = getConfiguration();
		List<ConfigurationProperty> cf = new ArrayList<ConfigurationProperty>();
		ConfigurationProperty[] cps = asc.getConfigurationProperty();
		for (ConfigurationProperty cp : cps) {
			configs.put(cp.getName(), cp.getContent());
			cf.add(cp);
		}
		return cf;
	}
	
	public void flushCategories(AlarmSystemConfiguration asc) {
		if (conf==null || !conf.isWriteable())
			throw new IllegalStateException("no writable configuration accessor");
		if(asc == null)
			throw new IllegalArgumentException("Null Alarm System Configuration argument");
		StringWriter FFWriter = new StringWriter();
		Marshaller FF_marshaller;
		try {
			FF_marshaller = new Marshaller(FFWriter);
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}
		FF_marshaller.setValidation(false);
		try {
			FF_marshaller.marshal(asc);
		} catch (MarshalException e) {
			e.printStackTrace();
			return;
		} catch (ValidationException e) {
			e.printStackTrace();
			return;
		}
		try {
			conf.deleteConfiguration(ALARM_SYSTEM_CONFIGURATION_PATH);
			conf.addConfiguration(ALARM_SYSTEM_CONFIGURATION_PATH, FFWriter.toString().replaceFirst("xsi:type=\".*\"", ""));
		} catch (Exception e) {
			e.printStackTrace();
			throw new IllegalStateException("Category already exists");
		}
	}
}
