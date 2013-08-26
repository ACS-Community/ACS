/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
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

import alma.alarmsystem.alarmmessage.generated.AlarmSystemConfiguration;
import alma.alarmsystem.alarmmessage.generated.ConfigurationProperty;
import alma.cdbErrType.CDBRecordDoesNotExistEx;

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
		} catch (CDBRecordDoesNotExistEx e) {
			asc = new AlarmSystemConfiguration();
			ConfigurationProperty cp = new ConfigurationProperty();
			cp.setName("Implementation");
			cp.setContent("ACS");
			asc.addConfigurationProperty(cp);
			return asc;
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
	
	public void flushConfiguration(AlarmSystemConfiguration asc) {
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
		} catch (CDBRecordDoesNotExistEx e) {
			try {
				conf.addConfiguration(ALARM_SYSTEM_CONFIGURATION_PATH, FFWriter.toString().replaceFirst("xsi:type=\".*\"", ""));
			} catch (Exception e1) {
				e1.printStackTrace();
			}
		} catch (org.omg.CORBA.UNKNOWN e) {
			try {
				conf.addConfiguration(ALARM_SYSTEM_CONFIGURATION_PATH, FFWriter.toString().replaceFirst("xsi:type=\".*\"", ""));
			} catch (Exception e1) {
				e1.printStackTrace();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
