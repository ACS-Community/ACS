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
/**
 * 
 */
package alma.TMCDB.alarm;

import java.util.Map;
import java.util.logging.Logger;

import org.hibernate.Session;

import alma.acs.tmcdb.Configuration;

import com.cosylab.acs.laser.dao.ConfigurationAccessor;
import com.cosylab.cdb.jdal.hibernate.InternalElementsMap;
import com.cosylab.cdb.jdal.hibernate.DOMJavaClassIntrospector.XMLSaver;
import com.cosylab.cdb.jdal.hibernate.plugin.HibernateWDALAlarmPluginImpl;

/**
 * @author msekoranja
 */
@SuppressWarnings("serial")
public class FaultFamiliesMap extends InternalElementsMap<String, alma.TMCDB.alarm.FaultFamily> implements XMLSaver {

	public static final String ALARM_CATEGORY_DEFINITION_PATH = "/Alarms/AlarmDefinitions";

	private final Session session;
	private final Configuration config;
	private final ConfigurationAccessor conf;
	private final Map<String, Object> rootMap;
	private final Logger m_logger;
	
	public FaultFamiliesMap(Session session, Configuration config, ConfigurationAccessor conf, Map<String, Object> rootMap, Logger m_logger) {
		this.session = session;
		this.config = config;
		this.conf = conf;
		this.rootMap = rootMap;
		this.m_logger = m_logger;
	}

	@Override
	public void save() {
		// noop
	}

	@Override
	public void save(String xml) {
		boolean rollback = false;
		String backup = null;
		try 
		{
			backup = conf.getConfiguration(ALARM_CATEGORY_DEFINITION_PATH);
			rollback = true;
			
			// replace with new
			conf.setConfiguration(ALARM_CATEGORY_DEFINITION_PATH, xml);

			// validate and update
			HibernateWDALAlarmPluginImpl.importAlarms(session, config, conf, m_logger);
			
			// alles OK, do not rollback
			rollback = false;

			// and reload
			HibernateWDALAlarmPluginImpl.loadEpilogue(session, config, rootMap, m_logger);
			
		} catch (Throwable th) {
			throw new RuntimeException(th);
		} finally {
			if (rollback) {
				try {
					conf.setConfiguration(ALARM_CATEGORY_DEFINITION_PATH, backup);
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
		}
	}

}
