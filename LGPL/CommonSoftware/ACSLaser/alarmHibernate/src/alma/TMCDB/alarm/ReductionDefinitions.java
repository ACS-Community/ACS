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

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.hibernate.Session;

import alma.acs.tmcdb.Configuration;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.cosylab.cdb.jdal.hibernate.ExtraDataFeature;

import com.cosylab.acs.laser.dao.ConfigurationAccessor;
import com.cosylab.cdb.jdal.hibernate.NameOverrideFeature;
import com.cosylab.cdb.jdal.hibernate.DOMJavaClassIntrospector.XMLSaver;
import com.cosylab.cdb.jdal.hibernate.plugin.HibernateWDALAlarmPluginImpl;

/**
 * @author msekoranja
 */
public class ReductionDefinitions implements ExtraDataFeature, NameOverrideFeature, XMLSaver {

	public static final String REDUCTION_DEFINITIONS_PATH = "/Alarms/Administrative/ReductionDefinitions";

	private final Session session;
	private final Configuration config;
	private final ConfigurationAccessor conf;
	private final Map<String, Object> rootMap;
	private final Logger m_logger;
	
	private ReductionLinks linksToCreate = new ReductionLinks("links-to-create");
	private ReductionLinks linksToRemove = new ReductionLinks("links-to-remove");
	private ReductionThresholds thresholds = new ReductionThresholds();
	
    // extra data support
    private Element _extraData;
    
    /* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.ExtraDataFeature#getExtraData()
	 */
	public Element getExtraData() {
		return _extraData;
	}

	public ReductionDefinitions(Session session, Configuration config, ConfigurationAccessor conf, Map<String, Object> rootMap, Logger m_logger) {
		this.session = session;
		this.config = config;
		this.conf = conf;
		this.rootMap = rootMap;
		this.m_logger = m_logger;

		// add xmlns attributes
		try
		{
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			DOMImplementation impl = builder.getDOMImplementation();
			Document xmldoc = impl.createDocument(null, "data", null);
			_extraData = xmldoc.getDocumentElement();
			_extraData.setAttribute("xmlns", "urn:schemas-cosylab-com:AcsAlarmSystem:1.0");
			_extraData.setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
		}
		catch (Throwable th)
		{
			th.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.NameOverrideFeature#getNameOverride()
	 */
	public String getNameOverride() {
		return "reduction-definitions";
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
			backup = conf.getConfiguration(REDUCTION_DEFINITIONS_PATH);
			rollback = true;
			
			// replace with new
			conf.setConfiguration(REDUCTION_DEFINITIONS_PATH, xml);

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
					conf.setConfiguration(REDUCTION_DEFINITIONS_PATH, backup);
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
		}
	}

	/**
	 * @return the thresholds
	 */
	public ReductionThresholds getThresholds() {
		return thresholds;
	}

	/**
	 * @return the linksToRemove
	 */
	public ReductionLinks getLinksToRemove() {
		return linksToRemove;
	}

	/**
	 * @return the linksToCreate
	 */
	public ReductionLinks getLinksToCreate() {
		return linksToCreate;
	}

}
