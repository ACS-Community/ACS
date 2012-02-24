/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2012
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
package alma.acs.commandcenter.serviceshelper;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;

import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;

import com.cosylab.cdb.jdal.hibernate.HibernateDBUtil;
import com.cosylab.cdb.jdal.hibernate.HibernateUtil;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.stat.CategorizedStatistics;

import alma.acs.tmcdb.AcsService;
import alma.acs.tmcdb.AcsServiceServiceType;
import alma.acs.tmcdb.Configuration;

/** 
 * Objects of this class help retireving the list of services to start.
 * <P>
 * The list of services is read from the TMCDB for the passed configuration.
 * To rad the TMCDB, Hibernate is used by passing the SQL statement {@link #sqlQuery}
 * with a nemed parameter ({@link #namedParameterName}).
 * <P> 
 * The services to start are returned as a list of {@link AcsServiceToStart}.
 * <P>
 * Considering that the query is in SQL format and the result of the query is a
 * list of values from columns of different objects, we could as well used directly JDBC.
 * 
 * @author  acaproni
 */
class TMCDBServicesHelper {
	
	/**
	 * A class to encapsulate the services to start
	 * 
	 * @author acaproni
	 *
	 */
	public static class AcsServiceToStart {
		
		/**
		 * The name of the service to start (can be <code>null</code>)
		 */
		public final String serviceName;
		
		/**
		 * The type of the service
		 */
		public final AcsServiceServiceType serviceType;
		
		/**
		 * The host where the service must be started
		 */
		public final String hostName;
		
		/**
		 * C'tor
		 */
		private AcsServiceToStart(String name, AcsServiceServiceType type, String host) {
			this.serviceName=name;
			this.serviceType=type;
			this.hostName=host;
		}
		
		/**
		 * Return a {@link AcsServiceToStart} for a row returned by the query.
		 * <P>
		 * The query executed in {@link TMCDBServicesHelper#getServicesList(String)} 
		 * returns a array of {@link Object}. The order of the columns is defined in the
		 * select and must be the same used to create this object i.e.
		 * <UL>
		 * 	<LI>Object[0] is the service name
		 *  <LI>Object[1] is the service name
		 *  <LI>Object[2] is the service name
		 * </UL>
		 * 
		 * @param row The row returned by the query.
		 * @return An {@link AcsServiceToStart}
		 */
		public static AcsServiceToStart instanceFromDBRow(Object[] row) {
			if (row==null || row.length!=3) {
				throw new IllegalArgumentException("Invalid row: must be not null and have a length of 3");						
			}
			return new AcsServiceToStart((String)row[0], AcsServiceServiceType.valueOf((String)row[1]), (String)row[2]);
		}
	}
	
	/**
	 * The named parameter used in the where clause of the SQL statement
	 * for the configuration name 
	 */
	private final String namedParameterName = "configname"; 
	
	/**
	 * The SQL query to get the list of services for the given configuration
	 */
	private final String sqlQuery =
			"select s.serviceinstancename, s.servicetype, nd.networkname " +
			"from configuration conf inner join acsservice s " +
			"  on conf.configurationid = s.configurationid " +
			"  inner join computer comp " +
			"  on comp.networkdeviceid=s.computerid " +
			"  inner join networkdevice nd " +
			"  on nd.networkdeviceid = comp.networkdeviceid " +
			"where conf.configurationname = :"+namedParameterName;
	
	/**
	 * The logger
	 */
	private final AcsLogger logger;
	
	/**
	 * The {@link Session} to query the TMCDB
	 */
	private final Session session;
	
	/**
	 * Constructor
	 * 
	 * @param logger The logger
	 */
	public TMCDBServicesHelper(AcsLogger logger, Session session) {
		if (logger==null) {
			throw new IllegalArgumentException("The logger can't be null!");
		}
		if (session==null) {
			throw new IllegalArgumentException("The session can't be null!");
		}
		this.logger=logger;
		this.session=session;
	}
	
	/**
	 * 
	 * @return The configuration names from the TMCDB
	 * 
	 * @throws HibernateException In case of error getting data from the TMCDB
	 */
	public Collection getConfigurationNames() throws HibernateException {
		Query query = session.createQuery("from Configuration");
		List list= query.list();
		return list;
	}
	
	/**
	 * Queries the TMCDB and return the list of services to start for the 
	 * passed configuration name.
	 * 
	 * @param configurationName The not <code>null</code> and not empty name of the configuration
	 * @return The not <code>null</code> list of services to start for the passed configuration
	 * @throws HibernateException In case of error getting data from the TMCDB
	 */
	public List<AcsServiceToStart> getServicesList(String configurationName) throws HibernateException {
		if (configurationName==null || configurationName.isEmpty()) {
			throw new IllegalArgumentException("Invalid null or empty name of configuration");
		}
		logger.log(AcsLogLevel.DEBUG,"Getting the list of services from TMCDB and configuration "+configurationName);
		Query query = session.createSQLQuery(sqlQuery);
		query.setString(namedParameterName, configurationName);
		List svcs= query.list();
		if (svcs!=null) {
			AcsLogLevel lvl = (svcs.size()==0)?AcsLogLevel.WARNING:AcsLogLevel.DEBUG;
			logger.log(lvl,"Got "+svcs.size()+" services from TMCDB");
		} else {
			logger.log(AcsLogLevel.WARNING,"Got a NULL list of services from TMCDB");
		}
		List<AcsServiceToStart> ret = new ArrayList<TMCDBServicesHelper.AcsServiceToStart>();
		for (Object s: svcs) {
        	ret.add(AcsServiceToStart.instanceFromDBRow((Object[])s));
        }
		return ret;
	}
	
}
