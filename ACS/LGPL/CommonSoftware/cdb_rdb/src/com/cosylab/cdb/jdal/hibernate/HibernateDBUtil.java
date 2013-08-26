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
package com.cosylab.cdb.jdal.hibernate;

import java.sql.Connection;
import java.util.Properties;
import java.util.logging.Logger;

import com.cosylab.cdb.jdal.hibernate.DBUtil.ConnectionData;
import com.cosylab.cdb.jdal.hibernate.plugin.HibernateWDALConfigurationPlugin;
import com.cosylab.cdb.jdal.hibernate.plugin.HibernateWDALPlugin;
import com.cosylab.cdb.jdal.hibernate.plugin.PluginFactory;


public class HibernateDBUtil {

	static final String HSQLDB_MEM_URL = "jdbc:hsqldb:mem:tmcdb";
	static final String HSQLDB_MEM_USER = "sa";
	static final String HSQLDB_MEM_PASSWORD = "";

	private final HibernateUtil hibernateUtil;
	
	private final String[] scripts;
	
	private final Logger logger;

	private final HibernateWDALConfigurationPlugin config;
	
	public HibernateDBUtil(Logger logger, HibernateWDALPlugin plugin) {
		this.logger = logger;
		config = PluginFactory.getConfigurationPlugin(logger);
		scripts = (plugin != null) ? plugin.getCreateTablesScriptList(config.getBackend()) : null;
		hibernateUtil = HibernateUtil.getInstance(logger);
	}

	public boolean setUp(boolean forceInMemory, boolean createTables) {
		return setUp(forceInMemory, createTables, true, scripts);
	}

	/**
	 * @param forceInMemory
	 * @param createTables
	 * @param closeConnection
	 * @return in-memory DB flag.
	 */
	public boolean setUp(boolean forceInMemory, boolean createTables, boolean closeConnection, String[] scripts) {
		Connection conn = null;
		try {
			boolean inMemory = false;
			
			String url;
			String user;
			String pwd;
			String backend;
			
			if (forceInMemory)
			{
				backend = DBUtil.HSQLDB_BACKEND_NAME;
				conn = DBUtil.connectHsqldb(user = HSQLDB_MEM_USER, pwd = HSQLDB_MEM_PASSWORD, url = HSQLDB_MEM_URL, logger);
				inMemory = true;
			}
			else
			{
				// let TMCDB to read and parse configuration (to have this code only at one place)
				ConnectionData data = DBUtil.connectDB(logger);
				backend = data.backend;
				url = data.url;
				user = data.username;
				pwd = data.password;
				conn = data.connection;
			}
			
			final Properties connectionProperties = new Properties();
			connectionProperties.setProperty("hibernate.dialect", "org.hibernate.dialect.HSQLDialect");
			connectionProperties.setProperty("hibernate.connection.driver_class", "org.hsqldb.jdbc.JDBCDriver");
			connectionProperties.setProperty("hibernate.connection.url", url);
			connectionProperties.setProperty("hibernate.connection.username", user);
			connectionProperties.setProperty("hibernate.connection.password", pwd);
			
			if (backend.equals(DBUtil.ORACLE_BACKEND_NAME))
			{
				connectionProperties.setProperty("hibernate.dialect", "org.hibernate.dialect.Oracle10gDialect");
				//connectionProperties.setProperty("hibernate.dialect", "com.cosylab.cdb.jdal.hibernate.Oracle9DialectWithSequenceIdentity");
				connectionProperties.setProperty("hibernate.connection.driver_class", "oracle.jdbc.driver.OracleDriver");
			}
			else if (backend.equals(DBUtil.HSQLDB_BACKEND_NAME))
			{
				connectionProperties.setProperty("hibernate.dialect", "org.hibernate.dialect.HSQLDialect");
				connectionProperties.setProperty("hibernate.connection.driver_class", "org.hsqldb.jdbc.JDBCDriver");
				
				// set inMemory flag
				if (url.indexOf(":mem:") != -1)
					inMemory = true;
			}
			else
			{
				// for all other backends, configuration needs to be provided manually via configuration
				// this way we ease configuration of default RDBMS and allow other
				connectionProperties.setProperty("hibernate.dialect", config.get("hibernate.dialect", null));
				connectionProperties.setProperty("hibernate.connection.driver_class", config.get("hibernate.connection.driver_class", null));
			}
				
				
			// connect hibernate
			hibernateUtil.setConfiguration(connectionProperties);
			
			logger.info("Connection to TMCDB established.");

			// create tables
			if (createTables || inMemory)
			{
				if (scripts != null)
				{
					for (String script : scripts)
						DBUtil.loadAndExecuteScript(conn, script);
					logger.info("TMCDB tables initialized.");
				}
			}
			
			return inMemory;
		} catch (Throwable e) {
		    e.printStackTrace();
			throw new RuntimeException("Exception when connecting to the DB.", e);
		} finally {
			// and close connection
			try {
				if (closeConnection && conn != null && !conn.isClosed())
					conn.close();
			} catch (Throwable th) { /* noop */ }
		}
	}
}
