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
/**
 * 
 */
package com.cosylab.cdb.jdal.hibernate.plugin;

import java.util.ArrayList;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.hibernate.Session;

import alma.acs.tmcdb.Component;
import alma.acs.tmcdb.Configuration;

import com.cosylab.cdb.client.CDBAccess;
import com.cosylab.cdb.jdal.hibernate.DBUtil;

/**
 * @author msekoranja
 */
public class PluginFactory {
	
	public static final String WDAL_PLUGIN_KEY = "cdb_rdb.plugins.wdal";

	public static final String CONFIGURATION_WDAL_PLUGIN_KEY = "cdb_rdb.plugins.configuration";

	public static HibernateWDALPlugin getPlugin(Logger logger) {
		DelegateHibernateWDALPlugin delegate = new DelegateHibernateWDALPlugin(logger);
		delegate.initialize(logger);
		return delegate;
	}
	
	private static class DelegateHibernateWDALPlugin implements HibernateWDALPlugin
	{
		private ArrayList<HibernateWDALPlugin> plugins = new ArrayList<HibernateWDALPlugin>();
		private Logger logger;
		
		public DelegateHibernateWDALPlugin(Logger logger)
		{
			plugins.add(new EssentialHibernateWDALPlugin());
			
			String classNames = System.getProperty(WDAL_PLUGIN_KEY);
			if (classNames == null)
			{
				logger.config("No plugins registered.");
				return;
			}
			
			StringTokenizer tokenizer = new StringTokenizer(classNames, ",");
			while (tokenizer.hasMoreTokens())
			{
				String className = tokenizer.nextToken();

				try {
					HibernateWDALPlugin plugin = (HibernateWDALPlugin)Class.forName(className).getConstructor((Class[])null).newInstance((Object[])null);
					plugins.add(plugin);
					logger.config("Plugin '" + plugin.getName() + "' registered.");
				} catch (Throwable th) {
					logger.log(Level.CONFIG, "Plugin with class-name '" + className + "' failed to register.", th);
				}
				
			}
		}
		
		/**
		 * Get plugin name.
		 * @return the plugin name.
		 */
		public String getName()
		{
			return "Delegation plugin";
		}
		
		/**

		 * @param logger
		 */
		public void initialize(Logger logger)
		{
			this.logger = logger;
			for (HibernateWDALPlugin plugin : plugins) {
				try {
					logger.finer("Calling initialize() on " + plugin.getName());
					plugin.initialize(logger);
				}
				catch (Throwable th) {
					logger.log(Level.SEVERE, "Call initialize() on " + plugin.getName() + " failed.", th);
				}
			}
		}
		
		// load
		public void loadPrologue(Session session, Configuration config, Map<String, Object> rootMap)
		{ 
			for (HibernateWDALPlugin plugin : plugins) {
				try {
					logger.finer("Calling loadPrologue() on " + plugin.getName());
					plugin.loadPrologue(session, config, rootMap);
				}
				catch (Throwable th) {
					logger.log(Level.SEVERE, "Call loadPrologue() on " + plugin.getName() + " failed.", th);
				}
			}
		}
		
		public void loadControlDevices(Session session, Configuration config, ControlDeviceBindCallback bindCallback)
		{
			for (HibernateWDALPlugin plugin : plugins) {
				try {
					logger.finer("Calling loadControlDevices() on " + plugin.getName());
					plugin.loadControlDevices(session, config, bindCallback);
				}
				catch (Throwable th) {
					logger.log(Level.SEVERE, "Call loadControlDevices() on " + plugin.getName() + " failed.", th);
				}
			}
		}
		
		public void loadEpilogue(Session session, Configuration config, Map<String, Object> rootMap)
		{
			for (HibernateWDALPlugin plugin : plugins) {
				try {
					logger.finer("Calling loadEpilogue() on " + plugin.getName());
					plugin.loadEpilogue(session, config, rootMap);
				}
				catch (Throwable th) {
					logger.log(Level.SEVERE, "Call loadEpilogue() on " + plugin.getName() + " failed.", th);
				}
			}
		}

		// import
		public void importPrologue(Session session, Configuration config, CDBAccess cdbAccess)
		{
			for (HibernateWDALPlugin plugin : plugins) {
				try {
					logger.finer("Calling importPrologue() on " + plugin.getName());
					plugin.importPrologue(session, config, cdbAccess);
				}
				catch (Throwable th) {
					logger.log(Level.SEVERE, "Call importPrologue() on " + plugin.getName() + " failed.", th);
				}
			}
		}
		
		public void controlDeviceImportEpilogue(Session session, Configuration config, CDBAccess cdbAccess, String cdbComponentName, Component component)
		{
			for (HibernateWDALPlugin plugin : plugins) {
				try {
					logger.finer("Calling controlDeviceImportEpilogue() on " + plugin.getName());
					plugin.controlDeviceImportEpilogue(session, config, cdbAccess, cdbComponentName, component);
				}
				catch (Throwable th) {
					logger.log(Level.SEVERE, "Call controlDeviceImportEpilogue() on " + plugin.getName() + " failed.", th);
				}
			}
		}

		public void importEpilogue(Session session, Configuration config, CDBAccess cdbAccess)
		{
			for (HibernateWDALPlugin plugin : plugins) {
				try {
					logger.finer("Calling importEpilogue() on " + plugin.getName());
					plugin.importEpilogue(session, config, cdbAccess);
				}
				catch (Throwable th) {
					logger.log(Level.SEVERE, "Call importEpilogue() on " + plugin.getName() + " failed.", th);
				}
			}
		}

		public String[] getCreateTablesScriptList(String backend) {
			ArrayList<String> scripts = new ArrayList<String>();
			for (HibernateWDALPlugin plugin : plugins) {
				try {
					logger.finer("Calling getCreateTablesScriptList() on " + plugin.getName());
					String[] list = plugin.getCreateTablesScriptList(backend);
					if (list != null)
					{
						for (String item : list)
							if (!scripts.contains(item))
								scripts.add(item);
					}
				}
				catch (Throwable th) {
					logger.log(Level.SEVERE, "Call getCreateTablesScriptList() on " + plugin.getName() + " failed.", th);
				}
			}
			return scripts.toArray(new String[scripts.size()]);
		}
	}

	private static class EssentialHibernateWDALPlugin implements HibernateWDALPlugin
	{
		public String getName()
		{
			return "EssentialHibernateWDALPlugin";
		}
		
		public void initialize(Logger logger) {}
		
		public void loadPrologue(Session session, Configuration config, Map<String, Object> rootMap) {}
		
		public void loadControlDevices(Session session, Configuration config, ControlDeviceBindCallback bindCallback) {}
		
		public void loadEpilogue(Session session, Configuration config, Map<String, Object> rootMap) {}

		public void importPrologue(Session session, Configuration config, CDBAccess cdbAccess) {}
		
		public void controlDeviceImportEpilogue(Session session, Configuration config, CDBAccess cdbAccess, String cdbComponentName, Component component) {}

		public void importEpilogue(Session session, Configuration config, CDBAccess cdbAccess) {}

		public String[] getCreateTablesScriptList(String backend) {
			
			String ddlDir = System.getProperty("ACS.ddlpath");
			if (ddlDir == null)
				ddlDir = ".";
			
			if (backend.equals(DBUtil.ORACLE_BACKEND_NAME)) { 
				return new String[] {
					ddlDir + "/oracle/TMCDB_swconfigcore/CreateOracleTables.sql",
					ddlDir + "/oracle/TMCDB_swconfigext/CreateOracleTables.sql"
				};
			}
			else if (backend.equals(DBUtil.HSQLDB_BACKEND_NAME)) { 
				return new String[] {
					ddlDir + "/hsqldb/TMCDB_swconfigcore/CreateHsqldbTables.sql",
					ddlDir + "/hsqldb/TMCDB_swconfigext/CreateHsqldbTables.sql"
				};
			}
			else
				return null;
		}

	}

	public static String BACKEND_KEY_NAME = "cdb_rdb.backend";
	public static String URL_KEY_NAME = "cdb_rdb.url";
	public static String USERNAME_KEY_NAME = "cdb_rdb.username";
	public static String PASSWORD_KEY_NAME = "cdb_rdb.password";
	
	
	private static class DefaultConfigurationPlugin implements HibernateWDALConfigurationPlugin
	{
		public String getName() {
			return "Default configuration plugin";
		}

		public String get(String name, String defaultValue) {
			return System.getProperty(name, defaultValue);
		}

		public String getBackend() {
			return System.getProperty(BACKEND_KEY_NAME, "hsqldb");
		}

		public String getURL() {
			return System.getProperty(URL_KEY_NAME, "jdbc:hsqldb:mem:tmcdb");
		}

		public String getUserName() {
			return System.getProperty(USERNAME_KEY_NAME, "sa");
		}

		public String getPassword() {
			return System.getProperty(PASSWORD_KEY_NAME, "");
		}

		public void initialize(Logger logger) {
		}
		
	}
	
	private static HibernateWDALConfigurationPlugin configurationPlugin = null;
	
	public static synchronized HibernateWDALConfigurationPlugin getConfigurationPlugin(Logger logger)
	{
		if (configurationPlugin == null)
		{
			String className = System.getProperty(CONFIGURATION_WDAL_PLUGIN_KEY);
			if (className == null)
				configurationPlugin = new DefaultConfigurationPlugin();
			else
			{
				try {
					configurationPlugin = (HibernateWDALConfigurationPlugin)Class.forName(className).getConstructor((Class[])null).newInstance((Object[])null);
					configurationPlugin.initialize(logger);
				} catch (Throwable th) {
					logger.log(Level.WARNING, "Failed to instantiate configuration plugin '" + className + "', using default plugin."/*, th*/);
					configurationPlugin = new DefaultConfigurationPlugin();
				}
			}
		}
		return configurationPlugin;
	}

}
