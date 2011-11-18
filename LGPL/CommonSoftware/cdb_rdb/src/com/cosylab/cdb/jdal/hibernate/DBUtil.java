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
package com.cosylab.cdb.jdal.hibernate;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.logging.Logger;

import org.hsqldb.jdbc.JDBCDataSource;

import com.cosylab.cdb.jdal.hibernate.plugin.HibernateWDALConfigurationPlugin;
import com.cosylab.cdb.jdal.hibernate.plugin.PluginFactory;

/**
 * @author msekoranja
 *
 */
public class DBUtil {
	
	public static final String HSQLDB_BACKEND_NAME = "hsqldb";
	public static final String ORACLE_BACKEND_NAME = "oracle";

	public static class ConnectionData {
		public String backend;
		public String url;
		public String username;
		public String password;
		public Connection connection;
		
		public ConnectionData(String backend, String url, String username, String password, Connection connection) {
			this.backend = backend;
			this.url = url;
			this.username = username;
			this.password = password;
			this.connection = connection;
		}
	}
	
	public static ConnectionData connectDB(Logger logger) throws SQLException, ClassNotFoundException
	{
		HibernateWDALConfigurationPlugin config = PluginFactory.getConfigurationPlugin(logger);
		
		// read config info
		String backend = config.getBackend();
		String user = config.getUserName();
		String pwd = config.getPassword();
		if (pwd == null) pwd = "";
		String url = config.getURL();
	
		if (backend==null) {
			throw new IllegalArgumentException("No backend specified for TMCDB, check configuration!");
		}
		
		Connection connection;
		if (backend.equalsIgnoreCase(ORACLE_BACKEND_NAME)) {
			connection = connectOracle(user, pwd, url, logger);
		} else if (backend.equalsIgnoreCase(HSQLDB_BACKEND_NAME)) {
			connection = connectHsqldb(user, pwd, url, logger);
		} else {
			final String CLAZZ_PROPERTY_NAME = "hibernate.connection.driver_class";
			String clazz = config.get(CLAZZ_PROPERTY_NAME, null);
			if (clazz == null)
				throw new RuntimeException(CLAZZ_PROPERTY_NAME + " property not specified for " + backend + " backend.");
			connection = connectGeneric(backend, clazz, user, pwd, url, logger);
		}
	
		return new ConnectionData(backend, url, user, pwd, connection);
	}

	public static Connection connectGeneric(String backend, String clazz, String dbUser, String dbPassword, String dbUrl, Logger logger) throws SQLException, ClassNotFoundException {
		if (logger != null) logger.info("Connecting to TMCDB in " + backend + " (not fully supported) as " + dbUser + " with: " + dbUrl);
	    Class.forName(clazz);
        Connection conn = DriverManager.getConnection(dbUrl, dbUser, dbPassword);
		conn.setAutoCommit(false);  // We have to commit explicitly.
		return conn;
	}
	
	public static Connection connectOracle(String dbUser, String dbPassword, String dbUrl, Logger logger) throws SQLException, ClassNotFoundException {
		if (logger != null) logger.info("Connecting to TMCDB in Oracle as " + dbUser + " with: " + dbUrl);
	    Class.forName("oracle.jdbc.driver.OracleDriver");
		//OracleDataSource ds = new OracleDataSource();
		//ds.setURL(dbUrl);
		//Connection conn = ds.getConnection(dbUser, dbPassword);
	    // since Oracle is not part of base distribution, we have to establish connection this way...
        Connection conn = DriverManager.getConnection(dbUrl, dbUser, dbPassword);
		conn.setAutoCommit(false);  // We have to commit explicitly.
		return conn;
	}

	
	public static Connection connectHsqldb(String dbUser, String dbPassword, String dbUrl, Logger logger) throws SQLException, ClassNotFoundException {
		
		// Load the HSQL Database Engine JDBC driver
        // hsqldb.jar should be in the class path or made part of the current jar
		if (logger != null) logger.info("Connecting to TMCDB in HsqlDB as " + dbUser + " with: " + dbUrl);
	    Class.forName("org.hsqldb.jdbc.JDBCDriver");
	    // ... from hsqldb.jar
	    JDBCDataSource hds = new JDBCDataSource();
	    hds.setDatabase(dbUrl);
	    Connection conn = hds.getConnection(dbUser, dbPassword);
        // Connection conn = DriverManager.getConnection(dbUrl, dbUser, dbPassword);
		conn.setAutoCommit(false); 
		return conn;
	}

	public static void loadAndExecuteScript(Connection connection, String filename) throws Exception {
		BufferedReader csr = null;
		Statement stmt = null;

		try {
			stmt = connection.createStatement();
			
			InputStream inputStream = DBUtil.class.getClassLoader().getResourceAsStream(filename);
			if (inputStream == null)
				throw new FileNotFoundException(filename);
			csr = new BufferedReader(new InputStreamReader(inputStream));

		} catch (FileNotFoundException e) {
			if (new java.io.File(filename).exists())
				csr = new BufferedReader(new java.io.FileReader(filename));
			else 
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
				throw e;
			}
		}
		String line;
		StringBuffer sb = new StringBuffer();
		boolean plsqlMode = false;
		if (csr != null && stmt != null) {
			try {
				while((line = csr.readLine()) != null) {
					line=line.trim();
					if (line.startsWith("--") || line.matches("^/")) continue;	// SQL Comment
					if (line.contains("CREATE OR REPLACE TRIGGER"))
						plsqlMode = true;
					if (line.contains(";")) {
						if (!plsqlMode)
							line = line.replace(";","");
						sb.append(line);
						sb.append(" ");
						if (plsqlMode && !line.startsWith("END"))
							continue;
						line = sb.toString();
						//System.out.println(line);
						try {
							stmt.execute(line);
						} catch (SQLException e) {
							// TODO Auto-generated catch block
							//e.printStackTrace();
							if (!line.matches("^DROP (TABLE|SEQUENCE).*")) 
								throw e;
							else
								System.out.println(e.getCause());  // Usually means that the table wasn't there, so don't worry
						}
						sb = new StringBuffer();
						plsqlMode = false;
					} else {
						sb.append(line);
						sb.append(" ");
					}
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				throw e;
			}
		}
	}

	
}
