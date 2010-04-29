/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 *
 *    Created on Apr 3, 2007
 *
 */

package com.cosylab.cdb.jdal.hibernate.plugin;

import java.util.Map;
import java.util.logging.Logger;

import org.hibernate.Session;

import alma.acs.tmcdb.Component;
import alma.acs.tmcdb.Configuration;

import com.cosylab.cdb.client.CDBAccess;

/**
 * HibernateWDAL plugin interface.
 * @author msekoranja
 */
public interface HibernateWDALPlugin  {

	/**
	 * Get plugin name.
	 * @return the plugin name.
	 */
	String getName();
	
	/**
	 * Initialize
	 * @param logger
	 */
	void initialize(Logger logger);
	
	String[] getCreateTablesScriptList(String backend);
	
	public interface ControlDeviceBindCallback {
		void bindToComponentBranch(String name, String path, Object objectToBind);
		void bindNonExpandedXMLToComponentBranch(Session session, Component component);
	}
	
	// load
	void loadPrologue(Session session, Configuration config, Map<String, Object> rootMap);
	void loadControlDevices(Session session, Configuration config, ControlDeviceBindCallback bindCallback);
	void loadEpilogue(Session session, Configuration config, Map<String, Object> rootMap);

	// import
	void importPrologue(Session session, Configuration config, CDBAccess cdbAccess);
	void controlDeviceImportEpilogue(Session session, Configuration config, CDBAccess cdbAccess, String cdbComponentName, Component component);
	void importEpilogue(Session session, Configuration config, CDBAccess cdbAccess);
}
