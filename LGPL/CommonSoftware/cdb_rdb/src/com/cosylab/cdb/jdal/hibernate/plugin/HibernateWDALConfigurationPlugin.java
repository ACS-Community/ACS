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

import java.util.logging.Logger;

/**
 * HibernateWDAL configuration plugin interface.
 * @author msekoranja
 */
public interface HibernateWDALConfigurationPlugin  {

	/**
	 * Get plugin name.
	 * @return the plugin name.
	 */
	String getName();

	/**
	 * Initialize
	 * @param configId
	 * @param logger
	 */
	void initialize(Logger logger);
	
	/// Standard DB config properties
	String getBackend();
	String getUserName();
	String getPassword();
	
	/**
	 * If file archiveConfig.properties gets used, this maps to <code>archive.tmcdb.connection</code>.
	 */
	String getURL();

	/**
	 * Get property value.
	 * @param name property name (e.g. username and NOT alma.tmcdb.username - it up to plugin to add this prefix)
	 * @return property value.
	 */
	String get(String name, String defaultValue);
}
