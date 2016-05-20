/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2016 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.xmlfilestore.alarm.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;

import alma.xmlFileStore.AlarmsXmlStoreOperations;


/** 
 * @author  acaproni
 * @since   ACS 2016.6
 */

public class XmlFileStoreAlarmLoggerImpl extends ComponentImplBase implements AlarmsXmlStoreOperations {
	
	/**
	 * Container services
	 */
	private ContainerServices cs;
	
	/**
	 * The logger
	 */
	private Logger m_logger;
	
	/**
	 *  Constructor
	 */
	public XmlFileStoreAlarmLoggerImpl() {
		super();
	}
	
	/**
	 * Life cycle
	 * @see alma.acs.component.ComponentLifecycle#initialize()
	 */
	@Override
	public void initialize(ContainerServices containerServices)
			throws ComponentLifecycleException {
		super.initialize(containerServices);
		cs = containerServices;
		m_logger = cs.getLogger();
	}
	
	/**
	 * Life cycle
	 * @see alma.acs.component.ComponentLifecycle#cleanUp()
	 */
	@Override
	public void cleanUp() throws alma.maciErrType.wrappers.AcsJComponentCleanUpEx {
		super.cleanUp();
		if (m_logger.isLoggable(Level.FINE)) m_logger.fine("cleaning up");
	}
}
