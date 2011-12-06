/*
 * ALMA - Atacama Large Millimiter Array
 * Copyright (c) European Southern Observatory, 2011 
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
package alma.acs.monitoring.blobber;

import java.util.List;
import java.util.logging.Logger;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntCreateObjectEx;
import alma.acs.container.ContainerServices;
import alma.acs.monitoring.DAO.MonitorDAO;

/**
 * Provides code to the blobber which builds after ACS (somewhere in <code>ARCHIVE/TMCDB/</code>).
 */
public abstract class BlobberPlugin
{
	protected final ContainerServices containerServices;
	protected final Logger logger;

	public BlobberPlugin(ContainerServices containerServices) {
		this.containerServices = containerServices;
		this.logger = containerServices.getLogger();
	}
	
	/**
	 * Reads profiling setting, possibly from class <code>TMCDBConfig</code>.
	 */
	public abstract boolean isProfilingEnabled();
	
	/**
	 * Gets the collector interval, possibly from class <code>TMCDBConfig</code>.
	 * <p>
	 * @TODO: Configurable collector intervals were introduced on the ACS-9_0_0-B  branch 
	 * in ARCHIVE/TMCDB/MonitorArchiver but not ported to the HEAD. 
	 * We include this feature in moving the monitoring framework code to ACS HEAD, 
	 * but it is unclear if it will be needed in the long term.
	 * 
	 * @return The interval in seconds for the blobber to read data from the monitor collectors. 
	 */
	public abstract int getCollectorIntervalSec();
	
	/**
	 * Will be called by the blobber component layer before calling {@link #getMonitorDAOs()}
	 * or {@link #getBlobberWatchDog()}, so that the plugin can create and install the DAOs and watchdog 
	 * or do other initialization tasks.
	 * @see #cleanUp()
	 */
	public abstract void init() throws AcsJCouldntCreateObjectEx;

	/**
	 * Will be called by the blobber component as part of component cleanUp. 
	 * The plugin should stop the DAOs, watchdog, and do other cleanup tasks.
	 * @see #init()
	 */
	public abstract void cleanUp();
	
    /**
     * The entire DAO implementation is in <code>ARCHIVE/TMCDB/DAO/</code>
     * which is why we defer creation of <code>alma.archive.tmcdb.DAO.MonitorDAOImpl</code>
     * or some mock implementation of the <code>MonitorDAO</code> interface to this plugin. 
     * <p>
     * Normally only one DAO object should be returned, but for exceptional cases we support running
     * several DAOs in parallel. This allows the Alma OSF to independently write the monitor data to the TMCDB
     * and also stream it via activeMQ to files on a web server (a workaround which will be removed in the future). 
     * <p>
     * The DAO object(s) must handle buffering and throwing away of data internally, so that the blobber
     * component does not get stuck when calling the DAO layer.
     */
	public abstract List<MonitorDAO> getMonitorDAOs();
	
	
	/**
	 * @return The watchdog object that also gets used by the DAO object(s).
	 */
	public abstract BlobberWatchDog getBlobberWatchDog();
}
