package alma.acs.monitoring.blobber;

import java.util.logging.Logger;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntCreateObjectEx;
import alma.acs.monitoring.DAO.MonitorDAO;

/**
 * Provides code to the blobber which builds after ACS (somewhere in <code>ARCHIVE/TMCDB/</code>).
 */
public abstract class BlobberPlugin
{
	protected final Logger logger;

	public BlobberPlugin(Logger logger) {
		this.logger = logger;
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
     * The entire DAO implementation is in <code>ARCHIVE/TMCDB/DAO/</code>
     * which is why we defer creation of <code>alma.archive.tmcdb.DAO.MonitorDAOImpl</code>
     * or some mock implementation of the <code>MonitorDAO</code> interface to this plugin. 
     */
	public abstract MonitorDAO createMonitorDAO() throws AcsJCouldntCreateObjectEx;
}
