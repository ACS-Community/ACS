package alma.acs.monitoring.blobber;

import java.io.IOException;
import java.util.Hashtable;
import java.util.logging.Logger;

import org.exolab.castor.xml.XMLException;

/**
 * This class contains code that was extracted from BlobberWorker.
 * TODO: Make it pluggable and move this impl outside of ACS, 
 * see http://ictjira.alma.cl/browse/ICT-497.
 * 
 * @author hsommer
 */
class MonitorPointExpert
{
	private final Logger logger;
	private final ACSMonitorPointNameResolver mpResolver;

	public MonitorPointExpert(Logger logger) {
		this.logger = logger;
		mpResolver = new ACSMonitorPointNameResolver();
		try {
			mpResolver.loadMonitorPointFromXML();
		} catch (XMLException ex) {
			ex.printStackTrace();
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}
	
	boolean isUniqueMonitorPoint(String propertyName) {
		Hashtable monitorPointCache = mpResolver.getMonitorPointCache();
		return !monitorPointCache.containsKey(propertyName + "_1");
	}
}
