package si.ijs.acs.objectexplorer.engine.BACI;

import java.util.logging.Logger;

import alma.acs.logging.ClientLogManager;

/**
 * Logger factory.
 * @author msekoranja
 */
public class BACILogger {

	private static Logger logger = null;
	
	/**
	 * Get logger for objexp application.
	 * @return	application logger.
	 */
	public static synchronized Logger getLogger()
	{
		if (logger == null)
		{
			logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("objexp", true);
		}
		
		if (logger == null)
		{
			System.err.println("BACILogger::getLogger()\tFailed to get application logger, using global.");
			return Logger.global;
		}
		else
			return logger;
	}
}
