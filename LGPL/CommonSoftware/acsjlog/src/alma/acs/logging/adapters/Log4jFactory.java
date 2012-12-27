package alma.acs.logging.adapters;

import java.util.HashMap;
import java.util.Map;

import org.apache.log4j.Hierarchy;
import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.DefaultRepositorySelector;
import org.apache.log4j.spi.LoggerFactory;
import org.apache.log4j.spi.RepositorySelector;
import org.apache.log4j.spi.RootLogger;
import org.apache.log4j.varia.NullAppender;

import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;

public class Log4jFactory implements LoggerFactory
{
	/**
	 * Laser alarm system logger name base.
	 */
	public static final String LASER_LOGGER_NAME_PREFIX = "laser";
	

	/**
	 * @see Log4jLogger#enableAcsLogging()
	 */
	private static class MyLog4jHierarchy extends Hierarchy {

		private final LoggerFactory myFactory;
		
		public MyLog4jHierarchy() {
			super(new RootLogger(Level.DEBUG));
			myFactory = new Log4jFactory();
		}

		public Logger getLogger(String name) {
			return getLogger(name, myFactory);
		}
	}

	
	/**
	 * This method must be called once in order to enable ACS logging behind the scenes of log4j logging.
	 * <p>
	 * The log4j framework is quite resistant against being substituted with a different logging framework.
	 * Event though it is possible to configure a custom logger factory using <code>log4j.loggerFactory</code>,
	 * that factory will not be used when 3rd party code calls the usual <code>Logger.getLogger(name)</code>.
	 * It seems to make sense only for cases where the custom logger is used as in <code>MyLogger.getLogger(name)</code>.
	 * log4j-over-slf4j (http://www.slf4j.org/legacy.html) simply re-implements the relevant log4j classes, 
	 * which is too much trouble here for us because only basic log4j features are being used. 
	 * <p>
	 * We make use of the RepositorySelector mechanism, which log4j foresees for a different purpose, 
	 * to separate logging contexts in an application server that does not have classloader separation.
	 * (See also http://articles.qos.ch/sc.html.) 
	 * It is not possible to configure this externally, so that an application must call this method.
	 * See also http://mail-archives.apache.org/mod_mbox/logging-log4j-user/200904.mbox/%3Ca44e15a30904020424g4b7d7fcx63ca32152c81f80d@mail.gmail.com%3E
	 * <p>
	 * @TODO: In the future we could let ClientLogManager call this method, 
	 * but currently we are afraid of side effects with frameworks other than the laser alarm system
	 * that also use log4j (see http://jira.alma.cl/browse/COMP-8423).
	 */
	public static void enableAcsLogging() {
		System.setProperty("log4j.defaultInitOverride", "true");
//		System.setProperty("log4j.debug", "true");

		Hierarchy h = new MyLog4jHierarchy();
		RepositorySelector repositorySelector = new DefaultRepositorySelector(h);
		LogManager.setRepositorySelector(repositorySelector, null);
		
		Logger rootLogger = Logger.getRootLogger();
		rootLogger.removeAllAppenders();
		rootLogger.addAppender(new NullAppender()); // to avoid "log4j:WARN No appenders could be found for logger (root)."
		rootLogger.setLevel(Level.ALL);
	}
	

	/**
	 * key = framework name (e.g. "laser"), value = Logger
	 */
	private final Map<String, Logger> loggerMap = new HashMap<String, Logger>();


	@Override
	public synchronized Logger makeNewLoggerInstance(String name) {
//		System.out.println("*** makeNewLoggerInstance called, name=" + name + " ***");
		
		// Check which framework is requesting the log4j logger
		String loggerNameBase = "unknown";
		StackTraceElement[] stackTrace = (new Exception()).getStackTrace();
		for (StackTraceElement stackTraceElement : stackTrace) {
			if (stackTraceElement.getClassName().contains("cern.laser.")) {
				loggerNameBase = LASER_LOGGER_NAME_PREFIX;
				break;
			}
			// TODO: add check for other frameworks that use log4j, once we have those
		}
		
		// Check if we already have a logger for the client framework, and create it if needed.
		Logger myLogger = loggerMap.get(loggerNameBase);
		if (myLogger == null) {
			AcsLogger delegate = ClientLogManager.getAcsLogManager().getLoggerForCorba(loggerNameBase, true);
			myLogger = new Log4jLogger(loggerNameBase, delegate);
			loggerMap.put(loggerNameBase, myLogger);
		}
		
		return myLogger;
	}

}
