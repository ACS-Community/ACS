package alma.acs.logging.adapters;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogConfigurationException;
import org.apache.commons.logging.impl.Jdk14Logger;
import org.apache.commons.logging.impl.LogFactoryImpl;

import alma.acs.logging.ClientLogManager;

/**
 * Frameworks that use Apache Commons Logging should get this logger factory enabled 
 * via system property <code>org.apache.commons.logging.LogFactory</code>
 * (see {@link org.apache.commons.logging.LogFactory#FACTORY_PROPERTY}).
 * <p>
 * Some frameworks will get recognized by the callstack, and for those logger 
 * requests this factory will return an apache logger that is backed by an ACS-enabled JDK logger.
 * Frameworks that are not recognized will get a normal apache logger.
 * Currently this factory supports and maps:
 * <ul>
 *   <li>Apache SCXML ("org.apache.commons.scxml" packages), logger name "scxml[@processName]"
 * </ul> 
 * <p>
 * The alternative to recognizing frameworks, at the expense of creating and checking the call stack,
 * would be to create a different ACS logger for every requested logger. 
 * This would conflict with the concept of ACS that logger names should represent components or other 
 * pieces of code, and not the names of implementation classes that request the loggers.
 * 
 * @author hsommer
 * @since ACS 10.2
 */
public class CommonsLoggingFactory extends LogFactoryImpl
{
	
	/**
	 * SCXML logger name base.
	 */
	public static final String SCXML_LOGGER_NAME_PREFIX = "scxml";
	

	private final Map<String, Log> loggerMap = new HashMap<String, Log>();

	@Override
	protected synchronized Log newInstance(String name) throws LogConfigurationException {
//		System.out.println("CommonsLoggingFactoryForACS#newInstance called for name=" + name);

		// Check which framework is requesting the apache commons logger
		String loggerNameBase = null;
		StackTraceElement[] stackTrace = (new Exception()).getStackTrace();
		for (StackTraceElement stackTraceElement : stackTrace) {
			if (stackTraceElement.getClassName().contains("org.apache.commons.scxml")) {
				loggerNameBase = SCXML_LOGGER_NAME_PREFIX;
				break;
			}
			// TODO: add check for other frameworks that use apache logging commons, once we have those
		}
		
		// Give up if we did not recognize the client framework
		if (loggerNameBase == null) {
			return super.newInstance(name);
		}
		
		// Check if we already have a logger for the client framework, and create it if needed.
		Log myLog = loggerMap.get(loggerNameBase);
		if (myLog == null) {
			final String loggerNameBaseFinal = loggerNameBase;
			myLog = new Jdk14Logger(loggerNameBase) {
				@Override
				public Logger getLogger() {
					// note that getLoggerForCorba implements a hack that assigns WARNING log levels 
					// to some known framework loggers, in the absence of other specific log configuration.
					return ClientLogManager.getAcsLogManager().getLoggerForCorba(loggerNameBaseFinal, true);
				}
			};
			loggerMap.put(loggerNameBase, myLog);
		}
		
		return myLog;
	}

}
