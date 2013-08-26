package alma.acs.logging;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import alma.acs.logging.config.LogConfig;
import alma.acs.logging.formatters.AcsXMLLogFormatter;

/**
 * An <code>AcsLogger</code> that prints to stdout the XML messages that normally
 * would be sent to the remote Log service.
 * <p>
 * The purpose of this class is to allow testing AcsLogger functionality
 * without the need for a running ACS infrastructure, which would typically be accessed using 
 * the class ComponentClient that only builds in a later module (jcont).
 *  
 * @author hsommer
 */
public class LocalOnlyAcsLogger extends AcsLogger {

	private static LocalOnlyAcsLogger instance;
	
	private LocalOnlyAcsLogger(String namespace, LogConfig testLogConfig) {
		super(namespace, null, testLogConfig);
		addLoggerClass(LocalOnlyAcsLogger.class); 
	}

	public static synchronized LocalOnlyAcsLogger getInstance(String namespace, Level level) {
		if (instance == null) {
			LogConfig testLogConfig = new LogConfig();
			testLogConfig.setDefaultMinLogLevelLocal(AcsLogLevel.getNativeLevel(level).getAcsLevel());
			instance = new LocalOnlyAcsLogger(namespace, testLogConfig);
			instance.setUseParentHandlers(false);
			Handler logHandler = new StdOutConsoleHandler(testLogConfig, namespace, null);
			logHandler.setFormatter(new AcsXMLLogFormatter() {
				public String format(LogRecord lr) {
					String xml = super.format(lr);
					return xml + '\n';
				}
			});
			logHandler.setLevel(level);
			instance.addHandler(logHandler);
		}
		return instance;
		
	}
}
