package alma.acs.logging.adapters;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.Priority;

import alma.acs.logging.AcsLogger;

/**
 * We intercept the log4j logs at the first level, the Logger itself.
 * It may have been good enough to inject the ACS logger as a log4j appender, 
 * but it seems that only the Logger allows us to implement <code>isDebugEnabled</code>
 * type of methods based on the ACS-configured log levels, 
 * rather than first creating and later dropping a LoggingEvent object.
 */
public class Log4jLogger extends org.apache.log4j.Logger
{

	private final AcsLogger delegate;

	/**
	 * 
	 */
	public Log4jLogger(String name, AcsLogger delegate) {
		super(name);
		this.delegate = delegate;
		delegate.addLoggerClass(Log4jLogger.class);
//		System.out.println("*** Created Log4jAcsLogger ***");
	}

	@Override
	public boolean isTraceEnabled() {
		return delegate.isLoggable(log4jLevelToJdkLevel(Level.TRACE));
	}

	@Override
	public boolean isDebugEnabled() {
		return delegate.isLoggable(log4jLevelToJdkLevel(Level.DEBUG));
	}

	@Override
	public boolean isInfoEnabled() {
		return delegate.isLoggable(log4jLevelToJdkLevel(Level.INFO));
	}

	
	@Override
	protected void forcedLog(String fqcn, Priority level, Object message, Throwable t) {
//		System.out.println("*** Log4jAcsLogger intercepted a log: " + message + " ***");
		delegate.log(log4jLevelToJdkLevel(level), message.toString(), t);
	}

	
	java.util.logging.Level log4jLevelToJdkLevel(Priority level) {
		switch (level.toInt()) {
		case Level.TRACE_INT:
			return java.util.logging.Level.FINEST;

		case Level.DEBUG_INT:
			return java.util.logging.Level.FINER; // or FINE ?

		case Level.INFO_INT:
			return java.util.logging.Level.INFO;

		case Level.FATAL_INT:
			return java.util.logging.Level.SEVERE;
			
		case Level.WARN_INT:
		case Level.ERROR_INT:
		default:
			return java.util.logging.Level.WARNING;
		}
	}
}
