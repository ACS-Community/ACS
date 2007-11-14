package alma.acs.gui.loglevel;

import com.cosylab.logging.engine.log.LogTypeHelper;

import si.ijs.maci.LoggingConfigurableOperations;
import si.ijs.maci.LoggingConfigurablePackage.LogLevels;

import alma.acs.logging.ACSCoreLevel;
import alma.maciErrType.LoggerDoesNotExistEx;

/**
 * This class translates between ACS log levels as used in the 
 * {@link LoggingConfigurableOperations} interface and the 
 * 0-index based log levels that this GUI reuses from jlog (com.cosylab.logging.engine.log.LogTypeHelper).
 * <p>
 * It is a release pressure hack. We really should define in IDL what the 
 * ACS log levels are, and have one central utility class per language.
 * Jlog and logLevelGUI should not use index based levels internally.
 */
public class LoggingConfigurableLevelTranslator implements LoggingConfigurableOperations
{

	private final LoggingConfigurableOperations delegate;

    public static final Integer ENTRYTYPE_TRACE = new Integer(0);
    public static final Integer ENTRYTYPE_DEBUG = new Integer(1);
    public static final Integer ENTRYTYPE_INFO = new Integer(2);
    public static final Integer ENTRYTYPE_NOTICE = new Integer(3);
    public static final Integer ENTRYTYPE_WARNING = new Integer(4);
    public static final Integer ENTRYTYPE_ERROR = new Integer(5);
    public static final Integer ENTRYTYPE_CRITICAL = new Integer(6);
    public static final Integer ENTRYTYPE_ALERT = new Integer(7);
    public static final Integer ENTRYTYPE_EMERGENCY = new Integer(8);

    
	private static int[] acsLevels = {
		ACSCoreLevel.ACS_LEVEL_TRACE, 
		ACSCoreLevel.ACS_LEVEL_DEBUG, 
		ACSCoreLevel.ACS_LEVEL_INFO, 
		ACSCoreLevel.ACS_LEVEL_NOTICE, 
		ACSCoreLevel.ACS_LEVEL_WARNING, 
		ACSCoreLevel.ACS_LEVEL_ERROR, 
		ACSCoreLevel.ACS_LEVEL_CRITICAL, 
		ACSCoreLevel.ACS_LEVEL_ALERT,
		ACSCoreLevel.ACS_LEVEL_EMERGENCY };
	
	
	public LoggingConfigurableLevelTranslator(LoggingConfigurableOperations delegate) {
		this.delegate = delegate;
	}
	
	short getIndexBasedLevel(int acsLevel) {
		switch (acsLevel) {
		case ACSCoreLevel.ACS_LEVEL_ALL:
			return LogTypeHelper.ENTRYTYPE_TRACE.shortValue();
			
		case ACSCoreLevel.ACS_LEVEL_TRACE:
			return LogTypeHelper.ENTRYTYPE_TRACE.shortValue();
			
		case ACSCoreLevel.ACS_LEVEL_DEBUG:
			return LogTypeHelper.ENTRYTYPE_DEBUG.shortValue();
			
		case ACSCoreLevel.ACS_LEVEL_INFO:
			return LogTypeHelper.ENTRYTYPE_INFO.shortValue();
			
		case ACSCoreLevel.ACS_LEVEL_NOTICE:
			return LogTypeHelper.ENTRYTYPE_NOTICE.shortValue();
			
		case ACSCoreLevel.ACS_LEVEL_WARNING:
			return LogTypeHelper.ENTRYTYPE_WARNING.shortValue();

		case ACSCoreLevel.ACS_LEVEL_ERROR:
			return LogTypeHelper.ENTRYTYPE_ERROR.shortValue();

		case ACSCoreLevel.ACS_LEVEL_CRITICAL:
			return LogTypeHelper.ENTRYTYPE_CRITICAL.shortValue();

		case ACSCoreLevel.ACS_LEVEL_ALERT:
			return LogTypeHelper.ENTRYTYPE_ALERT.shortValue();

		case ACSCoreLevel.ACS_LEVEL_EMERGENCY:
			return LogTypeHelper.ENTRYTYPE_EMERGENCY.shortValue();

		default:
			throw new IllegalArgumentException("Illegal ACS log level " + acsLevel);
		}
	}

	
	short getAcsLevel(short indexBasedLevel) {
		if (indexBasedLevel >= 0 && indexBasedLevel < acsLevels.length) {
			return (short) acsLevels[indexBasedLevel];
		}
		else {
			throw new IllegalArgumentException("Illegal index based level " + indexBasedLevel);
		}
	}
	
	/////////////////////////////////
	
	public LogLevels get_default_logLevels() {
		LogLevels realLevels = delegate.get_default_logLevels();
		LogLevels indexLevels = new LogLevels(
					realLevels.useDefault, 
					getIndexBasedLevel(realLevels.minLogLevel), 
					getIndexBasedLevel(realLevels.minLogLevelLocal) );
		return indexLevels;
	}

	public String[] get_logger_names() {
		return delegate.get_logger_names();
	}

	public LogLevels get_logLevels(String logger_name) throws LoggerDoesNotExistEx {
		LogLevels realLevels = delegate.get_logLevels(logger_name);
		LogLevels indexLevels = new LogLevels(
					realLevels.useDefault, 
					getIndexBasedLevel(realLevels.minLogLevel), 
					getIndexBasedLevel(realLevels.minLogLevelLocal) );
		return indexLevels;
	}

	public void refresh_logging_config() {
		delegate.refresh_logging_config();
	}

	public void set_default_logLevels(LogLevels indexLevels) {
		LogLevels realLevels = new LogLevels(
				indexLevels.useDefault, 
				getAcsLevel(indexLevels.minLogLevel), 
				getAcsLevel(indexLevels.minLogLevelLocal) );
		delegate.set_default_logLevels(realLevels);
	}

	public void set_logLevels(String logger_name, LogLevels indexLevels) throws LoggerDoesNotExistEx {
		LogLevels realLevels = new LogLevels(
				indexLevels.useDefault, 
				getAcsLevel(indexLevels.minLogLevel), 
				getAcsLevel(indexLevels.minLogLevelLocal) );
		delegate.set_logLevels(logger_name, realLevels);
	}
}
