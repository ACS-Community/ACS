package alma.acs.gui.loglevel;

import com.cosylab.logging.engine.log.LogTypeHelper;

import si.ijs.maci.LoggingConfigurableOperations;
import si.ijs.maci.LoggingConfigurablePackage.LogLevels;

import alma.ACSErrTypeCommon.IllegalArgumentEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
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
	
	public LoggingConfigurableLevelTranslator(LoggingConfigurableOperations delegate) {
		this.delegate = delegate;
	}
	
	
	short getAcsLevel(short indexBasedLevel) {
		return (short) LogTypeHelper.getAcsCoreLevel(new Integer(indexBasedLevel));
	}
	
	short getIndexBasedLevel(int acsLevel) throws AcsJIllegalArgumentEx {
		return LogTypeHelper.getIndexBasedLevel(acsLevel).shortValue();
	}
	
	/////////////////////////////////
	
	public LogLevels get_default_logLevels() {
		LogLevels realLevels = delegate.get_default_logLevels();
		try {
			LogLevels indexLevels = new LogLevels(
					realLevels.useDefault, 
					getIndexBasedLevel(realLevels.minLogLevel), 
					getIndexBasedLevel(realLevels.minLogLevelLocal) );
			return indexLevels;
		} catch (AcsJIllegalArgumentEx ex) {
			// should never happen 
			ex.printStackTrace();
			throw new IllegalArgumentException(ex);
		}
	}

	public String[] get_logger_names() {
		return delegate.get_logger_names();
	}

	public LogLevels get_logLevels(String logger_name) throws LoggerDoesNotExistEx {
		LogLevels realLevels = delegate.get_logLevels(logger_name);
		try {
			LogLevels indexLevels = new LogLevels(
					realLevels.useDefault, 
					getIndexBasedLevel(realLevels.minLogLevel), 
					getIndexBasedLevel(realLevels.minLogLevelLocal) );
			return indexLevels;
		} catch (AcsJIllegalArgumentEx ex) {
			// should never happen 
			ex.printStackTrace();
			throw new IllegalArgumentException(ex);
		}
	}

	public void refresh_logging_config() {
		delegate.refresh_logging_config();
	}

	public void set_default_logLevels(LogLevels indexLevels) throws IllegalArgumentEx {
		LogLevels realLevels = new LogLevels(
				indexLevels.useDefault, 
				getAcsLevel(indexLevels.minLogLevel), 
				getAcsLevel(indexLevels.minLogLevelLocal) );
		delegate.set_default_logLevels(realLevels);
	}

	public void set_logLevels(String logger_name, LogLevels indexLevels) throws LoggerDoesNotExistEx, IllegalArgumentEx {
		LogLevels realLevels = new LogLevels(
				indexLevels.useDefault, 
				getAcsLevel(indexLevels.minLogLevel), 
				getAcsLevel(indexLevels.minLogLevelLocal) );
		delegate.set_logLevels(logger_name, realLevels);
	}
}
