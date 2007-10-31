/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.logging.config;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.exolab.castor.core.exceptions.CastorException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALOperations;

import alma.cdbErrType.CDBRecordDoesNotExistEx;
import alma.cdbErrType.CDBXMLErrorEx;
import alma.maci.loggingconfig.LoggingConfig;
import alma.maci.loggingconfig.NamedLogger;
import alma.maci.loggingconfig.UnnamedLogger;

/**
 * Class that encapsulates all configuration sources (defaults, properties, CDB) for Java logging,
 * and supports runtime updates of logging configuration based on these sources.
 * This class can be used by a process such as the Java container or the manager, 
 * in order to configure all its loggers (e.g. container logger, component loggers, ORB logger).
 * @author hsommer
 */
public class LogConfig {

	// for the often-used min levels we offer properties, to avoid having an otherwise not needed CDB configuration 
	public final static String PROPERTYNAME_MIN_LOG_LEVEL_LOCAL = "ACS.logstdout"; // todo: rename to "ACS.log.minlevel.local"
	public final static String PROPERTYNAME_MIN_LOG_LEVEL = "ACS.log.minlevel.remote";

	
    /**
     * The logger for messages logged by this class.
     * Note that this field will be null at first, until the Logger gets provided, 
     * so better use {@link #log(Level, String, Throwable)} for safe access. 
     */
	private Logger logger;

    /**
     * The ACS CDB. This value is null unless it gets set in {@link #setCDB(DAL)}.
     */
    private DALOperations cdb;
    
    /** 
     * Path in the CDB to the element (container, manager etc) that contains a <code>LoggingConfig</code> child.
     * From here we get process-wide settings, default log levels, and optional named logger levels.
     * Note that named logger levels found here override those in {@link #cdbComponentPaths} in case of conflict. 
     */
    private String cdbLoggingConfigPath;

    /**
     * Paths in the CDB to the element (container, manager etc) that contains a <code>LoggingConfig</code> child.
     * key = component logger name, value = path in the CDB to the Component configuration.
     * Note that log levels given in the component configuration get overridden by named logger levels 
     * found in the container configuration, in case of conflicts. 
     * <p>
     * This only applies to container log config, since for example the manager logging config does not deal with components.
     */
    private Map<String, String> cdbComponentPaths;
    
    /**
     * The schema-generated logging config, either with default values, or read from the CDB.
     * The log levels can also be changed by properties (env var peers) and dynamically. 
     */
    private LoggingConfig loggingConfig;

	/**
	 * Logger names and optionally associated log levels.
	 * Loggers that use default levels are stored with their name (map key), 
	 * but a null value for the configuration is associated with them. 
	 * <p>
	 * Actual named logger configurations are stored as the map values, and can come from 
	 * <ul>
	 * <li>optional named logger children in the process configuration ({@link #loggingConfig}),
	 *     which are of subclass {@link NamedLogger}, or
	 * <li>optional log config children of CDB component configuration, or
	 * <li>dynamically set values that can create or override the above, see {@link #setNamedLoggerConfig(String, UnnamedLogger)}.
	 * </ul>
	 * 
	 * key = [String] logger name, value = [LockableUnnamedLogger or null] level config
	 */
	private Map<String, LockableUnnamedLogger> namedLoggerConfigs;

	/**
	 * Subscribers get notified of logging config changes
	 */
	private List<LogConfigSubscriber> subscriberList;


	
	public LogConfig() {
		cdbComponentPaths = new HashMap<String, String>();
		namedLoggerConfigs = new HashMap<String, LockableUnnamedLogger>();
		subscriberList = new ArrayList<LogConfigSubscriber>();
		loggingConfig = new LoggingConfig(); // comes with schema default values

		configureDefaultLevelsFromProperties();
	}


	/**
	 * Reads the properties <code>ACS.logstdout</code> (name defined as {@link #PROPERTYNAME_MIN_LOG_LEVEL}) and
	 * <code>ACS.log.minlevel.remote</code> (name defined as {@link #PROPERTYNAME_MIN_LOG_LEVEL_LOCAL}) and, if the
	 * property is defined, sets the respective default level. Prior values are lost.
	 */
	private void configureDefaultLevelsFromProperties() {
		// env vars / properties can override the schema defaults
		Integer minLevelLocalValue = Integer.getInteger(PROPERTYNAME_MIN_LOG_LEVEL_LOCAL);
		if (minLevelLocalValue != null) {
			loggingConfig.setMinLogLevelLocal(minLevelLocalValue.intValue());
		}
		Integer minLevelValue = Integer.getInteger(PROPERTYNAME_MIN_LOG_LEVEL);
		if (minLevelValue != null) {
			loggingConfig.setMinLogLevel(minLevelValue.intValue());
		}
	}

	/**
	 * Sets the reference to the CDB, which will then be used for configuration.
	 * Before this method is called, default values are used instead of CDB
	 * values.
	 * <p>
	 * Note that the reference is only set, but the CDB is not read
	 * automatically; for this, call {@link #initialize(boolean)}.
	 * 
	 * @param cdb  a reference to the CDB. Will be ignored if == null.
	 */
	public void setCDB(DALOperations cdb) {
		if (cdb != null) {
			this.cdb = cdb;
		} else {
			log(Level.FINE, "Ignoring call to setCDB(null)", null);
		}
	}

	/**
	 * Sets the path to the CDB's node that is <em>parent of</em> the
	 * &lt;LoggingConfig&gt; node, such as a container or the manager node.
	 * This call does not access the CDB.
	 * 
	 * @param path  for example "MACI/Containers/frodoContainer"
	 */
	public void setCDBLoggingConfigPath(String path) {
		cdbLoggingConfigPath = path;
	}

	/**
	 * Sets the path to the CDB's node that is <em>parent of</em> the
	 * &lt;log:UnnamedLogger/&gt; component logger node. 
	 * This call does not access the CDB.
	 * 
	 * @param compLoggerName
	 * @param path
	 */
	public void setCDBComponentPath(String compLoggerName, String path) {
		cdbComponentPaths.put(compLoggerName, path);
	}
    
    
	/**
	 * Initializes the values based on CDB settings, logging properties, etc.
	 * All subscribing classes are notified of the new configuration, see
	 * {@link LogConfigSubscriber#configureLogging(LogConfig)}.
	 * <p>
	 * This method can be called more than once: if some settings have changed,
	 * should be read in, and the logging classes should be notified of these
	 * changes. For example, the container could call this method when it gets
	 * notified that the logging configuration in the CDB has been changed at
	 * runtime.
	 * 
	 * @param cdbBeatsProperties
	 *            if true then the default logger level values from the CDB override the properties (env vars). 
	 *            True is foreseen for dynamic updates from the CDB, whereas for the initial configuration it should be a "false".
	 * 
	 * @throws LogConfigException
	 *             if reading the configuration data failed and thus default values were used, or if there were problems during
	 *             configuration even though some of the configuring went ok (best-effort approach).
	 */
	public void initialize(boolean cdbBeatsProperties) throws LogConfigException {
		StringBuffer errMsg = new StringBuffer();

		LoggingConfig newLoggingConfig = null; // castor class, generated from LogggingConfig.xsd
		if (cdb != null) {
			try {
				if (cdbLoggingConfigPath != null) {
					String loggingConfigXml = getLogConfigXml(cdbLoggingConfigPath, "LoggingConfig"); // todo derive from LoggingConfig.class.getName()
					if (loggingConfigXml == null) {
						// the LoggingConfig child is mandatory for containers and manager
						throw new LogConfigException("Parent node " + cdbLoggingConfigPath + " does not contain one LoggingConfig element.");
					}
					newLoggingConfig = LoggingConfig.unmarshalLoggingConfig(new StringReader(loggingConfigXml));
				} 
				else {
					errMsg.append("CDB reference was set, but not the path to the logging configuration. ");
				}
				if (newLoggingConfig != null) {
					loggingConfig = newLoggingConfig;
					
					// named logger configs under LoggingConfig we process right away, while separate configs (component loggers) we do separately
					
					// @TODO: check if we really want to lose named logger settings that had been added dynamically before this refresh from CDB.
					// If not, then we must dinstinguish between dynamic API and from-CDB config, and leave those objects that have no CDB-equivalent.

					// We don't call namedLoggerConfigs.clear() because we don't want to lose logger names but only their configurations.
					synchronized (namedLoggerConfigs) {
						for (String loggerName : namedLoggerConfigs.keySet()) {							
							storeNamedLoggerConfigs(loggerName, null);
						}
						
						// named logger levels from the LoggingConfig XML
						NamedLogger[] namedLoggers = loggingConfig.get();
						for (int i = 0; i < namedLoggers.length; i++) {
							storeNamedLoggerConfigs(namedLoggers[i].getName(), new LockableUnnamedLogger(namedLoggers[i]));
						}
						
						// named logger levels from separate component config
						for (String loggerName : cdbComponentPaths.keySet()) {
							// skip named logger if it's been already configured from the main XML, since those values have precedence
							if (!namedLoggerConfigs.containsKey(loggerName)) {
								String cdbPath = cdbComponentPaths.get(loggerName);
								String componentConfigXML = getLogConfigXml(cdbPath, "ComponentLogger");
								// the ComponentLogger xml child element is optional, we get a null if it's missing.
								if (componentConfigXML != null) {
									UnnamedLogger compLoggerConfig = UnnamedLogger.unmarshalUnnamedLogger(new StringReader(componentConfigXML));
									storeNamedLoggerConfigs(loggerName, new LockableUnnamedLogger(compLoggerConfig));
								}
							}
						}
					}
				} 
				else {
					throw new LogConfigException("LoggingConfig binding class obtained from CDB node '" + cdbLoggingConfigPath + "' was null.");
				}
			} catch (CDBXMLErrorEx ex) {
				errMsg.append("Failed to read node " + cdbLoggingConfigPath + " from the CDB (msg='"
						+ ex.errorTrace.shortDescription + "'). ");
			} catch (CDBRecordDoesNotExistEx ex) {
				errMsg.append("Node " + cdbLoggingConfigPath + " does not exist in the CDB (msg='"
						+ ex.errorTrace.shortDescription + "'). ");
			} catch (CastorException ex) {
				errMsg.append("Failed to parse XML for CDB node " + cdbLoggingConfigPath + " into binding classes (ex="
						+ ex.getClass().getName() + ", msg='" + ex.getMessage() + "'). ");
			} catch (Throwable thr) {
				errMsg.append("Failed to read node " + cdbLoggingConfigPath + " from the CDB (ex=" + thr.getClass().getName() + ", msg='" + thr.getMessage() + "'). ");
			}
		}

		if (!cdbBeatsProperties) {
			configureDefaultLevelsFromProperties();
		}

		notifySubscribers();

		// now that the subscribers had a chance to adjust their log levels according to the changes from the CDB (if
		// present), we can publish a trace log
		if (newLoggingConfig != null) {
			StringWriter writer = new StringWriter();
			String newXML = null;
			try {
				newLoggingConfig.marshal(writer);
				newXML = writer.toString();
			} catch (Throwable thr) {
				; // nothing
			}
			log(Level.FINER, "Updated default logging configuration based on CDB entry " + newXML, null);
			// @TODO: also log something for named loggers 
		} 
		else {
			log(Level.FINER, "Logging configuration has been initialized, but not from CDB settings.", null);
		}

		if (errMsg.length() > 0) {
			throw new LogConfigException("Log config initialization at least partially failed. " + errMsg.toString());
		}
	}

	
	
	/**
	 * Reads the CDB element from the given path as XML, 
	 * and extracts the child element of name <code>childName</code>.
	 * 
	 * @param cdbPathParent  path to container, manager, or component config node
	 * @param childName  "LoggingConfig" in case of manager or container, or "ComponentLogger" for a component node.
	 * @return XML String for the requested child, or <code>null</code> if no unique xml child element <code>childName</code> was found.
	 * @throws IllegalStateException if the cdb ref has not been set
	 */
	private String getLogConfigXml(String cdbPathParent, String childName) 
			throws CDBXMLErrorEx, CDBRecordDoesNotExistEx, ParserConfigurationException, SAXException, IOException, TransformerException 
	{
		if (cdb == null) {
			throw new IllegalStateException("CDB reference has not been set.");
		}
		String parentConfigXML = cdb.get_DAO(cdbPathParent);

		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		DocumentBuilder builder = factory.newDocumentBuilder();
		Document parentDoc = builder.parse(new InputSource(new StringReader(parentConfigXML)));

		Element parentDocElement = parentDoc.getDocumentElement();
		NodeList loggingConfigElements = parentDocElement.getElementsByTagName(childName);
		if (loggingConfigElements.getLength() != 1) {
//			throw new LogConfigException("Parent node does not contain one LoggingConfig element.");
			return null;
		}

		Node loggingConfigElement = loggingConfigElements.item(0);
		Document childDoc = builder.newDocument();
		loggingConfigElement = childDoc.importNode(loggingConfigElement, true);
		childDoc.appendChild(loggingConfigElement);

		TransformerFactory transformerFactory = TransformerFactory.newInstance();
		Transformer transformer = transformerFactory.newTransformer();
		StringWriter sw = new StringWriter();
		StreamResult result = new StreamResult(sw);
		transformer.transform(new DOMSource(childDoc), result);

		return sw.toString();
		
	}
	
    /**
     * Gets the logging configuration data that contains all configuration values that apply to the entire process,
     * ass well as the default log levels.
     * <p> 
     * Be careful to not modify this object!  
     * TODO: better return a cloned or wrapped instance
     */
    public LoggingConfig getLoggingConfig() {
        return this.loggingConfig;
    }



	
	///////////////////////////////////////////////////////////////////////////////////////////////////
	// Getter and setter methods for default logger and named logger levels, local and remote logging.
	///////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Gets the log level for stdout printing for default loggers 
	 * (those that don't have custom levels set).
	 */
	public int getDefaultMinLogLevelLocal() {
		return loggingConfig.getMinLogLevelLocal();
	}

	/**
	 * Sets the given log level for stdout printing of log records
	 * for all loggers that don't have a custom configuration. 
	 * All log config listeners.get notified of this change.
	 * The call is ignored for negative values of <code>newLevel</code>.
	 * @param newLevel
	 */
	public void setDefaultMinLogLevelLocal(int newLevel) {
		if (newLevel >= 0) {
			loggingConfig.setMinLogLevelLocal(newLevel);
			notifySubscribers();
		}
	}

	/**
	 * Gets the log level for centralized logging for default loggers 
	 * (those that don't have custom levels set).
	 */
	public int getDefaultMinLogLevel() {
		return loggingConfig.getMinLogLevel();
	}

	/**
	 * Sets the given log level for centralized logging of log records
	 * for all loggers that don't have a custom configuration. 
	 * All log config listeners.get notified of this change.
	 * The call is ignored for negative values of <code>newLevel</code>.
	 * @param newLevel
	 */
	public void setDefaultMinLogLevel(int newLevel) {
		if (newLevel >= 0) {
			loggingConfig.setMinLogLevel(newLevel);
			notifySubscribers();
		}
	}

	/**
	 * Gets the names of all known loggers, no matter if they use default or custom levels.
	 * <p>
	 * The loggers are registered automatically by calls to 
	 * {@link #getNamedLoggerConfig(String)} and {@link #setNamedLoggerConfig(String, UnnamedLogger)}.
	 */
	public Set<String> getLoggerNames() {
		synchronized (namedLoggerConfigs) { // to avoid map change during copy-ctor
			return new HashSet<String>(namedLoggerConfigs.keySet());
		}
	}

	public boolean hasCustomConfig(String loggerName) {
		return (namedLoggerConfigs.get(loggerName) != null);
	}

	/**
	 * Gets the (log level) configuration data for a named logger.
	 * Resorts to the default configuration if a specialized configuration is not available. 
	 * <p>
	 * Note that a copy of the config data is returned, so changes to it will
	 * not affect any other object's configuration.
	 * <p>
	 * A previously unknown logger gets registered by its name.
	 */
	public LockableUnnamedLogger getNamedLoggerConfig(String loggerName) {
		
		LockableUnnamedLogger ret = new LockableUnnamedLogger();
		
		synchronized (namedLoggerConfigs) {
			if (loggerName == null || loggerName.toLowerCase().equals("default") || namedLoggerConfigs.get(loggerName)==null) {
				ret.setMinLogLevel(loggingConfig.getMinLogLevel());
				ret.setMinLogLevelLocal(loggingConfig.getMinLogLevelLocal());
				if (loggerName != null && !loggerName.toLowerCase().equals("default")) {
					storeNamedLoggerConfigs(loggerName, null); // null means "apply default values"
				}
			}
			else {
				ret = new LockableUnnamedLogger(namedLoggerConfigs.get(loggerName));
			}
		}		
		// @TODO: resolve the int / short mismatch between xsd and idl definitions
//		ret.setMinLogLevel(Math.min(ret.getMinLogLevel(), Short.MAX_VALUE));
//		ret.setMinLogLevelLocal(Math.min(ret.getMinLogLevelLocal(), Short.MAX_VALUE));
		
		return ret;
	}
	
	/**
	 * Sets the given log levels for the named logger and notifies all listeners. 
	 * Ignores this call if any of the parameters are <code>null</code>.
	 * <p>
	 * A copy of the supplied <code>config</code> is made
	 * to isolate the stored data from later modifications.
	 */
	public void setNamedLoggerConfig(String loggerName, LockableUnnamedLogger config) {
		if (loggerName != null && config != null) {
			LockableUnnamedLogger config2 = new LockableUnnamedLogger(config);
			storeNamedLoggerConfigs(loggerName, config2);
			notifySubscribers();
		}
	}

	/**
	 * We keep this method next to {@link #setNamedLoggerConfig(String, alma.acs.logging.config.LogConfig.LockableUnnamedLogger)}
	 * in order to keep the LockableUnnamedLogger confined to this class. It may be exposed later, 
	 * and this method could then be removed.
	 * @param loggerName
	 * @param config
	 * @since ACS 7.0
	 */
	public void setNamedLoggerConfig(String loggerName, UnnamedLogger config) {
		LockableUnnamedLogger config2 = new LockableUnnamedLogger(config); // unlocked by default
		setNamedLoggerConfig(loggerName, config2);
	}
	
	
	/**
	 * Clears log level settings for the given named logger,
	 * so that it uses default log levels. 
	 * Notifies all listeners.
	 * Ignores this call if <code>loggerName</code> is <code>null</code>.
	 */
	public void clearNamedLoggerConfig(String loggerName) {
		if (loggerName != null) {
			storeNamedLoggerConfigs(loggerName, null);
			notifySubscribers();
		}
	}

	public void setMinLogLevelLocal(int newLevel, String loggerName) {
		LockableUnnamedLogger config = getNamedLoggerConfig(loggerName);  // new object, with cached or default values
		config.setMinLogLevelLocal(newLevel);
		setNamedLoggerConfig(loggerName, config);
	}

	public void setMinLogLevel(int newLevel, String loggerName) {
		LockableUnnamedLogger config = getNamedLoggerConfig(loggerName); // new object, with cached or default values
		config.setMinLogLevel(newLevel);
		setNamedLoggerConfig(loggerName, config);
	}

	/**
	 * Locking a remote log level is currently needed only for the container to ensure that the ORB logger of the container in which 
	 * the archive logger component runs is guaranteed to not produce any logs 
	 * (which would lead to log record explosion through positive feedback). 
	 * <p>
	 * The more abstract concept of locking log levels was chosen to keep the special scenario decribed above 
	 * out of the logging config code.
	 * 
	 * @param newLevel
	 * @param loggerName
	 */
	public void setAndLockMinLogLevel(int newLevel, String loggerName) {
		synchronized (namedLoggerConfigs) {	
			LockableUnnamedLogger config = getNamedLoggerConfig(loggerName); // new object, with cached or default values
			if (config.isLocked()) {
				logger.warning("Ignoring attempt to lock logger " + loggerName + " to level " + newLevel + " because it is already locked to remote level " + config.getMinLogLevel());
			}
			else {
				config.setMinLogLevel(newLevel);
				config.lock();
				setNamedLoggerConfig(loggerName, config);
			}
		}
	}

	/**
	 * Method that guards <code>namedLoggerConfigs.put(loggerName, config)</code>
	 * and denies access if the old config is locked.
	 * @see #namedLoggerConfigs
	 */
	private void storeNamedLoggerConfigs(String loggerName, LockableUnnamedLogger config) {
		synchronized (namedLoggerConfigs) {	
			LockableUnnamedLogger oldConfig = namedLoggerConfigs.get(loggerName);
			if (oldConfig == null || !oldConfig.isLocked()) {
				namedLoggerConfigs.put(loggerName, config);
			} 
			else {
				// todo: perhaps we should log the attempt, but should apply some repeat guard
				// because clearing the log configs would produce a lot of messages in a row.
			}
		}
	}
	
	private static class LockableUnnamedLogger extends UnnamedLogger {
		private boolean isLocked = false;
		
		LockableUnnamedLogger() {
			init(null);
		}		
		LockableUnnamedLogger(UnnamedLogger config) {
			init(config);
		}		
		LockableUnnamedLogger(LockableUnnamedLogger config) {
			init(config);
			isLocked = config.isLocked;
		}
		private void init(UnnamedLogger config) {
			unlock();
			if (config != null) {
				setMinLogLevel(config.getMinLogLevel());
				setMinLogLevelLocal(config.getMinLogLevelLocal());
			}
		}
		void lock() {
			isLocked = true;
		}
		void unlock() {
			isLocked = false;
		}
		boolean isLocked() {
			return isLocked;
		}
	}
	
    
    // ///////////////////////////////////////////////////////////////////
    // Propagation of configuration updates to various logging classes
    /////////////////////////////////////////////////////////////////////
    
	public void addSubscriber(LogConfigSubscriber subscriber) {
        if (!subscriberList.contains(subscriber)) {
            subscriberList.add(subscriber);
        }
	}
	
	void notifySubscribers() {
		for (Iterator<LogConfigSubscriber> iter = subscriberList.iterator(); iter.hasNext();) {
			LogConfigSubscriber subscriber = iter.next();
			subscriber.configureLogging(this);
		}
	}

    public void removeSubscriber(LogConfigSubscriber subscriber) {
        subscriberList.remove(subscriber);
    }

    // ///////////////////////////////////////////////////////////////////
	// Logging done by this class
	/////////////////////////////////////////////////////////////////////
	/**
	 * Sets the Logger to be used by this class and dependent classes for internal tracing.
	 * <p>
	 * Note that in the current design of ClientLogManager and LogConfig, 
	 * the Logger can not be provided already in the constructor, 
	 * because the Logger first must be configured, which in turn requires a LogConfig instance.
	 * That's why we have this setter method.
	 */
	public void setInternalLogger(Logger logger) {
		this.logger = logger;
	}

	/**
	 * Logs to the Logger given in {@link #setInternalLogger(Logger)}, or to System.out if no Logger has been provided.
	 */
	protected void log(Level level, String msg, Throwable thr) {
		if (logger != null) {
			logger.log(level, msg, thr);
		} 
		else {
			if (level.intValue() >= getDefaultMinLogLevelLocal()) {
				System.out.println(level.toString() + ": " + msg + (thr != null ? thr.toString() : ""));
			}
		}
	}

}
