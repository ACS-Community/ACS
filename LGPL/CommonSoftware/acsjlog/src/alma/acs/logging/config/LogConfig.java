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
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.exolab.castor.core.exceptions.CastorException;
import org.exolab.castor.xml.Unmarshaller;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALOperations;

import si.ijs.maci.LoggingConfigurable;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.util.IsoDateFormat;
import alma.cdbErrType.CDBRecordDoesNotExistEx;
import alma.cdbErrType.CDBXMLErrorEx;
import alma.maci.loggingconfig.LoggingConfig;
import alma.maci.loggingconfig.NamedLogger;
import alma.maci.loggingconfig.UnnamedLogger;
import alma.maci.loggingconfig.types.LogLevel;

/**
 * Class that encapsulates all configuration sources (defaults, properties, CDB) for Java logging,
 * and supports runtime updates of logging configuration based on these sources.
 * This class can be used by a process such as the Java container or the manager, 
 * in order to configure all its loggers (e.g. container logger, component loggers, ORB logger).
 * @author hsommer
 */
public class LogConfig {

	// for the often-used min levels we offer properties, to avoid having an otherwise not needed CDB configuration 
	public final static String PROPERTYNAME_MIN_LOG_LEVEL_LOCAL = "ACS.logstdout"; 
	public final static String PROPERTYNAME_MIN_LOG_LEVEL = "ACS.log.minlevel.remote"; // from env var ACS_LOG_CENTRAL

	/**
	 * Using this property, we can configure named loggers for processes other than containers and manager, 
	 * for which there is no logging configuration in the CDB.
	 * It is not expected to be used widely, which excuses the not so intuitive stuffing of data into a single property.
	 * <p>
	 * Example: <pre>-DACS.log.minlevel.namedloggers="hibernateSQL@CDB-RDB=2,3:hibernate@CDB-RDB=5,5"</pre>
	 * configures two loggers: "hibernateSQL@CDB-RDB" will use local log level 2 and remote level 3, 
	 * while "hibernate@CDB-RDB" will use local and remote level 5.
	 */
	public final static String PROPERTYNAME_NAMED_LOGGER_LEVELS = "ACS.log.minlevel.namedloggers";

	/**
	 * In CDB queries for the <code>LoggingConfig</code> child of a container or manager configuration
	 * we need the "LoggingConfig" string. 
	 * <p>
	 * For type safety we get that name from the schema-generated castor class. 
	 */
	final static String CDBNAME_LoggingConfig = LoggingConfig.class.getSimpleName();
	
	/**
	 * In CDB queries for the <code>ComponentLogger</code> child of a component configuration
	 * we need the "LoggingConfig" string.
	 * <p> 
	 * In the schema <code>Components.xsd</code> we are interested in the grand-child element
	 * <code>ComponentLogger</code>. The generated Java class corresponds to the type
	 * <code>UnnamedLogger</code> though.
	 * The element name is only present in the method name
	 * <code>alma.maci.componentconfig.components.ComponentInfo#getComponentLogger</code>
	 * which cannot be used to define a constant. Therefore we hardcode the name here.
	 */
	final static String CDBNAME_ComponentLogger = "ComponentLogger";
	
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
     * Paths in the CDB to the element that contains component configuration with child elements of type <code>UnnamedLogger</code>.
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
	 * <li>properties {@link #PROPERTYNAME_NAMED_LOGGER_LEVELS_LOCAL} or {@link #PROPERTYNAME_NAMED_LOGGER_LEVELS_REMOTE},
	 * <li>optional named logger children in the process configuration ({@link #loggingConfig}),
	 *     which are of subclass {@link NamedLogger}, or
	 * <li>optional log config children of CDB component configuration, or
	 * <li>dynamically set values that can create or override the above, see {@link #setNamedLoggerConfig(String, UnnamedLogger)}.
	 * </ul>
	 * 
	 * key = [String] logger name, value = [LockableUnnamedLogger or null] level config
	 */
	private final Map<String, LockableUnnamedLogger> namedLoggerConfigs;

	/**
	 * Subscribers get notified of logging config changes
	 */
	private final List<LogConfigSubscriber> subscriberList;


	
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
			try {
				loggingConfig.setMinLogLevelLocal(LogLevel.valueOf(minLevelLocalValue.toString()));
			} catch (IllegalArgumentException ex) {
				log(Level.INFO, "failed to pick up default stdout log level from property " + PROPERTYNAME_MIN_LOG_LEVEL_LOCAL, ex);
			}
		}
		Integer minLevelValue = Integer.getInteger(PROPERTYNAME_MIN_LOG_LEVEL);
		if (minLevelValue != null) {
			try {
				loggingConfig.setMinLogLevel(LogLevel.valueOf(minLevelValue.toString()));
			} catch (IllegalArgumentException ex) {
				log(Level.INFO, "failed to pick up default remote log level from property " + PROPERTYNAME_MIN_LOG_LEVEL, ex);
			}
		}
	}

	/**
	 * Reads the property <code>ACS.log.minlevel.namedloggers</code> (name defined as {@link #PROPERTYNAME_NAMED_LOGGER_LEVELS}) 
	 * and, if the property is defined, sets the respective named logger levels. Prior values are lost.
	 */
	private void configureNamedLoggerLevelsFromProperties() {
		String propVal = System.getProperty(PROPERTYNAME_NAMED_LOGGER_LEVELS);
		if (propVal != null) {
			try {
				for (String levelDef : propVal.split(":")) {
					String[] levelDefSplit = levelDef.split("=");
					try {
						String loggerName = levelDefSplit[0].trim();
						String[] levels = levelDefSplit[1].split(",");
						UnnamedLogger loggerConfig = new UnnamedLogger();
						loggerConfig.setMinLogLevelLocal(AcsLogLevelDefinition.xsdLevelFromInteger(Integer.parseInt(levels[0])));
						loggerConfig.setMinLogLevel(AcsLogLevelDefinition.xsdLevelFromInteger(Integer.parseInt(levels[1])));
						storeNamedLoggerConfig(loggerName, new LockableUnnamedLogger(loggerConfig));
						log(Level.INFO, "Set named logger levels from property. Name=" + loggerName + 
								" local=" + loggerConfig.getMinLogLevelLocal() + " remote=" + loggerConfig.getMinLogLevel(), null);
					} catch (Exception ex) {
						log(Level.WARNING, "Failed to process named logger level definition '" + levelDef + 
								"' given in property '" + PROPERTYNAME_NAMED_LOGGER_LEVELS + "'. ", ex);
					}
				}
			} catch (Exception ex) {
				log(Level.WARNING, "Failed to process named loggers from property " + PROPERTYNAME_NAMED_LOGGER_LEVELS, ex);
			}
		}
		else {
			log(Level.FINEST, PROPERTYNAME_NAMED_LOGGER_LEVELS + " not defined.", null);
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
	 * @param compLoggerName  the component logger name, e.g. <code>MOUNT1</code>.
	 * @param path  e.g. <code>MACI/Components</code>.
	 */
	public void setCDBComponentPath(String compLoggerName, String path) {
		cdbComponentPaths.put(compLoggerName, path);
	}


	/**
	 * Initializes the values based on CDB settings, logging properties, etc. All subscribing classes are notified of
	 * the new configuration, see {@link LogConfigSubscriber#configureLogging(LogConfig)}.
	 * <p>
	 * This method can be called more than once: if some settings have changed, should be read in, and the logging
	 * classes should be notified of these changes. For example, the container could call this method when it gets
	 * notified that the logging configuration in the CDB has been changed at runtime.
	 * 
	 * @param cdbBeatsProperties
	 *            if true then the default logger level values from the CDB override the properties (env vars).
	 *            <code>True</code> is foreseen for dynamic updates from the CDB, whereas for the initial
	 *            configuration it should be a <code>false</code>.
	 * 
	 * @throws LogConfigException
	 *            if reading the configuration data failed and thus default values were used, or if there were problems
	 *            during configuration even though some of the configuring went ok (best-effort approach).
	 */
	public void initialize(boolean cdbBeatsProperties) throws LogConfigException {
		StringBuffer errMsg = new StringBuffer();

		LoggingConfig newLoggingConfig = null; // schema binding class generated from LogggingConfig.xsd
		if (cdb != null) {
			try {
				if (cdbLoggingConfigPath != null) {
					String loggingConfigXml = getLogConfigXml(cdbLoggingConfigPath, "//" + CDBNAME_LoggingConfig);
					if (loggingConfigXml == null || loggingConfigXml.trim().isEmpty()) {
						// the LoggingConfig child is mandatory for containers and manager
						throw new LogConfigException("Node " + cdbLoggingConfigPath + " does not contain one LoggingConfig element.");
					}
					try {
						newLoggingConfig = LoggingConfig.unmarshalLoggingConfig(new StringReader(loggingConfigXml));
					} catch (Throwable thr) {
						log(Level.FINE, "Failed to unmarshal logging config xml '" + loggingConfigXml + "'.", thr);
						throw thr;
					}
				}
				else {
					errMsg.append("CDB reference was set, but not the path to the logging configuration. ");
				}
				if (newLoggingConfig != null) {
					loggingConfig = newLoggingConfig;
					
					// named logger configs under LoggingConfig we process right away, while other separate configs (those from component configurations) we do later.
					
					// @TODO: check if we really want to lose named logger settings that had been added dynamically before this refresh from CDB.
					// If not, then we must distinguish between dynamic API and from-CDB config, and leave those objects that have no CDB-equivalent.

					synchronized (namedLoggerConfigs) {
						// We don't call namedLoggerConfigs.clear() because we don't want to lose logger names 
						// but only null their configurations.
						for (String loggerName : namedLoggerConfigs.keySet()) {
							storeNamedLoggerConfig(loggerName, null);
						}
						
						// named logger levels from children of the <LoggingConfig/> 
						NamedLogger[] namedLoggers = loggingConfig.get();
						for (int i = 0; i < namedLoggers.length; i++) {
							storeNamedLoggerConfig(namedLoggers[i].getName(), new LockableUnnamedLogger(namedLoggers[i]));
						}
						
						// Named logger levels from separate component config:
						// check CDB config for all component loggers who got a CDB path configured
						for (String loggerName : cdbComponentPaths.keySet()) {
							// skip named logger if it's been already configured from the main XML, since those values have precedence
							if (!namedLoggerConfigs.containsKey(loggerName)) {
								String cdbPath = cdbComponentPaths.get(loggerName);
								String xpath = "//_[@Name='" + loggerName + "']/" + CDBNAME_ComponentLogger;
								String componentConfigXML = getLogConfigXml(cdbPath, xpath);
								// the ComponentLogger xml child element is optional, we get a null if it's missing.
								if (componentConfigXML != null) {
									UnnamedLogger compLoggerConfig;
									try {
										compLoggerConfig = UnnamedLogger.unmarshalUnnamedLogger(new StringReader(componentConfigXML));
									} catch (Throwable thr) {
										log(Level.FINE, "Failed to unmarshal component config xml '" + componentConfigXML + "'.", thr);
										throw thr;
									}
									storeNamedLoggerConfig(loggerName, new LockableUnnamedLogger(compLoggerConfig));
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

		// consider the env var based properties only if the CDB was not considered or if the CDB settings should not override the env vars 
		if (cdb == null || !cdbBeatsProperties) {
			configureDefaultLevelsFromProperties();
			configureNamedLoggerLevelsFromProperties();
		}

		notifySubscribers();

		// now that the subscribers had a chance to adjust their log levels according to the changes from the CDB (if
		// present), we can publish a trace log
		if (newLoggingConfig != null) {
			StringWriter writer = new StringWriter();
			String newXML = null;
			try {
				newLoggingConfig.marshal(writer);
				newXML = writer.toString().trim();
			} catch (Throwable thr) {
				; // nothing
			}
			String msg = "Updated logging configuration based on CDB entry " + newXML;
			msg += " with " + (cdbBeatsProperties ? "CDB" : "env vars" ) + " having precedence over " + (cdbBeatsProperties ? "env vars" : "CDB" ); 
			log(Level.FINER, msg, null);			
			// @TODO: also log something for named component loggers if any were considered 
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
	 * and extracts the child element that must be uniquely identified 
	 * by the XPath expression in <code>xpathLogConfigNode</code>.
	 * <p>
	 * TODO: move the Node-to-XML-String code to a general utility package
	 * and return just the Node -- any client can then easily turn it into a String if needed.
	 * For Castor-parsing, a node is fine, see {@link Unmarshaller#unmarshal(Node)}. 
	 * 
	 * @param cdbPathParent  path to container, manager, or component config node
	 * @param xpathExpression  For example, for containers and managers <code>//LoggingConfig</code>, 
	 *                         for multiple-components-xml <code>//_[@Name='myCompName']/ComponentLogger</code>
	 * @return XML String for the requested child, or <code>null</code> if no unique xml child element was found.
	 * @throws CDBRecordDoesNotExistEx 
	 * @throws CDBXMLErrorEx 
	 * @throws ParserConfigurationException 
	 * @throws IOException 
	 * @throws SAXException 
	 * @throws XPathExpressionException 
	 * @throws TransformerException 
	 * @throws IllegalStateException if the cdb ref has not been set
	 */
	String getLogConfigXml(String cdbPathParent, String xpathLogConfigNode) 
		throws CDBXMLErrorEx, CDBRecordDoesNotExistEx, ParserConfigurationException, SAXException, IOException, XPathExpressionException, TransformerException {
		if (cdb == null) {
			throw new IllegalStateException("CDB reference has not been set.");
		}
		String parentConfigXML = cdb.get_DAO(cdbPathParent);

		Node loggingConfigElement = null;
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(false);
		DocumentBuilder builder = factory.newDocumentBuilder();

		// parse the XML file into a DOM
		Document parentDoc;
		try {
			parentDoc = builder.parse(new InputSource(new StringReader(parentConfigXML)));
		} catch (SAXException ex) {
			String msg = "Failed to parse the following XML retrieved from CDB#get_DAO(" + cdbPathParent + "):\n" + parentConfigXML;
			log(Level.FINE, msg, ex);
			throw ex;
		}
		String encoding = parentDoc.getXmlEncoding();
		Element rootElement = parentDoc.getDocumentElement();
		
		// eval xpath expression
		XPath xpath = XPathFactory.newInstance().newXPath();
		Object xpathResult = xpath.evaluate(xpathLogConfigNode, rootElement, XPathConstants.NODE);
		
// debug output, remove later
//		try {
//			log(Level.FINER, "XML for logging config parent node " + cdbPathParent + " and xpath result:\n" + 
//					parentConfigXML + "\n" + 
//					xpathResult, 
//				null);
//		} catch (Throwable thr) {
//		}
// end debug output
		if (xpathResult == null || !(xpathResult instanceof Node)) {
			return null;
		}
		
		loggingConfigElement = (Node) xpathResult;
		
		Document childDoc = builder.newDocument();
		loggingConfigElement = childDoc.importNode(loggingConfigElement, true);
		childDoc.appendChild(loggingConfigElement);

		TransformerFactory transformerFactory = TransformerFactory.newInstance();
		Transformer transformer = transformerFactory.newTransformer();
		transformer.setOutputProperty(OutputKeys.ENCODING, encoding);
		StringWriter sw = new StringWriter();
		StreamResult result = new StreamResult(sw);
		transformer.transform(new DOMSource(childDoc), result);

		return sw.toString();
	}
	
	
	public String getCentralizedLogger() {
		return loggingConfig.getCentralizedLogger();
	}
	
	public int getDispatchPacketSize() {
		return loggingConfig.getDispatchPacketSize();
	}

	public AcsLogLevelDefinition getImmediateDispatchLevel() {
		return convertLegalLogLevel(loggingConfig.getImmediateDispatchLevel());
	}

	public int getFlushPeriodSeconds() {
		return loggingConfig.getFlushPeriodSeconds();
	}
	
	public int getMaxLogQueueSize() {
		return loggingConfig.getMaxLogQueueSize();
	}
	
	public int getMaxLogsPerSecond() {
		return loggingConfig.getMaxLogsPerSecond();
	}
	
	/**
	 * Limits the total number of logs emitted by the whole process,
	 * separately for local and remote logging.
	 * <p>
	 * Introduced with ACS 9.0 for only reading the log throttle configuration data,
	 * we add this setter method in preparation for future exposure in the LoggingConfigurable interface,
	 * and also for test setups.
	 */
	public void setMaxLogsPerSecond(int maxLogsPerSecond) {
		loggingConfig.setMaxLogsPerSecond(maxLogsPerSecond);
		notifySubscribers();
	}
	
	///////////////////////////////////////////////////////////////////////////////////////////////////
	// Getter and setter methods for default logger and named logger levels, local and remote logging.
	///////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Helper method that converts an integer log level to the matching enum literal.
	 * It suppresses the AcsJIllegalArgumentEx because the level must have been validated during the config init 
	 * (and we wouldn't be bothered about exceptions here if we had stored the converted enum literal instead of the castor class).
	 * Therefore a lame log and runtime ex are thrown just in case, but no AcsJIllegalArgumentEx gets thrown on.
	 */
	private AcsLogLevelDefinition convertLegalLogLevel(LogLevel legalLogLevel) {
		try {
			return AcsLogLevelDefinition.fromXsdLogLevel(legalLogLevel);
		} catch (AcsJIllegalArgumentEx ex) {
			log(Level.WARNING, "Failed to convert to AcsLogLevelDefinition the level integer " + legalLogLevel, ex);
			throw new RuntimeException(ex);
		}
	}
	
	/**
	 * Gets the log level for stdout printing for default loggers 
	 * (those that don't have custom levels set).
	 */
	public AcsLogLevelDefinition getDefaultMinLogLevelLocal() {
		return convertLegalLogLevel(loggingConfig.getMinLogLevelLocal());
	}

	/**
	 * Sets the given log level for stdout printing of log records
	 * for all loggers that don't have a custom configuration. 
	 * All log config listeners.get notified of this change.
	 * The call is ignored for negative values of <code>newLevel</code>.
	 * @param newLevel
	 */
	public void setDefaultMinLogLevelLocal(AcsLogLevelDefinition newLevel) {
		if (newLevel.value >= 0) {
			loggingConfig.setMinLogLevelLocal(newLevel.toXsdLevel());
			notifySubscribers();
		}
	}

	/**
	 * Gets the log level for centralized logging for default loggers 
	 * (those that don't have custom levels set).
	 */
	public AcsLogLevelDefinition getDefaultMinLogLevel() {
		return convertLegalLogLevel(loggingConfig.getMinLogLevel());
	}

	/**
	 * Sets the given log level for centralized logging of log records
	 * for all loggers that don't have a custom configuration. 
	 * All log config listeners.get notified of this change.
	 * The call is ignored for negative values of <code>newLevel</code>.
	 * @param newLevel
	 */
	public void setDefaultMinLogLevel(AcsLogLevelDefinition newLevel) {
		if (newLevel.value >= 0) {
			loggingConfig.setMinLogLevel(newLevel.toXsdLevel());
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

	/**
	 * Checks if the given logger name is known, either for default or custom config.
	 * <p>
	 * The current {@link LoggingConfigurable} interface semantics don't allow 
	 * configuration of a logger that does not yet exist. Therefore this method can be used
	 * to check first before calling {@link #getNamedLoggerConfig(String)} 
	 * or {@link #setNamedLoggerConfig(String, alma.acs.logging.config.LogConfig.LockableUnnamedLogger)},
	 * which would automatically add a previously unknown logger.
	 * 
	 * @param loggerName
	 * @since ACS 7.0.2
	 */
	public boolean isKnownLogger(String loggerName) {
		synchronized (namedLoggerConfigs) {
			return namedLoggerConfigs.containsKey(loggerName);
		}
	}
	
	
	/**
	 * Checks if there is a level configuration for this particular logger.
	 * @param loggerName  
	 * @return  false if the logger is not known or uses the default configuration
	 */
	public boolean hasCustomConfig(String loggerName) {
		synchronized (namedLoggerConfigs) {
			return (namedLoggerConfigs.get(loggerName) != null);
		}
	}

	
	
	/**
	 * Gets the (log level) configuration data for a named logger.
	 * Resorts to the default configuration if a specialized configuration is not available. 
	 * <p>
	 * Note that a copy of the config data is returned, so changes to it will
	 * not affect any other object's configuration.
	 * <p>
	 * A previously unknown logger gets registered by its name. 
	 * If this is not intended, check first with {@link #isKnownLogger(String)}.
	 */
	public LockableUnnamedLogger getNamedLoggerConfig(String loggerName) {
		
		LockableUnnamedLogger ret = new LockableUnnamedLogger();
		
		synchronized (namedLoggerConfigs) {
			if (loggerName == null || loggerName.toLowerCase().equals("default") || namedLoggerConfigs.get(loggerName)==null) {
				ret.setMinLogLevel(loggingConfig.getMinLogLevel());
				ret.setMinLogLevelLocal(loggingConfig.getMinLogLevelLocal());
				if (loggerName != null && !loggerName.toLowerCase().equals("default")) {
					// register new logger, with a null config which means "apply default values"
					storeNamedLoggerConfig(loggerName, null);
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
	 * <p>
	 * If <code>loggerName</code> is previously unknown, then this logger is added automatically 
	 * to the list of known loggers. If this is not intended, check first using {@link #isKnownLogger(String)}.
	 * 
	 * @throws AcsJIllegalArgumentEx if the log level integers inside <code>config</code> are illegal.
	 */
	public void setNamedLoggerConfig(String loggerName, LockableUnnamedLogger config) throws AcsJIllegalArgumentEx {
		if (loggerName != null && config != null) {
			AcsLogLevelDefinition.fromXsdLogLevel(config.getMinLogLevel());
			AcsLogLevelDefinition.fromXsdLogLevel(config.getMinLogLevelLocal());
			LockableUnnamedLogger config2 = new LockableUnnamedLogger(config);
			storeNamedLoggerConfig(loggerName, config2);
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
	public void setNamedLoggerConfig(String loggerName, UnnamedLogger config) throws AcsJIllegalArgumentEx {
		if (loggerName != null && config != null) {
			LockableUnnamedLogger config2 = new LockableUnnamedLogger(config); // unlocked by default
			setNamedLoggerConfig(loggerName, config2);
		}
	}
	
	
	/**
	 * Clears log level settings for the given named logger,
	 * so that it uses default log levels. 
	 * Notifies all listeners.
	 * Ignores this call if <code>loggerName</code> is <code>null</code>.
	 */
	public void clearNamedLoggerConfig(String loggerName) {
		if (loggerName != null) {
			storeNamedLoggerConfig(loggerName, null);
			notifySubscribers();
		}
	}

	public void setMinLogLevelLocal(AcsLogLevelDefinition newLevel, String loggerName) {
		LockableUnnamedLogger config = getNamedLoggerConfig(loggerName);  // new object, with cached or default values
		config.setMinLogLevelLocal(newLevel.toXsdLevel());
		try {
			setNamedLoggerConfig(loggerName, config);
		} catch (AcsJIllegalArgumentEx ex) {
			// cannot happen because the level integer comes from a valid AcsLogLevelDefinition
		}
	}

	public void setMinLogLevel(AcsLogLevelDefinition newLevel, String loggerName) {
		LockableUnnamedLogger config = getNamedLoggerConfig(loggerName); // new object, with cached or default values
		config.setMinLogLevel(newLevel.toXsdLevel());
		try {
			setNamedLoggerConfig(loggerName, config);
		} catch (AcsJIllegalArgumentEx ex) {
			// cannot happen because the level integer comes from a valid AcsLogLevelDefinition
		}
	}

	/**
	 * Locking a remote log level is currently needed only for the container to ensure that the ORB logger of the container in which 
	 * the archive logger component runs is guaranteed to not produce any logs 
	 * (which would lead to log record explosion through positive feedback). 
	 * <p>
	 * The more abstract concept of locking log levels was chosen to keep the special scenario described above 
	 * out of the logging config code.
	 * 
	 * @param newLevel  small integer log level (IDL value wrapped by a Java enum)
	 * @param loggerName
	 */
	public void setAndLockMinLogLevel(AcsLogLevelDefinition newLevel, String loggerName) {
		synchronized (namedLoggerConfigs) {	
			LockableUnnamedLogger config = getNamedLoggerConfig(loggerName); // new object, with cached or default values
			if (config.isLockedRemote()) {
				// only if the level is different we log the warning. This removes the need to check the lock status and level before calling this method.
				if (!newLevel.isEqualXsdLevel(config.getMinLogLevel())) {
					log(Level.WARNING, "Ignoring attempt to lock logger " + loggerName + " to level " + newLevel + " because it is already locked to remote level " + config.getMinLogLevel(), null);
				}
			}
			else if (!newLevel.isEqualXsdLevel(config.getMinLogLevel())) {
				config.setMinLogLevel(newLevel.toXsdLevel());
				config.lockRemote();
				try {
					setNamedLoggerConfig(loggerName, config);
				} catch (AcsJIllegalArgumentEx ex) {
					// cannot happen because the level integer comes from a valid AcsLogLevelDefinition 
				}
				log(Level.INFO, "Locked logger " + loggerName + " to remote level " + newLevel.value, null);
			}
		}
	}

	/**
	 * Method that guards <code>namedLoggerConfigs.put(loggerName, config)</code>
	 * and denies access if the old config is locked.
	 * @param loggerName
	 *             logger name, must not be null
	 * @param config
	 *             new level values, or <code>null</code> to "link" to default log levels.
	 * @see #namedLoggerConfigs
	 */
	private void storeNamedLoggerConfig(String loggerName, LockableUnnamedLogger config) {
		if (loggerName == null) {
			throw new IllegalArgumentException("loggerName must not be null");
		}
		synchronized (namedLoggerConfigs) {
			LockableUnnamedLogger oldConfig = namedLoggerConfigs.get(loggerName);
			if (oldConfig == null) {
				namedLoggerConfigs.put(loggerName, config);
			} 
			else if (config == null) {
				// null clears the named logger levels
				if (!oldConfig.isLockedLocal && !oldConfig.isLockedRemote) {
					namedLoggerConfigs.put(loggerName, null);
				}
				else {
					log(Level.INFO, "Ignoring attempt to clear locked logger config for " + loggerName, null);
				}
			}
			else {
				// need to selectively update the values, watching out for locked levels
				if (!oldConfig.isLockedRemote()) {
					oldConfig.setMinLogLevel(config.getMinLogLevel());
				}
				if (!oldConfig.isLockedLocal()) {
					oldConfig.setMinLogLevelLocal(config.getMinLogLevelLocal());
				}
			}
		}
	}
	
	/**
	 * Renaming of a logger can occur for example when an ORB logger undergoes a name change
	 * as soon as the process name is determined by the container logger.
	 * <p>
	 * Renaming is not a regular part of a logger's life though. By the time that tools can look at the list of loggers,
	 * no renaming should occur any more.
	 * 
	 * @param oldLoggerName
	 * @param newLoggerName
	 * @return true if the logger config was renamed. False for bad or unmatching parameters which result in no change. 
	 */
	public boolean renameNamedLoggerConfig(String oldLoggerName, String newLoggerName) {
		synchronized (namedLoggerConfigs) {	
			if (oldLoggerName == null || newLoggerName == null || !namedLoggerConfigs.containsKey(oldLoggerName)) {
				return false;
			}
			else {
				LockableUnnamedLogger config = namedLoggerConfigs.get(oldLoggerName);
				namedLoggerConfigs.put(newLoggerName, config);
				namedLoggerConfigs.remove(oldLoggerName);
				return true;
			}
		}
	}
	
	public static class LockableUnnamedLogger extends UnnamedLogger {
		private boolean isLockedRemote = false;
		private boolean isLockedLocal = false;
		
		LockableUnnamedLogger() {
			init(null);
		}		
		LockableUnnamedLogger(UnnamedLogger config) {
			init(config);
		}		
		LockableUnnamedLogger(LockableUnnamedLogger config) {
			init(config);
			isLockedRemote = config.isLockedRemote;
			isLockedLocal = config.isLockedLocal;
		}
		private void init(UnnamedLogger config) {
			unlockRemote();
			if (config != null) {
				setMinLogLevel(config.getMinLogLevel());
				setMinLogLevelLocal(config.getMinLogLevelLocal());
			}
		}
		void lockRemote() {
			isLockedRemote = true;
		}
		void lockLocal() {
			isLockedLocal = true;
		}
		void unlockRemote() {
			isLockedRemote = false;
		}
		void unlockLocal() {
			isLockedLocal = false;
		}
		boolean isLockedRemote() {
			return isLockedRemote;
		}
		boolean isLockedLocal() {
			return isLockedLocal;
		}
	}
	

    // ///////////////////////////////////////////////////////////////////
    // Propagation of configuration updates to various logging classes
    /////////////////////////////////////////////////////////////////////
    
	public void addSubscriber(LogConfigSubscriber subscriber) {
		synchronized (subscriberList) {
	        if (!subscriberList.contains(subscriber)) {
	            subscriberList.add(subscriber);
	        }
		}
	}
	
	void notifySubscribers() {
		synchronized (subscriberList) {
			for (LogConfigSubscriber subscriber : subscriberList) {
				subscriber.configureLogging(this);
			}
		}
	}

    public void removeSubscriber(LogConfigSubscriber subscriber) {
		synchronized (subscriberList) {
			subscriberList.remove(subscriber);
		}
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
			if (AcsLogLevel.getNativeLevel(level).getAcsLevel().compareTo(getDefaultMinLogLevelLocal()) >= 0) {
				System.out.println(IsoDateFormat.formatCurrentDate()+ " "+ AcsLogLevel.getNativeLevel(level).getAcsLevel().toString() + " [alma.acs.logging.config.LogConfig] " + msg + (thr != null ? thr.toString() : ""));
			}
		}
	}

}
