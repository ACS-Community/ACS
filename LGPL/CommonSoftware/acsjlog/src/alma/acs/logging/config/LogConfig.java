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

import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.exolab.castor.core.exceptions.CastorException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import alma.cdbErrType.CDBRecordDoesNotExistEx;
import alma.cdbErrType.CDBXMLErrorEx;
import alma.cdbErrType.wrappers.AcsJCDBRecordDoesNotExistEx;
import alma.cdbErrType.wrappers.AcsJCDBXMLErrorEx;
import alma.maci.loggingconfig.LoggingConfig;
import alma.maci.loggingconfig.NamedLogger;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALOperations;

/**
 * Class that encapsulates all configuration sources (defaults, properties, CDB) for Java logging,
 * and supports runtime updates of logging configuration based on these sources.
 * 
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
     * Path in the CDB to the logging configuration.
     */
    private String cdbLoggingConfigPath;

    /** 
     * key = component logger name, value = path in the CDB to the Component configuration 
     */
    private Map<String, String> cdbComponentPaths;
    
    private LoggingConfig loggingConfig;

	/** 
     * key = component logger name, value = NamedLogger log levels 
     */
    private Map<String, NamedLogger> namedLoggers;
    
    /**
     * Subscribers get notified of logging config changes
     */
    private List<LogConfigSubscriber> subscriberList;


	
	public LogConfig() {
        cdbComponentPaths = new HashMap<String, String>();
        namedLoggers = new HashMap<String, NamedLogger>();
		subscriberList = new ArrayList<LogConfigSubscriber>();
		loggingConfig = new LoggingConfig(); // comes with schema default values
		
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
     * Before this method is called, default values are used instead of CDB values.
     * <p>
     * Note that the reference is only set, but the CDB is not read automatically;
     * for this, call {@link #initialize()}.
     * @param cdb a reference to the CDB. Will be ignored if == null.
     */
    public void setCDB(DALOperations cdb) {
        if (cdb != null) {
            this.cdb = cdb;
        }
        else {
        	log(Level.FINE, "Ignoring call to setCDB(null)", null);
        }
    }
    
    /**
     * Sets the path to the CDB's node that is parent of the &lt;LoggingConfig&gt; node.
     * @param path
     */
    public void setCDBLoggingConfigPath(String path) {
    	cdbLoggingConfigPath = path;
    }
    
    public void setCDBComponentPath(String compLoggerName, String path) {
        cdbComponentPaths.put(compLoggerName, path);
    }
    
    
	/**
	 * Initializes the values based on CDB settings, logging properties, etc.
     * All subscribing classes are notified of the new configuration,
     * see {@link LogConfigSubscriber#configureLogging(LogConfig)}.
     * <p>
	 * This method can be called more than once: if some settings have changed,
     * should be read in, and the logging classes should be notified of these changes.
     * For example, the container could call this method when it gets notified 
     * that the logging configuration in the CDB has been changed at runtime.
     * 
     * @throws LogConfigException if reading the configuration data failed and thus default values were used, 
     *                            or if there were problems during configuration even though some 
     *                            of the configuring went ok (best-effort approach).
	 */
	public void initialize() throws LogConfigException {
        StringBuffer errMsg = new StringBuffer();
        
       	LoggingConfig newLoggingConfig = null;
        if (cdb != null) {
        	try {
				if (cdbLoggingConfigPath != null) {
					String containerConfigXML = cdb.get_DAO(cdbLoggingConfigPath);

					DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
					DocumentBuilder builder = factory.newDocumentBuilder();
					Document doc = builder.parse(new InputSource(new StringReader(containerConfigXML)));

					Element documentElement = doc.getDocumentElement();
					NodeList loggingConfigElements = documentElement.getElementsByTagName("LoggingConfig");
					if (loggingConfigElements.getLength() != 1)
						throw new LogConfigException("Parent node does not contain one LoggingConfig element.");

					Node loggingConfigElement = loggingConfigElements.item(0);

					Document newDoc = builder.newDocument();
					loggingConfigElement = newDoc.importNode(loggingConfigElement, true);
					newDoc.appendChild(loggingConfigElement);

					TransformerFactory transformerFactory = TransformerFactory.newInstance();
					Transformer transformer = transformerFactory.newTransformer();
					StringWriter sw = new StringWriter();
					StreamResult result = new StreamResult(sw);
					transformer.transform(new DOMSource(newDoc), result);

					newLoggingConfig = LoggingConfig.unmarshalLoggingConfig(new StringReader(sw.toString()));
				}
                else {
                	errMsg.append("CDB reference was set, but not the path to the logging configuration. ");
                }
               	if (newLoggingConfig != null) {
               		loggingConfig = newLoggingConfig;
               	}
               	else {
               		throw new LogConfigException("LoggingConfig binding class obtained from CDB node '" + cdbLoggingConfigPath + "' was null.");
               	}
        	} catch (CDBXMLErrorEx ex) { 
        		errMsg.append("Failed to read node " + cdbLoggingConfigPath + " from the CDB (msg='" + ex.errorTrace.shortDescription + "'). ");
        	} catch (CDBRecordDoesNotExistEx ex) { 
        		errMsg.append("Node " + cdbLoggingConfigPath + " does not exist in the CDB (msg='" + ex.errorTrace.shortDescription + "'). ");
        	} catch (CastorException ex) {
        		errMsg.append("Failed to parse XML for CDB node " + cdbLoggingConfigPath + " into binding classes (ex=" + ex.getClass().getName() + ", msg='" + ex.getMessage() + "'). ");
        	} catch (Throwable thr) { 
        		errMsg.append("Failed to read node " + cdbLoggingConfigPath + " from the CDB (ex=" + thr.getClass().getName() + ", msg='" + thr.getMessage() + "'). ");
        	}
        }
            
        namedLoggers.clear();
        // parse component logger configs and thus populate 'namedLoggers' map
        for (String loggerName : cdbComponentPaths.keySet()) {                    
        	try {
        		getSpecialLoggerConfig(loggerName);
        	} catch (LogConfigException ex) {
        		errMsg.append(ex.getMessage());
        	} catch (Throwable thr) { // CDBXMLErrorEx, CDBRecordDoesNotExistEx, etc
        		errMsg.append("Failed to read config for logger '" + loggerName + "' from the CDB (msg='" + thr.getMessage() + "'). ");
        	}
        }

        notifySubscribers();
        
        // now that the subscribers had a chance to adjust their log levels according to the changes from the CDB (if present), we can publish a trace log
    	if (newLoggingConfig != null) {
    		StringWriter writer = new StringWriter(); 
    		String newXML = null;
    		try {
    			newLoggingConfig.marshal(writer);
    			newXML = writer.toString();
    		}
    		catch (Throwable thr) {
    			; //nothing
    		}
    		log(Level.FINER, "Updated default logging configuration based on CDB entry " + newXML, null);
    		
    		// @TODO: also log something for named loggers once supported
    	}
    	else {
    		log(Level.FINER, "Logging configuration has been initialized, but not from CDB settings.", null);
    	}
        
        if (errMsg.length() > 0) {
            throw new LogConfigException("Log config initialization at least partially failed. " + errMsg.toString());
        }        
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

    /**
     * Gets the specialized logging configuration data for a given logger, 
     * which at the moment contains log levels.
     * Resorts to the default configuration if a specialized configuration is not available 
     * (in that case, <code>NamedLogger.getName()</code> returns <code>null</code>).
     * <p> 
     * Note that a copy of the config data is returned, so changes to it 
     * will not affect any other object's configuration.
     */
    public NamedLogger getSpecialLoggerConfig(String loggerName) throws LogConfigException, AcsJCDBXMLErrorEx, AcsJCDBRecordDoesNotExistEx {

    	NamedLogger defaultNamedLogger = new NamedLogger();    	
    	defaultNamedLogger.setMinLogLevel(loggingConfig.getMinLogLevel());
    	defaultNamedLogger.setMinLogLevelLocal(loggingConfig.getMinLogLevelLocal());

    	if (loggerName == null) {
            return defaultNamedLogger;
        }
    	    	
        NamedLogger ret = namedLoggers.get(loggerName);
        if (ret == null) {
            // in any case we can use the default config
            ret = defaultNamedLogger;
            // check if we have a CDB path for this particular logger which should modify the default settings
            String cdbPath = cdbComponentPaths.get(loggerName);
            if (cdbPath != null) {
                // CDB should have specialized config for this logger -- as of ACS 6.0, this should never happen            	
                try {
					String componentConfigXML = cdb.get_DAO(cdbPath);
				} catch (CDBXMLErrorEx ex) {
					throw AcsJCDBXMLErrorEx.fromCDBXMLErrorEx(ex);
				} catch (CDBRecordDoesNotExistEx ex) {
					throw AcsJCDBRecordDoesNotExistEx.fromCDBRecordDoesNotExistEx(ex);
				}
                // @TODO: try to instantiate castor class for types Components, Component, CharacteristicComponent or whatever other type might be used,
                //        and get its NamedLogger element instead of throwing this exception
                throw new LogConfigException("Named logger configuration not yet supported. This code should never have been called for logger " + loggerName);
            }
        }
        return ret;
    }
    
    
    /**
     * Sets the logger configuration for a named logger.
     * <p>
     * From outside of this class, this method should be used only for tests and temporary hacks, 
     * as the {@link LogConfigData} is supposed to be created on demand in {@link #getLogConfigData(String)} 
     * where it should modify itself based on the CDB readings.
     *  
     * @param logConfigData
     */
    public void setSpecialLoggerConfig(String loggerName, NamedLogger loggerConfig) {
    	namedLoggers.put(loggerName, loggerConfig);
    }
    
    
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
    		System.out.println(level.toString() + ": " + msg + (thr != null ? thr.toString() : ""));
    	}
    }
   
    public void setMinLogLevelLocal(int level){
        loggingConfig.setMinLogLevelLocal(level);
        notifySubscribers();
    }

    public void setMinLogLevel(int level){
        loggingConfig.setMinLogLevel(level);
        notifySubscribers();
    }
 

    public int getMinLogLevelLocal(){
        return loggingConfig.getMinLogLevelLocal();
    }

    public int getMinLogLevel(){
        return loggingConfig.getMinLogLevel();
    }
    
    /////////////////////////////////////////////////////////////////////
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

}
